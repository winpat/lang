const std = @import("std");
const tst = std.testing;
const Allocator = std.mem.Allocator;
const meta = std.meta;
const Tag = meta.Tag;
const StringHashMap = std.StringHashMapUnmanaged;
const ArrayList = std.ArrayList;

const Compiler = @import("compiler.zig").Compiler;
const Gc = @import("garbage_collector.zig").GarbageCollector;
const object = @import("object.zig");
const Object = object.Object;
const Func = object.Func;
const Symbol = object.Symbol;
const Op = @import("bytecode.zig").Op;
const Parser = @import("parser.zig").Parser;
const Value = @import("value.zig").Value;

const stack_depth_max: usize = 256;

pub const VirtualMachine = struct {
    allocator: Allocator,
    gc: *Gc,
    globals: StringHashMap(Value) = .{},
    frames: ArrayList(CallFrame) = .{},
    stack: Stack = .{},

    pub fn init(allocator: Allocator, gc: *Gc) VirtualMachine {
        return .{
            .allocator = allocator,
            .gc = gc,
        };
    }

    pub fn deinit(self: *VirtualMachine) void {
        var iter = self.globals.keyIterator();
        while (iter.next()) |key| self.allocator.free(key.*);
        self.globals.deinit(self.allocator);
        self.frames.deinit(self.allocator);
    }

    pub fn run(self: *VirtualMachine, callable: *Func) RuntimeError!Value {
        const allocator = self.allocator;
        var stack = &self.stack;
        var frames = &self.frames;
        var globals = &self.globals;

        // Slot 0 of the stack holds the function being called
        try stack.pushAs(.object, &callable.obj);

        try frames.append(allocator, .{ .func = callable, .fp = &stack.items });
        var frame = &frames.items[0];

        while (!frame.atEnd()) {
            switch (frame.readOp()) {
                .load_constant => {
                    const cidx = frame.readByte();
                    try stack.push(frame.getConstant(cidx));
                },
                .load_local => {
                    const sidx = frame.readByte();
                    try stack.push(frame.fp[sidx]);
                },
                .load_global => {
                    const sym = try stack.popObjectAs(.symbol);
                    const val = globals.get(sym.slice()) orelse return RuntimeError.UnboundSymbol;
                    try stack.push(val);
                },
                .load_zero => try stack.push(.{ .number = 0 }),
                .load_one => try stack.push(.{ .number = 1 }),
                .load_nil => try stack.push(.{ .nil = {} }),
                .load_true => try stack.push(.{ .boolean = true }),
                .load_false => try stack.push(.{ .boolean = false }),
                .load_empty_list => try stack.push(.{ .list = .{} }),
                .define_global => {
                    const val = try stack.pop();
                    const sym = try stack.popObjectAs(.symbol);

                    const name = try allocator.dupe(u8, sym.slice());
                    errdefer allocator.free(name);

                    const result = try globals.getOrPut(allocator, name);

                    if (result.found_existing) {
                        allocator.free(result.key_ptr.*);
                        result.key_ptr.* = name;
                    } else {
                        result.key_ptr.* = name;
                    }

                    result.value_ptr.* = val;

                    try stack.pushAs(.nil, {});
                },
                .add => {
                    var sum = try stack.popAs(.number);
                    const argc = frame.readByte();
                    var i: u8 = 1;
                    while (i < argc) : (i += 1)
                        sum += try stack.popAs(.number);

                    try stack.pushAs(.number, sum);
                },
                .sub => {
                    var subtrahend = try stack.popAs(.number);
                    const argc = frame.readByte();
                    var i: u8 = 1;
                    while (i < argc - 1) : (i += 1)
                        subtrahend += try stack.popAs(.number);

                    const minuend = try stack.popAs(.number);
                    try stack.pushAs(.number, minuend - subtrahend);
                },
                .mul => {
                    var product = try stack.popAs(.number);
                    const argc = frame.readByte();
                    var i: u8 = 1;
                    while (i < argc) : (i += 1)
                        product *= try stack.popAs(.number);

                    try stack.pushAs(.number, product);
                },
                .div => {
                    const argc = frame.readByte();
                    if (argc == 1) {
                        const divisor = try stack.popAs(.number);
                        try stack.pushAs(.number, 1 / divisor);
                    } else {
                        var divisor = try stack.popAs(.number);
                        var n: u8 = 1;
                        while (n < argc - 1) : (n += 1)
                            divisor *= try stack.popAs(.number);

                        const divident = try stack.popAs(.number);
                        try stack.pushAs(.number, divident / divisor);
                    }
                },
                .eq => {
                    const val = try stack.pop();
                    const argc = frame.readByte();
                    var i: u8 = 1;
                    const is_equal = while (i < argc) : (i += 1) {
                        if (!val.equal(try stack.pop())) {
                            break false;
                        }
                    } else true;

                    try stack.pushAs(.boolean, is_equal);
                },
                .gt => {
                    const argc = frame.readByte();
                    if (argc == 1) {
                        _ = try stack.pop();
                        try stack.pushAs(.boolean, true);
                    } else {
                        var prev = try stack.popAs(.number);
                        var i: u8 = 1;
                        var is_greater = true;
                        while (i < argc) : (i += 1) {
                            const curr = try stack.popAs(.number);
                            if (curr <= prev) is_greater = false;
                            prev = curr;
                        }

                        try stack.pushAs(.boolean, is_greater);
                    }
                },
                .ge => {
                    const argc = frame.readByte();
                    if (argc == 1) {
                        _ = try stack.pop();
                        try stack.pushAs(.boolean, true);
                    } else {
                        var prev = try stack.popAs(.number);
                        var i: u8 = 1;
                        var is_greater_equal = true;
                        while (i < argc) : (i += 1) {
                            const curr = try stack.popAs(.number);
                            if (curr < prev) is_greater_equal = false;
                            prev = curr;
                        }

                        try stack.pushAs(.boolean, is_greater_equal);
                    }
                },
                .lt => {
                    const argc = frame.readByte();

                    if (argc == 1) {
                        _ = try stack.pop();
                        try stack.pushAs(.boolean, true);
                    } else {
                        var prev = try stack.popAs(.number);
                        var i: u8 = 1;
                        var is_less = true;
                        while (i < argc) : (i += 1) {
                            const curr = try stack.popAs(.number);
                            if (curr >= prev) is_less = false;
                            prev = curr;
                        }

                        try stack.pushAs(.boolean, is_less);
                    }
                },
                .le => {
                    const argc = frame.readByte();
                    if (argc == 1) {
                        _ = try stack.pop();
                        try stack.pushAs(.boolean, true);
                    } else {
                        var prev = try stack.popAs(.number);
                        var i: u8 = 1;
                        var is_less_equal = true;
                        while (i < argc) : (i += 1) {
                            const curr = try stack.popAs(.number);
                            if (curr > prev) is_less_equal = false;
                            prev = curr;
                        }

                        try stack.pushAs(.boolean, is_less_equal);
                    }
                },
                .neg => try stack.pushAs(.number, -(try stack.popAs(.number))),
                .not => {
                    const val = try self.stack.pop();
                    try self.stack.pushAs(.boolean, val.falsy());
                },
                .jmp => frame.jumpBy(frame.readShort()),
                .jmpf => {
                    const offset = frame.readShort();
                    const val = try stack.peek();
                    if (val.falsy()) frame.jumpBy(offset);
                },
                .jmpt => {
                    const offset = frame.readShort();
                    const val = try stack.peek();
                    if (val.truthy()) frame.jumpBy(offset);
                },
                .pop => _ = try stack.pop(),
                .ppop_n => {
                    const tmp = try stack.pop();

                    frame = &frames.items[frames.items.len - 1];

                    var n = frame.readByte();
                    while (n > 0) : (n -= 1)
                        _ = try stack.pop();

                    try stack.push(tmp);
                },
                .call => {
                    const argc = frame.readByte();
                    const callable_idx = stack.pos - argc - 1;

                    const val = stack.items[callable_idx];
                    if (val != .object or val.object.tag != .func)
                        return RuntimeError.NotCallable;

                    const func = val.object.as(Func);
                    if (func.arity != argc) return RuntimeError.UnsupportedArity;

                    try frames.append(
                        allocator,
                        CallFrame{ .func = func, .fp = stack.items[callable_idx..].ptr },
                    );
                    frame = &frames.items[frames.items.len - 1];
                },
                .ret => {
                    const result = try stack.pop();

                    const completed_frame = frames.pop().?;

                    if (frames.items.len == 0) {
                        // Pop the top-level function from slot 0
                        _ = try stack.popObjectAs(.func);
                        return result;
                    }

                    const stack_base_ptr = &stack.items;
                    const frame_ptr = completed_frame.fp;
                    self.stack.pos = frame_ptr - stack_base_ptr;

                    try stack.push(result);

                    frame = &frames.items[frames.items.len - 1];
                },
            }
        }

        unreachable;
    }
};

pub const RuntimeError = error{
    UnboundSymbol,
    NotCallable,
    UnsupportedArity,
} || StackError || Allocator.Error;

const Stack = struct {
    items: [stack_depth_max]Value = undefined,
    pos: usize = 0,

    fn push(self: *Stack, val: Value) StackError!void {
        if (self.pos >= stack_depth_max)
            return RuntimeError.StackOverflow;

        self.items[self.pos] = val;
        self.pos += 1;
    }

    pub fn pushAs(self: *Stack, comptime tag: Tag(Value), val: @FieldType(Value, @tagName(tag))) StackError!void {
        try self.push(@unionInit(Value, @tagName(tag), val));
    }

    fn peek(self: *Stack) StackError!Value {
        if (self.pos == 0) return RuntimeError.StackUnderflow;
        return self.items[self.pos - 1];
    }

    fn pop(self: *Stack) StackError!Value {
        if (self.pos == 0) return RuntimeError.StackUnderflow;
        self.pos -= 1;
        return self.items[self.pos];
    }

    pub fn popAs(self: *Stack, comptime tag: Tag(Value)) StackError!@FieldType(Value, @tagName(tag)) {
        const val = try self.pop();

        if (meta.activeTag(val) != tag)
            return RuntimeError.ValueTypeMismatch;

        return @field(val, @tagName(tag));
    }

    pub fn popObjectAs(self: *Stack, comptime tag: Object.Tag) StackError!*Object.typeByTag(tag) {
        const obj = try self.popAs(.object);
        if (obj.tag != tag) return StackError.ObjectTypeMismatch;
        return obj.as(Object.typeByTag(tag));
    }
};

const StackError = error{
    StackOverflow,
    StackUnderflow,
    ValueTypeMismatch,
    ObjectTypeMismatch,
};

const CallFrame = struct {
    func: *Func,
    fp: [*]Value,
    ip: usize = 0,

    fn readOp(self: *CallFrame) Op {
        return @enumFromInt(self.readByte());
    }

    fn readByte(self: *CallFrame) u8 {
        const byte = self.func.code[self.ip];
        self.ip += 1;
        return byte;
    }

    fn readShort(self: *CallFrame) u16 {
        const low = self.func.code[self.ip];
        self.ip += 1;
        const high = self.func.code[self.ip];
        self.ip += 1;
        return (@as(u16, high) << 8) | low;
    }

    fn jumpBy(self: *CallFrame, offset: u16) void {
        self.ip += offset;
    }

    fn getConstant(self: *CallFrame, idx: u8) Value {
        return self.func.constants[idx];
    }

    fn atEnd(self: *CallFrame) bool {
        return self.ip >= self.func.code.len;
    }
};

fn expectEvalTo(input: []const u8, expected: ?Value) !void {
    const allocator = tst.allocator;

    var parser = Parser.init(allocator, input);
    defer parser.deinit();
    const ast = try parser.parse();

    var gc = Gc.init(tst.allocator);
    defer gc.deinit();

    var compiler = Compiler.init(allocator, &gc);
    const func = try compiler.compile(ast);

    var vm = VirtualMachine.init(allocator, &gc);
    defer vm.deinit();
    const result = try vm.run(func);

    try tst.expectEqualDeep(expected, result);
    try tst.expectEqual(vm.stack.pos, 0);
}

test "Eval constant" {
    try expectEvalTo("2", .{ .number = 2 });
}

test "Eval bytecode-encoded data literals" {
    try expectEvalTo("0", .{ .number = 0 });
    try expectEvalTo("1", .{ .number = 1 });
    try expectEvalTo("nil", .{ .nil = {} });
    try expectEvalTo("true", .{ .boolean = true });
    try expectEvalTo("false", .{ .boolean = false });
    try expectEvalTo("()", .{ .list = .{} });
}

test "Eval arithmetic" {
    try expectEvalTo("(+)", .{ .number = 0 });
    try expectEvalTo("(+ 2)", .{ .number = 2 });
    try expectEvalTo("(+ 2 3)", .{ .number = 5 });
    try expectEvalTo("(+ 2 3 4)", .{ .number = 9 });

    try expectEvalTo("(- 2)", .{ .number = -2 });
    try expectEvalTo("(- 2 3)", .{ .number = -1 });
    try expectEvalTo("(- 6 3 2)", .{ .number = 1 });

    try expectEvalTo("(*)", .{ .number = 1 });
    try expectEvalTo("(* 2)", .{ .number = 2 });
    try expectEvalTo("(* 2 3)", .{ .number = 6 });
    try expectEvalTo("(* 2 3 4)", .{ .number = 24 });

    try expectEvalTo("(/ 2)", .{ .number = 0.5 });
    try expectEvalTo("(/ 4 2)", .{ .number = 2 });
    try expectEvalTo("(/ 6 3 2)", .{ .number = 1 });
}

test "Eval comparison" {
    try expectEvalTo("(= 1)", .{ .boolean = true });
    try expectEvalTo("(= 1 1)", .{ .boolean = true });
    try expectEvalTo("(= 1 2)", .{ .boolean = false });

    try expectEvalTo("(= \"hello\" \"hello\")", .{ .boolean = true });
    try expectEvalTo("(= \"hello\" \"world\")", .{ .boolean = false });

    try expectEvalTo("(> 2 1)", .{ .boolean = true });
    try expectEvalTo("(> 1 2)", .{ .boolean = false });
    try expectEvalTo("(> 2 2)", .{ .boolean = false });
    try expectEvalTo("(> 3 2 1)", .{ .boolean = true });
    try expectEvalTo("(> 3 2 3)", .{ .boolean = false });

    try expectEvalTo("(>= 2 1)", .{ .boolean = true });
    try expectEvalTo("(>= 1 2)", .{ .boolean = false });
    try expectEvalTo("(>= 2 2)", .{ .boolean = true });

    try expectEvalTo("(< 1 2)", .{ .boolean = true });
    try expectEvalTo("(< 2 1)", .{ .boolean = false });
    try expectEvalTo("(< 2 2)", .{ .boolean = false });
    try expectEvalTo("(< 1 2 3)", .{ .boolean = true });
    try expectEvalTo("(< 3 2 3)", .{ .boolean = false });

    try expectEvalTo("(<= 1 2)", .{ .boolean = true });
    try expectEvalTo("(<= 2 1)", .{ .boolean = false });
    try expectEvalTo("(<= 2 2)", .{ .boolean = true });
}

test "Eval logical operators" {
    try expectEvalTo("(not true)", .{ .boolean = false });
    try expectEvalTo("(not false)", .{ .boolean = true });

    try expectEvalTo("(and true true)", .{ .boolean = true });
    try expectEvalTo("(and false true)", .{ .boolean = false });

    try expectEvalTo("(or true true)", .{ .boolean = true });
    try expectEvalTo("(or false true)", .{ .boolean = true });
    try expectEvalTo("(or false false)", .{ .boolean = false });
}

test "Eval if" {
    try expectEvalTo("(if true 1 2)", .{ .number = 1 });
    try expectEvalTo("(if false 1 2)", .{ .number = 2 });
}

test "Eval let" {
    try expectEvalTo("(let (x 1) x)", .{ .number = 1 });
    try expectEvalTo("(let (x 4 y 5) (* x y))", .{ .number = 20 });
}

test "Eval def global" {
    try expectEvalTo("(def x 1)", .{ .nil = {} });
}

test "Eval nested function" {
    try expectEvalTo("((fn identity (x) x) 1)", .{ .number = 1 });
}
