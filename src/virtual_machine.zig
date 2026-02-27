const std = @import("std");
const tst = std.testing;
const Allocator = std.mem.Allocator;
const meta = std.meta;
const Tag = meta.Tag;

const Compiler = @import("compiler.zig").Compiler;
const Func = @import("object.zig").Func;
const Op = @import("bytecode.zig").Op;
const Parser = @import("parser.zig").Parser;
const Value = @import("value.zig").Value;

const stack_depth_max: usize = 256;

pub const VirtualMachine = struct {
    allocator: Allocator,
    stack: [stack_depth_max]Value = undefined,
    sp: usize = 0,

    pub fn init(allocator: Allocator) VirtualMachine {
        return .{ .allocator = allocator };
    }

    fn push(self: *VirtualMachine, val: Value) RuntimeError!void {
        if (self.sp >= stack_depth_max)
            return RuntimeError.StackOverflow;

        self.stack[self.sp] = val;
        self.sp += 1;
    }

    pub fn pushAs(self: *VirtualMachine, comptime tag: Tag(Value), val: @FieldType(Value, @tagName(tag))) RuntimeError!void {
        try self.push(@unionInit(Value, @tagName(tag), val));
    }

    fn pop(self: *VirtualMachine) RuntimeError!Value {
        if (self.sp == 0)
            return RuntimeError.StackUnderflow;

        self.sp -= 1;
        return self.stack[self.sp];
    }

    pub fn popAs(self: *VirtualMachine, comptime tag: Tag(Value)) RuntimeError!@FieldType(Value, @tagName(tag)) {
        const val = try self.pop();

        if (meta.activeTag(val) != tag)
            return RuntimeError.ValueTypeMismatch;

        return @field(val, @tagName(tag));
    }

    pub fn run(self: *VirtualMachine, func: *Func) RuntimeError!Value {
        var ip: usize = 0;

        while (ip < func.code.len) {
            const op: Op = @enumFromInt(func.code[ip]);
            switch (op) {
                .load_constant => {
                    ip += 1;
                    const cidx = func.code[ip];
                    try self.push(func.constants[cidx]);
                },
                .load_local => {
                    ip += 1;
                    const sidx = func.code[ip];
                    try self.push(self.stack[sidx]);
                },
                .load_zero => try self.push(.{ .number = 0 }),
                .load_one => try self.push(.{ .number = 1 }),
                .load_nil => try self.push(.{ .nil = {} }),
                .load_true => try self.push(.{ .boolean = true }),
                .load_false => try self.push(.{ .boolean = false }),
                .load_empty_list => try self.push(.{ .list = .{} }),
                .add => {
                    ip += 1;
                    const argc = func.code[ip];

                    var sum = try self.popAs(.number);
                    var i: u8 = 1;
                    while (i < argc) : (i += 1)
                        sum += try self.popAs(.number);

                    try self.pushAs(.number, sum);
                },
                .sub => {
                    ip += 1;
                    const argc = func.code[ip];

                    var subtrahend = try self.popAs(.number);
                    var i: u8 = 1;
                    while (i < argc - 1) : (i += 1)
                        subtrahend += try self.popAs(.number);

                    const minuend = try self.popAs(.number);
                    try self.pushAs(.number, minuend - subtrahend);
                },
                .mul => {
                    ip += 1;
                    const argc = func.code[ip];

                    var product = try self.popAs(.number);
                    var i: u8 = 1;
                    while (i < argc) : (i += 1)
                        product *= try self.popAs(.number);

                    try self.pushAs(.number, product);
                },
                .div => {
                    ip += 1;
                    const argc = func.code[ip];

                    if (argc == 1) {
                        const divisor = try self.popAs(.number);
                        try self.pushAs(.number, 1 / divisor);
                    } else {
                        var divisor = try self.popAs(.number);
                        var n: u8 = 1;
                        while (n < argc - 1) : (n += 1)
                            divisor *= try self.popAs(.number);

                        const divident = try self.popAs(.number);
                        try self.pushAs(.number, divident / divisor);
                    }
                },
                .eq => {
                    ip += 1;
                    const argc = func.code[ip];

                    const val = try self.pop();
                    var i: u8 = 1;
                    const is_equal = while (i < argc) : (i += 1) {
                        if (!val.equal(try self.pop())) {
                            break false;
                        }
                    } else true;

                    try self.pushAs(.boolean, is_equal);
                },
                .gt => {
                    ip += 1;
                    const argc = func.code[ip];

                    if (argc == 1) {
                        _ = try self.pop();
                        try self.pushAs(.boolean, true);
                    } else {
                        var prev = try self.popAs(.number);
                        var i: u8 = 1;
                        var is_greater = true;
                        while (i < argc) : (i += 1) {
                            const curr = try self.popAs(.number);
                            if (curr <= prev) is_greater = false;
                            prev = curr;
                        }

                        try self.pushAs(.boolean, is_greater);
                    }
                },
                .ge => {
                    ip += 1;
                    const argc = func.code[ip];

                    if (argc == 1) {
                        _ = try self.pop();
                        try self.pushAs(.boolean, true);
                    } else {
                        var prev = try self.popAs(.number);
                        var i: u8 = 1;
                        var is_greater_equal = true;
                        while (i < argc) : (i += 1) {
                            const curr = try self.popAs(.number);
                            if (curr < prev) is_greater_equal = false;
                            prev = curr;
                        }

                        try self.pushAs(.boolean, is_greater_equal);
                    }
                },
                .lt => {
                    ip += 1;
                    const argc = func.code[ip];

                    if (argc == 1) {
                        _ = try self.pop();
                        try self.pushAs(.boolean, true);
                    } else {
                        var prev = try self.popAs(.number);
                        var i: u8 = 1;
                        var is_less = true;
                        while (i < argc) : (i += 1) {
                            const curr = try self.popAs(.number);
                            if (curr >= prev) is_less = false;
                            prev = curr;
                        }

                        try self.pushAs(.boolean, is_less);
                    }
                },
                .le => {
                    ip += 1;
                    const argc = func.code[ip];

                    if (argc == 1) {
                        _ = try self.pop();
                        try self.pushAs(.boolean, true);
                    } else {
                        var prev = try self.popAs(.number);
                        var i: u8 = 1;
                        var is_less_equal = true;
                        while (i < argc) : (i += 1) {
                            const curr = try self.popAs(.number);
                            if (curr > prev) is_less_equal = false;
                            prev = curr;
                        }

                        try self.pushAs(.boolean, is_less_equal);
                    }
                },
                .neg => try self.pushAs(.number, -(try self.popAs(.number))),
                .pop => _ = try self.pop(),
                .ppop_n => {
                    const tmp = try self.pop();

                    ip += 1;
                    var n = func.code[ip];
                    while (n > 0) : (n -= 1)
                        _ = try self.pop();

                    try self.push(tmp);
                },
                .ret => return self.pop(),
                else => @panic("Unsupported op."),
            }
            ip += 1;
        }

        unreachable;
    }
};

const RuntimeError = error{
    StackOverflow,
    StackUnderflow,
    ValueTypeMismatch,
} || Allocator.Error;

fn expectEvalTo(input: []const u8, expected: ?Value) !void {
    const allocator = tst.allocator;

    var parser = Parser.init(allocator, input);
    defer parser.deinit();
    const ast = try parser.parse();

    var compiler = Compiler.init(allocator);
    const func = try compiler.compile(ast);
    defer {
        func.deinit(allocator);
        allocator.destroy(func);
    }

    var vm = VirtualMachine.init(allocator);
    const result = try vm.run(func);

    try tst.expectEqualDeep(expected, result);
    try tst.expectEqual(vm.sp, 0);
}

test "Eval bytecode-encoded data literals" {
    try expectEvalTo("0", .{ .number = 0 });
    try expectEvalTo("1", .{ .number = 1 });
    try expectEvalTo("nil", .{ .nil = {} });
    try expectEvalTo("true", .{ .boolean = true });
    try expectEvalTo("false", .{ .boolean = false });
    try expectEvalTo("()", .{ .list = .{} });
}

test "Eval constant" {
    try expectEvalTo("2", .{ .number = 2 });
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

test "Eval let" {
    try expectEvalTo("(let (x 1) x)", .{ .number = 1 });
    try expectEvalTo("(let (x 4 y 5) (* x y))", .{ .number = 20 });
}
