const std = @import("std");
const ArrayList = std.ArrayList;
const StaticStringMap = std.StaticStringMap;
const tst = std.testing;
const mem = std.mem;
const meta = std.meta;
const Io = std.Io;
const fs = std.fs;
const SourceLocation = std.builtin.SourceLocation;
const Allocator = mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const opts = @import("opts");

const bytecode = @import("bytecode.zig");
const Op = bytecode.Op;
const disassemble = @import("disassembler.zig").disassemble;
const object = @import("object.zig");
const Func = object.Func;
const String = object.String;
const Symbol = object.Symbol;
const par = @import("parser.zig");
const Ast = par.AbstractSyntaxTree;
const Node = par.Node;
const Parser = @import("parser.zig").Parser;
const value = @import("value.zig");
const Value = value.Value;
const List = value.List;

pub const Compiler = struct {
    allocator: Allocator,

    pub fn init(allocator: Allocator) Compiler {
        return .{ .allocator = allocator };
    }

    pub fn compile(self: *Compiler, ast: Ast) CompileError!*Func {
        var target = try FuncState.init(self.allocator, 0);
        defer target.deinit();

        for (ast.nodes) |node|
            try emitExpr(&target, node);

        const return_line = if (ast.nodes.len > 0)
            ast.nodes[ast.nodes.len - 1].getLine()
        else
            1;

        try target.addOp(Op.ret, return_line);

        const func = try self.allocator.create(Func);
        func.* = try target.build(self.allocator);
        return func;
    }
};

const CompileError = error{
    UnknownSymbol,
    InvalidForm,
    InvalidLetForm,
    UnsupportedArity,
    JumpOffsetTooLarge,
    ConstantPoolExceeded,
    LocalPoolExceeded,
    DuplicateLocal,
    MaxScopeDepthExceeded,
    TooManyArguments,
} || Allocator.Error;

const Builtin = *const fn (target: *FuncState, form: Node.List) CompileError!void;

// TODO Implement def for binding globals
// TODO Implement fn for creating functions
// TODO Implement defn for creating and binding functions
const builtins = StaticStringMap(Builtin).initComptime(
    .{
        .{ "+", emitAdd },
        .{ "-", emitSubtract },
        .{ "*", emitMultiply },
        .{ "/", emitDivide },
        .{ "=", emitEqual },
        .{ ">", emitGreaterThan },
        .{ ">=", emitGreaterThanEqual },
        .{ "<", emitLessThan },
        .{ "<=", emitLessThanEqual },
        .{ "not", emitNot },
        .{ "and", emitAnd },
        .{ "or", emitOr },
        .{ "if", emitIf },
        .{ "do", emitDo },
        .{ "let", emitLet },
    },
);

fn emitExpr(target: *FuncState, node: Node) CompileError!void {
    try switch (node) {
        .number => |number| emitNumber(target, number),
        .boolean => |boolean| emitBoolean(target, boolean),
        .nil => |nil| emitNil(target, nil),
        .string => |string| emitString(target, string),
        .symbol => |symbol| emitSymbolLookup(target, symbol),
        .list => |list| emitForm(target, list),
    };
}

fn emitBoolean(target: *FuncState, node: Node.Boolean) CompileError!void {
    const op = if (node.val) Op.load_true else Op.load_false;
    try target.addOp(op, node.line);
}

fn emitNil(target: *FuncState, node: Node.Nil) CompileError!void {
    try target.addOp(Op.load_nil, node.line);
}

fn emitNumber(target: *FuncState, node: Node.Number) CompileError!void {
    if (node.val == 0)
        try target.addOp(.load_zero, node.line)
    else if (node.val == 1)
        try target.addOp(.load_one, node.line)
    else
        try emitConstant(target, .{ .number = node.val }, node.line);
}

fn emitString(target: *FuncState, node: Node.String) CompileError!void {
    if (findStringConstant(target, node.chars)) |cidx| {
        try target.addOpWithByte(.load_constant, cidx, node.line);
    } else {
        const string = try target.allocator.create(String);
        string.* = try String.init(target.allocator, node.chars);

        const cidx = try target.addConstant(.{ .object = &string.obj });
        try target.addOpWithByte(.load_constant, cidx, node.line);
    }
}

fn findStringConstant(target: *FuncState, chars: []const u8) ?u8 {
    for (target.constants.items, 0..) |constant, idx| {
        if (constant != .object) continue;

        const obj = constant.object;
        if (obj.tag != .string) continue;

        const string = obj.as(String);
        if (mem.eql(u8, string.slice(), chars))
            return @intCast(idx);
    }
    return null;
}

fn emitSymbol(target: *FuncState, node: Node.Symbol) CompileError!void {
    if (findSymbolConstant(target, node.name)) |cidx| {
        try target.addOpWithByte(.load_constant, cidx, node.line);
    } else {
        const symbol = try target.allocator.create(Symbol);
        symbol.* = try Symbol.init(target.allocator, node.name);

        const cidx = try target.addConstant(.{ .object = &symbol.obj });
        try target.addOpWithByte(.load_constant, cidx, node.line);
    }
}

fn findSymbolConstant(target: *FuncState, name: []const u8) ?u8 {
    for (target.constants.items, 0..) |constant, idx| {
        if (constant != .object) continue;
        const obj = constant.object;
        if (obj.tag != .symbol) continue;

        const symbol = obj.as(Symbol);
        if (mem.eql(u8, symbol.slice(), name)) {
            return @intCast(idx);
        }
    }
    return null;
}

fn emitSymbolLookup(target: *FuncState, node: Node.Symbol) CompileError!void {
    if (target.resolveLocal(node.name)) |idx|
        return try target.addOpWithByte(.load_local, idx, node.line);

    try emitSymbol(target, node);
}

fn emitConstant(target: *FuncState, constant: Value, line: u32) CompileError!void {
    const cidx: u8 = for (target.constants.items, 0..) |existing, idx| {
        if (existing.equal(constant)) break @intCast(idx);
    } else try target.addConstant(constant);

    try target.addOpWithByte(.load_constant, cidx, line);
}

fn emitForm(target: *FuncState, form: Node.List) CompileError!void {
    if (form.items.len == 0)
        return try target.addOp(.load_empty_list, form.line);

    const head = form.items[0];
    switch (head) {
        .symbol => |symbol| {
            const builtin = builtins.get(symbol.name) orelse
                return CompileError.UnknownSymbol;

            return try builtin(target, form);
        },
        .list => |list| try emitForm(target, list),
        else => return CompileError.InvalidForm,
    }

    for (form.items[1..]) |arg| try emitExpr(target, arg);

    const argc = try argCount(form);
    try target.addOpWithByte(.call, argc, form.line);
}

fn emitAdd(target: *FuncState, form: Node.List) CompileError!void {
    const argc = try argCount(form);

    for (form.items[1..]) |arg|
        try emitExpr(target, arg);

    try switch (argc) {
        // Additive identity
        0 => target.addOp(.load_zero, form.line),
        else => target.addOpWithByte(.add, argc, form.line),
    };
}

fn emitSubtract(target: *FuncState, form: Node.List) CompileError!void {
    const argc = try argCount(form);

    for (form.items[1..]) |arg|
        try emitExpr(target, arg);

    try switch (argc) {
        0 => return CompileError.UnsupportedArity,
        1 => target.addOp(.neg, form.line),
        else => target.addOpWithByte(.sub, argc, form.line),
    };
}

fn emitMultiply(target: *FuncState, form: Node.List) CompileError!void {
    const argc = try argCount(form);

    for (form.items[1..]) |arg|
        try emitExpr(target, arg);

    try switch (argc) {
        // Multiplicative identity
        0 => target.addOp(.load_one, form.line),
        else => target.addOpWithByte(.mul, argc, form.line),
    };
}

fn emitDivide(target: *FuncState, form: Node.List) CompileError!void {
    const argc = try argCount(form);

    for (form.items[1..]) |arg|
        try emitExpr(target, arg);

    try switch (argc) {
        0 => return CompileError.UnsupportedArity,
        else => target.addOpWithByte(.div, argc, form.line),
    };
}

fn emitEqual(target: *FuncState, form: Node.List) CompileError!void {
    const argc = try argCount(form);

    for (form.items[1..]) |arg|
        try emitExpr(target, arg);

    switch (argc) {
        0 => return CompileError.UnsupportedArity,
        else => try target.addOpWithByte(.eq, argc, form.line),
    }
}

fn emitLessThan(target: *FuncState, form: Node.List) CompileError!void {
    try emitComparisonOp(target, .lt, form);
}

fn emitLessThanEqual(target: *FuncState, form: Node.List) CompileError!void {
    try emitComparisonOp(target, .le, form);
}

fn emitGreaterThan(target: *FuncState, form: Node.List) CompileError!void {
    try emitComparisonOp(target, .gt, form);
}

fn emitGreaterThanEqual(target: *FuncState, form: Node.List) CompileError!void {
    try emitComparisonOp(target, .ge, form);
}

fn emitComparisonOp(target: *FuncState, comptime op: Op, form: Node.List) CompileError!void {
    const argc = try argCount(form);

    for (form.items[1..]) |arg|
        try emitExpr(target, arg);

    switch (argc) {
        0 => return CompileError.UnsupportedArity,
        else => try target.addOpWithByte(op, argc, form.line),
    }
}

fn emitNot(target: *FuncState, form: Node.List) CompileError!void {
    try switch (form.items.len - 1) {
        0 => target.addOp(.load_true, form.line),
        1 => {
            try emitExpr(target, form.items[1]);
            try target.addOp(.not, form.line);
        },
        else => return CompileError.UnsupportedArity,
    };
}

fn emitAnd(target: *FuncState, form: Node.List) CompileError!void {
    const argc = form.items.len - 1;
    if (argc == 0) return try target.addOp(.load_nil, form.line);

    const allocator = target.allocator;
    var jumps = try ArrayList(usize).initCapacity(allocator, form.items.len - 1);
    defer jumps.deinit(allocator);

    for (form.items[1..], 0..) |arg, idx| {
        // Don't emit jump + pop for the last operand of the form.
        if (idx > 0) {
            const jump = try emitJumpPlaceholder(target, .jmpf, form.line);
            jumps.appendAssumeCapacity(jump);
            try target.addOp(.pop, form.line);
        }

        try emitExpr(target, arg);
    }

    const dst = target.code.items.len;
    for (jumps.items) |jmp_pos| try patchJump(target, jmp_pos, dst);
}

fn emitOr(target: *FuncState, form: Node.List) CompileError!void {
    const argc = form.items.len - 1;
    if (argc == 0) return try target.addOp(.load_nil, form.line);

    const allocator = target.allocator;
    var jumps = try ArrayList(usize).initCapacity(allocator, form.items.len - 1);
    defer jumps.deinit(allocator);

    for (form.items[1..], 0..) |arg, idx| {
        if (idx > 0) {
            const jump = try emitJumpPlaceholder(target, .jmpt, form.line);
            jumps.appendAssumeCapacity(jump);
            try target.addOp(.pop, form.line);
        }

        try emitExpr(target, arg);
    }

    const dst = target.code.items.len;
    for (jumps.items) |jmp_pos|
        try patchJump(target, jmp_pos, dst);
}

fn emitJumpPlaceholder(target: *FuncState, op: Op, line: u32) CompileError!usize {
    try target.addOp(op, line);
    try target.addShort(0, line);
    return target.code.items.len - 3;
}

pub fn patchJump(target: *FuncState, jmp_pos: usize, dst: usize) CompileError!void {
    // Take into account that encoding of the offset takes up two
    // bytes. Making a jump a 3 byte instruction, whos bytes will
    // be already processed processed by the VM when it adds the
    // jump offset.
    const offset = dst - jmp_pos - 3;

    if (offset > std.math.maxInt(u16))
        return CompileError.JumpOffsetTooLarge;

    target.code.items[jmp_pos + 1] = @truncate(offset);
    target.code.items[jmp_pos + 2] = @truncate(offset >> 8);
}

fn emitLet(target: *FuncState, form: Node.List) CompileError!void {
    try target.beginScope();

    if (form.items.len < 2 or form.items[1] != .list)
        return CompileError.InvalidLetForm;

    const binding_form = form.items[1].list;
    if (binding_form.items.len % 2 != 0)
        return CompileError.InvalidLetForm;

    var i: usize = 0;
    while (i < binding_form.items.len) : (i += 2) {
        const lhs = binding_form.items[i];

        if (lhs != .symbol)
            return CompileError.InvalidLetForm;

        const sym = lhs.symbol;
        try target.addLocal(sym.name);

        const rhs = binding_form.items[i + 1];
        try emitExpr(target, rhs);
    }

    // Compile the body.
    try emitBlock(target, form.items[2..], form.line);
    try target.endScope(form.line);
}

fn emitIf(target: *FuncState, form: Node.List) CompileError!void {
    if (form.items.len != 4)
        return CompileError.UnsupportedArity;

    // Condition.
    try emitExpr(target, form.items[1]);
    const else_branch_jmp = try emitJumpPlaceholder(target, .jmpf, form.line);

    // Then branch
    try target.addOp(.pop, form.line);
    try emitExpr(target, form.items[2]);
    const end_jmp = try emitJumpPlaceholder(target, .jmp, form.line);

    // Else branch
    try patchJump(target, else_branch_jmp, target.code.items.len);
    try target.addOp(.pop, form.line);
    try emitExpr(target, form.items[3]);

    // End
    try patchJump(target, end_jmp, target.code.items.len);
}

fn emitDo(target: *FuncState, form: Node.List) CompileError!void {
    try emitBlock(target, form.items[1..], form.line);
}

fn emitBlock(target: *FuncState, nodes: []const Node, line: u32) CompileError!void {
    if (nodes.len == 0)
        return try target.addOp(.load_nil, line);

    for (nodes, 0..) |node, idx| {
        if (idx > 0) try target.addOp(.pop, node.getLine());
        try emitExpr(target, node);
    }
}

fn argCount(form: Node.List) CompileError!u8 {
    if (form.items.len - 1 > std.math.maxInt(u8))
        return CompileError.TooManyArguments;

    return @intCast(form.items.len - 1);
}

const FuncState = struct {
    allocator: Allocator,
    scope_depth: u16,
    constants: ArrayList(Value) = .{},
    code: ArrayList(u8) = .{},
    // TODO Encode line information more memory efficiently (e.g. run length)
    lines: ArrayList(u32) = .{},
    locals: ArrayList(Local) = .{},

    fn init(allocator: Allocator, scope_depth: u16) Allocator.Error!FuncState {
        return .{
            .allocator = allocator,
            .scope_depth = scope_depth,
        };
    }

    fn deinit(self: *FuncState) void {
        // build() empties the ArrayLists, so if build() succeeds, deinit() operates on empty lists.
        for (self.constants.items) |constant| constant.deinit(self.allocator);
        self.constants.deinit(self.allocator);
        self.code.deinit(self.allocator);
        self.lines.deinit(self.allocator);
        self.locals.deinit(self.allocator);
    }

    fn build(self: *FuncState, allocator: Allocator) Allocator.Error!Func {
        return Func{
            .constants = try self.constants.toOwnedSlice(allocator),
            .code = try self.code.toOwnedSlice(allocator),
            .lines = try self.lines.toOwnedSlice(allocator),
        };
    }

    fn beginScope(self: *FuncState) CompileError!void {
        if (self.scope_depth >= std.math.maxInt(u16))
            return CompileError.MaxScopeDepthExceeded;

        self.scope_depth += 1;
    }

    fn endScope(self: *FuncState, line: u32) CompileError!void {
        self.scope_depth -= 1;

        var locals = &self.locals;
        var n: u8 = 0;
        var i = locals.items.len;
        while (i > 0 and locals.items[i - 1].depth > self.scope_depth) : (i -= 1) {
            _ = locals.pop() orelse unreachable;
            n += 1;
        }

        if (n > 0)
            try self.addOpWithByte(.ppop_n, n, line);
    }

    fn addOpWithByte(self: *FuncState, op: Op, byte: u8, line: u32) Allocator.Error!void {
        const allocator = self.allocator;
        try self.code.append(allocator, @intFromEnum(op));
        try self.code.append(allocator, byte);
        try self.lines.append(allocator, line);
        try self.lines.append(allocator, line);
    }

    fn addOp(self: *FuncState, op: Op, line: u32) Allocator.Error!void {
        const allocator = self.allocator;
        try self.code.append(allocator, @intFromEnum(op));
        try self.lines.append(allocator, line);
    }

    fn addShort(self: *FuncState, short: u16, line: u32) Allocator.Error!void {
        const allocator = self.allocator;
        try self.code.append(allocator, @intCast(short));
        try self.code.append(allocator, @intCast(short >> 8));
        try self.lines.append(allocator, line);
        try self.lines.append(allocator, line);
    }

    fn addConstant(self: *FuncState, constant: Value) CompileError!u8 {
        if (self.constants.items.len >= std.math.maxInt(u8))
            return CompileError.ConstantPoolExceeded;

        try self.constants.append(self.allocator, constant);
        return @intCast(self.constants.items.len - 1);
    }

    fn addLocal(self: *FuncState, name: []const u8) CompileError!void {
        if (self.locals.items.len >= std.math.maxInt(u8))
            return CompileError.LocalPoolExceeded;

        var locals = &self.locals;
        var n = locals.items.len;
        while (n > 0) : (n -= 1) {
            const local = locals.items[n - 1];

            if (local.depth < self.scope_depth)
                break;

            // Symbols can only be bound once in the same lexical scope.
            if (mem.eql(u8, local.name, name))
                return CompileError.DuplicateLocal;
        }

        try locals.append(self.allocator, Local{ .name = name, .depth = self.scope_depth });
    }

    fn resolveLocal(self: FuncState, name: []const u8) ?u8 {
        const locals = &self.locals;
        var i: u8 = @intCast(locals.items.len);
        return while (i > 0) : (i -= 1) {
            const idx = i - 1;
            const local = locals.items[idx];
            if (mem.eql(u8, local.name, name)) break idx;
        } else null;
    }
};

const Local = struct {
    name: []const u8,
    depth: u16,
};

fn expectCompileError(input: []const u8, expected: CompileError) !void {
    var parser = Parser.init(tst.allocator, input);
    defer parser.deinit();

    const ast = try parser.parse();

    var cpl = Compiler.init(tst.allocator);
    try tst.expectError(expected, cpl.compile(ast));
}

pub fn expectDisasmSnapshot(name: []const u8, loc: SourceLocation, input: []const u8) !void {
    const allocator = tst.allocator;

    const path = try mem.concat(allocator, u8, &.{ opts.snapshot_dir, "/", name, ".txt" });
    defer allocator.free(path);

    const existing = try loadSnapshot(allocator, path);

    if (existing == null) {
        if (opts.update_snapshots) {
            const snap = try createSnapshot(allocator, input, loc);
            defer allocator.free(snap);

            return try updateSnapshot(path, snap);
        } else {
            return SnapshotError.Missing;
        }
    }

    const expected = existing.?;
    defer allocator.free(expected);

    const actual = try createSnapshot(allocator, input, loc);
    defer allocator.free(actual);

    tst.expectEqualSlices(u8, expected, actual) catch if (opts.update_snapshots)
        try updateSnapshot(path, actual)
    else
        return SnapshotError.Mismatch;
}

fn loadSnapshot(allocator: Allocator, path: []const u8) !?[]const u8 {
    const file = fs.cwd().openFile(path, .{}) catch |err| switch (err) {
        error.FileNotFound => return null,
        else => return err,
    };
    defer file.close();

    const file_stat = try file.stat();
    const buf = try allocator.alloc(u8, file_stat.size);

    _ = try file.read(buf);
    return buf;
}

fn createSnapshot(allocator: Allocator, input: []const u8, loc: SourceLocation) ![]const u8 {
    const func = try compileSnapshot(allocator, input);
    defer {
        func.deinit(allocator);
        allocator.destroy(func);
    }

    var buf = Io.Writer.Allocating.init(tst.allocator);
    defer buf.deinit();

    try renderSnapshot(&buf.writer, loc, func);

    return try buf.toOwnedSlice();
}

fn compileSnapshot(allocator: Allocator, input: []const u8) !*Func {
    var parser = Parser.init(allocator, input);
    defer parser.deinit();

    const ast = try parser.parse();

    var cpl = Compiler.init(allocator);
    return try cpl.compile(ast);
}

fn renderSnapshot(writer: *Io.Writer, loc: SourceLocation, func: *Func) !void {
    try writer.print(
        \\== {s} ==
        \\
        \\=== Bytecode ===
        \\
    , .{loc.fn_name});

    try disassemble(writer, func);

    if (func.constants.len > 0) {
        try writer.writeAll(
            \\
            \\=== Constants ===
            \\
            ,
        );

        for (func.constants, 0..) |constant, idx|
            try writer.print(
                "  {:<3} {t:<10} {f}\n",
                .{ idx, meta.activeTag(constant), constant },
            );
    }

    try writer.flush();
}

fn updateSnapshot(path: []const u8, snap: []const u8) !void {
    var file = try fs.cwd().createFile(path, .{ .truncate = true });
    defer file.close();
    _ = try file.write(snap);
}

pub const SnapshotError = error{
    Missing,
    Mismatch,
};

test "Compile data literals" {
    try expectDisasmSnapshot("number_12", @src(), "12");
    try expectDisasmSnapshot("number_0", @src(), "0");
    try expectDisasmSnapshot("number_1", @src(), "1");

    try expectDisasmSnapshot("boolean_true", @src(), "true");
    try expectDisasmSnapshot("boolean_false", @src(), "false");

    try expectDisasmSnapshot("nil", @src(), "nil");

    try expectDisasmSnapshot("string", @src(), "\"Hello World!\"");

    try expectDisasmSnapshot("symbol", @src(), "reduce");

    try expectDisasmSnapshot("empty_list", @src(), "()");
}

test "Test constants resuse" {
    try expectDisasmSnapshot("duplicate_numeric_constants", @src(), "3 3");
    try expectDisasmSnapshot("duplicate_string_constants", @src(), "\"hello\" \"hello\"");
    try expectDisasmSnapshot("duplicate_symbol_constants", @src(), "map map");
}

test "Compile arithmetic forms" {
    try expectDisasmSnapshot("add_no_args", @src(), "(+)");
    try expectDisasmSnapshot("add_one_arg", @src(), "(+ 1)");
    try expectDisasmSnapshot("add_two_args", @src(), "(+ 2 3)");

    try expectCompileError("(-)", CompileError.UnsupportedArity);
    try expectDisasmSnapshot("subtract_one_arg", @src(), "(- 1)");
    try expectDisasmSnapshot("subtract_two_args", @src(), "(- 2 3)");

    try expectDisasmSnapshot("mul_no_args", @src(), "(*)");
    try expectDisasmSnapshot("mul_one_arg", @src(), "(* 1)");
    try expectDisasmSnapshot("mul_two_args", @src(), "(* 2 3)");

    try expectCompileError("(/)", CompileError.UnsupportedArity);
    try expectDisasmSnapshot("div_one_arg", @src(), "(/ 1)");
    try expectDisasmSnapshot("div_two_args", @src(), "(/ 2 3)");
}

test "Compile comparison forms" {
    try expectCompileError("(=)", CompileError.UnsupportedArity);
    try expectDisasmSnapshot("equal_one_arg", @src(), "(= 1)");
    try expectDisasmSnapshot("equal_two_args", @src(), "(= 2 3)");

    try expectCompileError("(>)", CompileError.UnsupportedArity);
    try expectDisasmSnapshot("greater_than_one_arg", @src(), "(> 1)");
    try expectDisasmSnapshot("greater_than_two_args", @src(), "(> 2 3)");

    try expectCompileError("(>=)", CompileError.UnsupportedArity);
    try expectDisasmSnapshot("greater_than_equal_one_arg", @src(), "(>= 1)");
    try expectDisasmSnapshot("greater_than_equal_two_args", @src(), "(>= 2 3)");

    try expectCompileError("(<)", CompileError.UnsupportedArity);
    try expectDisasmSnapshot("less_than_one_arg", @src(), "(< 1)");
    try expectDisasmSnapshot("less_than_two_args", @src(), "(< 2 3)");

    try expectCompileError("(<=)", CompileError.UnsupportedArity);
    try expectDisasmSnapshot("less_than_equal_one_arg", @src(), "(<= 1)");
    try expectDisasmSnapshot("less_than_equal_two_args", @src(), "(<= 2 3)");
}

test "Compile logical operator forms" {
    try expectDisasmSnapshot("not_no_args", @src(), "(not)");
    try expectDisasmSnapshot("not_one_arg", @src(), "(not true)");
    try expectCompileError("(not 1 2)", CompileError.UnsupportedArity);

    try expectDisasmSnapshot("and_no_args", @src(), "(and)");
    try expectDisasmSnapshot("and_one_arg", @src(), "(and 2)");
    try expectDisasmSnapshot("and_two_args", @src(), "(and 2 3)");

    try expectDisasmSnapshot("or_no_args", @src(), "(or)");
    try expectDisasmSnapshot("or_one_arg", @src(), "(or 2)");
    try expectDisasmSnapshot("or_two_args", @src(), "(or nil 3)");
}

test "Compile let binding" {
    try expectCompileError("(let ((x 1)) x)", CompileError.InvalidLetForm);
    try expectCompileError("(let (x 1 3) x)", CompileError.InvalidLetForm);
    try expectDisasmSnapshot("let", @src(), "(let (x 4) (+ 1 x))");
}

test "Compile if form" {
    try expectCompileError("(if true 1)", CompileError.UnsupportedArity);
    try expectDisasmSnapshot("if", @src(), "(if true 1 2)");
}
