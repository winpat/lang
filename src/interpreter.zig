const std = @import("std");
const tst = std.testing;
const Allocator = std.mem.Allocator;

const compile = @import("compiler.zig");
const Compiler = compile.Compiler;
const CompileError = compile.CompileError;
const Gc = @import("garbage_collector.zig").GarbageCollector;
const parse = @import("parser.zig");
const Parser = parse.Parser;
const ParseError = parse.ParseError;
const Value = @import("value.zig").Value;
const virtual_machine = @import("virtual_machine.zig");
const Vm = virtual_machine.VirtualMachine;
const RuntimeError = virtual_machine.RuntimeError;

pub const Interpreter = struct {
    allocator: Allocator,
    gc: *Gc,
    vm: *Vm,

    pub fn init(allocator: Allocator) Allocator.Error!Interpreter {
        const gc = try allocator.create(Gc);
        errdefer allocator.destroy(gc);

        gc.* = Gc.init(allocator);
        errdefer gc.deinit();

        const vm = try allocator.create(Vm);
        errdefer allocator.destroy(vm);

        vm.* = Vm.init(allocator, gc);
        errdefer vm.deinit();

        gc.vm = vm;

        return .{
            .allocator = allocator,
            .gc = gc,
            .vm = vm,
        };
    }

    pub fn deinit(self: *Interpreter) void {
        self.vm.deinit();
        self.allocator.destroy(self.vm);
        self.gc.deinit();
        self.allocator.destroy(self.gc);
    }

    pub fn run(self: *Interpreter, input: []const u8) LangError!Value {
        var parser = Parser.init(self.allocator, input);
        defer parser.deinit();
        const ast = try parser.parse();

        var compiler = Compiler.init(self.allocator, self.gc);
        const func = try compiler.compile(ast);

        return try self.vm.run(func);
    }
};

pub const LangError = ParseError || CompileError || RuntimeError;

test "Interpreter run expression" {
    var interpreter = try Interpreter.init(tst.allocator);
    defer interpreter.deinit();

    const result = try interpreter.run("(+ 41 1)");
    try tst.expectEqual(Value{ .number = 42 }, result);
}
