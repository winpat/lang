const std = @import("std");
const tst = std.testing;
const Io = std.Io;
const fs = std.fs;
const Allocator = std.mem.Allocator;

const builtin = @import("builtin.zig");
const compile = @import("compiler.zig");
const Compiler = compile.Compiler;
const CompileError = compile.CompileError;
const Gc = @import("garbage_collector.zig").GarbageCollector;
const NativeFunc = @import("object.zig").NativeFunc;
const parse = @import("parser.zig");
const Value = @import("value.zig").Value;
const virtual_machine = @import("virtual_machine.zig");
const Vm = virtual_machine.VirtualMachine;
const RuntimeError = virtual_machine.RuntimeError;

pub const Interpreter = struct {
    allocator: Allocator,

    stdin: *Io.Reader,
    stdout: *Io.Writer,
    stderr: *Io.Writer,

    gc: *Gc,
    vm: *Vm,
    cpl: *Compiler,

    pub fn init(allocator: Allocator, stdin: *Io.Reader, stdout: *Io.Writer, stderr: *Io.Writer) LangError!Interpreter {
        const gc = try allocator.create(Gc);
        errdefer allocator.destroy(gc);

        gc.* = Gc.init(allocator);
        errdefer gc.deinit();

        const cpl = try allocator.create(Compiler);
        errdefer allocator.destroy(cpl);

        cpl.* = Compiler.init(allocator, gc);
        errdefer cpl.deinit();

        const vm = try allocator.create(Vm);
        errdefer allocator.destroy(vm);

        vm.* = try Vm.init(allocator, stdin, stdout, stderr, gc, &cpl.module_cache);
        errdefer vm.deinit();

        gc.vm = vm;

        var self = Interpreter{
            .allocator = allocator,
            .stdin = stdin,
            .stdout = stdout,
            .stderr = stderr,
            .gc = gc,
            .cpl = cpl,
            .vm = vm,
        };
        try self.registerBuiltins();
        try self.loadPrelude();

        return self;
    }

    pub fn deinit(self: *Interpreter) void {
        self.vm.deinit();
        self.allocator.destroy(self.vm);
        self.cpl.deinit();
        self.allocator.destroy(self.cpl);
        self.gc.deinit();
        self.allocator.destroy(self.gc);
    }

    pub fn run(self: *Interpreter, input: []const u8) LangError!Value {
        const func = try self.cpl.compile(input);
        return try self.vm.run(func);
    }

    fn registerBuiltins(self: *Interpreter) Allocator.Error!void {
        for (builtin.funcs) |func| {
            const native_func = try self.gc.create(NativeFunc, .{ func.name, func.impl });
            try self.vm.defineGlobal(func.name, .{ .object = &native_func.obj });
        }
    }

    fn loadPrelude(self: *Interpreter) LangError!void {
        const prelude = @embedFile("lib/core.lang");
        _ = try self.run(prelude);
    }
};

pub const LangError = CompileError || RuntimeError;

pub fn expectEvalTo(input: []const u8, expected: Value) !void {
    var stdin_buf = [_]u8{0} ** 1024;
    var stdin_reader = fs.File.stdin().reader(&stdin_buf);

    var stdout = Io.Writer.Allocating.init(tst.allocator);
    defer stdout.deinit();

    var stderr = Io.Writer.Allocating.init(tst.allocator);
    defer stderr.deinit();

    var interpreter = try Interpreter.init(
        tst.allocator,
        &stdin_reader.interface,
        &stdout.writer,
        &stderr.writer,
    );
    defer interpreter.deinit();

    const result = try interpreter.run(input);
    try tst.expectEqual(expected, result);
}

test "Interpreter run expression" {
    try expectEvalTo("(+ 41 1)", .{ .number = 42 });
}

test "Interpreter run builtin" {
    try expectEvalTo("(nil? nil)", .{ .boolean = true });
}

test "Interpreter run prelude function" {
    try expectEvalTo("(inc 1)", .{ .number = 2 });
}
