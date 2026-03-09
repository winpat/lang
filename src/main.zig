const std = @import("std");
const fs = std.fs;
const Io = std.Io;
const heap = std.heap;
const process = std.process;
const Allocator = std.mem.Allocator;

const Interpreter = @import("interpreter.zig").Interpreter;
const repl = @import("repl.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var stdin_buf = [_]u8{0} ** 1024;
    var stdin_reader = fs.File.stdin().reader(&stdin_buf);
    const stdin = &stdin_reader.interface;

    var stdout_buf = [_]u8{0} ** 1024;
    var stdout_writer = fs.File.stdout().writer(&stdout_buf);
    const stdout = &stdout_writer.interface;
    defer stdout.flush() catch {};

    var stderr_buf = [_]u8{0} ** 1024;
    var stderr_writer = fs.File.stderr().writer(&stderr_buf);
    const stderr = &stderr_writer.interface;
    defer stderr.flush() catch {};

    const args = try process.argsAlloc(allocator);
    defer process.argsFree(allocator, args);

    var interpreter = try Interpreter.init(allocator, stdin, stdout, stderr);
    defer interpreter.deinit();

    switch (args.len - 1) {
        0 => runRepl(allocator, &interpreter, stdin, stdout, stderr) catch process.exit(1),
        1 => runScript(allocator, &interpreter, args[1]) catch process.exit(1),
        else => |n| {
            try stderr.print("Unsupported number of arguments: {}\n", .{n});
            process.exit(1);
        },
    }
}

fn runRepl(allocator: Allocator, interpreter: *Interpreter, stdin: *Io.Reader, stdout: *Io.Writer, stderr: *Io.Writer) !void {
    repl.readEvalPrintLoop(allocator, interpreter, stdin, stdout, stderr) catch |err| {
        return switch (err) {
            repl.ReplError.EndOfInput => {},
            else => err,
        };
    };
}

fn runScript(allocator: Allocator, interpreter: *Interpreter, path: []const u8) !void {
    const file = try fs.cwd().openFile(path, .{});
    defer file.close();

    var reader = file.reader(&.{});
    const size = try reader.getSize();

    const source = try allocator.alloc(u8, size);
    defer allocator.free(source);
    try reader.interface.readSliceAll(source);

    _ = try interpreter.run(source);
}
