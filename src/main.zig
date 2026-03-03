const std = @import("std");
const fs = std.fs;
const heap = std.heap;
const process = std.process;
const Allocator = std.mem.Allocator;

const Interpreter = @import("interpreter.zig").Interpreter;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const args = try process.argsAlloc(allocator);
    defer process.argsFree(allocator, args);

    var stderr_writer = fs.File.stderr().writer(&.{});
    const stderr = &stderr_writer.interface;

    switch (args.len - 1) {
        0 => try readEvalPrintLoop(allocator),
        1 => try runScript(allocator, args[1]),
        else => |n| {
            try stderr.print("Unsupported number of arguments: {}\n", .{n});
            process.exit(1);
        },
    }
}

const repl_input_length_max = 4096;

const repl_prompt = "> ";

pub fn readEvalPrintLoop(allocator: Allocator) !void {
    var stdout_writer = fs.File.stdout().writer(&.{});
    const stdout = &stdout_writer.interface;

    var stderr_writer = fs.File.stderr().writer(&.{});
    const stderr = &stderr_writer.interface;

    var input_buf = [_]u8{0} ** repl_input_length_max;
    var stdin_reader = fs.File.stdin().reader(&input_buf);
    const stdin = &stdin_reader.interface;

    var interpreter = try Interpreter.init(allocator);
    defer interpreter.deinit();

    while (true) {
        try stdout.writeAll(repl_prompt);
        const input = try stdin.takeDelimiter('\n') orelse continue;

        if (interpreter.run(input)) |result|
            try stdout.print("{f}\n", .{result})
        else |err|
            try stderr.print("{}\n", .{err});
    }
}

const script_size_bytes_max = 1048576;

fn runScript(allocator: Allocator, path: []const u8) !void {
    const file = try fs.cwd().openFile(path, .{});
    const input = try file.readToEndAlloc(allocator, script_size_bytes_max);
    var interp = try Interpreter.init(allocator);
    _ = try interp.run(input);
}
