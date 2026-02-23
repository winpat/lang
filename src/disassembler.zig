const std = @import("std");
const tst = std.testing;
const Io = std.Io;

const Compiler = @import("compiler.zig").Compiler;
const Func = @import("object.zig").Func;
const Op = @import("bytecode.zig").Op;
const Parser = @import("parser.zig").Parser;
const Value = @import("value.zig").Value;

/// Length of longest bytecode op string representation.
const inst_repr_len_max = blk: {
    const fields = @typeInfo(Op).@"enum".fields;
    var max: usize = 0;
    for (fields) |field| max = @max(max, field.name.len);
    break :blk max;
};

pub fn disassemble(writer: *Io.Writer, func: *const Func) DisassembleError!void {
    var pos: usize = 0;
    while (pos < func.code.len) {
        const consumed = disassembleOp(writer, func, pos) catch |err| {
            try writer.print(
                "{}: Failed to decode bytecode at position {}.\n",
                .{ err, pos },
            );
            return err;
        };

        pos += consumed + 1;
    }

    try writer.flush();
}

fn disassembleOp(writer: *Io.Writer, func: *const Func, offset: usize) DisassembleError!usize {
    var pos = offset;

    const line: u32 = func.lines[pos];
    try writer.print("  {d:0>4} {d:>4}    ", .{ pos, line });

    const code = func.code;
    const op: Op = @enumFromInt(code[pos]);
    const op_name = @tagName(op);

    switch (op) {
        .load_constant,
        .load_local,
        .add,
        .sub,
        .mul,
        .div,
        .eq,
        .gt,
        .ge,
        .lt,
        .le,
        .ppop_n,
        .call,
        => {
            const op_fmt = std.fmt.comptimePrint(
                "{{s:<{d}}} {{}}",
                .{inst_repr_len_max + 2},
            );
            pos += 1;
            try writer.print(op_fmt, .{ op_name, code[pos] });
        },
        .jmp, .jmpf, .jmpt => {
            const op_fmt = std.fmt.comptimePrint(
                "{{s:<{d}}} {{}}",
                .{inst_repr_len_max + 2},
            );
            pos += 1;
            const low = code[pos];
            const high = code[pos + 1];
            const short = (@as(u16, high) << 8) | low;
            pos += 1;
            try writer.print(op_fmt, .{ op_name, short });
        },
        .load_nil,
        .load_true,
        .load_false,
        .load_empty_list,
        .load_one,
        .load_zero,
        .neg,
        .not,
        .pop,
        .ret,
        => {
            try writer.print("{s}", .{op_name});
        },
    }

    try writer.writeByte('\n');

    return pos - offset;
}

const DisassembleError = error{MissingConstant} || Io.Writer.Error;

test "Disassemble bytecode" {
    const input =
        \\(+ 1 2)
        \\(- -2)
    ;

    var parser = Parser.init(tst.allocator, input);
    defer parser.deinit();

    const ast = try parser.parse();

    var cpl = Compiler.init(tst.allocator);
    const func = try cpl.compile(ast);

    defer {
        func.deinit(tst.allocator);
        tst.allocator.destroy(func);
    }

    const expected =
        \\  0000    1    load_one
        \\  0001    1    load_constant     0
        \\  0003    1    add               2
        \\  0005    2    load_constant     1
        \\  0007    2    neg
        \\  0008    2    ret
        \\
    ;

    var buf = Io.Writer.Allocating.init(tst.allocator);
    defer buf.deinit();

    try disassemble(&buf.writer, func);

    const actual = try buf.toOwnedSlice();
    defer tst.allocator.free(actual);

    try tst.expectEqualSlices(u8, expected, actual);
}
