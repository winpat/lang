const std = @import("std");
const tst = std.testing;
const Io = std.Io;
const meta = std.meta;

const Compiler = @import("compiler.zig").Compiler;
const Func = @import("object.zig").Func;
const Gc = @import("garbage_collector.zig").GarbageCollector;
const Op = @import("bytecode.zig").Op;
const Value = @import("value.zig").Value;

/// Length of longest bytecode op string representation.
const inst_repr_len_max = blk: {
    const fields = @typeInfo(Op).@"enum".fields;
    var max: usize = 0;
    for (fields) |field| max = @max(max, field.name.len);
    break :blk max;
};

pub fn disassemble(writer: *Io.Writer, func: *const Func) DisassembleError!void {
    try disassembleFunc(writer, func, 0);
}

fn disassembleFunc(writer: *Io.Writer, func: *const Func, depth: usize) DisassembleError!void {
    try writer.splatByteAll('=', depth + 3);
    try writer.print(" {f} ", .{func});
    try writer.splatByteAll('=', depth + 3);

    try writer.writeAll(
        \\
        \\ Bytecode
        \\
    );

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

    if (func.constants.len > 0) {
        try writer.writeAll(
            \\ Constants
            \\
        );
        for (func.constants, 0..) |constant, idx|
            try writer.print(
                "  {:<3} {t:<10} {f}\n",
                .{ idx, meta.activeTag(constant), constant },
            );
    }

    for (func.constants) |constant| {
        if (constant == .object and constant.object.tag == .func) {
            try writer.writeByte('\n');
            try disassembleFunc(writer, constant.object.as(Func), depth + 1);
        }
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
        .load_upvalue,
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
        .close_upvalues,
        .call,
        .tail_call,
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
        .create_closure => {
            const op_fmt = std.fmt.comptimePrint(
                "{{s:<{d}}} {{}}",
                .{inst_repr_len_max + 2},
            );

            pos += 1;
            const cidx = code[pos];

            try writer.print(op_fmt, .{ op_name, cidx });

            const upvalue_count = func.constants[cidx].object.as(Func).upvalue_count;

            if (upvalue_count > 0) {
                for (0..upvalue_count) |_| {
                    try writer.writeByte('\n');

                    pos += 1;
                    const local = if (code[pos] == 1) "local" else "upvalue";

                    pos += 1;
                    const upvalue_idx = code[pos];

                    try writer.print(
                        "               |  {s:>2}   {d}",
                        .{ local, upvalue_idx },
                    );
                }
            }
        },
        .load_nil,
        .load_true,
        .load_false,
        .load_empty_list,
        .load_one,
        .load_zero,
        .load_global,
        .define_global,
        .neg,
        .not,
        .pop,
        .import,
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

    var gc = Gc.init(tst.allocator);
    defer gc.deinit();

    var cpl = Compiler.init(tst.allocator, &gc);
    defer cpl.deinit();

    const func = try cpl.compile(input);

    const expected =
        \\=== <fn *main*> ===
        \\ Bytecode
        \\  0000    1    load_one
        \\  0001    1    load_constant     0
        \\  0003    1    add               2
        \\  0005    2    pop
        \\  0006    2    load_constant     1
        \\  0008    2    neg
        \\  0009    2    ret
        \\ Constants
        \\  0   number     2
        \\  1   number     -2
        \\
    ;

    var buf = Io.Writer.Allocating.init(tst.allocator);
    defer buf.deinit();

    try disassemble(&buf.writer, func);

    const actual = try buf.toOwnedSlice();
    defer tst.allocator.free(actual);

    try tst.expectEqualSlices(u8, expected, actual);
}
