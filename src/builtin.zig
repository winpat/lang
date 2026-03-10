const std = @import("std");
const tst = std.testing;
const Allocator = std.mem.Allocator;
const fs = std.fs;
const Io = std.Io;

const Gc = @import("garbage_collector.zig").GarbageCollector;
const o = @import("object.zig");
const Node = o.Node;
const String = o.String;
const Vector = o.Vector;
const NativeFunc = o.NativeFunc;
const Context = NativeFunc.Context;
const RuntimeError = @import("virtual_machine.zig").RuntimeError;
const v = @import("value.zig");
const Value = v.Value;
const List = v.List;

pub const funcs = [_]struct { name: []const u8, impl: NativeFunc.ImplPtr }{
    // Value predicates
    .{ .name = "nil?", .impl = isNil },
    .{ .name = "number?", .impl = isNumber },
    .{ .name = "boolean?", .impl = isBoolean },
    .{ .name = "list?", .impl = isList },
    .{ .name = "object?", .impl = isObject },
    // Object predicates
    .{ .name = "string?", .impl = isString },
    .{ .name = "symbol?", .impl = isSymbol },
    .{ .name = "keyword?", .impl = isKeyword },
    .{ .name = "vector?", .impl = isVector },
    .{ .name = "function?", .impl = isFunction },
    // Sequence
    .{ .name = "list", .impl = list },
    .{ .name = "count", .impl = count },
    .{ .name = "conj", .impl = conj },
    .{ .name = "concat", .impl = concat },
    .{ .name = "nth", .impl = nth },
    .{ .name = "first", .impl = first },
    .{ .name = "second", .impl = second },
    .{ .name = "rest", .impl = rest },
    .{ .name = "last", .impl = last },
    // Misc
    .{ .name = "min", .impl = min },
    .{ .name = "max", .impl = max },
    // IO
    .{ .name = "print", .impl = print },
};

pub fn nil() Value {
    return .{ .nil = {} };
}

pub fn boolean(b: v.Boolean) Value {
    return .{ .boolean = b };
}

pub fn num(n: v.Number) Value {
    return .{ .number = n };
}

pub fn empty_list() Value {
    return .{ .list = .{} };
}

pub fn object(obj: *o.Object) Value {
    return .{ .object = obj };
}

pub fn valueOfType(comptime tag: v.Value.Tag) NativeFunc.ImplPtr {
    return struct {
        fn impl(ctx: Context, args: []const Value) RuntimeError!Value {
            _ = ctx;
            if (args.len != 1) return RuntimeError.UnsupportedArity;
            return boolean(args[0] == tag);
        }
    }.impl;
}

const isNil = valueOfType(.nil);

test "nil?" {
    try expectCases(isNil, &.{
        .{ &.{nil()}, boolean(true) },
        .{ &.{boolean(true)}, boolean(false) },
        .{ &.{}, RuntimeError.UnsupportedArity },
        .{ &.{ nil(), nil() }, RuntimeError.UnsupportedArity },
    });
}

const isNumber = valueOfType(.number);

test "number?" {
    try expectCases(isNumber, &.{
        .{ &.{num(1)}, boolean(true) },
        .{ &.{boolean(true)}, boolean(false) },
        .{ &.{}, RuntimeError.UnsupportedArity },
        .{ &.{ nil(), nil() }, RuntimeError.UnsupportedArity },
    });
}

const isBoolean = valueOfType(.boolean);

test "boolean?" {
    try expectCases(isBoolean, &.{
        .{ &.{boolean(true)}, boolean(true) },
        .{ &.{boolean(false)}, boolean(true) },
        .{ &.{nil()}, boolean(false) },
        .{ &.{}, RuntimeError.UnsupportedArity },
        .{ &.{ nil(), nil() }, RuntimeError.UnsupportedArity },
    });
}

const isList = valueOfType(.list);

test "list?" {
    try expectCases(isList, &.{
        .{ &.{empty_list()}, boolean(true) },
        .{ &.{nil()}, boolean(false) },
        .{ &.{}, RuntimeError.UnsupportedArity },
        .{ &.{ nil(), nil() }, RuntimeError.UnsupportedArity },
    });
}

const isObject = valueOfType(.object);

test "object?" {
    var string_obj = o.Object{ .tag = .string };

    try expectCases(isObject, &.{
        .{ &.{object(&string_obj)}, boolean(true) },
        .{ &.{empty_list()}, boolean(false) },
        .{ &.{}, RuntimeError.UnsupportedArity },
        .{ &.{ nil(), nil() }, RuntimeError.UnsupportedArity },
    });
}

pub fn objectOfType(comptime tag: o.Object.Tag) NativeFunc.ImplPtr {
    return struct {
        fn impl(ctx: Context, args: []const Value) RuntimeError!Value {
            _ = ctx;
            if (args.len != 1) return RuntimeError.UnsupportedArity;
            const val = args[0];
            return boolean(val == .object and val.object.tag == tag);
        }
    }.impl;
}

const isString = objectOfType(.string);

test "string?" {
    var string_obj = o.Object{ .tag = .string };

    try expectCases(isString, &.{
        .{ &.{object(&string_obj)}, boolean(true) },
        .{ &.{empty_list()}, boolean(false) },
        .{ &.{}, RuntimeError.UnsupportedArity },
        .{ &.{ nil(), nil() }, RuntimeError.UnsupportedArity },
    });
}

const isSymbol = objectOfType(.symbol);

test "symbol?" {
    var symbol_obj = o.Object{ .tag = .symbol };

    try expectCases(isSymbol, &.{
        .{ &.{object(&symbol_obj)}, boolean(true) },
        .{ &.{empty_list()}, boolean(false) },
        .{ &.{}, RuntimeError.UnsupportedArity },
        .{ &.{ nil(), nil() }, RuntimeError.UnsupportedArity },
    });
}

const isKeyword = objectOfType(.keyword);

test "keyword?" {
    var keyword_obj = o.Object{ .tag = .keyword };

    try expectCases(isKeyword, &.{
        .{ &.{object(&keyword_obj)}, boolean(true) },
        .{ &.{empty_list()}, boolean(false) },
        .{ &.{}, RuntimeError.UnsupportedArity },
        .{ &.{ nil(), nil() }, RuntimeError.UnsupportedArity },
    });
}

const isVector = objectOfType(.vector);

test "vector?" {
    var vec_obj = o.Object{ .tag = .vector };

    try expectCases(isVector, &.{
        .{ &.{object(&vec_obj)}, boolean(true) },
        .{ &.{empty_list()}, boolean(false) },
        .{ &.{}, RuntimeError.UnsupportedArity },
        .{ &.{ nil(), nil() }, RuntimeError.UnsupportedArity },
    });
}

fn isFunction(ctx: Context, args: []const Value) RuntimeError!Value {
    _ = ctx;
    if (args.len != 1) return RuntimeError.UnsupportedArity;
    const val = args[0];
    if (val != .object) return boolean(false);
    const obj = val.object;
    return boolean(obj.tag == .func or obj.tag == .closure or obj.tag == .native_func);
}

test "function?" {
    var func_obj = o.Object{ .tag = .func };
    var closure_obj = o.Object{ .tag = .closure };
    var native_func_obj = o.Object{ .tag = .native_func };

    try expectCases(isFunction, &.{
        .{ &.{object(&func_obj)}, boolean(true) },
        .{ &.{object(&closure_obj)}, boolean(true) },
        .{ &.{object(&native_func_obj)}, boolean(true) },
        .{ &.{empty_list()}, boolean(false) },
        .{ &.{}, RuntimeError.UnsupportedArity },
        .{ &.{ nil(), nil() }, RuntimeError.UnsupportedArity },
    });
}

fn list(ctx: Context, args: []const Value) RuntimeError!Value {
    const gc = ctx.gc;

    gc.disable();
    defer gc.enable();

    var node: ?*Node = null;
    var n = args.len;
    while (n > 0) : (n -= 1) {
        const new_node = try gc.create(Node, .{ args[n - 1], node });
        node = new_node;
    }

    return .{ .list = .{ .head = node } };
}

test "list" {
    var nil_node = Node{ .value = nil() };
    try expectCases(list, &.{
        .{ &.{}, empty_list() },
        .{ &.{nil()}, .{ .list = .{ .head = &nil_node } } },
    });
}

fn first(ctx: Context, args: []const Value) RuntimeError!Value {
    _ = ctx;
    if (args.len != 1) return RuntimeError.UnsupportedArity;
    if (args[0] != .list) return RuntimeError.ValueTypeMismatch;

    if (args[0].list.head) |head| {
        return head.value;
    } else {
        return nil();
    }
}

test "first" {
    var num_node = Node{ .value = num(1) };
    try expectCases(first, &.{
        .{ &.{empty_list()}, nil() },
        .{ &.{.{ .list = .{ .head = &num_node } }}, num(1) },
    });
}

fn second(ctx: Context, args: []const Value) RuntimeError!Value {
    _ = ctx;
    if (args.len != 1) return RuntimeError.UnsupportedArity;
    if (args[0] != .list) return RuntimeError.ValueTypeMismatch;

    const second_node = if (args[0].list.head) |head| head.next else null;
    return if (second_node) |node| node.value else nil();
}

test "second" {
    var second_node = Node{ .value = num(2) };
    var first_node = Node{ .value = num(1), .next = &second_node };
    var lone_node = Node{ .value = num(1) };
    try expectCases(second, &.{
        .{ &.{empty_list()}, nil() },
        .{ &.{.{ .list = .{ .head = &lone_node } }}, nil() },
        .{ &.{.{ .list = .{ .head = &first_node } }}, num(2) },
        .{ &.{}, RuntimeError.UnsupportedArity },
        .{ &.{ nil(), nil() }, RuntimeError.UnsupportedArity },
    });
}

fn rest(ctx: Context, args: []const Value) RuntimeError!Value {
    _ = ctx;
    if (args.len != 1) return RuntimeError.UnsupportedArity;
    if (args[0] != .list) return RuntimeError.ValueTypeMismatch;

    const next = if (args[0].list.head) |head| head.next else null;
    return .{ .list = .{ .head = next } };
}

test "rest" {
    var third_node = Node{ .value = num(3) };
    var second_node = Node{ .value = num(2), .next = &third_node };
    var first_node = Node{ .value = num(1), .next = &second_node };
    try expectCases(rest, &.{
        .{ &.{empty_list()}, empty_list() },
        .{ &.{.{ .list = .{ .head = &first_node } }}, .{ .list = .{ .head = &second_node } } },
        .{ &.{}, RuntimeError.UnsupportedArity },
        .{ &.{ nil(), nil() }, RuntimeError.UnsupportedArity },
    });
}

fn last(ctx: Context, args: []const Value) RuntimeError!Value {
    _ = ctx;
    if (args.len != 1) return RuntimeError.UnsupportedArity;
    if (args[0] != .list) return RuntimeError.ValueTypeMismatch;

    var node = args[0].list.head orelse return nil();
    while (node.next) |next| node = next;
    return node.value;
}

test "last" {
    var third_node = Node{ .value = num(3) };
    var second_node = Node{ .value = num(2), .next = &third_node };
    var first_node = Node{ .value = num(1), .next = &second_node };
    try expectCases(last, &.{
        .{ &.{empty_list()}, nil() },
        .{ &.{.{ .list = .{ .head = &first_node } }}, num(3) },
        .{ &.{}, RuntimeError.UnsupportedArity },
        .{ &.{ nil(), nil() }, RuntimeError.UnsupportedArity },
    });
}

fn nth(ctx: Context, args: []const Value) RuntimeError!Value {
    _ = ctx;
    if (args.len != 2) return RuntimeError.UnsupportedArity;
    if (args[1] != .number) return RuntimeError.ValueTypeMismatch;

    const idx_f = args[1].number;
    if (idx_f < 0 or idx_f != @trunc(idx_f)) return RuntimeError.IndexOutOfBounds;
    const idx: usize = @intFromFloat(idx_f);

    switch (args[0]) {
        .list => |lst| {
            var curr = lst.head;
            var i: usize = 0;
            while (curr) |node| : ({
                curr = node.next;
                i += 1;
            }) {
                if (i == idx) return node.value;
            }
            return RuntimeError.IndexOutOfBounds;
        },
        .object => |obj| switch (obj.tag) {
            .vector => {
                const vec = obj.as(Vector);
                if (idx >= vec.items.len) return RuntimeError.IndexOutOfBounds;
                return vec.items[idx];
            },
            else => return RuntimeError.ValueTypeMismatch,
        },
        else => return RuntimeError.ValueTypeMismatch,
    }
}

test "nth" {
    var third_node = Node{ .value = num(3) };
    var second_node = Node{ .value = num(2), .next = &third_node };
    var first_node = Node{ .value = num(1), .next = &second_node };

    var vec = Vector{ .items = &.{ num(10), num(20), num(30) } };

    try expectCases(nth, &.{
        // List access
        .{ &.{ .{ .list = .{ .head = &first_node } }, num(0) }, num(1) },
        .{ &.{ .{ .list = .{ .head = &first_node } }, num(2) }, num(3) },
        .{ &.{ .{ .list = .{ .head = &first_node } }, num(3) }, RuntimeError.IndexOutOfBounds },
        .{ &.{ empty_list(), num(0) }, RuntimeError.IndexOutOfBounds },
        // Vector access
        .{ &.{ object(&vec.obj), num(0) }, num(10) },
        .{ &.{ object(&vec.obj), num(2) }, num(30) },
        .{ &.{ object(&vec.obj), num(3) }, RuntimeError.IndexOutOfBounds },
        // Negative and non-integer indices
        .{ &.{ object(&vec.obj), num(-1) }, RuntimeError.IndexOutOfBounds },
        .{ &.{ object(&vec.obj), num(1.5) }, RuntimeError.IndexOutOfBounds },
        // Arity and type errors
        .{ &.{}, RuntimeError.UnsupportedArity },
        .{ &.{empty_list()}, RuntimeError.UnsupportedArity },
        .{ &.{ num(1), num(0) }, RuntimeError.ValueTypeMismatch },
        .{ &.{ empty_list(), boolean(true) }, RuntimeError.ValueTypeMismatch },
    });
}

fn count(ctx: Context, args: []const Value) RuntimeError!Value {
    _ = ctx;
    if (args.len != 1) return RuntimeError.UnsupportedArity;
    switch (args[0]) {
        .list => |lst| {
            var n: v.Number = 0;
            var curr = lst.head;
            while (curr) |node| : (curr = node.next) n += 1;
            return num(n);
        },
        .object => |obj| switch (obj.tag) {
            .string => return num(@floatFromInt(obj.as(String).len)),
            .vector => return num(@floatFromInt(obj.as(Vector).items.len)),
            else => return RuntimeError.ValueTypeMismatch,
        },
        else => return RuntimeError.ValueTypeMismatch,
    }
}

test "count" {
    var third_node = Node{ .value = num(3) };
    var second_node = Node{ .value = num(2), .next = &third_node };
    var first_node = Node{ .value = num(1), .next = &second_node };

    const hello = "hello";
    var hello_str = String{ .len = hello.len, .ptr = hello.ptr };

    var vec = Vector{ .items = &.{} };

    try expectCases(count, &.{
        .{ &.{empty_list()}, num(0) },
        .{ &.{.{ .list = .{ .head = &first_node } }}, num(3) },
        .{ &.{object(&hello_str.obj)}, num(5) },
        .{ &.{object(&vec.obj)}, num(0) },
        .{ &.{}, RuntimeError.UnsupportedArity },
        .{ &.{ nil(), nil() }, RuntimeError.UnsupportedArity },
    });
}

fn conj(ctx: Context, args: []const Value) RuntimeError!Value {
    if (args.len != 2) return RuntimeError.UnsupportedArity;
    switch (args[0]) {
        .list => {
            const new_node = try ctx.gc.create(Node, .{ args[1], args[0].list.head });
            return .{ .list = .{ .head = new_node } };
        },
        .object => |obj| switch (obj.tag) {
            .vector => {
                const vec = obj.as(Vector);
                const new_vec = try vec.append(ctx.gc, args[1]);
                return .{ .object = &new_vec.obj };
            },
            else => return RuntimeError.ValueTypeMismatch,
        },
        else => return RuntimeError.ValueTypeMismatch,
    }
}

test "conj" {
    var gc = Gc.init(tst.allocator);
    defer gc.deinit();

    var stdin_reader = fs.File.stdin().reader(&.{});
    var stdout_writer = fs.File.stdout().writer(&.{});
    var stderr_writer = fs.File.stderr().writer(&.{});

    const ctx = Context{
        .gc = &gc,
        .stdin = &stdin_reader.interface,
        .stdout = &stdout_writer.interface,
        .stderr = &stderr_writer.interface,
    };

    // Conj on to empty list
    {
        const result = try conj(ctx, &.{ empty_list(), num(1) });
        try tst.expectEqual(num(1), result.list.head.?.value);
        try tst.expect(result.list.head.?.next == null);
    }

    // Conj on to non-empty list
    {
        var existing = Node{ .value = num(2) };
        const result = try conj(ctx, &.{ .{ .list = .{ .head = &existing } }, num(1) });
        try tst.expectEqual(num(1), result.list.head.?.value);
        try tst.expectEqual(num(2), result.list.head.?.next.?.value);
    }

    // Conj on to empty vector
    {
        var vec = Vector{ .items = &.{} };
        const vec_val = Value{ .object = &vec.obj };

        const result = try conj(ctx, &.{ vec_val, num(1) });
        const new_vec = result.object.as(Vector);

        try tst.expectEqual(1, new_vec.items.len);
        try tst.expectEqualSlices(Value, &.{num(1)}, new_vec.items);
    }

    // Conj on to non-empty vector
    {
        var vec = Vector{ .items = &.{num(1)} };
        const vec_val = Value{ .object = &vec.obj };

        const result = try conj(ctx, &.{ vec_val, num(2) });
        const new_vec = result.object.as(Vector);

        try tst.expectEqual(2, new_vec.items.len);
        try tst.expectEqualSlices(Value, &.{ num(1), num(2) }, new_vec.items);
    }

    try expectCases(conj, &.{
        .{ &.{}, RuntimeError.UnsupportedArity },
        .{ &.{ num(1), num(2) }, RuntimeError.ValueTypeMismatch },
    });
}

fn min(ctx: Context, args: []const Value) RuntimeError!Value {
    _ = ctx;
    if (args.len == 0) return RuntimeError.UnsupportedArity;
    if (args[0] != .number) return RuntimeError.ValueTypeMismatch;
    var result = args[0].number;
    for (args[1..]) |arg| {
        if (arg != .number) return RuntimeError.ValueTypeMismatch;
        if (arg.number < result) result = arg.number;
    }
    return num(result);
}

test "min" {
    try expectCases(min, &.{
        .{ &.{num(3)}, num(3) },
        .{ &.{ num(3), num(1), num(2) }, num(1) },
        .{ &.{}, RuntimeError.UnsupportedArity },
        .{ &.{boolean(true)}, RuntimeError.ValueTypeMismatch },
    });
}

fn max(ctx: Context, args: []const Value) RuntimeError!Value {
    _ = ctx;
    if (args.len == 0) return RuntimeError.UnsupportedArity;
    if (args[0] != .number) return RuntimeError.ValueTypeMismatch;
    var result = args[0].number;
    for (args[1..]) |arg| {
        if (arg != .number) return RuntimeError.ValueTypeMismatch;
        if (arg.number > result) result = arg.number;
    }
    return num(result);
}

test "max" {
    try expectCases(max, &.{
        .{ &.{num(3)}, num(3) },
        .{ &.{ num(1), num(3), num(2) }, num(3) },
        .{ &.{}, RuntimeError.UnsupportedArity },
        .{ &.{boolean(true)}, RuntimeError.ValueTypeMismatch },
    });
}

fn concat(ctx: Context, args: []const Value) RuntimeError!Value {
    if (args.len == 0) return empty_list();

    const gc = ctx.gc;
    const allocator = gc.allocator();

    switch (args[0]) {
        .list => {
            for (args) |arg|
                if (arg != .list) return RuntimeError.ValueTypeMismatch;

            // Collect all values from all lists into a flat temp slice.
            var total: usize = 0;
            for (args) |arg| {
                var node = arg.list.head;
                while (node) |n| : (node = n.next) total += 1;
            }

            const vals = try allocator.alloc(Value, total);
            defer allocator.free(vals);

            var idx: usize = 0;
            for (args) |arg| {
                var node = arg.list.head;
                while (node) |n| : (node = n.next) {
                    vals[idx] = n.value;
                    idx += 1;
                }
            }

            // Build the new list back to front so each node's next is already set.
            gc.disable();
            defer gc.enable();

            var head: ?*Node = null;
            var i = total;
            while (i > 0) : (i -= 1) {
                head = try gc.create(Node, .{ vals[i - 1], head });
            }
            return .{ .list = .{ .head = head } };
        },
        .object => |obj| {
            if (obj.tag != .string) return RuntimeError.ValueTypeMismatch;
            for (args[1..]) |arg|
                if (arg != .object or arg.object.tag != .string) return RuntimeError.ValueTypeMismatch;

            var total_len: usize = 0;
            for (args) |arg| total_len += arg.object.as(String).len;

            const buf = try allocator.alloc(u8, total_len);
            defer allocator.free(buf);

            var pos: usize = 0;
            for (args) |arg| {
                const s = arg.object.as(String).slice();
                @memcpy(buf[pos..][0..s.len], s);
                pos += s.len;
            }

            const result = try gc.create(String, .{buf});
            return .{ .object = &result.obj };
        },
        else => return RuntimeError.ValueTypeMismatch,
    }
}

test "concat strings" {
    var gc = Gc.init(tst.allocator);
    defer gc.deinit();

    var stdin_reader = fs.File.stdin().reader(&.{});
    var stdout_writer = fs.File.stdout().writer(&.{});
    var stderr_writer = fs.File.stderr().writer(&.{});

    const ctx = Context{
        .gc = &gc,
        .stdin = &stdin_reader.interface,
        .stdout = &stdout_writer.interface,
        .stderr = &stderr_writer.interface,
    };

    const hello = "hello";
    const world = " world";
    var hello_str = String{ .len = hello.len, .ptr = hello.ptr };
    var world_str = String{ .len = world.len, .ptr = world.ptr };

    const single = try concat(ctx, &.{object(&hello_str.obj)});
    try tst.expectEqualStrings("hello", single.object.as(String).slice());

    const two = try concat(ctx, &.{ object(&hello_str.obj), object(&world_str.obj) });
    try tst.expectEqualStrings("hello world", two.object.as(String).slice());

    try expectCases(concat, &.{
        .{ &.{num(1)}, RuntimeError.ValueTypeMismatch },
    });
}

test "concat lists" {
    var gc = Gc.init(tst.allocator);
    defer gc.deinit();

    var stdin_reader = fs.File.stdin().reader(&.{});
    var stdout_writer = fs.File.stdout().writer(&.{});
    var stderr_writer = fs.File.stderr().writer(&.{});

    const ctx = Context{
        .gc = &gc,
        .stdin = &stdin_reader.interface,
        .stdout = &stdout_writer.interface,
        .stderr = &stderr_writer.interface,
    };

    // (concat) => ()
    try tst.expectEqual(empty_list(), try concat(ctx, &.{}));

    // (concat '(1 2) '(3 4)) => (1 2 3 4)
    var b = Node{ .value = num(2) };
    var a = Node{ .value = num(1), .next = &b };
    var d = Node{ .value = num(4) };
    var c = Node{ .value = num(3), .next = &d };
    const result = try concat(ctx, &.{
        .{ .list = .{ .head = &a } },
        .{ .list = .{ .head = &c } },
    });
    try tst.expectEqual(num(1), result.list.head.?.value);
    try tst.expectEqual(num(2), result.list.head.?.next.?.value);
    try tst.expectEqual(num(3), result.list.head.?.next.?.next.?.value);
    try tst.expectEqual(num(4), result.list.head.?.next.?.next.?.next.?.value);
    try tst.expect(result.list.head.?.next.?.next.?.next.?.next == null);

    // (concat '() '(1)) => (1)
    var lone = Node{ .value = num(1) };
    const with_empty = try concat(ctx, &.{ empty_list(), .{ .list = .{ .head = &lone } } });
    try tst.expectEqual(num(1), with_empty.list.head.?.value);

    try expectCases(concat, &.{
        .{ &.{ empty_list(), num(1) }, RuntimeError.ValueTypeMismatch },
    });
}

fn print(ctx: Context, args: []const Value) RuntimeError!Value {
    const stdout = ctx.stdout;
    for (args, 0..) |arg, idx| {
        if (idx > 0) try stdout.writeByte(' ');
        try stdout.print("{f}", .{arg});
    }
    try stdout.writeByte('\n');
    try stdout.flush();
    return nil();
}

test "print" {
    var gc = Gc.init(tst.allocator);
    defer gc.deinit();

    var stdin_reader = fs.File.stdin().reader(&.{});
    var stderr_writer = fs.File.stderr().writer(&.{});

    var stdout_writer = Io.Writer.Allocating.init(tst.allocator);
    defer stdout_writer.deinit();

    const ctx = Context{
        .gc = &gc,
        .stdin = &stdin_reader.interface,
        .stdout = &stdout_writer.writer,
        .stderr = &stderr_writer.interface,
    };

    const result = try print(ctx, &.{ num(1), num(2) });
    try tst.expectEqual(nil(), result);

    const output = try stdout_writer.toOwnedSlice();
    defer tst.allocator.free(output);

    try tst.expectEqualSlices(u8, "1 2\n", output);
}

const Case = struct { []const Value, RuntimeError!Value };

fn expectCases(impl: NativeFunc.ImplPtr, cases: []const Case) !void {
    for (cases) |case| {
        const args = case[0];
        const expected = case[1];
        try if (expected) |val|
            expectValue(impl, args, val)
        else |err|
            expectRuntimeError(impl, args, err);
    }
}

fn expectValue(impl: NativeFunc.ImplPtr, args: []const Value, expected: Value) !void {
    var gc = Gc.init(tst.allocator);
    defer gc.deinit();

    var stdin_reader = fs.File.stdin().reader(&.{});
    var stdout_writer = fs.File.stdout().writer(&.{});
    var stderr_writer = fs.File.stderr().writer(&.{});

    const ctx = Context{
        .gc = &gc,
        .stdin = &stdin_reader.interface,
        .stdout = &stdout_writer.interface,
        .stderr = &stderr_writer.interface,
    };

    try tst.expectEqualDeep(expected, try impl(ctx, args));
}

fn expectRuntimeError(impl: NativeFunc.ImplPtr, args: []const Value, expected: RuntimeError) !void {
    var gc = Gc.init(tst.allocator);
    defer gc.deinit();

    var stdin_reader = fs.File.stdin().reader(&.{});
    var stdout_writer = fs.File.stdout().writer(&.{});
    var stderr_writer = fs.File.stderr().writer(&.{});

    const ctx = Context{
        .gc = &gc,
        .stdin = &stdin_reader.interface,
        .stdout = &stdout_writer.interface,
        .stderr = &stderr_writer.interface,
    };

    try tst.expectError(expected, impl(ctx, args));
}
