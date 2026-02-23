const std = @import("std");
const tst = std.testing;
const Io = std.Io;
const Allocator = std.mem.Allocator;

const obj = @import("object.zig");
const Object = obj.Object;
const Node = obj.Node;

pub const Value = union(enum) {
    nil: Nil,
    boolean: Boolean,
    number: Number,
    list: List,
    object: *Object,

    pub fn deinit(self: Value, allocator: Allocator) void {
        switch (self) {
            .object => |object| object.deinit(allocator),
            else => {},
        }
    }

    pub fn format(self: Value, writer: *Io.Writer) Io.Writer.Error!void {
        switch (self) {
            .number => |number| try writer.print("{d}", .{number}),
            .boolean => |boolean| try writer.print("{any}", .{boolean}),
            .nil => try writer.writeAll("nil"),
            .list => |list| try writer.print("{f}", .{list}),
            .object => |object| try writer.print("{f}", .{object}),
        }
    }

    pub fn equal(self: Value, other: Value) bool {
        return switch (self) {
            .nil => other == .nil,
            .number => other == .number and self.number == other.number,
            .boolean => other == .boolean and self.boolean == other.boolean,
            .list => other == .list and self.list.equal(other.list),
            .object => other == .object and self.object.equal(other.object),
        };
    }

    pub fn truthy(self: Value) bool {
        return !self.falsy();
    }

    pub fn falsy(self: Value) bool {
        return self == .nil or (self == .boolean and self.boolean == false);
    }
};

const Nil = void;
const Boolean = bool;
const Number = f64;

pub const List = struct {
    head: ?*Node = null,

    pub fn format(self: List, writer: *Io.Writer) Io.Writer.Error!void {
        try writer.writeAll("(");
        var curr = self.head;
        while (curr) |node| : (curr = node.next) {
            try writer.print("{f}", .{node.value});
            if (node.next != null) try writer.writeAll(" ");
        }
        try writer.writeAll(")");
    }

    pub fn equal(self: List, other: List) bool {
        var a = self.head;
        var b = other.head;

        while (a) |node_a| {
            const node_b = b orelse return false;
            if (!node_a.equal(node_b.*)) return false;
            a = node_a.next;
            b = node_b.next;
        }

        return b == null;
    }
};

test "Truthiness and falsiness" {
    const nil: Value = .{ .nil = {} };
    try tst.expect(!nil.truthy());
    try tst.expect(nil.falsy());

    const boolean_true: Value = .{ .boolean = true };
    try tst.expect(boolean_true.truthy());
    try tst.expect(!boolean_true.falsy());

    const boolean_false: Value = .{ .boolean = false };
    try tst.expect(!boolean_false.truthy());
    try tst.expect(boolean_false.falsy());

    const number: Value = .{ .number = 0 };
    try tst.expect(number.truthy());
    try tst.expect(!number.falsy());

    const empty_list: Value = .{ .list = .{} };
    try tst.expect(empty_list.truthy());
    try tst.expect(!empty_list.falsy());
}

test "Equality" {
    var a: Value = .{ .list = .{} };
    const b: Value = .{ .list = .{} };

    try tst.expect(a.equal(b));

    var node_num_1 = Node{ .value = .{ .number = 1 } };
    a.list.head = &node_num_1;

    try tst.expect(!a.equal(b));
}
