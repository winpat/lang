const std = @import("std");
const Allocator = std.mem.Allocator;
const Io = std.Io;

const Value = @import("value.zig").Value;

pub const Object = struct {
    pub const Tag = enum {
        node,
        string,
        symbol,
        func,
    };

    tag: Tag,

    pub fn deinit(self: *Object, allocator: Allocator) void {
        switch (self.tag) {
            .string => {
                const string = self.as(String);
                string.deinit(allocator);
                allocator.destroy(string);
            },
            .symbol => {
                const symbol = self.as(Symbol);
                symbol.deinit(allocator);
                allocator.destroy(symbol);
            },
            .func => {
                const func = self.as(Func);
                func.deinit(allocator);
                allocator.destroy(func);
            },

            else => {},
        }
    }

    pub fn as(self: *const Object, comptime T: type) *T {
        return @alignCast(@constCast(@fieldParentPtr("obj", self)));
    }

    pub fn format(self: *Object, writer: *Io.Writer) Io.Writer.Error!void {
        switch (self.tag) {
            .node => try writer.print("{f}", .{self.as(Node)}),
            .string => try writer.print("{f}", .{self.as(String)}),
            .symbol => try writer.print("{f}", .{self.as(String)}),
            .func => try writer.print("{f}", .{self.as(Func)}),
        }
    }

    pub fn equal(self: *const Object, other: *const Object) bool {
        if (self == other) return true;
        if (self.tag != other.tag) return false;
        return switch (self.tag) {
            .string => self.as(String).equal(other.as(String).*),
            .symbol => self.as(Symbol).equal(other.as(Symbol).*),
            .node, .func => false,
        };
    }
};

pub const Node = struct {
    obj: Object = .{ .tag = .node },
    value: Value,
    next: ?*Node = null,

    pub fn equal(self: Node, other: Node) bool {
        return self.value.equal(other.value);
    }

    pub fn format(self: Node, writer: *Io.Writer) Io.Writer.Error!void {
        try writer.print("<node {f}>", .{self.value});
    }
};

pub const String = struct {
    obj: Object = .{ .tag = .string },
    len: usize,
    ptr: [*]const u8,

    pub fn init(allocator: Allocator, chars: []const u8) Allocator.Error!String {
        const duped = try allocator.dupe(u8, chars);
        return .{ .len = duped.len, .ptr = duped.ptr };
    }

    pub fn deinit(self: String, allocator: Allocator) void {
        allocator.free(self.slice());
    }

    pub fn format(self: String, writer: *Io.Writer) Io.Writer.Error!void {
        try writer.print("{s}", .{self.slice()});
    }

    pub fn slice(self: String) []const u8 {
        return self.ptr[0..self.len];
    }

    pub fn equal(self: String, other: String) bool {
        return self.len == other.len and
            std.mem.eql(u8, self.ptr[0..self.len], other.ptr[0..other.len]);
    }
};

pub const Symbol = struct {
    obj: Object = .{ .tag = .symbol },
    len: usize,
    ptr: [*]const u8,

    pub fn init(allocator: Allocator, name: []const u8) Allocator.Error!Symbol {
        const duped = try allocator.dupe(u8, name);
        return .{ .len = duped.len, .ptr = duped.ptr };
    }

    pub fn deinit(self: Symbol, allocator: Allocator) void {
        allocator.free(self.slice());
    }

    pub fn format(self: Symbol, writer: *Io.Writer) Io.Writer.Error!void {
        try writer.print("{s}", .{self.slice()});
    }

    pub fn slice(self: Symbol) []const u8 {
        return self.ptr[0..self.len];
    }

    pub fn equal(self: Symbol, other: Symbol) bool {
        return self.len == other.len and
            std.mem.eql(u8, self.ptr[0..self.len], other.ptr[0..other.len]);
    }
};

pub const Func = struct {
    obj: Object = .{ .tag = .func },
    constants: []const Value,
    code: []const u8,
    lines: []const u32,

    pub fn deinit(self: *Func, allocator: Allocator) void {
        for (self.constants) |constant| constant.deinit(allocator);
        allocator.free(self.constants);
        allocator.free(self.code);
        allocator.free(self.lines);
    }

    pub fn format(self: *const Func, writer: *Io.Writer) Io.Writer.Error!void {
        try writer.print("<fn {*}>", .{self});
    }
};
