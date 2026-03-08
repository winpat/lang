const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Io = std.Io;

const Gc = @import("garbage_collector.zig").GarbageCollector;
const RuntimeError = @import("virtual_machine.zig").RuntimeError;
const Value = @import("value.zig").Value;

pub const Object = struct {
    pub const Tag = enum {
        node,
        string,
        symbol,
        keyword,
        func,
        native_func,
        upvalue,
        closure,
    };

    tag: Tag,
    marked: bool = false,
    next: ?*Object = null,

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
            .keyword => {
                const keyword = self.as(Keyword);
                keyword.deinit(allocator);
                allocator.destroy(keyword);
            },
            .func => {
                const func = self.as(Func);
                func.deinit(allocator);
                allocator.destroy(func);
            },
            .native_func => {
                const native_func = self.as(NativeFunc);
                allocator.destroy(native_func);
            },
            .node => {
                const node = self.as(Node);
                allocator.destroy(node);
            },
            .upvalue => {
                allocator.destroy(self.as(Upvalue));
            },
            .closure => {
                const closure = self.as(Closure);
                closure.deinit(allocator);
                allocator.destroy(closure);
            },
        }
    }

    pub fn as(self: *const Object, comptime T: type) *T {
        return @alignCast(@constCast(@fieldParentPtr("obj", self)));
    }

    pub fn typeByTag(tag: Tag) type {
        return switch (tag) {
            .func => Func,
            .native_func => NativeFunc,
            .string => String,
            .symbol => Symbol,
            .keyword => Keyword,
            .node => Node,
            .upvalue => Upvalue,
            .closure => Closure,
        };
    }

    pub fn format(self: *Object, writer: *Io.Writer) Io.Writer.Error!void {
        switch (self.tag) {
            .node => try writer.print("{f}", .{self.as(Node)}),
            .string => try writer.print("{f}", .{self.as(String)}),
            .symbol => try writer.print("{f}", .{self.as(Symbol)}),
            .keyword => try writer.print("{f}", .{self.as(Keyword)}),
            .func => try writer.print("{f}", .{self.as(Func)}),
            .native_func => try writer.print("{f}", .{self.as(NativeFunc)}),
            .upvalue => try writer.print("{f}", .{self.as(Upvalue)}),
            .closure => try writer.print("{f}", .{self.as(Closure)}),
        }
    }

    pub fn equal(self: *const Object, other: *const Object) bool {
        if (self == other) return true;
        if (self.tag != other.tag) return false;
        return switch (self.tag) {
            .string => self.as(String).equal(other.as(String).*),
            .symbol => self.as(Symbol).equal(other.as(Symbol).*),
            .keyword => self.as(Keyword).equal(other.as(Keyword).*),
            .node, .func, .native_func, .upvalue, .closure => false,
        };
    }
};

pub const Node = struct {
    obj: Object = .{ .tag = .node },
    value: Value,
    next: ?*Node = null,

    pub fn init(val: Value, next: ?*Node) Allocator.Error!Node {
        return .{ .value = val, .next = next };
    }

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

pub const Keyword = struct {
    obj: Object = .{ .tag = .keyword },
    len: usize,
    ptr: [*]const u8,

    pub fn init(allocator: Allocator, name: []const u8) Allocator.Error!Keyword {
        const duped = try allocator.dupe(u8, name);
        return .{ .len = duped.len, .ptr = duped.ptr };
    }

    pub fn deinit(self: Keyword, allocator: Allocator) void {
        allocator.free(self.slice());
    }

    pub fn format(self: Keyword, writer: *Io.Writer) Io.Writer.Error!void {
        try writer.print(":{s}", .{self.slice()});
    }

    pub fn slice(self: Keyword) []const u8 {
        return self.ptr[0..self.len];
    }

    pub fn equal(self: Keyword, other: Keyword) bool {
        return self.len == other.len and
            std.mem.eql(u8, self.ptr[0..self.len], other.ptr[0..other.len]);
    }
};

const func_name_fallback = "*unknown*";

pub const Func = struct {
    obj: Object = .{ .tag = .func },
    name: ?[]const u8 = null,
    arity: u8,
    constants: []const Value,
    code: []const u8,
    lines: []const u32,
    upvalue_count: u8,

    pub fn init(
        allocator: Allocator,
        name: ?[]const u8,
        arity: u8,
        constants: []const Value,
        code: []const u8,
        lines: []const u32,
        upvalue_count: u8,
    ) Allocator.Error!Func {
        return .{
            .name = if (name != null) try allocator.dupe(u8, name.?) else null,
            .arity = arity,
            .constants = try allocator.dupe(Value, constants),
            .code = try allocator.dupe(u8, code),
            .lines = try allocator.dupe(u32, lines),
            .upvalue_count = upvalue_count,
        };
    }

    pub fn deinit(self: *Func, allocator: Allocator) void {
        if (self.name) |name| allocator.free(name);
        allocator.free(self.constants);
        allocator.free(self.code);
        allocator.free(self.lines);
    }

    pub fn format(self: *const Func, writer: *Io.Writer) Io.Writer.Error!void {
        if (self.name) |name|
            try writer.print("<fn {s}>", .{name})
        else
            try writer.print("<fn {s}>", .{func_name_fallback});
    }
};

pub const NativeFunc = struct {
    obj: Object = .{ .tag = .native_func },
    name: []const u8,
    impl: ImplPtr,
    pub const ImplPtr = *const fn (ctx: Context, args: []const Value) RuntimeError!Value;
    pub const Context = struct {
        gc: *Gc,
        stdin: *Io.Reader,
        stdout: *Io.Writer,
        stderr: *Io.Writer,
    };

    pub fn init(name: []const u8, impl: ImplPtr) NativeFunc {
        return .{ .name = name, .impl = impl };
    }

    pub fn format(self: *const NativeFunc, writer: *Io.Writer) Io.Writer.Error!void {
        try writer.print("<native fn {s}>", .{self.name});
    }
};

pub const Upvalue = struct {
    obj: Object = .{ .tag = .upvalue },
    slot: Value = .{ .nil = {} },
    location: *Value,
    next: ?*Upvalue = null,

    pub fn init(val: *Value) Upvalue {
        return .{ .location = val };
    }

    pub fn closed(self: *const Upvalue) bool {
        return self.location == &self.slot;
    }

    pub fn format(self: Upvalue, writer: *Io.Writer) Io.Writer.Error!void {
        _ = self;
        try writer.writeAll("<upvalue>");
    }
};

pub const Closure = struct {
    obj: Object = .{ .tag = .closure },
    func: *Func,
    upvalues: []?*Upvalue,

    pub fn init(allocator: Allocator, func: *Func) Allocator.Error!Closure {
        const upvalues = try allocator.alloc(?*Upvalue, func.upvalue_count);
        @memset(upvalues, null);
        return .{
            .func = func,
            .upvalues = upvalues,
        };
    }

    pub fn deinit(self: *Closure, allocator: Allocator) void {
        allocator.free(self.upvalues);
    }

    pub fn format(self: Closure, writer: *Io.Writer) Io.Writer.Error!void {
        const func_name = if (self.func.name) |name| name else func_name_fallback;
        try writer.print("<closure {s}>", .{func_name});
    }
};
