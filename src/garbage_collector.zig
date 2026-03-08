const std = @import("std");
const mem = std.mem;
const tst = std.testing;
const Alignment = mem.Alignment;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const object = @import("object.zig");
const Object = object.Object;
const String = object.String;
const Symbol = object.Symbol;
const Keyword = object.Keyword;
const Func = object.Func;
const Node = object.Node;
const Upvalue = object.Upvalue;
const Closure = object.Closure;
const Value = @import("value.zig").Value;
const VirtualMachine = @import("virtual_machine.zig").VirtualMachine;

const gc_threshold_grow_factor = 2;
const gc_initial_threshold_bytes = 4096;

pub const GarbageCollector = struct {
    child_allocator: Allocator,
    enabled: bool = true,
    allocated_bytes: usize = 0,
    allocated_last: ?*Object = null,
    greystack: ArrayList(*Object) = .{},
    gc_threshold_bytes: usize = gc_initial_threshold_bytes,
    vm: ?*VirtualMachine = null,

    pub fn init(child_allocator: Allocator) GarbageCollector {
        return .{ .child_allocator = child_allocator };
    }

    pub fn deinit(self: *GarbageCollector) void {
        const tracking_allocator = self.allocator();
        var curr = self.allocated_last;
        while (curr) |obj| {
            curr = obj.next;
            obj.deinit(tracking_allocator);
        }
        self.greystack.deinit(self.child_allocator);
    }

    pub fn enable(self: *GarbageCollector) void {
        self.enabled = true;
    }

    pub fn disable(self: *GarbageCollector) void {
        self.enabled = false;
    }

    pub fn create(self: *GarbageCollector, comptime T: type, args: anytype) Allocator.Error!*T {
        try self.collect();
        const tracking_allocator = self.allocator();

        const result = try tracking_allocator.create(T);
        errdefer tracking_allocator.destroy(result);

        const init_fn = @typeInfo(@TypeOf(T.init)).@"fn";
        const takes_allocator = init_fn.params.len > 0 and init_fn.params[0].type == Allocator;
        const init_args = if (takes_allocator) .{tracking_allocator} ++ args else args;
        const is_fallible = if (init_fn.return_type) |rt| @typeInfo(rt) == .error_union else false;
        result.* = if (is_fallible) try @call(.auto, T.init, init_args) else @call(.auto, T.init, init_args);

        result.obj.next = self.allocated_last;
        self.allocated_last = &result.obj;

        return result;
    }

    pub fn collect(self: *GarbageCollector) Allocator.Error!void {
        if (!self.enabled or self.allocated_bytes < self.gc_threshold_bytes) return;
        try self.markRoots();
        try self.traceReferences();
        self.sweep();
        self.gc_threshold_bytes = self.allocated_bytes * gc_threshold_grow_factor;
    }

    fn markRoots(self: *GarbageCollector) Allocator.Error!void {
        if (self.vm) |vm| {
            for (vm.stack.items[0..vm.stack.pos]) |val|
                try self.markValue(val);

            var mod_iter = vm.module_cache.iterator();
            while (mod_iter.next()) |entry| {
                const mod = entry.value_ptr.*;
                // The GC is disabled during module compilation, after
                // compilation has finished there should always be a module
                // func.
                if (mod.func) |func|
                    try self.markObject(&func.obj)
                else
                    @panic("Encountered uncompiled module while marking roots.");
            }

            var global_iter = vm.globals.iterator();
            while (global_iter.next()) |entry| {
                const val = entry.value_ptr.*;
                try self.markValue(val);
            }

            for (vm.frames.items) |frame| {
                try switch (frame.callee) {
                    .func => |func| self.markObject(&func.obj),
                    .closure => |closure| self.markObject(&closure.obj),
                };
            }

            var curr = vm.open_upvalues;
            while (curr) |upvalue| : (curr = upvalue.next)
                try self.markObject(&upvalue.obj);
        }
    }

    fn traceReferences(self: *GarbageCollector) Allocator.Error!void {
        while (self.greystack.pop()) |obj| try self.blackenObject(obj);
    }

    fn markValue(self: *GarbageCollector, val: Value) Allocator.Error!void {
        switch (val) {
            .list => |list| if (list.head) |node| try self.markObject(&node.obj),
            .object => |obj| try self.markObject(obj),
            .nil, .boolean, .number => {},
        }
    }

    fn markObject(self: *GarbageCollector, obj: *Object) Allocator.Error!void {
        if (obj.marked) return;
        obj.marked = true;
        try self.greystack.append(self.child_allocator, obj);
    }

    fn blackenObject(self: *GarbageCollector, obj: *Object) Allocator.Error!void {
        switch (obj.tag) {
            .string, .symbol, .keyword, .native_func => {},
            .node => {
                const node = obj.as(Node);
                try self.markValue(node.value);
                if (node.next) |next_node|
                    try self.markObject(&next_node.obj);
            },
            .func => {
                const func = obj.as(Func);
                for (func.constants) |constant|
                    try self.markValue(constant);
            },
            .upvalue => {
                const upvalue = obj.as(Upvalue);
                if (upvalue.closed())
                    try self.markValue(upvalue.slot);
            },
            .closure => {
                const closure = obj.as(Closure);
                try self.markObject(&closure.func.obj);
                for (closure.upvalues) |upvalue| {
                    if (upvalue == null) break;
                    try self.markObject(&upvalue.?.obj);
                }
            },
        }
    }

    fn sweep(self: *GarbageCollector) void {
        const tracking_allocator = self.allocator();

        var prev: ?*Object = null;
        var curr: ?*Object = self.allocated_last;
        while (curr) |obj| {
            if (obj.marked) {
                obj.marked = false;
                prev = curr;
                curr = obj.next;
                continue;
            }

            if (prev) |prev_obj| {
                prev_obj.next = obj.next;
            } else {
                self.allocated_last = obj.next;
            }

            curr = obj.next;
            obj.deinit(tracking_allocator);
        }
    }

    pub fn allocator(self: *GarbageCollector) Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .remap = remap,
                .free = free,
            },
        };
    }

    fn alloc(ctx: *anyopaque, len: usize, alignment: Alignment, ret_addr: usize) ?[*]u8 {
        var self: *GarbageCollector = @ptrCast(@alignCast(ctx));
        const result = self.child_allocator.rawAlloc(len, alignment, ret_addr);
        if (result != null) self.allocated_bytes += len;
        return result;
    }

    fn resize(ctx: *anyopaque, memory: []u8, alignment: Alignment, new_len: usize, ret_addr: usize) bool {
        var self: *GarbageCollector = @ptrCast(@alignCast(ctx));
        const resized = self.child_allocator.rawResize(memory, alignment, new_len, ret_addr);
        if (resized) self.allocated_bytes = self.allocated_bytes - memory.len + new_len;
        return resized;
    }

    fn remap(ctx: *anyopaque, memory: []u8, alignment: Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
        var self: *GarbageCollector = @ptrCast(@alignCast(ctx));
        const result = self.child_allocator.rawRemap(memory, alignment, new_len, ret_addr);
        if (result != null) self.allocated_bytes = self.allocated_bytes - memory.len + new_len;
        return result;
    }

    fn free(ctx: *anyopaque, memory: []u8, alignment: Alignment, ret_addr: usize) void {
        var self: *GarbageCollector = @ptrCast(@alignCast(ctx));
        self.allocated_bytes -= memory.len;
        self.child_allocator.rawFree(memory, alignment, ret_addr);
    }
};

test "GC create object" {
    var gc = GarbageCollector.init(tst.allocator);
    defer gc.deinit();

    _ = try gc.create(String, .{"hello"});
    _ = try gc.create(String, .{"world"});
    _ = try gc.create(Symbol, .{"map"});
    _ = try gc.create(Symbol, .{"reduce"});
}
