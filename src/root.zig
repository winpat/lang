pub const scanner = @import("scanner.zig");

test {
    @import("std").testing.refAllDecls(@This());
}
