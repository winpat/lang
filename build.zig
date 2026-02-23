const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const mod = b.addModule("lang", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
    });

    const exe = b.addExecutable(.{
        .name = "lang",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{.{ .name = "lang", .module = mod }},
        }),
    });
    b.installArtifact(exe);

    const opts = b.addOptions();
    opts.addOption(
        []const u8,
        "snapshot_dir",
        b.path("test/snapshots").getPath(b),
    );
    opts.addOption(
        bool,
        "update_snapshots",
        b.option(bool, "update_snapshots", "Update snapshots.") orelse false,
    );
    mod.addImport("opts", opts.createModule());

    // zig build run
    const run_step = b.step("run", "Run the app");
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| run_cmd.addArgs(args);
    run_step.dependOn(&run_cmd.step);

    // zig build test
    const test_step = b.step("test", "Run tests");
    const mod_tests = b.addTest(.{ .root_module = mod });
    const run_mod_tests = b.addRunArtifact(mod_tests);
    test_step.dependOn(&run_mod_tests.step);
}
