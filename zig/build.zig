const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "airgradientz",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });

    exe.root_module.addCSourceFile(.{
        .file = b.path("../c/sqlite3.c"),
        .flags = &.{
            "-DSQLITE_THREADSAFE=1",
            "-DSQLITE_ENABLE_WAL=1",
        },
    });
    exe.root_module.addIncludePath(b.path("../c"));

    b.installArtifact(exe);

    const run_step = b.step("run", "Run the application");
    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // Test step — tests db.zig and http_server.zig via test_root.zig
    const test_mod = b.createModule(.{
        .root_source_file = b.path("src/test_root.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });

    const tests = b.addTest(.{
        .name = "tests",
        .root_module = test_mod,
    });

    tests.root_module.addCSourceFile(.{
        .file = b.path("../c/sqlite3.c"),
        .flags = &.{
            "-DSQLITE_THREADSAFE=1",
            "-DSQLITE_ENABLE_WAL=1",
        },
    });
    tests.root_module.addIncludePath(b.path("../c"));

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&b.addRunArtifact(tests).step);
}
