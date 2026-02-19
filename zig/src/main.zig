const std = @import("std");
const config_mod = @import("config.zig");
const db_mod = @import("db.zig");
const poller_mod = @import("poller.zig");
const http_server = @import("http_server.zig");
const time_c = @cImport(@cInclude("time.h"));

pub const std_options: std.Options = .{
    .logFn = timestampedLog,
};

fn timestampedLog(
    comptime level: std.log.Level,
    comptime scope: @Type(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    // Skip timestamps for fatal exit messages
    const is_fatal = comptime std.mem.indexOf(u8, format, "FATAL") != null;

    var buffer: [64]u8 = undefined;
    const stderr = std.debug.lockStderrWriter(&buffer);
    defer std.debug.unlockStderrWriter();

    if (!is_fatal) {
        // Get wall-clock time via libc
        var now: time_c.time_t = undefined;
        _ = time_c.time(&now);
        var tm: time_c.struct_tm = undefined;
        _ = time_c.localtime_r(&now, &tm);

        nosuspend stderr.print("[{d:0>4}-{d:0>2}-{d:0>2} {d:0>2}:{d:0>2}:{d:0>2}] ", .{
            @as(u32, @intCast(tm.tm_year)) + 1900,
            @as(u32, @intCast(tm.tm_mon)) + 1,
            @as(u32, @intCast(tm.tm_mday)),
            @as(u32, @intCast(tm.tm_hour)),
            @as(u32, @intCast(tm.tm_min)),
            @as(u32, @intCast(tm.tm_sec)),
        }) catch return;
    }

    _ = level;
    _ = scope;
    nosuspend stderr.print(format ++ "\n", args) catch return;
}

var g_shutdown = std.atomic.Value(bool).init(false);

fn signalHandler(_: c_int) callconv(.c) void {
    g_shutdown.store(true, .release);
}

pub fn main() !void {
    const config = config_mod.fromEnv();

    std.log.info("[server] Opening database at {s}", .{config.db_path});

    const db = try db_mod.openDb(config.db_path);
    defer db_mod.closeDb(db);

    try db_mod.initialize(db);

    var db_mutex = std.Thread.Mutex{};
    var health_mutex = std.Thread.Mutex{};

    // Zero-initialized health array
    var health = [_]poller_mod.DeviceHealth{poller_mod.DeviceHealth.init("", "")} ** config_mod.max_devices;

    // Signal handling
    const sa = std.posix.Sigaction{
        .handler = .{ .handler = signalHandler },
        .mask = std.posix.sigemptyset(),
        .flags = 0,
    };
    std.posix.sigaction(std.posix.SIG.INT, &sa, null);
    std.posix.sigaction(std.posix.SIG.TERM, &sa, null);

    const started_at = db_mod.nowMillis();

    // Poller state
    var poller_state = poller_mod.PollerState{
        .db = db,
        .db_mutex = &db_mutex,
        .health = &health,
        .health_mutex = &health_mutex,
        .config = &config,
        .shutdown = &g_shutdown,
    };

    // Spawn poller thread
    const poller_thread = try std.Thread.spawn(.{}, poller_mod.run, .{&poller_state});

    // Server state
    var server_state = http_server.ServerState{
        .db = db,
        .db_mutex = &db_mutex,
        .health = &health,
        .health_mutex = &health_mutex,
        .config = &config,
        .shutdown = &g_shutdown,
        .poller_state = &poller_state,
        .started_at = started_at,
    };

    // Run HTTP server (blocks)
    http_server.run(&server_state);

    // Shutdown
    g_shutdown.store(true, .release);
    poller_thread.join();

    std.log.info("[server] Shut down cleanly", .{});
}
