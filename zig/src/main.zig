const std = @import("std");
const config_mod = @import("config.zig");
const db_mod = @import("db.zig");
const poller_mod = @import("poller.zig");
const http_server = @import("http_server.zig");

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
    };

    // Run HTTP server (blocks)
    http_server.run(&server_state);

    // Shutdown
    g_shutdown.store(true, .release);
    poller_thread.join();

    std.log.info("[server] Shut down cleanly", .{});
}
