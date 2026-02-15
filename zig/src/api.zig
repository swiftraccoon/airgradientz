const std = @import("std");
const db_mod = @import("db.zig");
const poller_mod = @import("poller.zig");
const http_server = @import("http_server.zig");

pub fn handleReadings(allocator: std.mem.Allocator, state: *http_server.ServerState, query: []const u8) !std.json.Value {
    const now = db_mod.nowMillis();
    const default_from = now - 24 * 60 * 60 * 1000;

    const from = http_server.parseI64Param(query, "from", default_from);
    const to = http_server.parseI64Param(query, "to", now);
    const device = http_server.queryParam(query, "device");

    const max: i64 = @as(i64, state.config.max_api_rows);
    const requested_limit = http_server.parseI64Param(query, "limit", max);
    const effective_limit = if (requested_limit > 0 and requested_limit < max) requested_limit else max;

    // Validate time range
    const safe_from = if (from <= to) from else default_from;
    const safe_to = if (to >= safe_from) to else now;

    const q = db_mod.ReadingQuery{
        .device = device orelse "all",
        .from = safe_from,
        .to = safe_to,
        .limit = effective_limit,
    };

    state.db_mutex.lock();
    defer state.db_mutex.unlock();
    const readings = try db_mod.queryReadings(allocator, state.db, q);

    var arr = std.json.Array.init(allocator);
    for (readings) |*r| {
        try arr.append(try db_mod.readingToJson(allocator, r));
    }

    return .{ .array = arr };
}

pub fn handleReadingsLatest(allocator: std.mem.Allocator, state: *http_server.ServerState) !std.json.Value {
    state.db_mutex.lock();
    defer state.db_mutex.unlock();
    const readings = try db_mod.getLatestReadings(allocator, state.db);

    var arr = std.json.Array.init(allocator);
    for (readings) |*r| {
        try arr.append(try db_mod.readingToJson(allocator, r));
    }

    return .{ .array = arr };
}

pub fn handleDevices(allocator: std.mem.Allocator, state: *http_server.ServerState) !std.json.Value {
    state.db_mutex.lock();
    defer state.db_mutex.unlock();
    const devices = try db_mod.getDevices(allocator, state.db);

    var arr = std.json.Array.init(allocator);
    for (devices) |*d| {
        try arr.append(try db_mod.deviceSummaryToJson(allocator, d));
    }

    return .{ .array = arr };
}

pub fn handleHealth(allocator: std.mem.Allocator, state: *http_server.ServerState) !std.json.Value {
    return poller_mod.getHealthJson(
        allocator,
        state.health,
        state.config.device_count,
        state.health_mutex,
    );
}

pub fn handleConfig(allocator: std.mem.Allocator, state: *http_server.ServerState) !std.json.Value {
    var devices_arr = std.json.Array.init(allocator);
    for (0..state.config.device_count) |i| {
        var d = std.json.ObjectMap.init(allocator);
        try d.put("ip", .{ .string = state.config.devices[i].ip });
        try d.put("label", .{ .string = state.config.devices[i].label });
        try devices_arr.append(.{ .object = d });
    }

    var cfg = std.json.ObjectMap.init(allocator);
    try cfg.put("pollIntervalMs", .{ .integer = @as(i64, state.config.poll_interval_ms) });
    try cfg.put("devices", .{ .array = devices_arr });

    return .{ .object = cfg };
}
