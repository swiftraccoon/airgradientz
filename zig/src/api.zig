const std = @import("std");
const db_mod = @import("db.zig");
const poller_mod = @import("poller.zig");
const http_server = @import("http_server.zig");

pub const ReadingsError = error{ InvalidDownsample, InternalError };

pub fn handleReadings(allocator: std.mem.Allocator, state: *http_server.ServerState, query: []const u8) ReadingsError!std.json.Value {
    const now = db_mod.nowMillis();
    const default_from = now - 24 * 60 * 60 * 1000;

    const from = http_server.parseI64Param(query, "from", default_from);
    const to = http_server.parseI64Param(query, "to", now);
    const device = http_server.queryParam(query, "device");

    const max: i64 = @as(i64, state.config.max_api_rows);
    const requested_limit = http_server.parseI64Param(query, "limit", max);
    const effective_limit = if (requested_limit > 0 and requested_limit < max) requested_limit else max;

    // Parse downsample param
    var downsample_ms: i64 = 0;
    if (http_server.queryParam(query, "downsample")) |ds| {
        if (ds.len > 0) {
            downsample_ms = db_mod.downsampleLookup(ds) orelse return ReadingsError.InvalidDownsample;
        }
    }

    // Validate time range
    const safe_from = if (from <= to) from else default_from;
    const safe_to = if (to >= safe_from) to else now;

    const q = db_mod.ReadingQuery{
        .device = device orelse "all",
        .from = safe_from,
        .to = safe_to,
        .limit = effective_limit,
        .downsample_ms = downsample_ms,
    };

    state.db_mutex.lock();
    defer state.db_mutex.unlock();
    const readings = db_mod.queryReadings(allocator, state.db, q) catch return ReadingsError.InternalError;

    var arr = std.json.Array.init(allocator);
    for (readings) |*r| {
        arr.append(db_mod.readingToJson(allocator, r) catch return ReadingsError.InternalError) catch return ReadingsError.InternalError;
    }

    return .{ .array = arr };
}

pub fn handleReadingsCount(allocator: std.mem.Allocator, state: *http_server.ServerState, query: []const u8) !std.json.Value {
    const now = db_mod.nowMillis();

    const from = http_server.parseI64Param(query, "from", 0);
    const to = http_server.parseI64Param(query, "to", now);
    const device = http_server.queryParam(query, "device");

    state.db_mutex.lock();
    defer state.db_mutex.unlock();
    const count = try db_mod.getFilteredCount(state.db, from, to, device);

    var obj = std.json.ObjectMap.init(allocator);
    try obj.put("count", .{ .integer = count });

    return .{ .object = obj };
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

fn getMemoryRssBytes() i64 {
    const file = std.fs.openFileAbsolute("/proc/self/statm", .{}) catch return 0;
    defer file.close();

    var buf: [256]u8 = [_]u8{0} ** 256;
    const n = file.readAll(&buf) catch return 0;
    const content = buf[0..n];

    // statm format: size resident shared text lib data dt
    // We want the second field (resident) * page size
    var it = std.mem.splitScalar(u8, content, ' ');
    _ = it.next(); // skip size
    const resident_str = it.next() orelse return 0;
    const pages = std.fmt.parseInt(i64, resident_str, 10) catch return 0;
    const page_size: i64 = @intCast(std.heap.page_size_min);
    return pages * page_size;
}

fn getDbSizeBytes(db_path: []const u8) i64 {
    const file = std.fs.cwd().openFile(db_path, .{}) catch return 0;
    defer file.close();
    const stat = file.stat() catch return 0;
    const size = stat.size;
    // Cap to i64 range
    if (size > @as(u64, @intCast(std.math.maxInt(i64)))) return 0;
    return @intCast(size);
}

fn saturatingCastI64(val: u64) i64 {
    if (val > @as(u64, @intCast(std.math.maxInt(i64)))) return std.math.maxInt(i64);
    return @intCast(val);
}

pub fn handleStats(allocator: std.mem.Allocator, state: *http_server.ServerState) !std.json.Value {
    const now = db_mod.nowMillis();
    const uptime_ms = now - state.started_at;

    const memory_rss_bytes = getMemoryRssBytes();
    const db_size_bytes = getDbSizeBytes(state.config.db_path);

    const readings_count = blk: {
        state.db_mutex.lock();
        defer state.db_mutex.unlock();
        break :blk db_mod.getReadingsCount(state.db) catch 0;
    };

    const pid = std.os.linux.getpid();

    var obj = std.json.ObjectMap.init(allocator);
    try obj.put("implementation", .{ .string = "zig" });
    try obj.put("pid", .{ .integer = @as(i64, pid) });
    try obj.put("started_at", .{ .integer = state.started_at });
    try obj.put("uptime_ms", .{ .integer = uptime_ms });
    try obj.put("memory_rss_bytes", .{ .integer = memory_rss_bytes });
    try obj.put("db_size_bytes", .{ .integer = db_size_bytes });
    try obj.put("readings_count", .{ .integer = readings_count });
    try obj.put("requests_served", .{ .integer = saturatingCastI64(state.requests_served.load(.monotonic)) });
    try obj.put("active_connections", .{ .integer = @as(i64, state.active_connections.load(.monotonic)) });
    try obj.put("poll_successes", .{ .integer = saturatingCastI64(state.poller_state.poll_successes.load(.monotonic)) });
    try obj.put("poll_failures", .{ .integer = saturatingCastI64(state.poller_state.poll_failures.load(.monotonic)) });
    try obj.put("pool_alloc_count", .{ .integer = saturatingCastI64(state.arena_resets.load(.monotonic)) });
    try obj.put("pool_bytes_used", .{ .integer = 0 });

    return .{ .object = obj };
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
    try cfg.put("downsampleThreshold", .{ .integer = @as(i64, state.config.downsample_threshold) });
    try cfg.put("devices", .{ .array = devices_arr });

    return .{ .object = cfg };
}
