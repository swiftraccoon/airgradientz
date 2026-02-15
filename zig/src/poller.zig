const std = @import("std");
const config_mod = @import("config.zig");
const db_mod = @import("db.zig");
const http_client = @import("http_client.zig");

const checkpoint_interval_polls = 10;

pub const HealthStatus = enum {
    unknown,
    ok,
    err,
};

pub const DeviceHealth = struct {
    ip: []const u8,
    label: []const u8,
    status: HealthStatus,
    last_success: i64,
    last_error: i64,
    last_error_message: [256]u8,
    last_error_message_len: usize,
    consecutive_failures: u32,

    pub fn init(ip: []const u8, label: []const u8) DeviceHealth {
        return DeviceHealth{
            .ip = ip,
            .label = label,
            .status = .unknown,
            .last_success = 0,
            .last_error = 0,
            .last_error_message = [_]u8{0} ** 256,
            .last_error_message_len = 0,
            .consecutive_failures = 0,
        };
    }

    fn recordSuccess(self: *DeviceHealth) void {
        self.status = .ok;
        self.last_success = db_mod.nowMillis();
        self.last_error_message_len = 0;
        self.consecutive_failures = 0;
    }

    fn recordFailure(self: *DeviceHealth, msg: []const u8) void {
        self.status = .err;
        self.last_error = db_mod.nowMillis();
        const copy_len = @min(msg.len, self.last_error_message.len);
        @memcpy(self.last_error_message[0..copy_len], msg[0..copy_len]);
        self.last_error_message_len = copy_len;
        self.consecutive_failures +|= 1; // saturating add — no overflow
    }

    fn errorMessage(self: *const DeviceHealth) []const u8 {
        return self.last_error_message[0..self.last_error_message_len];
    }
};

pub const PollerState = struct {
    db: db_mod.SqliteDb,
    db_mutex: *std.Thread.Mutex,
    health: []DeviceHealth,
    health_mutex: *std.Thread.Mutex,
    config: *const config_mod.Config,
    shutdown: *std.atomic.Value(bool),
    poll_successes: std.atomic.Value(u64) = std.atomic.Value(u64).init(0),
    poll_failures: std.atomic.Value(u64) = std.atomic.Value(u64).init(0),
};

fn formatOptF64(buf: []u8, val: ?f64) []const u8 {
    if (val) |v| {
        return std.fmt.bufPrint(buf, "{d:.2}", .{v}) catch "?";
    }
    return "N/A";
}

fn formatOptI64(buf: []u8, val: ?i64) []const u8 {
    if (val) |v| {
        return std.fmt.bufPrint(buf, "{d}", .{v}) catch "?";
    }
    return "N/A";
}

fn fetchDevice(state: *PollerState, idx: usize) void {
    const ip = state.config.devices[idx].ip;
    const label = state.config.devices[idx].label;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const body = http_client.httpGet(allocator, ip, "/measures/current", state.config.fetch_timeout_ms) catch |e| {
        var err_buf: [256]u8 = [_]u8{0} ** 256;
        const err_msg = std.fmt.bufPrint(&err_buf, "fetch failed: {s}", .{@errorName(e)}) catch "fetch failed";
        std.log.err("[poller] {s} ({s}): {s}", .{ label, ip, err_msg });
        _ = state.poll_failures.fetchAdd(1, .monotonic);
        state.health_mutex.lock();
        defer state.health_mutex.unlock();
        state.health[idx].recordFailure(err_msg);
        return;
    };

    const parsed = std.json.parseFromSlice(std.json.Value, allocator, body, .{}) catch {
        const msg = "JSON parse error";
        std.log.err("[poller] {s} ({s}): {s}", .{ label, ip, msg });
        _ = state.poll_failures.fetchAdd(1, .monotonic);
        state.health_mutex.lock();
        defer state.health_mutex.unlock();
        state.health[idx].recordFailure(msg);
        return;
    };
    const data = parsed.value;

    if (data != .object) {
        const msg = "unexpected response type: not an object";
        std.log.err("[poller] {s} ({s}): {s}", .{ label, ip, msg });
        _ = state.poll_failures.fetchAdd(1, .monotonic);
        state.health_mutex.lock();
        defer state.health_mutex.unlock();
        state.health[idx].recordFailure(msg);
        return;
    }

    // Insert into DB
    {
        state.db_mutex.lock();
        defer state.db_mutex.unlock();
        db_mod.insertReading(state.db, ip, data, body) catch {
            const msg = "DB insert failed";
            std.log.err("[poller] {s} ({s}): {s}", .{ label, ip, msg });
            _ = state.poll_failures.fetchAdd(1, .monotonic);
            state.health_mutex.lock();
            defer state.health_mutex.unlock();
            state.health[idx].recordFailure(msg);
            return;
        };
    }

    // Extract values for log line (safe — returns null for NaN/Inf)
    const pm02 = extractOptF64(data, "pm02");
    const rco2 = extractOptI64(data, "rco2");
    const atmp = extractOptF64(data, "atmp");

    var pm02_buf: [32]u8 = [_]u8{0} ** 32;
    var rco2_buf: [32]u8 = [_]u8{0} ** 32;
    var atmp_buf: [32]u8 = [_]u8{0} ** 32;
    const pm02_s = formatOptF64(&pm02_buf, pm02);
    const rco2_s = formatOptI64(&rco2_buf, rco2);
    const atmp_s = formatOptF64(&atmp_buf, atmp);

    _ = state.poll_successes.fetchAdd(1, .monotonic);

    {
        state.health_mutex.lock();
        defer state.health_mutex.unlock();
        state.health[idx].recordSuccess();
    }

    std.log.info("[poller] {s} ({s}): OK — PM2.5={s}, CO2={s}, T={s}°C", .{ label, ip, pm02_s, rco2_s, atmp_s });
}

fn extractOptF64(data: std.json.Value, key: []const u8) ?f64 {
    const val = data.object.get(key) orelse return null;
    return switch (val) {
        .float => |f| if (std.math.isNan(f) or std.math.isInf(f)) null else f,
        .integer => |i| @as(f64, @floatFromInt(i)),
        else => null,
    };
}

fn extractOptI64(data: std.json.Value, key: []const u8) ?i64 {
    const val = data.object.get(key) orelse return null;
    return switch (val) {
        .integer => |i| i,
        .float => |f| blk: {
            if (std.math.isNan(f) or std.math.isInf(f)) break :blk null;
            if (f > @as(f64, @floatFromInt(std.math.maxInt(i64)))) break :blk null;
            if (f < @as(f64, @floatFromInt(std.math.minInt(i64)))) break :blk null;
            break :blk @as(i64, @intFromFloat(f));
        },
        else => null,
    };
}

fn pollAll(state: *PollerState) void {
    for (0..state.config.device_count) |i| {
        fetchDevice(state, i);
    }
}

pub fn run(state: *PollerState) void {
    std.log.info("[poller] Starting — polling {d} devices every {d}s", .{
        state.config.device_count,
        state.config.poll_interval_ms / 1000,
    });

    // Initialize health
    {
        state.health_mutex.lock();
        defer state.health_mutex.unlock();
        for (0..state.config.device_count) |i| {
            state.health[i] = DeviceHealth.init(
                state.config.devices[i].ip,
                state.config.devices[i].label,
            );
        }
    }

    // Initial poll
    pollAll(state);

    var poll_count: u32 = 0;
    const sleep_ns: u64 = @as(u64, state.config.poll_interval_ms) * 1_000_000;

    while (true) {
        std.Thread.sleep(sleep_ns);

        if (state.shutdown.load(.acquire)) {
            std.log.info("[poller] Stopped", .{});
            break;
        }

        pollAll(state);
        poll_count +|= 1; // saturating

        if (poll_count % checkpoint_interval_polls == 0) {
            state.db_mutex.lock();
            defer state.db_mutex.unlock();
            db_mod.checkpoint(state.db) catch {
                std.log.err("[poller] WAL checkpoint failed", .{});
            };
        }
    }
}

pub fn getHealthJson(allocator: std.mem.Allocator, health: []const DeviceHealth, count: usize, health_mutex: *std.Thread.Mutex) !std.json.Value {
    var arr = std.json.Array.init(allocator);

    health_mutex.lock();
    defer health_mutex.unlock();

    for (0..count) |i| {
        const h = &health[i];
        var obj = std.json.ObjectMap.init(allocator);

        try obj.put("ip", .{ .string = h.ip });
        try obj.put("label", .{ .string = h.label });

        const status_str: []const u8 = switch (h.status) {
            .unknown => "unknown",
            .ok => "ok",
            .err => "error",
        };
        try obj.put("status", .{ .string = status_str });

        try obj.put("lastSuccess", if (h.last_success != 0) .{ .integer = h.last_success } else .null);
        try obj.put("lastError", if (h.last_error != 0) .{ .integer = h.last_error } else .null);

        if (h.last_error_message_len > 0) {
            const msg = try allocator.dupe(u8, h.errorMessage());
            try obj.put("lastErrorMessage", .{ .string = msg });
        } else {
            try obj.put("lastErrorMessage", .null);
        }

        try obj.put("consecutiveFailures", .{ .integer = @as(i64, h.consecutive_failures) });

        try arr.append(.{ .object = obj });
    }

    return .{ .array = arr };
}
