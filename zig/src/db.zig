const std = @import("std");
const c = @cImport(@cInclude("sqlite3.h"));

pub const SqliteDb = ?*c.sqlite3;

pub const Reading = struct {
    id: i64,
    timestamp: i64,
    device_id: []const u8,
    device_type: []const u8,
    device_ip: []const u8,
    pm01: ?f64,
    pm02: ?f64,
    pm10: ?f64,
    pm02_compensated: ?f64,
    rco2: ?i64,
    atmp: ?f64,
    atmp_compensated: ?f64,
    rhum: ?f64,
    rhum_compensated: ?f64,
    tvoc_index: ?f64,
    nox_index: ?f64,
    wifi: ?i64,
};

pub const DeviceSummary = struct {
    device_id: []const u8,
    device_type: []const u8,
    device_ip: []const u8,
    last_seen: i64,
    reading_count: i64,
};

pub const ReadingQuery = struct {
    device: ?[]const u8,
    from: i64,
    to: i64,
    limit: i64,
};

pub fn nowMillis() i64 {
    const ts = std.posix.clock_gettime(.REALTIME) catch {
        std.log.err("[db] clock_gettime failed, using epoch", .{});
        return 0;
    };
    return @as(i64, ts.sec) * 1000 + @divTrunc(@as(i64, ts.nsec), 1_000_000);
}

pub fn initialize(db: SqliteDb) !void {
    const pragmas =
        "PRAGMA journal_mode = WAL;" ++
        "PRAGMA busy_timeout = 5000;" ++
        "PRAGMA foreign_keys = ON;";

    var errmsg: [*c]u8 = null;
    var rc = c.sqlite3_exec(db, pragmas, null, null, &errmsg);
    if (rc != c.SQLITE_OK) {
        if (errmsg) |msg| {
            std.log.err("[db] pragma error: {s}", .{msg});
            c.sqlite3_free(msg);
        }
        return error.SqliteError;
    }

    // Read shared schema from file (checked at ../schema.sql then ./schema.sql)
    const schema_sql = readSchemaFile() orelse {
        std.log.err("[db] failed to read schema.sql", .{});
        return error.SqliteError;
    };

    rc = c.sqlite3_exec(db, schema_sql.ptr, null, null, &errmsg);
    if (rc != c.SQLITE_OK) {
        if (errmsg) |msg| {
            std.log.err("[db] schema error: {s}", .{msg});
            c.sqlite3_free(msg);
        }
        return error.SqliteError;
    }
}

fn readSchemaFile() ?[:0]const u8 {
    const paths = [_][]const u8{ "../schema.sql", "./schema.sql" };
    for (&paths) |path| {
        const file = std.fs.cwd().openFile(path, .{}) catch continue;
        defer file.close();
        const stat = file.stat() catch continue;
        if (stat.size == 0 or stat.size > 65536) continue;
        const allocator = std.heap.page_allocator;
        const buf = allocator.allocSentinel(u8, stat.size, 0) catch continue;
        const n = file.readAll(buf) catch continue;
        // Null-terminate at the actual read length
        buf[n] = 0;
        return buf[0..n :0];
    }
    return null;
}

// Safe float-to-int: reject NaN, Inf, and out-of-range values
fn safeFloatToI64(f: f64) ?i64 {
    if (std.math.isNan(f) or std.math.isInf(f)) return null;
    if (f > @as(f64, @floatFromInt(std.math.maxInt(i64)))) return null;
    if (f < @as(f64, @floatFromInt(std.math.minInt(i64)))) return null;
    return @intFromFloat(f);
}

fn extractF64(obj: std.json.Value, key: []const u8) ?f64 {
    const val = obj.object.get(key) orelse return null;
    return switch (val) {
        .float => |f| if (std.math.isNan(f) or std.math.isInf(f)) null else f,
        .integer => |i| @as(f64, @floatFromInt(i)),
        else => null,
    };
}

fn extractI64(obj: std.json.Value, key: []const u8) ?i64 {
    const val = obj.object.get(key) orelse return null;
    return switch (val) {
        .integer => |i| i,
        .float => |f| safeFloatToI64(f),
        else => null,
    };
}

fn extractStr(obj: std.json.Value, key: []const u8) ?[]const u8 {
    const val = obj.object.get(key) orelse return null;
    return switch (val) {
        .string => |s| s,
        else => null,
    };
}

// Safe text binding: clamp length to c_int range
fn bindText(stmt: ?*c.sqlite3_stmt, idx: c_int, text: []const u8) void {
    const len: c_int = if (text.len > std.math.maxInt(c_int))
        std.math.maxInt(c_int)
    else
        @intCast(text.len);
    _ = c.sqlite3_bind_text(stmt, idx, text.ptr, len, c.SQLITE_TRANSIENT);
}

fn bindOptF64(stmt: ?*c.sqlite3_stmt, idx: c_int, obj: std.json.Value, key: []const u8) void {
    if (extractF64(obj, key)) |val| {
        _ = c.sqlite3_bind_double(stmt, idx, val);
    } else {
        _ = c.sqlite3_bind_null(stmt, idx);
    }
}

fn bindOptI64(stmt: ?*c.sqlite3_stmt, idx: c_int, obj: std.json.Value, key: []const u8) void {
    if (extractI64(obj, key)) |val| {
        _ = c.sqlite3_bind_int64(stmt, idx, val);
    } else {
        _ = c.sqlite3_bind_null(stmt, idx);
    }
}

pub fn insertReading(db: SqliteDb, ip: []const u8, data: std.json.Value, raw_json: []const u8) !void {
    const model_str = extractStr(data, "model") orelse "";
    const device_type: []const u8 = if (model_str.len >= 2 and model_str[0] == 'I' and model_str[1] == '-')
        "indoor"
    else
        "outdoor";

    const serial = extractStr(data, "serialno") orelse "unknown";

    const sql =
        "INSERT INTO readings (" ++
        "    timestamp, device_id, device_type, device_ip," ++
        "    pm01, pm02, pm10, pm02_compensated," ++
        "    rco2, atmp, atmp_compensated, rhum, rhum_compensated," ++
        "    tvoc_index, nox_index, wifi, raw_json" ++
        ") VALUES (" ++
        "    ?1, ?2, ?3, ?4," ++
        "    ?5, ?6, ?7, ?8," ++
        "    ?9, ?10, ?11, ?12, ?13," ++
        "    ?14, ?15, ?16, ?17" ++
        ")";

    var stmt: ?*c.sqlite3_stmt = null;
    var rc = c.sqlite3_prepare_v2(db, sql, -1, &stmt, null);
    if (rc != c.SQLITE_OK) {
        std.log.err("[db] prepare insert error: {s}", .{c.sqlite3_errmsg(db)});
        return error.SqliteError;
    }
    defer _ = c.sqlite3_finalize(stmt);

    _ = c.sqlite3_bind_int64(stmt, 1, nowMillis());
    bindText(stmt, 2, serial);
    bindText(stmt, 3, device_type);
    bindText(stmt, 4, ip);

    bindOptF64(stmt, 5, data, "pm01");
    bindOptF64(stmt, 6, data, "pm02");
    bindOptF64(stmt, 7, data, "pm10");
    bindOptF64(stmt, 8, data, "pm02Compensated");
    bindOptI64(stmt, 9, data, "rco2");
    bindOptF64(stmt, 10, data, "atmp");
    bindOptF64(stmt, 11, data, "atmpCompensated");
    bindOptF64(stmt, 12, data, "rhum");
    bindOptF64(stmt, 13, data, "rhumCompensated");
    bindOptF64(stmt, 14, data, "tvocIndex");
    bindOptF64(stmt, 15, data, "noxIndex");
    bindOptI64(stmt, 16, data, "wifi");

    bindText(stmt, 17, raw_json);

    rc = c.sqlite3_step(stmt);
    if (rc != c.SQLITE_DONE) {
        std.log.err("[db] insert step error: {s}", .{c.sqlite3_errmsg(db)});
        return error.SqliteError;
    }
}

const query_cols =
    "id, timestamp, device_id, device_type, device_ip, " ++
    "pm01, pm02, pm10, pm02_compensated, rco2, " ++
    "atmp, atmp_compensated, rhum, rhum_compensated, " ++
    "tvoc_index, nox_index, wifi";

fn colIsNull(stmt: ?*c.sqlite3_stmt, col: c_int) bool {
    return c.sqlite3_column_type(stmt, col) == c.SQLITE_NULL;
}

fn colText(allocator: std.mem.Allocator, stmt: ?*c.sqlite3_stmt, col: c_int) ![]const u8 {
    const ptr = c.sqlite3_column_text(stmt, col);
    if (ptr == null) return allocator.dupe(u8, "");
    const raw_len = c.sqlite3_column_bytes(stmt, col);
    if (raw_len < 0) return allocator.dupe(u8, "");
    const len: usize = @intCast(raw_len);
    return allocator.dupe(u8, ptr[0..len]);
}

fn colOptF64(stmt: ?*c.sqlite3_stmt, col: c_int) ?f64 {
    if (colIsNull(stmt, col)) return null;
    const val = c.sqlite3_column_double(stmt, col);
    if (std.math.isNan(val) or std.math.isInf(val)) return null;
    return val;
}

fn colOptI64(stmt: ?*c.sqlite3_stmt, col: c_int) ?i64 {
    if (colIsNull(stmt, col)) return null;
    return c.sqlite3_column_int64(stmt, col);
}

fn rowToReading(allocator: std.mem.Allocator, stmt: ?*c.sqlite3_stmt) !Reading {
    return Reading{
        .id = c.sqlite3_column_int64(stmt, 0),
        .timestamp = c.sqlite3_column_int64(stmt, 1),
        .device_id = try colText(allocator, stmt, 2),
        .device_type = try colText(allocator, stmt, 3),
        .device_ip = try colText(allocator, stmt, 4),
        .pm01 = colOptF64(stmt, 5),
        .pm02 = colOptF64(stmt, 6),
        .pm10 = colOptF64(stmt, 7),
        .pm02_compensated = colOptF64(stmt, 8),
        .rco2 = colOptI64(stmt, 9),
        .atmp = colOptF64(stmt, 10),
        .atmp_compensated = colOptF64(stmt, 11),
        .rhum = colOptF64(stmt, 12),
        .rhum_compensated = colOptF64(stmt, 13),
        .tvoc_index = colOptF64(stmt, 14),
        .nox_index = colOptF64(stmt, 15),
        .wifi = colOptI64(stmt, 16),
    };
}

pub fn queryReadings(allocator: std.mem.Allocator, db: SqliteDb, q: ReadingQuery) ![]Reading {
    const want_device = if (q.device) |d| !std.mem.eql(u8, d, "all") else false;

    // Build SQL dynamically using bufPrint for safety
    var sql_buf: [1024]u8 = [_]u8{0} ** 1024;
    var fbs = std.io.fixedBufferStream(&sql_buf);
    const writer = fbs.writer();

    writer.print("SELECT {s} FROM readings WHERE ", .{query_cols}) catch return error.SqliteError;

    if (want_device) {
        writer.writeAll("device_id = ?3 AND ") catch return error.SqliteError;
    }

    writer.writeAll("timestamp >= ?1 AND timestamp <= ?2 ORDER BY timestamp ASC") catch return error.SqliteError;

    if (q.limit > 0) {
        if (want_device) {
            writer.writeAll(" LIMIT ?4") catch return error.SqliteError;
        } else {
            writer.writeAll(" LIMIT ?3") catch return error.SqliteError;
        }
    }

    const sql_len = fbs.pos;

    var stmt: ?*c.sqlite3_stmt = null;
    var rc = c.sqlite3_prepare_v2(db, &sql_buf, @intCast(sql_len), &stmt, null);
    if (rc != c.SQLITE_OK) {
        std.log.err("[db] prepare query error: {s}", .{c.sqlite3_errmsg(db)});
        return error.SqliteError;
    }
    defer _ = c.sqlite3_finalize(stmt);

    _ = c.sqlite3_bind_int64(stmt, 1, q.from);
    _ = c.sqlite3_bind_int64(stmt, 2, q.to);

    if (want_device and q.limit > 0) {
        const d = q.device.?;
        bindText(stmt, 3, d);
        _ = c.sqlite3_bind_int64(stmt, 4, q.limit);
    } else if (want_device) {
        const d = q.device.?;
        bindText(stmt, 3, d);
    } else if (q.limit > 0) {
        _ = c.sqlite3_bind_int64(stmt, 3, q.limit);
    }

    var readings: std.ArrayList(Reading) = .empty;
    while (true) {
        rc = c.sqlite3_step(stmt);
        if (rc == c.SQLITE_ROW) {
            try readings.append(allocator, try rowToReading(allocator, stmt));
        } else {
            break;
        }
    }

    if (rc != c.SQLITE_DONE) {
        return error.SqliteError;
    }

    return readings.toOwnedSlice(allocator);
}

pub fn getLatestReadings(allocator: std.mem.Allocator, db: SqliteDb) ![]Reading {
    const sql =
        "SELECT r.id, r.timestamp, r.device_id, r.device_type, r.device_ip, " ++
        "r.pm01, r.pm02, r.pm10, r.pm02_compensated, r.rco2, " ++
        "r.atmp, r.atmp_compensated, r.rhum, r.rhum_compensated, " ++
        "r.tvoc_index, r.nox_index, r.wifi " ++
        "FROM readings r " ++
        "INNER JOIN (" ++
        "    SELECT device_id, MAX(id) as max_id " ++
        "    FROM readings " ++
        "    GROUP BY device_id" ++
        ") latest ON r.id = latest.max_id";

    var stmt: ?*c.sqlite3_stmt = null;
    var rc = c.sqlite3_prepare_v2(db, sql, -1, &stmt, null);
    if (rc != c.SQLITE_OK) return error.SqliteError;
    defer _ = c.sqlite3_finalize(stmt);

    var readings: std.ArrayList(Reading) = .empty;
    while (true) {
        rc = c.sqlite3_step(stmt);
        if (rc == c.SQLITE_ROW) {
            try readings.append(allocator, try rowToReading(allocator, stmt));
        } else {
            break;
        }
    }

    if (rc != c.SQLITE_DONE) return error.SqliteError;
    return readings.toOwnedSlice(allocator);
}

pub fn getDevices(allocator: std.mem.Allocator, db: SqliteDb) ![]DeviceSummary {
    const sql =
        "SELECT device_id, device_type, device_ip, " ++
        "       MAX(timestamp) as last_seen, " ++
        "       COUNT(*) as reading_count " ++
        "FROM readings " ++
        "GROUP BY device_id " ++
        "ORDER BY device_type";

    var stmt: ?*c.sqlite3_stmt = null;
    var rc = c.sqlite3_prepare_v2(db, sql, -1, &stmt, null);
    if (rc != c.SQLITE_OK) return error.SqliteError;
    defer _ = c.sqlite3_finalize(stmt);

    var devices: std.ArrayList(DeviceSummary) = .empty;
    while (true) {
        rc = c.sqlite3_step(stmt);
        if (rc == c.SQLITE_ROW) {
            try devices.append(allocator, DeviceSummary{
                .device_id = try colText(allocator, stmt, 0),
                .device_type = try colText(allocator, stmt, 1),
                .device_ip = try colText(allocator, stmt, 2),
                .last_seen = c.sqlite3_column_int64(stmt, 3),
                .reading_count = c.sqlite3_column_int64(stmt, 4),
            });
        } else {
            break;
        }
    }

    if (rc != c.SQLITE_DONE) return error.SqliteError;
    return devices.toOwnedSlice(allocator);
}

pub fn checkpoint(db: SqliteDb) !void {
    var errmsg: [*c]u8 = null;
    const rc = c.sqlite3_exec(db, "PRAGMA wal_checkpoint(TRUNCATE);", null, null, &errmsg);
    if (rc != c.SQLITE_OK) {
        if (errmsg) |msg| {
            std.log.err("[db] checkpoint error: {s}", .{msg});
            c.sqlite3_free(msg);
        }
        return error.SqliteError;
    }
}

pub fn getReadingsCount(db: SqliteDb) !i64 {
    const sql = "SELECT COUNT(*) FROM readings";
    var stmt: ?*c.sqlite3_stmt = null;
    var rc = c.sqlite3_prepare_v2(db, sql, -1, &stmt, null);
    if (rc != c.SQLITE_OK) {
        std.log.err("[db] prepare count error: {s}", .{c.sqlite3_errmsg(db)});
        return error.SqliteError;
    }
    defer _ = c.sqlite3_finalize(stmt);

    rc = c.sqlite3_step(stmt);
    if (rc == c.SQLITE_ROW) {
        return c.sqlite3_column_int64(stmt, 0);
    }
    return error.SqliteError;
}

// JSON conversion helpers

fn jsonOptF64(val: ?f64) std.json.Value {
    if (val) |v| return .{ .float = v };
    return .null;
}

fn jsonOptI64(val: ?i64) std.json.Value {
    if (val) |v| return .{ .integer = v };
    return .null;
}

pub fn readingToJson(allocator: std.mem.Allocator, r: *const Reading) !std.json.Value {
    var obj = std.json.ObjectMap.init(allocator);
    try obj.put("id", .{ .integer = r.id });
    try obj.put("timestamp", .{ .integer = r.timestamp });
    try obj.put("device_id", .{ .string = r.device_id });
    try obj.put("device_type", .{ .string = r.device_type });
    try obj.put("device_ip", .{ .string = r.device_ip });
    try obj.put("pm01", jsonOptF64(r.pm01));
    try obj.put("pm02", jsonOptF64(r.pm02));
    try obj.put("pm10", jsonOptF64(r.pm10));
    try obj.put("pm02_compensated", jsonOptF64(r.pm02_compensated));
    try obj.put("rco2", jsonOptI64(r.rco2));
    try obj.put("atmp", jsonOptF64(r.atmp));
    try obj.put("atmp_compensated", jsonOptF64(r.atmp_compensated));
    try obj.put("rhum", jsonOptF64(r.rhum));
    try obj.put("rhum_compensated", jsonOptF64(r.rhum_compensated));
    try obj.put("tvoc_index", jsonOptF64(r.tvoc_index));
    try obj.put("nox_index", jsonOptF64(r.nox_index));
    try obj.put("wifi", jsonOptI64(r.wifi));
    return .{ .object = obj };
}

pub fn deviceSummaryToJson(allocator: std.mem.Allocator, d: *const DeviceSummary) !std.json.Value {
    var obj = std.json.ObjectMap.init(allocator);
    try obj.put("device_id", .{ .string = d.device_id });
    try obj.put("device_type", .{ .string = d.device_type });
    try obj.put("device_ip", .{ .string = d.device_ip });
    try obj.put("last_seen", .{ .integer = d.last_seen });
    try obj.put("reading_count", .{ .integer = d.reading_count });
    return .{ .object = obj };
}

pub fn openDb(path: []const u8) !SqliteDb {
    var db: SqliteDb = null;
    var path_buf: [512]u8 = [_]u8{0} ** 512;
    if (path.len >= path_buf.len) return error.PathTooLong;
    @memcpy(path_buf[0..path.len], path);
    path_buf[path.len] = 0;
    const rc = c.sqlite3_open(&path_buf, &db);
    if (rc != c.SQLITE_OK) {
        std.log.err("[db] Failed to open database: {s}", .{c.sqlite3_errmsg(db)});
        return error.SqliteError;
    }
    return db;
}

pub fn closeDb(db: SqliteDb) void {
    _ = c.sqlite3_close(db);
}
