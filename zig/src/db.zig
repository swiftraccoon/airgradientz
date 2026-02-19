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
    downsample_ms: i64 = 0,
};

// ---- loaded queries from queries.sql ----

var loaded_query_cols: ?[]const u8 = null;
var loaded_insert_sql: ?[:0]const u8 = null;
var loaded_latest_sql: ?[:0]const u8 = null;
var loaded_devices_sql: ?[:0]const u8 = null;
var loaded_count_sql: ?[:0]const u8 = null;

fn loadQueries() void {
    const allocator = std.heap.page_allocator;

    const content = readQueriesFile(allocator) orelse {
        std.log.warn("[db] queries.sql not found, using defaults", .{});
        return;
    };

    parseQueriesSql(allocator, content);
}

fn readQueriesFile(allocator: std.mem.Allocator) ?[]const u8 {
    const paths = [_][]const u8{ "../queries.sql", "./queries.sql" };
    for (&paths) |path| {
        const file = std.fs.cwd().openFile(path, .{}) catch continue;
        defer file.close();
        const stat = file.stat() catch continue;
        if (stat.size == 0 or stat.size > 65536) continue;
        const buf = allocator.alloc(u8, stat.size) catch continue;
        const n = file.readAll(buf) catch continue;
        return buf[0..n];
    }
    return null;
}

fn parseQueriesSql(allocator: std.mem.Allocator, content: []const u8) void {
    var name: ?[]const u8 = null;
    var lines_buf: [32][]const u8 = undefined;
    var line_count: usize = 0;

    var line_iter = std.mem.splitScalar(u8, content, '\n');
    while (line_iter.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");

        if (std.mem.startsWith(u8, trimmed, "-- name: ")) {
            // Save previous query
            if (name) |n| {
                saveQuery(allocator, n, lines_buf[0..line_count]);
            }
            name = trimmed[9..];
            line_count = 0;
        } else if (!std.mem.startsWith(u8, trimmed, "--") and trimmed.len > 0) {
            if (line_count < lines_buf.len) {
                lines_buf[line_count] = trimmed;
                line_count += 1;
            }
        }
    }
    // Save last query
    if (name) |n| {
        saveQuery(allocator, n, lines_buf[0..line_count]);
    }
}

fn saveQuery(allocator: std.mem.Allocator, name: []const u8, lines: []const []const u8) void {
    if (lines.len == 0) return;

    // Join lines with newline
    var total_len: usize = 0;
    for (lines) |l| {
        if (total_len > 0) total_len += 1; // newline
        total_len += l.len;
    }

    const joined = allocator.alloc(u8, total_len) catch return;
    var pos: usize = 0;
    for (lines) |l| {
        if (pos > 0) {
            joined[pos] = '\n';
            pos += 1;
        }
        @memcpy(joined[pos .. pos + l.len], l);
        pos += l.len;
    }

    // Strip trailing semicolons and whitespace
    var end = pos;
    while (end > 0 and (joined[end - 1] == ';' or joined[end - 1] == ' ' or joined[end - 1] == '\n')) {
        end -= 1;
    }

    const sql = joined[0..end];

    if (std.mem.eql(u8, name, "reading_columns")) {
        loaded_query_cols = sql;
    } else if (std.mem.eql(u8, name, "insert_reading")) {
        loaded_insert_sql = convertPlaceholders(allocator, sql);
    } else if (std.mem.eql(u8, name, "select_latest")) {
        loaded_latest_sql = allocNullTerminated(allocator, sql);
    } else if (std.mem.eql(u8, name, "select_devices")) {
        loaded_devices_sql = allocNullTerminated(allocator, sql);
    } else if (std.mem.eql(u8, name, "count_readings")) {
        loaded_count_sql = allocNullTerminated(allocator, sql);
    }
}

fn allocNullTerminated(allocator: std.mem.Allocator, s: []const u8) ?[:0]const u8 {
    const buf = allocator.allocSentinel(u8, s.len, 0) catch return null;
    @memcpy(buf[0..s.len], s);
    return buf[0..s.len :0];
}

fn convertPlaceholders(allocator: std.mem.Allocator, sql: []const u8) ?[:0]const u8 {
    var buf: [4096]u8 = [_]u8{0} ** 4096;
    var out: usize = 0;
    var n: u32 = 1;
    var i: usize = 0;

    while (i < sql.len) {
        if (sql[i] == ':' and i + 1 < sql.len and std.ascii.isAlphabetic(sql[i + 1])) {
            // Skip :name
            i += 1;
            while (i < sql.len and (std.ascii.isAlphanumeric(sql[i]) or sql[i] == '_')) {
                i += 1;
            }
            const written = std.fmt.bufPrint(buf[out..], "?{d}", .{n}) catch break;
            out += written.len;
            n += 1;
        } else {
            if (out < buf.len) {
                buf[out] = sql[i];
                out += 1;
            }
            i += 1;
        }
    }

    const result = allocator.allocSentinel(u8, out, 0) catch return null;
    @memcpy(result[0..out], buf[0..out]);
    return result[0..out :0];
}

pub fn nowMillis() i64 {
    const ts = std.posix.clock_gettime(.REALTIME) catch {
        std.log.err("[db] clock_gettime failed, using epoch", .{});
        return 0;
    };
    return @as(i64, ts.sec) * 1000 + @divTrunc(@as(i64, ts.nsec), 1_000_000);
}

pub fn initialize(db: SqliteDb) !void {
    loadQueries();

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

    const fallback_insert_sql =
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
    const sql: [*:0]const u8 = if (loaded_insert_sql) |q| q.ptr else fallback_insert_sql.ptr;

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

// Downsampled rows: no id column; columns start at timestamp (col 0)
fn rowToDownsampledReading(allocator: std.mem.Allocator, stmt: ?*c.sqlite3_stmt) !Reading {
    return Reading{
        .id = 0,
        .timestamp = c.sqlite3_column_int64(stmt, 0),
        .device_id = try colText(allocator, stmt, 1),
        .device_type = try colText(allocator, stmt, 2),
        .device_ip = try colText(allocator, stmt, 3),
        .pm01 = colOptF64(stmt, 4),
        .pm02 = colOptF64(stmt, 5),
        .pm10 = colOptF64(stmt, 6),
        .pm02_compensated = colOptF64(stmt, 7),
        .rco2 = colOptI64(stmt, 8),
        .atmp = colOptF64(stmt, 9),
        .atmp_compensated = colOptF64(stmt, 10),
        .rhum = colOptF64(stmt, 11),
        .rhum_compensated = colOptF64(stmt, 12),
        .tvoc_index = colOptF64(stmt, 13),
        .nox_index = colOptF64(stmt, 14),
        .wifi = colOptI64(stmt, 15),
    };
}

pub fn queryReadings(allocator: std.mem.Allocator, db: SqliteDb, q: ReadingQuery) ![]Reading {
    const want_device = if (q.device) |d| !std.mem.eql(u8, d, "all") else false;

    // Build SQL dynamically using fixedBufferStream for safety
    var sql_buf: [2048]u8 = [_]u8{0} ** 2048;
    var fbs = std.io.fixedBufferStream(&sql_buf);
    const writer = fbs.writer();

    const is_downsampled = q.downsample_ms > 0;

    if (is_downsampled) {
        // Downsampled query: bucket_ms is a trusted constant from downsample_map
        writer.print(
            "SELECT (timestamp / {d}) * {d} AS timestamp" ++
                ", device_id, device_type, device_ip" ++
                ", AVG(pm01) AS pm01, AVG(pm02) AS pm02, AVG(pm10) AS pm10" ++
                ", AVG(pm02_compensated) AS pm02_compensated" ++
                ", CAST(AVG(rco2) AS INTEGER) AS rco2" ++
                ", AVG(atmp) AS atmp, AVG(atmp_compensated) AS atmp_compensated" ++
                ", AVG(rhum) AS rhum, AVG(rhum_compensated) AS rhum_compensated" ++
                ", AVG(tvoc_index) AS tvoc_index, AVG(nox_index) AS nox_index" ++
                ", CAST(AVG(wifi) AS INTEGER) AS wifi" ++
                " FROM readings WHERE ",
            .{ q.downsample_ms, q.downsample_ms },
        ) catch return error.SqliteError;
    } else {
        const cols = loaded_query_cols orelse query_cols;
        writer.print("SELECT {s} FROM readings WHERE ", .{cols}) catch return error.SqliteError;
    }

    if (want_device) {
        writer.writeAll("device_id = ?3 AND ") catch return error.SqliteError;
    }

    writer.writeAll("timestamp >= ?1 AND timestamp <= ?2") catch return error.SqliteError;

    if (is_downsampled) {
        writer.print(" GROUP BY (timestamp / {d}), device_id ORDER BY timestamp ASC", .{q.downsample_ms}) catch return error.SqliteError;
    } else {
        writer.writeAll(" ORDER BY timestamp ASC") catch return error.SqliteError;
    }

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
            if (is_downsampled) {
                try readings.append(allocator, try rowToDownsampledReading(allocator, stmt));
            } else {
                try readings.append(allocator, try rowToReading(allocator, stmt));
            }
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
    const fallback_latest_sql =
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
    const sql: [*:0]const u8 = if (loaded_latest_sql) |q| q.ptr else fallback_latest_sql.ptr;

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
    const fallback_devices_sql =
        "SELECT device_id, device_type, device_ip, " ++
        "       MAX(timestamp) as last_seen, " ++
        "       COUNT(*) as reading_count " ++
        "FROM readings " ++
        "GROUP BY device_id " ++
        "ORDER BY device_type";
    const sql: [*:0]const u8 = if (loaded_devices_sql) |q| q.ptr else fallback_devices_sql.ptr;

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

pub fn getFilteredCount(db: SqliteDb, from: i64, to: i64, device: ?[]const u8) !i64 {
    const want_device = if (device) |d| !std.mem.eql(u8, d, "all") and d.len > 0 else false;

    var sql_buf: [512]u8 = [_]u8{0} ** 512;
    var fbs = std.io.fixedBufferStream(&sql_buf);
    const writer = fbs.writer();

    writer.writeAll("SELECT COUNT(*) FROM readings WHERE ") catch return error.SqliteError;

    if (want_device) {
        writer.writeAll("device_id = ?3 AND ") catch return error.SqliteError;
    }

    writer.writeAll("timestamp >= ?1 AND timestamp <= ?2") catch return error.SqliteError;

    const sql_len = fbs.pos;

    var stmt: ?*c.sqlite3_stmt = null;
    var rc = c.sqlite3_prepare_v2(db, &sql_buf, @intCast(sql_len), &stmt, null);
    if (rc != c.SQLITE_OK) {
        std.log.err("[db] prepare filtered count error: {s}", .{c.sqlite3_errmsg(db)});
        return error.SqliteError;
    }
    defer _ = c.sqlite3_finalize(stmt);

    _ = c.sqlite3_bind_int64(stmt, 1, from);
    _ = c.sqlite3_bind_int64(stmt, 2, to);

    if (want_device) {
        bindText(stmt, 3, device.?);
    }

    rc = c.sqlite3_step(stmt);
    if (rc == c.SQLITE_ROW) {
        return c.sqlite3_column_int64(stmt, 0);
    }
    return error.SqliteError;
}

pub fn getReadingsCount(db: SqliteDb) !i64 {
    const fallback_count_sql = "SELECT COUNT(*) FROM readings";
    const sql: [*:0]const u8 = if (loaded_count_sql) |q| q.ptr else fallback_count_sql.ptr;
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
    // Downsampled rows have id=0; omit id from JSON output
    if (r.id != 0) {
        try obj.put("id", .{ .integer = r.id });
    }
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

// ---- inline tests ----

const testing = std.testing;

fn testOpenDb() !SqliteDb {
    const db = try openDb(":memory:");
    try initialize(db);
    return db;
}

fn loadFixtureJson(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
    const content = try std.fs.cwd().readFileAlloc(allocator, "../test-fixtures.json", 1_048_576);
    defer allocator.free(content);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, content, .{});
    defer parsed.deinit();
    const fixture = parsed.value.object.get(name) orelse return error.FixtureNotFound;
    return std.json.Stringify.valueAlloc(allocator, fixture, .{});
}

const test_no_serial_json =
    \\{"wifi":-30,"model":"I-9PSL","pm02":5}
;

fn testParseAndInsert(db: SqliteDb, ip: []const u8, json_str: []const u8) !void {
    const parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, json_str, .{});
    defer parsed.deinit();
    try insertReading(db, ip, parsed.value, json_str);
}

test "nowMillis returns positive value" {
    const ms = nowMillis();
    try testing.expect(ms > 0);
}

test "insert and query indoor reading" {
    const db = try testOpenDb();
    defer closeDb(db);

    const fixture = try loadFixtureJson(testing.allocator, "indoorFull");
    defer testing.allocator.free(fixture);
    try testParseAndInsert(db, "192.168.1.1", fixture);

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const readings = try queryReadings(alloc, db, .{
        .device = null,
        .from = 0,
        .to = nowMillis() + 1000,
        .limit = 100,
    });

    try testing.expectEqual(@as(usize, 1), readings.len);
    try testing.expectEqualStrings("indoor", readings[0].device_type);
    try testing.expectEqualStrings("84fce602549c", readings[0].device_id);
    try testing.expectEqualStrings("192.168.1.1", readings[0].device_ip);
    try testing.expectEqual(@as(?f64, 41.67), readings[0].pm02);
    try testing.expectEqual(@as(?i64, 489), readings[0].rco2);
    try testing.expectEqual(@as(?f64, 20.78), readings[0].atmp);
}

test "device type classification" {
    const db = try testOpenDb();
    defer closeDb(db);

    const indoor = try loadFixtureJson(testing.allocator, "indoorFull");
    defer testing.allocator.free(indoor);
    const outdoor = try loadFixtureJson(testing.allocator, "outdoorFull");
    defer testing.allocator.free(outdoor);
    try testParseAndInsert(db, "10.0.0.1", indoor);
    try testParseAndInsert(db, "10.0.0.2", outdoor);

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const readings = try queryReadings(alloc, db, .{
        .device = null,
        .from = 0,
        .to = nowMillis() + 1000,
        .limit = 100,
    });

    try testing.expectEqual(@as(usize, 2), readings.len);
    // Ordered by timestamp ASC, both inserted near-instantly
    var found_indoor = false;
    var found_outdoor = false;
    for (readings) |r| {
        if (std.mem.eql(u8, r.device_type, "indoor")) found_indoor = true;
        if (std.mem.eql(u8, r.device_type, "outdoor")) found_outdoor = true;
    }
    try testing.expect(found_indoor);
    try testing.expect(found_outdoor);
}

test "null fields handling" {
    const db = try testOpenDb();
    defer closeDb(db);

    const fixture = try loadFixtureJson(testing.allocator, "afterBoot");
    defer testing.allocator.free(fixture);
    try testParseAndInsert(db, "10.0.0.1", fixture);

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const readings = try queryReadings(alloc, db, .{
        .device = null,
        .from = 0,
        .to = nowMillis() + 1000,
        .limit = 100,
    });

    try testing.expectEqual(@as(usize, 1), readings.len);
    try testing.expectEqual(@as(?f64, null), readings[0].pm01);
    try testing.expectEqual(@as(?f64, null), readings[0].pm02);
    try testing.expectEqual(@as(?i64, null), readings[0].rco2);
    try testing.expectEqual(@as(?f64, null), readings[0].atmp);
    try testing.expectEqual(@as(?i64, -59), readings[0].wifi);
}

test "zero compensated values are not null" {
    const db = try testOpenDb();
    defer closeDb(db);

    const fixture = try loadFixtureJson(testing.allocator, "zeroCompensated");
    defer testing.allocator.free(fixture);
    try testParseAndInsert(db, "10.0.0.1", fixture);

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const readings = try queryReadings(alloc, db, .{
        .device = null,
        .from = 0,
        .to = nowMillis() + 1000,
        .limit = 100,
    });

    try testing.expectEqual(@as(usize, 1), readings.len);
    try testing.expectEqual(@as(?f64, 0.0), readings[0].pm02_compensated);
    try testing.expectEqual(@as(?f64, 0.0), readings[0].atmp_compensated);
    try testing.expectEqual(@as(?f64, 0.0), readings[0].rhum_compensated);
}

test "missing serialno defaults to unknown" {
    const db = try testOpenDb();
    defer closeDb(db);

    try testParseAndInsert(db, "10.0.0.1", test_no_serial_json);

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const readings = try queryReadings(alloc, db, .{
        .device = null,
        .from = 0,
        .to = nowMillis() + 1000,
        .limit = 100,
    });

    try testing.expectEqual(@as(usize, 1), readings.len);
    try testing.expectEqualStrings("unknown", readings[0].device_id);
}

test "query with device filter" {
    const db = try testOpenDb();
    defer closeDb(db);

    const indoor = try loadFixtureJson(testing.allocator, "indoorFull");
    defer testing.allocator.free(indoor);
    const outdoor = try loadFixtureJson(testing.allocator, "outdoorFull");
    defer testing.allocator.free(outdoor);
    try testParseAndInsert(db, "10.0.0.1", indoor);
    try testParseAndInsert(db, "10.0.0.2", outdoor);

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const readings = try queryReadings(alloc, db, .{
        .device = "84fce602549c",
        .from = 0,
        .to = nowMillis() + 1000,
        .limit = 100,
    });

    try testing.expectEqual(@as(usize, 1), readings.len);
    try testing.expectEqualStrings("84fce602549c", readings[0].device_id);
}

test "query with device=all returns all" {
    const db = try testOpenDb();
    defer closeDb(db);

    const indoor = try loadFixtureJson(testing.allocator, "indoorFull");
    defer testing.allocator.free(indoor);
    const outdoor = try loadFixtureJson(testing.allocator, "outdoorFull");
    defer testing.allocator.free(outdoor);
    try testParseAndInsert(db, "10.0.0.1", indoor);
    try testParseAndInsert(db, "10.0.0.2", outdoor);

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const readings = try queryReadings(alloc, db, .{
        .device = "all",
        .from = 0,
        .to = nowMillis() + 1000,
        .limit = 100,
    });

    try testing.expectEqual(@as(usize, 2), readings.len);
}

test "query with limit" {
    const db = try testOpenDb();
    defer closeDb(db);

    const indoor = try loadFixtureJson(testing.allocator, "indoorFull");
    defer testing.allocator.free(indoor);
    const outdoor = try loadFixtureJson(testing.allocator, "outdoorFull");
    defer testing.allocator.free(outdoor);
    try testParseAndInsert(db, "10.0.0.1", indoor);
    try testParseAndInsert(db, "10.0.0.2", outdoor);

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const readings = try queryReadings(alloc, db, .{
        .device = null,
        .from = 0,
        .to = nowMillis() + 1000,
        .limit = 1,
    });

    try testing.expectEqual(@as(usize, 1), readings.len);
}

test "getLatestReadings returns one per device" {
    const db = try testOpenDb();
    defer closeDb(db);

    const indoor = try loadFixtureJson(testing.allocator, "indoorFull");
    defer testing.allocator.free(indoor);
    const outdoor = try loadFixtureJson(testing.allocator, "outdoorFull");
    defer testing.allocator.free(outdoor);
    try testParseAndInsert(db, "10.0.0.1", indoor);
    try testParseAndInsert(db, "10.0.0.2", outdoor);
    // Insert second indoor reading
    try testParseAndInsert(db, "10.0.0.1", indoor);

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const latest = try getLatestReadings(alloc, db);
    try testing.expectEqual(@as(usize, 2), latest.len);
}

test "getDevices returns unique devices" {
    const db = try testOpenDb();
    defer closeDb(db);

    const indoor = try loadFixtureJson(testing.allocator, "indoorFull");
    defer testing.allocator.free(indoor);
    const outdoor = try loadFixtureJson(testing.allocator, "outdoorFull");
    defer testing.allocator.free(outdoor);
    try testParseAndInsert(db, "10.0.0.1", indoor);
    try testParseAndInsert(db, "10.0.0.1", indoor);
    try testParseAndInsert(db, "10.0.0.2", outdoor);

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const devices = try getDevices(alloc, db);
    try testing.expectEqual(@as(usize, 2), devices.len);
}

test "getReadingsCount" {
    const db = try testOpenDb();
    defer closeDb(db);

    const indoor = try loadFixtureJson(testing.allocator, "indoorFull");
    defer testing.allocator.free(indoor);
    const outdoor = try loadFixtureJson(testing.allocator, "outdoorFull");
    defer testing.allocator.free(outdoor);
    try testParseAndInsert(db, "10.0.0.1", indoor);
    try testParseAndInsert(db, "10.0.0.2", outdoor);

    const count = try getReadingsCount(db);
    try testing.expectEqual(@as(i64, 2), count);
}

test "empty query returns empty" {
    const db = try testOpenDb();
    defer closeDb(db);

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const readings = try queryReadings(alloc, db, .{
        .device = null,
        .from = 0,
        .to = nowMillis() + 1000,
        .limit = 100,
    });

    try testing.expectEqual(@as(usize, 0), readings.len);
}

test "readingToJson includes all fields" {
    const db = try testOpenDb();
    defer closeDb(db);

    const fixture = try loadFixtureJson(testing.allocator, "indoorFull");
    defer testing.allocator.free(fixture);
    try testParseAndInsert(db, "10.0.0.1", fixture);

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const readings = try queryReadings(alloc, db, .{
        .device = null,
        .from = 0,
        .to = nowMillis() + 1000,
        .limit = 100,
    });

    const json = try readingToJson(alloc, &readings[0]);
    try testing.expect(json.object.contains("device_id"));
    try testing.expect(json.object.contains("pm02"));
    try testing.expect(json.object.contains("rco2"));
    try testing.expect(json.object.contains("timestamp"));
}

test "readingToJson with null fields" {
    const db = try testOpenDb();
    defer closeDb(db);

    const fixture = try loadFixtureJson(testing.allocator, "afterBoot");
    defer testing.allocator.free(fixture);
    try testParseAndInsert(db, "10.0.0.1", fixture);

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const readings = try queryReadings(alloc, db, .{
        .device = null,
        .from = 0,
        .to = nowMillis() + 1000,
        .limit = 100,
    });

    const json = try readingToJson(alloc, &readings[0]);
    try testing.expectEqual(std.json.Value.null, json.object.get("pm01").?);
    try testing.expectEqual(std.json.Value.null, json.object.get("rco2").?);
}

test "deviceSummaryToJson" {
    const summary = DeviceSummary{
        .device_id = "test123",
        .device_type = "indoor",
        .device_ip = "10.0.0.1",
        .last_seen = 1000,
        .reading_count = 5,
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const json = try deviceSummaryToJson(alloc, &summary);
    try testing.expectEqualStrings("test123", json.object.get("device_id").?.string);
    try testing.expectEqual(@as(i64, 5), json.object.get("reading_count").?.integer);
}

test "checkpoint does not error" {
    const db = try testOpenDb();
    defer closeDb(db);

    try checkpoint(db);
}

fn testInsertRaw(db: SqliteDb, ts: i64, device_id: []const u8, pm02: f64, rco2: i64) !void {
    const sql = "INSERT INTO readings (timestamp, device_id, device_type, device_ip, pm02, rco2, raw_json) VALUES (?1, ?2, 'indoor', '10.0.0.1', ?3, ?4, '{}')";
    var stmt: ?*c.sqlite3_stmt = null;
    var rc = c.sqlite3_prepare_v2(db, sql, -1, &stmt, null);
    if (rc != c.SQLITE_OK) return error.SqliteError;
    defer _ = c.sqlite3_finalize(stmt);

    _ = c.sqlite3_bind_int64(stmt, 1, ts);
    bindText(stmt, 2, device_id);
    _ = c.sqlite3_bind_double(stmt, 3, pm02);
    _ = c.sqlite3_bind_int64(stmt, 4, rco2);

    rc = c.sqlite3_step(stmt);
    if (rc != c.SQLITE_DONE) return error.SqliteError;
}

test "downsampled query groups readings" {
    const db = try testOpenDb();
    defer closeDb(db);

    // Insert readings with specific timestamps in the same 1h bucket
    const ts_base: i64 = 1_000_000_000_000;
    try testInsertRaw(db, ts_base, "dev1", 10.0, 400);
    try testInsertRaw(db, ts_base + 60_000, "dev1", 20.0, 500);
    try testInsertRaw(db, ts_base + 120_000, "dev1", 30.0, 600);

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Query with 1h downsample — should get 1 grouped row
    const readings = try queryReadings(alloc, db, .{
        .device = null,
        .from = ts_base - 1000,
        .to = ts_base + 200_000,
        .limit = 100,
        .downsample_ms = 3_600_000,
    });

    try testing.expectEqual(@as(usize, 1), readings.len);
    // id should be 0 for downsampled rows
    try testing.expectEqual(@as(i64, 0), readings[0].id);
    // pm02 should be average of 10, 20, 30 = 20
    try testing.expect(readings[0].pm02 != null);
    const pm02_val = readings[0].pm02.?;
    try testing.expect(@abs(pm02_val - 20.0) < 0.01);
    // rco2 should be average of 400, 500, 600 = 500
    try testing.expectEqual(@as(?i64, 500), readings[0].rco2);
}

test "downsampled rows omit id in JSON" {
    const db = try testOpenDb();
    defer closeDb(db);

    const ts_base: i64 = 1_000_000_000_000;
    try testInsertRaw(db, ts_base, "dev1", 10.0, 400);

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const readings = try queryReadings(alloc, db, .{
        .device = null,
        .from = ts_base - 1000,
        .to = ts_base + 1000,
        .limit = 100,
        .downsample_ms = 3_600_000,
    });

    try testing.expectEqual(@as(usize, 1), readings.len);
    const json = try readingToJson(alloc, &readings[0]);
    // Downsampled row should not have "id" key
    try testing.expect(!json.object.contains("id"));
    // But should still have other fields
    try testing.expect(json.object.contains("timestamp"));
    try testing.expect(json.object.contains("device_id"));
}

test "getFilteredCount all devices" {
    const db = try testOpenDb();
    defer closeDb(db);

    const indoor = try loadFixtureJson(testing.allocator, "indoorFull");
    defer testing.allocator.free(indoor);
    const outdoor = try loadFixtureJson(testing.allocator, "outdoorFull");
    defer testing.allocator.free(outdoor);
    try testParseAndInsert(db, "10.0.0.1", indoor);
    try testParseAndInsert(db, "10.0.0.2", outdoor);

    const count = try getFilteredCount(db, 0, nowMillis() + 1000, null);
    try testing.expectEqual(@as(i64, 2), count);
}

test "getFilteredCount with device filter" {
    const db = try testOpenDb();
    defer closeDb(db);

    const indoor = try loadFixtureJson(testing.allocator, "indoorFull");
    defer testing.allocator.free(indoor);
    const outdoor = try loadFixtureJson(testing.allocator, "outdoorFull");
    defer testing.allocator.free(outdoor);
    try testParseAndInsert(db, "10.0.0.1", indoor);
    try testParseAndInsert(db, "10.0.0.2", outdoor);

    const count = try getFilteredCount(db, 0, nowMillis() + 1000, "84fce602549c");
    try testing.expectEqual(@as(i64, 1), count);
}

test "getFilteredCount with device=all returns all" {
    const db = try testOpenDb();
    defer closeDb(db);

    const indoor = try loadFixtureJson(testing.allocator, "indoorFull");
    defer testing.allocator.free(indoor);
    const outdoor = try loadFixtureJson(testing.allocator, "outdoorFull");
    defer testing.allocator.free(outdoor);
    try testParseAndInsert(db, "10.0.0.1", indoor);
    try testParseAndInsert(db, "10.0.0.2", outdoor);

    const count = try getFilteredCount(db, 0, nowMillis() + 1000, "all");
    try testing.expectEqual(@as(i64, 2), count);
}

test "getFilteredCount empty db" {
    const db = try testOpenDb();
    defer closeDb(db);

    const count = try getFilteredCount(db, 0, nowMillis() + 1000, null);
    try testing.expectEqual(@as(i64, 0), count);
}

// ---- test-spec.json aligned tests ----

// Downsample bucket verification: all 7 buckets from test-spec.json
test "spec: downsample buckets map to correct ms values" {
    const Expected = struct { param: []const u8, ms: i64 };
    const buckets = [_]Expected{
        .{ .param = "5m", .ms = 300000 },
        .{ .param = "10m", .ms = 600000 },
        .{ .param = "15m", .ms = 900000 },
        .{ .param = "30m", .ms = 1800000 },
        .{ .param = "1h", .ms = 3600000 },
        .{ .param = "1d", .ms = 86400000 },
        .{ .param = "1w", .ms = 604800000 },
    };

    // Load and parse the config JSON directly (same path resolution as config.zig)
    const allocator = testing.allocator;
    const content = std.fs.cwd().readFileAlloc(allocator, "../airgradientz.json", 1_048_576) catch |e| {
        std.debug.print("Failed to read config: {}\n", .{e});
        return error.TestSetup;
    };
    defer allocator.free(content);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, content, .{});
    defer parsed.deinit();

    const ds_obj = parsed.value.object.get("downsampleBuckets") orelse return error.TestSetup;
    try testing.expect(ds_obj == .object);

    // Verify all 7 buckets exist with correct values
    for (&buckets) |b| {
        const val = ds_obj.object.get(b.param) orelse {
            std.debug.print("Missing bucket: {s}\n", .{b.param});
            return error.TestUnexpectedResult;
        };
        try testing.expect(val == .integer);
        try testing.expectEqual(b.ms, val.integer);
    }

    // Verify exactly 7 buckets (no extras)
    try testing.expectEqual(@as(usize, 7), ds_obj.object.count());
}

// Query edge cases from test-spec.json

test "spec: from > to returns empty results" {
    const db = try testOpenDb();
    defer closeDb(db);

    const fixture = try loadFixtureJson(testing.allocator, "indoorFull");
    defer testing.allocator.free(fixture);
    try testParseAndInsert(db, "10.0.0.1", fixture);

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // from=9999999999999, to=1 — reversed range
    const readings = try queryReadings(alloc, db, .{
        .device = null,
        .from = 9_999_999_999_999,
        .to = 1,
        .limit = 100,
    });

    try testing.expectEqual(@as(usize, 0), readings.len);
}

test "spec: nonexistent device returns empty results" {
    const db = try testOpenDb();
    defer closeDb(db);

    const fixture = try loadFixtureJson(testing.allocator, "indoorFull");
    defer testing.allocator.free(fixture);
    try testParseAndInsert(db, "10.0.0.1", fixture);

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const readings = try queryReadings(alloc, db, .{
        .device = "nonexistent-serial-xyz",
        .from = 0,
        .to = nowMillis() + 1000,
        .limit = 100,
    });

    try testing.expectEqual(@as(usize, 0), readings.len);
}

test "spec: limit=1 returns exactly 1 result" {
    const db = try testOpenDb();
    defer closeDb(db);

    const indoor = try loadFixtureJson(testing.allocator, "indoorFull");
    defer testing.allocator.free(indoor);
    const outdoor = try loadFixtureJson(testing.allocator, "outdoorFull");
    defer testing.allocator.free(outdoor);
    try testParseAndInsert(db, "10.0.0.1", indoor);
    try testParseAndInsert(db, "10.0.0.2", outdoor);

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const readings = try queryReadings(alloc, db, .{
        .device = null,
        .from = 0,
        .to = nowMillis() + 1000,
        .limit = 1,
    });

    try testing.expectEqual(@as(usize, 1), readings.len);
}

test "spec: count with from > to returns zero" {
    const db = try testOpenDb();
    defer closeDb(db);

    const fixture = try loadFixtureJson(testing.allocator, "indoorFull");
    defer testing.allocator.free(fixture);
    try testParseAndInsert(db, "10.0.0.1", fixture);

    const count = try getFilteredCount(db, 9_999_999_999_999, 1, null);
    try testing.expectEqual(@as(i64, 0), count);
}

test "spec: count with nonexistent device returns zero" {
    const db = try testOpenDb();
    defer closeDb(db);

    const fixture = try loadFixtureJson(testing.allocator, "indoorFull");
    defer testing.allocator.free(fixture);
    try testParseAndInsert(db, "10.0.0.1", fixture);

    const count = try getFilteredCount(db, 0, nowMillis() + 1000, "nonexistent-serial-xyz");
    try testing.expectEqual(@as(i64, 0), count);
}

// Response shape tests from test-spec.json

test "spec: reading JSON has all required fields and no raw_json" {
    const db = try testOpenDb();
    defer closeDb(db);

    const fixture = try loadFixtureJson(testing.allocator, "indoorFull");
    defer testing.allocator.free(fixture);
    try testParseAndInsert(db, "10.0.0.1", fixture);

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const readings = try queryReadings(alloc, db, .{
        .device = null,
        .from = 0,
        .to = nowMillis() + 1000,
        .limit = 100,
    });

    try testing.expectEqual(@as(usize, 1), readings.len);
    const json = try readingToJson(alloc, &readings[0]);

    // All required fields from test-spec.json responseShapes.reading.requiredFields
    const required = [_][]const u8{
        "id",             "timestamp",      "device_id",        "device_type",
        "device_ip",      "pm01",           "pm02",             "pm10",
        "pm02_compensated", "rco2",         "atmp",             "atmp_compensated",
        "rhum",           "rhum_compensated", "tvoc_index",     "nox_index",
        "wifi",
    };
    for (&required) |field| {
        if (!json.object.contains(field)) {
            std.debug.print("Missing required field: {s}\n", .{field});
            return error.TestUnexpectedResult;
        }
    }

    // Forbidden fields from test-spec.json responseShapes.reading.forbiddenFields
    try testing.expect(!json.object.contains("raw_json"));
}

test "spec: downsampled reading JSON has all required fields, no raw_json or id" {
    const db = try testOpenDb();
    defer closeDb(db);

    const ts_base: i64 = 1_000_000_000_000;
    try testInsertRaw(db, ts_base, "dev1", 10.0, 400);

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const readings = try queryReadings(alloc, db, .{
        .device = null,
        .from = ts_base - 1000,
        .to = ts_base + 1000,
        .limit = 100,
        .downsample_ms = 3_600_000,
    });

    try testing.expectEqual(@as(usize, 1), readings.len);
    const json = try readingToJson(alloc, &readings[0]);

    // All required fields from test-spec.json responseShapes.readingDownsampled.requiredFields
    const required = [_][]const u8{
        "timestamp",      "device_id",        "device_type",    "device_ip",
        "pm01",           "pm02",             "pm10",           "pm02_compensated",
        "rco2",           "atmp",             "atmp_compensated",
        "rhum",           "rhum_compensated", "tvoc_index",     "nox_index",
        "wifi",
    };
    for (&required) |field| {
        if (!json.object.contains(field)) {
            std.debug.print("Missing required field: {s}\n", .{field});
            return error.TestUnexpectedResult;
        }
    }

    // Forbidden fields from test-spec.json responseShapes.readingDownsampled.forbiddenFields
    try testing.expect(!json.object.contains("raw_json"));
    try testing.expect(!json.object.contains("id"));
}

test "spec: device summary JSON has all required fields and no first_seen" {
    const summary = DeviceSummary{
        .device_id = "test123",
        .device_type = "indoor",
        .device_ip = "10.0.0.1",
        .last_seen = 1000,
        .reading_count = 5,
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const json = try deviceSummaryToJson(alloc, &summary);

    // All required fields from test-spec.json responseShapes.device.requiredFields
    const required = [_][]const u8{
        "device_id", "device_type", "device_ip", "last_seen", "reading_count",
    };
    for (&required) |field| {
        if (!json.object.contains(field)) {
            std.debug.print("Missing required field: {s}\n", .{field});
            return error.TestUnexpectedResult;
        }
    }

    // Forbidden fields from test-spec.json responseShapes.device.forbiddenFields
    try testing.expect(!json.object.contains("first_seen"));
}

test "spec: device summary JSON field count matches required exactly" {
    const summary = DeviceSummary{
        .device_id = "abc",
        .device_type = "outdoor",
        .device_ip = "10.0.0.2",
        .last_seen = 2000,
        .reading_count = 3,
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const json = try deviceSummaryToJson(alloc, &summary);

    // Exactly 5 fields: device_id, device_type, device_ip, last_seen, reading_count
    try testing.expectEqual(@as(usize, 5), json.object.count());
}
