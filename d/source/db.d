module db;

import std.json;
import std.conv : to;
import std.datetime : Clock;
import std.typecons : Nullable;

import d2sqlite3 : Database, Statement, Row;

// Package-level SQL loaded from queries.sql (or hardcoded fallbacks).
private string loadedQueryCols;
private string loadedInsertSQL;
private string loadedLatestSQL;
private string loadedDevicesSQL;
private string loadedCountSQL;
private bool queriesLoaded = false;

long nowMillis() {
    import core.time : convert;
    auto t = Clock.currStdTime(); // hnsecs since Jan 1, 1 AD
    // Convert to unix milliseconds
    enum hnsecsSinceUnixEpoch = 621_355_968_000_000_000L;
    return (t - hnsecsSinceUnixEpoch) / 10_000;
}

private void loadQueries() {
    if (queriesLoaded) return;
    queriesLoaded = true;

    import std.file : readText;
    import std.stdio : stderr;

    string content;
    try {
        content = readText("../queries.sql");
    } catch (Exception e) {
        stderr.writefln("[db] queries.sql not found (%s), using defaults", e.msg);
        setDefaultQueries();
        return;
    }

    auto queries = parseQueriesSQL(content);

    if (auto p = "reading_columns" in queries)
        loadedQueryCols = *p;
    else
        loadedQueryCols = DEFAULT_QUERY_COLS;

    if (auto p = "insert_reading" in queries)
        loadedInsertSQL = convertPlaceholders(*p);
    else
        loadedInsertSQL = DEFAULT_INSERT_SQL;

    if (auto p = "select_latest" in queries)
        loadedLatestSQL = *p;
    else
        loadedLatestSQL = DEFAULT_LATEST_SQL;

    if (auto p = "select_devices" in queries)
        loadedDevicesSQL = *p;
    else
        loadedDevicesSQL = DEFAULT_DEVICES_SQL;

    if (auto p = "count_readings" in queries)
        loadedCountSQL = *p;
    else
        loadedCountSQL = DEFAULT_COUNT_SQL;

    stderr.writefln("[db] loaded %d queries from queries.sql", queries.length);
}

private void setDefaultQueries() {
    loadedQueryCols = DEFAULT_QUERY_COLS;
    loadedInsertSQL = DEFAULT_INSERT_SQL;
    loadedLatestSQL = DEFAULT_LATEST_SQL;
    loadedDevicesSQL = DEFAULT_DEVICES_SQL;
    loadedCountSQL = DEFAULT_COUNT_SQL;
}

private enum DEFAULT_QUERY_COLS = "id, timestamp, device_id, device_type, device_ip, " ~
    "pm01, pm02, pm10, pm02_compensated, rco2, " ~
    "atmp, atmp_compensated, rhum, rhum_compensated, " ~
    "tvoc_index, nox_index, wifi";

private enum DEFAULT_INSERT_SQL = "INSERT INTO readings (" ~
    "timestamp, device_id, device_type, device_ip, " ~
    "pm01, pm02, pm10, pm02_compensated, " ~
    "rco2, atmp, atmp_compensated, rhum, rhum_compensated, " ~
    "tvoc_index, nox_index, wifi, raw_json" ~
    ") VALUES (" ~
    "?, ?, ?, ?, " ~
    "?, ?, ?, ?, " ~
    "?, ?, ?, ?, ?, " ~
    "?, ?, ?, ?" ~
    ")";

private enum DEFAULT_LATEST_SQL = "SELECT r.id, r.timestamp, r.device_id, r.device_type, r.device_ip, " ~
    "r.pm01, r.pm02, r.pm10, r.pm02_compensated, r.rco2, " ~
    "r.atmp, r.atmp_compensated, r.rhum, r.rhum_compensated, " ~
    "r.tvoc_index, r.nox_index, r.wifi " ~
    "FROM readings r " ~
    "INNER JOIN (" ~
    " SELECT device_id, MAX(id) AS max_id" ~
    " FROM readings" ~
    " GROUP BY device_id" ~
    ") latest ON r.id = latest.max_id";

private enum DEFAULT_DEVICES_SQL = "SELECT device_id, device_type, device_ip, " ~
    "MAX(timestamp) AS last_seen, " ~
    "COUNT(*) AS reading_count " ~
    "FROM readings " ~
    "GROUP BY device_id " ~
    "ORDER BY device_type";

private enum DEFAULT_COUNT_SQL = "SELECT COUNT(*) FROM readings";

package string[string] parseQueriesSQL(string content) {
    import std.string : indexOf, strip, join;
    import std.algorithm : startsWith;

    string[string] queries;
    string name;
    string[] lines;

    foreach (line; content.lineSplitter()) {
        auto trimmed = line.strip();
        if (trimmed.startsWith("-- name: ")) {
            if (name.length > 0) {
                auto body = lines.join("\n").strip();
                if (body.length > 0 && body[$ - 1] == ';')
                    body = body[0 .. $ - 1];
                queries[name] = body;
            }
            name = trimmed["-- name: ".length .. $].strip();
            lines = [];
        } else if (trimmed.startsWith("--") || trimmed.length == 0) {
            // skip comments and blank lines
        } else {
            lines ~= trimmed;
        }
    }
    if (name.length > 0) {
        auto body = lines.join("\n").strip();
        if (body.length > 0 && body[$ - 1] == ';')
            body = body[0 .. $ - 1];
        queries[name] = body;
    }
    return queries;
}

private auto lineSplitter(string s) {
    import std.algorithm : std_splitter = splitter;
    return std_splitter(s, "\n");
}

package string convertPlaceholders(string sql) {
    import std.regex : regex, replaceAll;
    auto re = regex(`:[a-z][a-z0-9_]*`);
    return sql.replaceAll(re, "?");
}

Database initDb(string path) {
    import std.stdio : stderr;
    import std.file : readText;

    stderr.writefln("[db] Opening database at %s", path);

    loadQueries();

    auto db = Database(path);
    db.run("PRAGMA journal_mode = WAL;");
    db.run("PRAGMA busy_timeout = 5000;");
    db.run("PRAGMA foreign_keys = ON;");

    string schema;
    try {
        schema = readText("../schema.sql");
    } catch (Exception e) {
        stderr.writefln("[db] Failed to read ../schema.sql: %s, using fallback", e.msg);
        schema = "CREATE TABLE IF NOT EXISTS readings (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            timestamp INTEGER NOT NULL,
            device_id TEXT NOT NULL,
            device_type TEXT NOT NULL CHECK(device_type IN ('indoor', 'outdoor')),
            device_ip TEXT NOT NULL,
            pm01 REAL,
            pm02 REAL,
            pm10 REAL,
            pm02_compensated REAL,
            rco2 INTEGER,
            atmp REAL,
            atmp_compensated REAL,
            rhum REAL,
            rhum_compensated REAL,
            tvoc_index REAL,
            nox_index REAL,
            wifi INTEGER,
            raw_json TEXT NOT NULL
        );
        CREATE INDEX IF NOT EXISTS idx_readings_ts ON readings(timestamp);
        CREATE INDEX IF NOT EXISTS idx_readings_device ON readings(device_id, timestamp);";
    }

    db.run(schema);

    return db;
}

long getReadingsCount(ref Database db) {
    auto stmt = db.prepare(loadedCountSQL);
    foreach (row; stmt.execute()) {
        return row.peek!long(0);
    }
    stmt.reset();
    return 0;
}

private Nullable!double extractF64(JSONValue data, string key) {
    if (auto p = key in data) {
        if (p.type == JSONType.float_) return Nullable!double(p.get!double);
        if (p.type == JSONType.integer) return Nullable!double(cast(double) p.get!long);
        if (p.type == JSONType.uinteger) return Nullable!double(cast(double) p.get!ulong);
    }
    return Nullable!double.init;
}

private Nullable!long extractI64(JSONValue data, string key) {
    if (auto p = key in data) {
        if (p.type == JSONType.integer) return Nullable!long(p.get!long);
        if (p.type == JSONType.uinteger) return Nullable!long(cast(long) p.get!ulong);
        if (p.type == JSONType.float_) return Nullable!long(cast(long) p.get!double);
    }
    return Nullable!long.init;
}

private string extractStr(JSONValue data, string key) {
    if (auto p = key in data) {
        if (p.type == JSONType.string) return p.get!string;
    }
    return null;
}

void insertReading(ref Database db, string ip, JSONValue data) {
    auto model = extractStr(data, "model");
    if (model is null) model = "";
    string deviceType = (model.length >= 2 && model[0] == 'I' && model[1] == '-') ? "indoor" : "outdoor";

    auto serial = extractStr(data, "serialno");
    if (serial is null) serial = "unknown";

    auto rawJson = data.toString();

    auto stmt = db.prepare(loadedInsertSQL);

    stmt.bind(1, nowMillis());
    stmt.bind(2, serial);
    stmt.bind(3, deviceType);
    stmt.bind(4, ip);
    bindNullableDouble(stmt, 5, extractF64(data, "pm01"));
    bindNullableDouble(stmt, 6, extractF64(data, "pm02"));
    bindNullableDouble(stmt, 7, extractF64(data, "pm10"));
    bindNullableDouble(stmt, 8, extractF64(data, "pm02Compensated"));
    bindNullableLong(stmt, 9, extractI64(data, "rco2"));
    bindNullableDouble(stmt, 10, extractF64(data, "atmp"));
    bindNullableDouble(stmt, 11, extractF64(data, "atmpCompensated"));
    bindNullableDouble(stmt, 12, extractF64(data, "rhum"));
    bindNullableDouble(stmt, 13, extractF64(data, "rhumCompensated"));
    bindNullableDouble(stmt, 14, extractF64(data, "tvocIndex"));
    bindNullableDouble(stmt, 15, extractF64(data, "noxIndex"));
    bindNullableLong(stmt, 16, extractI64(data, "wifi"));
    stmt.bind(17, rawJson);

    stmt.execute();
    stmt.reset();
}

private void bindNullableDouble(ref Statement stmt, int idx, Nullable!double val) {
    if (val.isNull)
        stmt.bind(idx, null);
    else
        stmt.bind(idx, val.get);
}

private void bindNullableLong(ref Statement stmt, int idx, Nullable!long val) {
    if (val.isNull)
        stmt.bind(idx, null);
    else
        stmt.bind(idx, val.get);
}

struct Reading {
    long id;
    long timestamp;
    string deviceId;
    string deviceType;
    string deviceIp;
    Nullable!double pm01, pm02, pm10, pm02Compensated;
    Nullable!long rco2;
    Nullable!double atmp, atmpCompensated;
    Nullable!double rhum, rhumCompensated;
    Nullable!double tvocIndex, noxIndex;
    Nullable!long wifi;

    JSONValue toJson() const {
        JSONValue obj = JSONValue(string[string].init);
        obj["id"] = JSONValue(id);
        obj["timestamp"] = JSONValue(timestamp);
        obj["device_id"] = JSONValue(deviceId);
        obj["device_type"] = JSONValue(deviceType);
        obj["device_ip"] = JSONValue(deviceIp);
        obj["pm01"] = nullableDoubleJson(pm01);
        obj["pm02"] = nullableDoubleJson(pm02);
        obj["pm10"] = nullableDoubleJson(pm10);
        obj["pm02_compensated"] = nullableDoubleJson(pm02Compensated);
        obj["rco2"] = nullableLongJson(rco2);
        obj["atmp"] = nullableDoubleJson(atmp);
        obj["atmp_compensated"] = nullableDoubleJson(atmpCompensated);
        obj["rhum"] = nullableDoubleJson(rhum);
        obj["rhum_compensated"] = nullableDoubleJson(rhumCompensated);
        obj["tvoc_index"] = nullableDoubleJson(tvocIndex);
        obj["nox_index"] = nullableDoubleJson(noxIndex);
        obj["wifi"] = nullableLongJson(wifi);
        return obj;
    }
}

private JSONValue nullableDoubleJson(Nullable!double v) {
    return v.isNull ? JSONValue(null) : JSONValue(v.get);
}

private JSONValue nullableLongJson(Nullable!long v) {
    return v.isNull ? JSONValue(null) : JSONValue(v.get);
}

private string queryCols() {
    return loadedQueryCols.length > 0 ? loadedQueryCols : DEFAULT_QUERY_COLS;
}

private Reading rowToReading(Row)(auto ref Row row) {
    Reading r;
    r.id = row.peek!long(0);
    r.timestamp = row.peek!long(1);
    r.deviceId = row.peek!string(2);
    r.deviceType = row.peek!string(3);
    r.deviceIp = row.peek!string(4);
    r.pm01 = row.peek!(Nullable!double)(5);
    r.pm02 = row.peek!(Nullable!double)(6);
    r.pm10 = row.peek!(Nullable!double)(7);
    r.pm02Compensated = row.peek!(Nullable!double)(8);
    r.rco2 = row.peek!(Nullable!long)(9);
    r.atmp = row.peek!(Nullable!double)(10);
    r.atmpCompensated = row.peek!(Nullable!double)(11);
    r.rhum = row.peek!(Nullable!double)(12);
    r.rhumCompensated = row.peek!(Nullable!double)(13);
    r.tvocIndex = row.peek!(Nullable!double)(14);
    r.noxIndex = row.peek!(Nullable!double)(15);
    r.wifi = row.peek!(Nullable!long)(16);
    return r;
}

Reading[] queryReadings(ref Database db, string device, long from, long to, uint limit) {
    bool wantDevice = device !is null && device != "all";

    string sql;
    if (!wantDevice && limit == 0)
        sql = "SELECT " ~ queryCols() ~ " FROM readings WHERE timestamp >= ? AND timestamp <= ? ORDER BY timestamp ASC";
    else if (!wantDevice)
        sql = "SELECT " ~ queryCols() ~ " FROM readings WHERE timestamp >= ? AND timestamp <= ? ORDER BY timestamp ASC LIMIT ?";
    else if (limit == 0)
        sql = "SELECT " ~ queryCols() ~ " FROM readings WHERE device_id = ? AND timestamp >= ? AND timestamp <= ? ORDER BY timestamp ASC";
    else
        sql = "SELECT " ~ queryCols() ~ " FROM readings WHERE device_id = ? AND timestamp >= ? AND timestamp <= ? ORDER BY timestamp ASC LIMIT ?";

    auto stmt = db.prepare(sql);

    if (!wantDevice && limit == 0) {
        stmt.bind(1, from);
        stmt.bind(2, to);
    } else if (!wantDevice) {
        stmt.bind(1, from);
        stmt.bind(2, to);
        stmt.bind(3, cast(long) limit);
    } else if (limit == 0) {
        stmt.bind(1, device);
        stmt.bind(2, from);
        stmt.bind(3, to);
    } else {
        stmt.bind(1, device);
        stmt.bind(2, from);
        stmt.bind(3, to);
        stmt.bind(4, cast(long) limit);
    }

    Reading[] results;
    foreach (row; stmt.execute()) {
        results ~= rowToReading(row);
    }
    stmt.reset();
    return results;
}

struct DeviceSummary {
    string deviceId;
    string deviceType;
    string deviceIp;
    long lastSeen;
    long readingCount;

    JSONValue toJson() const {
        JSONValue obj = JSONValue(string[string].init);
        obj["device_id"] = JSONValue(deviceId);
        obj["device_type"] = JSONValue(deviceType);
        obj["device_ip"] = JSONValue(deviceIp);
        obj["last_seen"] = JSONValue(lastSeen);
        obj["reading_count"] = JSONValue(readingCount);
        return obj;
    }
}

DeviceSummary[] getDevices(ref Database db) {
    auto stmt = db.prepare(loadedDevicesSQL);

    DeviceSummary[] results;
    foreach (row; stmt.execute()) {
        DeviceSummary d;
        d.deviceId = row.peek!string(0);
        d.deviceType = row.peek!string(1);
        d.deviceIp = row.peek!string(2);
        d.lastSeen = row.peek!long(3);
        d.readingCount = row.peek!long(4);
        results ~= d;
    }
    stmt.reset();
    return results;
}

Reading[] getLatestReadings(ref Database db) {
    auto stmt = db.prepare(loadedLatestSQL);
    Reading[] results;
    foreach (row; stmt.execute()) {
        results ~= rowToReading(row);
    }
    stmt.reset();
    return results;
}

void checkpoint(ref Database db) {
    db.run("PRAGMA wal_checkpoint(TRUNCATE);");
}
