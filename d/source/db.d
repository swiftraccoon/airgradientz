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
    import log : logf;

    string content;
    try {
        content = readText("../queries.sql");
    } catch (Exception e) {
        logf("[db] queries.sql not found (%s), using defaults", e.msg);
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

    logf("[db] loaded %d queries from queries.sql", queries.length);
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
    import std.file : readText;
    import log : logf;

    logf("[db] Opening database at %s", path);

    loadQueries();

    auto db = Database(path);
    db.run("PRAGMA journal_mode = WAL;");
    db.run("PRAGMA busy_timeout = 5000;");
    db.run("PRAGMA foreign_keys = ON;");

    string schema;
    try {
        schema = readText("../schema.sql");
    } catch (Exception e) {
        logf("[db] Failed to read ../schema.sql: %s, using fallback", e.msg);
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
    bool downsampled;

    JSONValue toJson() const {
        JSONValue obj = JSONValue(string[string].init);
        if (!downsampled)
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

private Reading rowToDownsampledReading(Row)(auto ref Row row) {
    Reading r;
    r.downsampled = true;
    r.timestamp = row.peek!long(0);
    r.deviceId = row.peek!string(1);
    r.deviceType = row.peek!string(2);
    r.deviceIp = row.peek!string(3);
    r.pm01 = row.peek!(Nullable!double)(4);
    r.pm02 = row.peek!(Nullable!double)(5);
    r.pm10 = row.peek!(Nullable!double)(6);
    r.pm02Compensated = row.peek!(Nullable!double)(7);
    r.rco2 = row.peek!(Nullable!long)(8);
    r.atmp = row.peek!(Nullable!double)(9);
    r.atmpCompensated = row.peek!(Nullable!double)(10);
    r.rhum = row.peek!(Nullable!double)(11);
    r.rhumCompensated = row.peek!(Nullable!double)(12);
    r.tvocIndex = row.peek!(Nullable!double)(13);
    r.noxIndex = row.peek!(Nullable!double)(14);
    r.wifi = row.peek!(Nullable!long)(15);
    return r;
}

Reading[] queryReadingsDownsampled(ref Database db, string device, long from, long to, uint limit, long bucketMs) {
    import std.format : format;

    bool wantDevice = device !is null && device != "all";

    string sql = format("SELECT (timestamp / %d) * %d AS timestamp, " ~
        "device_id, device_type, device_ip, " ~
        "AVG(pm01) AS pm01, AVG(pm02) AS pm02, AVG(pm10) AS pm10, " ~
        "AVG(pm02_compensated) AS pm02_compensated, " ~
        "CAST(AVG(rco2) AS INTEGER) AS rco2, " ~
        "AVG(atmp) AS atmp, AVG(atmp_compensated) AS atmp_compensated, " ~
        "AVG(rhum) AS rhum, AVG(rhum_compensated) AS rhum_compensated, " ~
        "AVG(tvoc_index) AS tvoc_index, AVG(nox_index) AS nox_index, " ~
        "CAST(AVG(wifi) AS INTEGER) AS wifi " ~
        "FROM readings WHERE timestamp >= ? AND timestamp <= ?%s " ~
        "GROUP BY (timestamp / %d), device_id " ~
        "ORDER BY timestamp ASC%s",
        bucketMs, bucketMs,
        wantDevice ? " AND device_id = ?" : "",
        bucketMs,
        limit > 0 ? " LIMIT ?" : "");

    auto stmt = db.prepare(sql);

    int idx = 1;
    stmt.bind(idx++, from);
    stmt.bind(idx++, to);
    if (wantDevice)
        stmt.bind(idx++, device);
    if (limit > 0)
        stmt.bind(idx++, cast(long) limit);

    Reading[] results;
    foreach (row; stmt.execute()) {
        results ~= rowToDownsampledReading(row);
    }
    stmt.reset();
    return results;
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

long getFilteredReadingsCount(ref Database db, string device, long from, long to) {
    bool wantDevice = device !is null && device != "all";

    string sql;
    if (wantDevice)
        sql = "SELECT COUNT(*) FROM readings WHERE timestamp >= ? AND timestamp <= ? AND device_id = ?";
    else
        sql = "SELECT COUNT(*) FROM readings WHERE timestamp >= ? AND timestamp <= ?";

    auto stmt = db.prepare(sql);
    stmt.bind(1, from);
    stmt.bind(2, to);
    if (wantDevice)
        stmt.bind(3, device);

    foreach (row; stmt.execute()) {
        auto count = row.peek!long(0);
        stmt.reset();
        return count;
    }
    stmt.reset();
    return 0;
}

void checkpoint(ref Database db) {
    db.run("PRAGMA wal_checkpoint(TRUNCATE);");
}

// ---- unit tests ----
unittest {
    import std.stdio : stderr;

    // Helper: open in-memory DB with schema
    Database openTestDb() {
        setDefaultQueries();  // ensure queries are available without queries.sql
        auto testDb = Database(":memory:");
        testDb.run("PRAGMA journal_mode = WAL;");
        testDb.run("PRAGMA busy_timeout = 5000;");
        testDb.run("PRAGMA foreign_keys = ON;");
        testDb.run("CREATE TABLE IF NOT EXISTS readings (
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
        CREATE INDEX IF NOT EXISTS idx_readings_device ON readings(device_id, timestamp);");
        return testDb;
    }

    void insertRaw(ref Database testDb, long ts, string deviceId, string deviceType, string deviceIp,
                   double pm02, long rco2) {
        import std.format : format;
        auto sql = format("INSERT INTO readings (timestamp, device_id, device_type, device_ip, pm02, rco2, raw_json) VALUES (%d, '%s', '%s', '%s', %g, %d, '{}')",
            ts, deviceId, deviceType, deviceIp, pm02, rco2);
        testDb.run(sql);
    }

    // Test: readings ordered by timestamp ASC
    {
        auto testDb = openTestDb();
        auto now = nowMillis();
        insertRaw(testDb, now - 3000, "dev1", "indoor", "1.1.1.1", 10.0, 400);
        insertRaw(testDb, now - 2000, "dev1", "indoor", "1.1.1.1", 20.0, 500);
        insertRaw(testDb, now - 1000, "dev1", "indoor", "1.1.1.1", 30.0, 600);

        auto readings = queryReadings(testDb, "all", 0, now + 1000, 100);
        assert(readings.length == 3, "expected 3 readings");
        assert(readings[0].id < readings[1].id, "first id < second id");
        assert(readings[1].id < readings[2].id, "second id < third id");
        assert(readings[0].timestamp <= readings[1].timestamp, "ASC timestamp order");
        stderr.writeln("  pass: readings ordered ASC");
    }

    // Test: latest by MAX(id) not MAX(timestamp)
    {
        auto testDb = openTestDb();
        auto now = nowMillis();
        insertRaw(testDb, now, "dev1", "indoor", "1.1.1.1", 10.0, 400);
        insertRaw(testDb, now, "dev1", "indoor", "1.1.1.1", 99.0, 999);

        auto latest = getLatestReadings(testDb);
        assert(latest.length == 1, "expected 1 latest reading");
        assert(!latest[0].pm02.isNull, "pm02 should not be null");
        assert(latest[0].pm02.get == 99.0, "latest should have pm02=99 (higher id)");
        assert(!latest[0].rco2.isNull, "rco2 should not be null");
        assert(latest[0].rco2.get == 999, "latest should have rco2=999 (higher id)");
        stderr.writeln("  pass: latest by MAX(id)");
    }

    // Test: devices response does not contain first_seen
    {
        auto testDb = openTestDb();
        auto now = nowMillis();
        insertRaw(testDb, now, "dev1", "indoor", "1.1.1.1", 10.0, 400);

        auto devices = getDevices(testDb);
        assert(devices.length == 1, "expected 1 device");
        auto j = devices[0].toJson();
        assert(("first_seen" in j.object) is null, "devices should NOT have first_seen");
        assert(("device_id" in j.object) !is null, "devices should have device_id");
        assert(("last_seen" in j.object) !is null, "devices should have last_seen");
        assert(("reading_count" in j.object) !is null, "devices should have reading_count");
        stderr.writeln("  pass: devices no first_seen");
    }

    // Test: insertReading with full indoor data
    {
        auto testDb = openTestDb();
        auto indoorJson = parseJSON(`{"wifi":-47,"serialno":"84fce602549c","model":"I-9PSL","pm01":5,"pm02":41.67,"pm10":44.83,"pm003Count":1638,"pm005Count":516.67,"pm01Count":79.83,"pm02Count":16.67,"rco2":489,"atmp":20.78,"atmpCompensated":20.78,"rhum":44.32,"rhumCompensated":44.32,"tvocIndex":100,"tvocRaw":26108,"noxIndex":1,"noxRaw":16370,"boot":1577,"bootCount":1577,"firmware":"3.6.2","pm02Compensated":41.67}`);
        insertReading(testDb, "10.0.0.1", indoorJson);

        auto now = nowMillis();
        auto readings = queryReadings(testDb, "all", 0, now + 1000, 100);
        assert(readings.length == 1, "expected 1 reading after insertReading");
        assert(readings[0].deviceId == "84fce602549c", "device_id should be serialno");
        assert(readings[0].deviceType == "indoor", "model I-* should be indoor");
        assert(!readings[0].pm02.isNull, "pm02 should not be null");
        assert(readings[0].pm02.get == 41.67, "pm02 should be 41.67");
        assert(!readings[0].rco2.isNull, "rco2 should not be null");
        assert(readings[0].rco2.get == 489, "rco2 should be 489");
        stderr.writeln("  pass: insertReading with full data");
    }

    // Test: insertReading with null fields
    {
        auto testDb = openTestDb();
        auto nullJson = parseJSON(`{"wifi":-59,"serialno":"84fce602549c","model":"I-9PSL","firmware":"3.6.2","boot":0,"bootCount":0}`);
        insertReading(testDb, "10.0.0.2", nullJson);

        auto now = nowMillis();
        auto readings = queryReadings(testDb, "all", 0, now + 1000, 100);
        assert(readings.length == 1, "expected 1 reading after insertReading with nulls");
        assert(readings[0].pm01.isNull, "pm01 should be null");
        assert(readings[0].pm02.isNull, "pm02 should be null");
        assert(readings[0].rco2.isNull, "rco2 should be null");
        assert(!readings[0].wifi.isNull, "wifi should not be null");
        assert(readings[0].wifi.get == -59, "wifi should be -59");
        stderr.writeln("  pass: insertReading with null fields");
    }
}
