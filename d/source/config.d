module config;

import std.json : JSONValue;
import std.process : environment;
import std.conv : to;

struct DeviceConfig {
    string ip;
    string label;
}

struct Config {
    ushort port;
    string dbPath = "./airgradientz.db";
    DeviceConfig[] devices;
    uint pollIntervalMs;
    uint fetchTimeoutMs;
    uint maxApiRows;
    long[string] downsampleBuckets;

    static Config fromEnv() {
        import core.stdc.stdlib : exit;
        import std.stdio : stderr;

        Config c;

        // 1. Config file (mandatory)
        loadConfigFile(c);

        // 2. Env var overrides (highest priority)
        if (auto p = environment.get("PORT")) {
            try {
                int port = p.to!int;
                if (port > 0 && port <= 65535)
                    c.port = cast(ushort) port;
            } catch (Exception) {}
        }

        if (auto db = environment.get("DB_PATH"))
            c.dbPath = db;

        // 3. Validate required keys
        string[] missing;
        if (c.pollIntervalMs == 0)
            missing ~= "pollIntervalMs";
        if (c.fetchTimeoutMs == 0)
            missing ~= "fetchTimeoutMs";
        if (c.maxApiRows == 0)
            missing ~= "maxApiRows";
        if (c.downsampleBuckets.length == 0)
            missing ~= "downsampleBuckets";
        if (c.devices.length == 0)
            missing ~= "devices";
        if (c.port == 0)
            missing ~= "ports.d";

        if (missing.length > 0) {
            import std.array : join;
            stderr.writefln("fatal: missing required config keys: %s", missing.join(", "));
            exit(1);
        }

        return c;
    }
}

private void loadConfigFile(ref Config c) {
    import std.file : readText;
    import std.json : parseJSON, JSONType;
    import std.stdio : stderr;
    import core.stdc.stdlib : exit;

    string path = findConfigPath();
    if (path is null) {
        stderr.writefln("fatal: config file not found");
        exit(1);
    }

    string content;
    try {
        content = readText(path);
    } catch (Exception e) {
        stderr.writefln("[config] Failed to read %s: %s", path, e.msg);
        exit(1);
    }

    typeof(parseJSON("")) root;
    try {
        root = parseJSON(content);
    } catch (Exception e) {
        stderr.writefln("[config] JSON parse error: %s", e.msg);
        exit(1);
    }

    if (root.type != JSONType.object) {
        stderr.writefln("[config] JSON root is not an object");
        exit(1);
    }

    // Read values from top-level keys
    applyConfigValues(c, root);

    // Port from ports.d
    if (auto portsVal = "ports" in root) {
        if (portsVal.type == JSONType.object) {
            if (auto dPort = "d" in *portsVal) {
                if (dPort.type == JSONType.integer || dPort.type == JSONType.uinteger) {
                    auto n = dPort.get!long;
                    if (n > 0 && n <= 65535)
                        c.port = cast(ushort) n;
                }
            }
        }
    }
}

private void applyConfigValues(ref Config c, ref const JSONValue obj) {
    import std.json : JSONType, JSONValue;

    if (auto devicesVal = "devices" in obj) {
        if (devicesVal.type == JSONType.array) {
            DeviceConfig[] parsed;
            foreach (ref dev; devicesVal.array) {
                if (dev.type != JSONType.object) continue;
                auto ipVal = "ip" in dev;
                auto labelVal = "label" in dev;
                if (ipVal && labelVal &&
                    ipVal.type == JSONType.string &&
                    labelVal.type == JSONType.string) {
                    parsed ~= DeviceConfig(ipVal.str, labelVal.str);
                }
            }
            if (parsed.length > 0)
                c.devices = parsed;
        }
    }

    if (auto v = "pollIntervalMs" in obj) {
        if (v.type == JSONType.integer || v.type == JSONType.uinteger) {
            auto n = v.get!long;
            if (n > 0)
                c.pollIntervalMs = cast(uint) n;
        }
    }

    if (auto v = "fetchTimeoutMs" in obj) {
        if (v.type == JSONType.integer || v.type == JSONType.uinteger) {
            auto n = v.get!long;
            if (n > 0)
                c.fetchTimeoutMs = cast(uint) n;
        }
    }

    if (auto v = "maxApiRows" in obj) {
        if (v.type == JSONType.integer || v.type == JSONType.uinteger) {
            auto n = v.get!long;
            if (n > 0)
                c.maxApiRows = cast(uint) n;
        }
    }

    if (auto v = "downsampleBuckets" in obj) {
        if (v.type == JSONType.object) {
            long[string] buckets;
            foreach (key, val; v.object) {
                if (val.type == JSONType.integer || val.type == JSONType.uinteger) {
                    buckets[key] = val.get!long;
                }
            }
            if (buckets.length > 0)
                c.downsampleBuckets = buckets;
        }
    }
}

// ---- unit tests (test-spec.json aligned) ----
unittest {
    import std.stdio : stderr;
    import std.json : parseJSON, JSONType;

    // Helper: create a config from parsed JSON (no file I/O)
    Config parseTestConfig(string json) {
        Config c;
        auto root = parseJSON(json);
        applyConfigValues(c, root);
        return c;
    }

    // Test: all 7 downsample buckets map to correct millisecond values
    {
        auto c = parseTestConfig(`{
            "downsampleBuckets": {
                "5m": 300000,
                "10m": 600000,
                "15m": 900000,
                "30m": 1800000,
                "1h": 3600000,
                "1d": 86400000,
                "1w": 604800000
            }
        }`);

        assert(c.downsampleBuckets.length == 7, "expected 7 downsample buckets");
        assert(c.downsampleBuckets["5m"]  == 300_000,     "5m should be 300000ms");
        assert(c.downsampleBuckets["10m"] == 600_000,     "10m should be 600000ms");
        assert(c.downsampleBuckets["15m"] == 900_000,     "15m should be 900000ms");
        assert(c.downsampleBuckets["30m"] == 1_800_000,   "30m should be 1800000ms");
        assert(c.downsampleBuckets["1h"]  == 3_600_000,   "1h should be 3600000ms");
        assert(c.downsampleBuckets["1d"]  == 86_400_000,  "1d should be 86400000ms");
        assert(c.downsampleBuckets["1w"]  == 604_800_000, "1w should be 604800000ms");
        stderr.writeln("  pass: all 7 downsample buckets have correct ms values");
    }

    // Test: invalid downsample key not present
    {
        auto c = parseTestConfig(`{
            "downsampleBuckets": {
                "5m": 300000,
                "10m": 600000,
                "15m": 900000,
                "30m": 1800000,
                "1h": 3600000,
                "1d": 86400000,
                "1w": 604800000
            }
        }`);
        assert(("2h" in c.downsampleBuckets) is null, "2h should not be a valid bucket");
        assert(("3d" in c.downsampleBuckets) is null, "3d should not be a valid bucket");
        stderr.writeln("  pass: invalid downsample keys absent");
    }

    // Test: config parses devices, pollIntervalMs, fetchTimeoutMs, maxApiRows
    {
        auto c = parseTestConfig(`{
            "devices": [
                {"ip": "192.168.1.1", "label": "outdoor"},
                {"ip": "192.168.1.2", "label": "indoor"}
            ],
            "pollIntervalMs": 15000,
            "fetchTimeoutMs": 5000,
            "maxApiRows": 10000
        }`);
        assert(c.devices.length == 2, "expected 2 devices");
        assert(c.devices[0].ip == "192.168.1.1", "first device ip");
        assert(c.devices[0].label == "outdoor", "first device label");
        assert(c.pollIntervalMs == 15000, "pollIntervalMs");
        assert(c.fetchTimeoutMs == 5000, "fetchTimeoutMs");
        assert(c.maxApiRows == 10000, "maxApiRows");
        stderr.writeln("  pass: config parses all required fields");
    }

    // Test: downsampleBuckets with wrong type values are skipped
    {
        auto c = parseTestConfig(`{
            "downsampleBuckets": {
                "5m": "not_a_number",
                "10m": 600000
            }
        }`);
        assert(c.downsampleBuckets.length == 1, "only valid numeric entries kept");
        assert(("5m" in c.downsampleBuckets) is null, "string value should be skipped");
        assert(c.downsampleBuckets["10m"] == 600_000, "valid entry preserved");
        stderr.writeln("  pass: non-numeric bucket values skipped");
    }
}

private string findConfigPath() {
    import std.file : exists;
    import std.process : environment;
    import log : logf;

    if (auto envPath = environment.get("CONFIG_PATH")) {
        if (exists(envPath)) {
            logf("[config] Loaded config from CONFIG_PATH: %s", envPath);
            return envPath;
        }
        logf("[config] CONFIG_PATH set but unreadable: %s", envPath);
    }

    if (exists("./airgradientz.json")) {
        logf("[config] Loaded config from ./airgradientz.json");
        return "./airgradientz.json";
    }

    if (exists("../airgradientz.json")) {
        logf("[config] Loaded config from ../airgradientz.json");
        return "../airgradientz.json";
    }

    return null;
}
