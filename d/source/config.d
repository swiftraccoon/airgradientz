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
