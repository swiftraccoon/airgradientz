module config;

import std.process : environment;
import std.conv : to;

struct DeviceConfig {
    string ip;
    string label;
}

struct Config {
    ushort port = 3014;
    string dbPath = "./airgradientz.db";
    DeviceConfig[] devices;
    uint pollIntervalMs = 15_000;
    uint fetchTimeoutMs = 5_000;
    uint maxApiRows = 10_000;

    static Config fromEnv() {
        Config c;

        // 1. Hardcoded defaults
        c.devices = [
            DeviceConfig("192.168.88.6", "outdoor"),
            DeviceConfig("192.168.88.159", "indoor"),
        ];

        // 2. Config file overrides
        loadConfigFile(c);

        // 3. Env var overrides (highest priority)
        if (auto p = environment.get("PORT")) {
            try {
                int port = p.to!int;
                if (port > 0 && port <= 65535)
                    c.port = cast(ushort) port;
            } catch (Exception) {}
        }

        if (auto db = environment.get("DB_PATH"))
            c.dbPath = db;

        return c;
    }
}

private void loadConfigFile(ref Config c) {
    import std.file : exists, readText;
    import std.json : parseJSON, JSONType;
    import std.stdio : stderr;

    string path = findConfigPath();
    if (path is null)
        return;

    string content;
    try {
        content = readText(path);
    } catch (Exception e) {
        stderr.writefln("[config] Failed to read %s: %s", path, e.msg);
        return;
    }

    typeof(parseJSON("")) root;
    try {
        root = parseJSON(content);
    } catch (Exception e) {
        stderr.writefln("[config] JSON parse error: %s", e.msg);
        return;
    }

    if (root.type != JSONType.object)
        return;

    // Devices
    if (auto devicesVal = "devices" in root) {
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

    // Scalar fields
    if (auto v = "pollIntervalMs" in root) {
        if (v.type == JSONType.integer || v.type == JSONType.uinteger) {
            auto n = v.get!long;
            if (n > 0)
                c.pollIntervalMs = cast(uint) n;
        }
    }

    if (auto v = "fetchTimeoutMs" in root) {
        if (v.type == JSONType.integer || v.type == JSONType.uinteger) {
            auto n = v.get!long;
            if (n > 0)
                c.fetchTimeoutMs = cast(uint) n;
        }
    }

    if (auto v = "maxApiRows" in root) {
        if (v.type == JSONType.integer || v.type == JSONType.uinteger) {
            auto n = v.get!long;
            if (n > 0)
                c.maxApiRows = cast(uint) n;
        }
    }
}

private string findConfigPath() {
    import std.file : exists;
    import std.stdio : stderr;
    import std.process : environment;

    if (auto envPath = environment.get("CONFIG_PATH")) {
        if (exists(envPath)) {
            stderr.writefln("[config] Loaded config from CONFIG_PATH: %s", envPath);
            return envPath;
        }
        stderr.writefln("[config] CONFIG_PATH set but unreadable: %s", envPath);
    }

    if (exists("./airgradientz.json")) {
        stderr.writefln("[config] Loaded config from ./airgradientz.json");
        return "./airgradientz.json";
    }

    if (exists("../airgradientz.json")) {
        stderr.writefln("[config] Loaded config from ../airgradientz.json");
        return "../airgradientz.json";
    }

    return null;
}
