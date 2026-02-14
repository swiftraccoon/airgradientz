module config;

import std.process : environment;
import std.conv : to;

struct DeviceConfig {
    string ip;
    string label;
}

struct Config {
    ushort port = 3012;
    string dbPath = "./airgradientz.db";
    DeviceConfig[] devices;
    uint pollIntervalMs = 15_000;
    uint fetchTimeoutMs = 5_000;
    uint maxApiRows = 10_000;

    static Config fromEnv() {
        Config c;

        if (auto p = environment.get("PORT")) {
            try {
                int port = p.to!int;
                if (port > 0 && port <= 65535)
                    c.port = cast(ushort) port;
            } catch (Exception) {}
        }

        if (auto db = environment.get("DB_PATH"))
            c.dbPath = db;

        c.devices = [
            DeviceConfig("192.168.88.6", "outdoor"),
            DeviceConfig("192.168.88.159", "indoor"),
        ];

        return c;
    }
}
