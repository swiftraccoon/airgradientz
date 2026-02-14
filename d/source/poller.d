module poller;

import std.stdio : stderr;
import std.format : format;
import std.json : JSONValue, parseJSON, JSONType;
import std.conv : to;
import core.thread : Thread;
import core.time : dur;

import d2sqlite3 : Database;

import http.client : httpGet;
import db;
import state : AppState;

enum CHECKPOINT_INTERVAL_POLLS = 10;

enum HealthStatus { unknown, ok, error }

struct DeviceHealth {
    string ip;
    string label;
    HealthStatus status = HealthStatus.unknown;
    long lastSuccess = 0;
    long lastError = 0;
    string lastErrorMessage;
    uint consecutiveFailures = 0;

    JSONValue toJson() const {
        JSONValue obj = JSONValue(string[string].init);
        obj["ip"] = JSONValue(ip);
        obj["label"] = JSONValue(label);
        obj["status"] = JSONValue(statusString);
        obj["lastSuccess"] = (lastSuccess > 0) ? JSONValue(lastSuccess) : JSONValue(null);
        obj["lastError"] = (lastError > 0) ? JSONValue(lastError) : JSONValue(null);
        obj["lastErrorMessage"] = (lastErrorMessage.length > 0) ? JSONValue(lastErrorMessage) : JSONValue(null);
        obj["consecutiveFailures"] = JSONValue(cast(long) consecutiveFailures);
        return obj;
    }

    private string statusString() const {
        final switch (status) {
            case HealthStatus.unknown: return "unknown";
            case HealthStatus.ok:      return "ok";
            case HealthStatus.error:   return "error";
        }
    }
}

JSONValue getHealthJson(AppState appState) {
    return appState.withHealthRead((const DeviceHealth[string] health) {
        JSONValue[] items;
        foreach (dev; appState.config.devices) {
            if (auto h = dev.ip in health)
                items ~= h.toJson();
        }
        return JSONValue(items);
    });
}

private void recordSuccess(ref DeviceHealth[string] health, string ip) {
    if (auto h = ip in health) {
        h.status = HealthStatus.ok;
        h.lastSuccess = db.nowMillis();
        h.lastErrorMessage = null;
        h.consecutiveFailures = 0;
    }
}

private void recordFailure(ref DeviceHealth[string] health, string ip, string message) {
    if (auto h = ip in health) {
        h.status = HealthStatus.error;
        h.lastError = db.nowMillis();
        h.lastErrorMessage = message;
        h.consecutiveFailures++;
    }
}

private void fetchDevice(AppState appState, string ip, string label) {
    string body_;
    try {
        body_ = httpGet(ip, "/measures/current", dur!"msecs"(appState.config.fetchTimeoutMs));
    } catch (Exception e) {
        auto msg = e.msg;
        stderr.writefln("[poller] %s (%s): fetch failed: %s", label, ip, msg);
        appState.withHealthWrite((ref DeviceHealth[string] health) {
            recordFailure(health, ip, msg);
        });
        return;
    }

    JSONValue data;
    try {
        data = parseJSON(body_);
    } catch (Exception e) {
        auto msg = "JSON parse error: " ~ e.msg;
        stderr.writefln("[poller] %s (%s): %s", label, ip, msg);
        appState.withHealthWrite((ref DeviceHealth[string] health) {
            recordFailure(health, ip, msg);
        });
        return;
    }

    if (data.type != JSONType.object) {
        auto msg = "unexpected response type: not an object";
        stderr.writefln("[poller] %s (%s): %s", label, ip, msg);
        appState.withHealthWrite((ref DeviceHealth[string] health) {
            recordFailure(health, ip, msg);
        });
        return;
    }

    // Insert into database
    try {
        appState.withDb((ref Database db_) {
            db.insertReading(db_, ip, data);
        });
    } catch (Exception e) {
        auto msg = "DB insert failed: " ~ e.msg;
        stderr.writefln("[poller] %s (%s): %s", label, ip, msg);
        appState.withHealthWrite((ref DeviceHealth[string] health) {
            recordFailure(health, ip, msg);
        });
        return;
    }

    // Log success
    string pm02Str = "N/A", rco2Str = "N/A", atmpStr = "N/A";
    if (auto p = "pm02" in data) {
        if (p.type == JSONType.float_) pm02Str = format!"%.1f"(p.get!double);
        else if (p.type == JSONType.integer) pm02Str = p.get!long.to!string;
    }
    if (auto p = "rco2" in data) {
        if (p.type == JSONType.integer) rco2Str = p.get!long.to!string;
        else if (p.type == JSONType.float_) rco2Str = format!"%.0f"(p.get!double);
    }
    if (auto p = "atmp" in data) {
        if (p.type == JSONType.float_) atmpStr = format!"%.1f"(p.get!double);
        else if (p.type == JSONType.integer) atmpStr = p.get!long.to!string;
    }

    appState.withHealthWrite((ref DeviceHealth[string] health) {
        recordSuccess(health, ip);
    });

    stderr.writefln("[poller] %s (%s): OK — PM2.5=%s, CO2=%s, T=%s°C",
        label, ip, pm02Str, rco2Str, atmpStr);
}

private void pollAll(AppState appState) {
    foreach (dev; appState.config.devices) {
        fetchDevice(appState, dev.ip, dev.label);
    }
}

void startPoller(AppState appState) {
    auto t = new Thread({
        runPoller(appState);
    });
    t.isDaemon = true;
    t.start();
}

private void runPoller(AppState appState) {
    stderr.writefln("[poller] Starting — polling %d devices every %ds",
        appState.config.devices.length,
        appState.config.pollIntervalMs / 1000);

    // Initialize health
    appState.withHealthWrite((ref DeviceHealth[string] health) {
        foreach (dev; appState.config.devices) {
            health[dev.ip] = DeviceHealth(dev.ip, dev.label);
        }
    });

    // Initial poll
    pollAll(appState);

    uint pollCount = 0;

    while (!appState.isShutdown) {
        Thread.sleep(dur!"msecs"(appState.config.pollIntervalMs));

        if (appState.isShutdown) break;

        pollAll(appState);
        pollCount++;

        if (pollCount % CHECKPOINT_INTERVAL_POLLS == 0) {
            try {
                appState.withDb((ref Database db_) {
                    db.checkpoint(db_);
                });
            } catch (Exception e) {
                stderr.writefln("[poller] WAL checkpoint failed: %s", e.msg);
            }
        }
    }

    stderr.writeln("[poller] Stopped");
}
