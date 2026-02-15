module api;

import std.json : JSONValue;
import std.conv : to;
import std.stdio : stderr;

import d2sqlite3 : Database;

import http.request : HttpRequest;
import http.response : HttpResponse;
import db;
import state : AppState;
import poller : getHealthJson, getPollStats;

HttpResponse handleReadings(AppState appState, HttpRequest req) {
    auto now = db.nowMillis();
    auto defaultFrom = now - 24 * 60 * 60 * 1000;

    long from = defaultFrom;
    long to = now;
    string device = "all";
    uint limit = appState.config.maxApiRows;

    if (auto s = req.queryParam("from")) {
        try { from = s.to!long; } catch (Exception) {}
    }
    if (auto s = req.queryParam("to")) {
        try { to = s.to!long; } catch (Exception) {}
    }
    if (auto s = req.queryParam("device"))
        device = s;
    if (auto s = req.queryParam("limit")) {
        try {
            auto requested = s.to!uint;
            if (requested > 0)
                limit = (requested < appState.config.maxApiRows) ? requested : appState.config.maxApiRows;
        } catch (Exception) {}
    }

    try {
        auto readings = appState.withDb((ref Database db_) {
            return db.queryReadings(db_, device, from, to, limit);
        });

        JSONValue[] items;
        foreach (ref r; readings)
            items ~= r.toJson();
        return HttpResponse.okJson(JSONValue(items));
    } catch (Exception e) {
        stderr.writefln("[api] query_readings error: %s", e.msg);
        return HttpResponse.internalError("Internal server error");
    }
}

HttpResponse handleReadingsLatest(AppState appState) {
    try {
        auto readings = appState.withDb((ref Database db_) {
            return db.getLatestReadings(db_);
        });

        JSONValue[] items;
        foreach (ref r; readings)
            items ~= r.toJson();
        return HttpResponse.okJson(JSONValue(items));
    } catch (Exception e) {
        stderr.writefln("[api] get_latest_readings error: %s", e.msg);
        return HttpResponse.internalError("Internal server error");
    }
}

HttpResponse handleDevices(AppState appState) {
    try {
        auto devices = appState.withDb((ref Database db_) {
            return db.getDevices(db_);
        });

        JSONValue[] items;
        foreach (ref d; devices)
            items ~= d.toJson();
        return HttpResponse.okJson(JSONValue(items));
    } catch (Exception e) {
        stderr.writefln("[api] get_devices error: %s", e.msg);
        return HttpResponse.internalError("Internal server error");
    }
}

HttpResponse handleHealth(AppState appState) {
    auto json = getHealthJson(appState);
    return HttpResponse.okJson(json);
}

HttpResponse handleConfig(AppState appState) {
    JSONValue[] devicesJson;
    foreach (dev; appState.config.devices) {
        JSONValue d = JSONValue(string[string].init);
        d["ip"] = JSONValue(dev.ip);
        d["label"] = JSONValue(dev.label);
        devicesJson ~= d;
    }

    JSONValue cfg = JSONValue(string[string].init);
    cfg["pollIntervalMs"] = JSONValue(cast(long) appState.config.pollIntervalMs);
    cfg["devices"] = JSONValue(devicesJson);

    return HttpResponse.okJson(cfg);
}

HttpResponse handleStats(AppState appState) {
    import core.sys.posix.unistd : getpid;
    import std.file : getSize;

    auto now = db.nowMillis();
    auto pollStats = getPollStats();

    // Read RSS from /proc/self/statm
    long memoryRssBytes = 0;
    try {
        import std.file : readText;
        auto statm = readText("/proc/self/statm");
        // Format: size resident shared text lib data dt
        // We want the second field (resident) * page size (4096)
        import std.algorithm : splitter;
        import std.uni : isWhite;
        size_t fieldIdx = 0;
        foreach (field; statm.splitter!isWhite) {
            if (field.length == 0) continue;
            if (fieldIdx == 1) {
                try {
                    memoryRssBytes = field.to!long * 4096;
                } catch (Exception) {}
                break;
            }
            fieldIdx++;
        }
    } catch (Exception) {}

    // Get DB size
    long dbSizeBytes = 0;
    try {
        dbSizeBytes = cast(long) getSize(appState.config.dbPath);
    } catch (Exception) {}

    // Get readings count
    long readingsCount = 0;
    try {
        readingsCount = appState.withDb((ref Database db_) {
            return db.getReadingsCount(db_);
        });
    } catch (Exception e) {
        stderr.writefln("[api] getReadingsCount error: %s", e.msg);
    }

    JSONValue obj = JSONValue(string[string].init);
    obj["implementation"] = JSONValue("d");
    obj["pid"] = JSONValue(cast(long) getpid());
    obj["uptime_ms"] = JSONValue(now - appState.startedAt);
    obj["memory_rss_bytes"] = JSONValue(memoryRssBytes);
    obj["db_size_bytes"] = JSONValue(dbSizeBytes);
    obj["readings_count"] = JSONValue(readingsCount);
    obj["requests_served"] = JSONValue(appState.getRequestsServed());
    obj["active_connections"] = JSONValue(appState.getActiveConnections());
    obj["poll_successes"] = JSONValue(pollStats.successes);
    obj["poll_failures"] = JSONValue(pollStats.failures);
    obj["started_at"] = JSONValue(appState.startedAt);

    return HttpResponse.okJson(obj);
}
