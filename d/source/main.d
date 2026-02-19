module main;

import core.stdc.signal;
import core.atomic : atomicStore;

import config : Config;
import state : AppState;
import poller : startPoller;
import http.server : runServer;
import log : logf;

private shared bool g_shutdown = false;
private __gshared AppState g_state;

extern(C) void onSignal(int) nothrow @nogc @system {
    atomicStore(g_shutdown, true);
}

void main() {
    signal(SIGINT, &onSignal);
    signal(SIGTERM, &onSignal);

    auto cfg = Config.fromEnv();

    logf("[main] AirGradient Dashboard (D)");
    logf("[main] Port: %d, DB: %s, Devices: %d",
        cfg.port, cfg.dbPath, cfg.devices.length);

    auto appState = new AppState(cfg, &g_shutdown);
    g_state = appState;

    startPoller(appState);
    runServer(appState);
}
