#include "app.h"
#include "db.h"
#include "http_server.h"

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static AppState g_state;

static void signal_handler(int sig)
{
    (void)sig;
    atomic_store(&g_state.shutdown, true);
}

int main(void)
{
    g_state.config = config_from_env();
    atomic_store(&g_state.shutdown, false);

    fprintf(stderr, "[server] Opening database at %s\n", g_state.config.db_path);

    int rc = sqlite3_open(g_state.config.db_path, &g_state.db);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "[server] Failed to open database: %s\n",
                sqlite3_errmsg(g_state.db));
        return 1;
    }

    if (db_initialize(g_state.db) != 0) {
        fprintf(stderr, "[server] Failed to initialize database\n");
        sqlite3_close(g_state.db);
        return 1;
    }

    pthread_mutex_init(&g_state.db_mutex, NULL);
    pthread_rwlock_init(&g_state.health_lock, NULL);

    /* Signal handling */
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = signal_handler;
    sigemptyset(&sa.sa_mask);
    sigaction(SIGINT, &sa, NULL);
    sigaction(SIGTERM, &sa, NULL);

    /* Spawn poller thread */
    pthread_t poller_tid;
    if (pthread_create(&poller_tid, NULL, poller_run, &g_state) != 0) {
        fprintf(stderr, "[server] Failed to start poller thread\n");
        sqlite3_close(g_state.db);
        return 1;
    }

    /* Run HTTP server (blocks) */
    http_server_run(&g_state);

    /* Cleanup */
    atomic_store(&g_state.shutdown, true);
    pthread_join(poller_tid, NULL);

    pthread_mutex_destroy(&g_state.db_mutex);
    pthread_rwlock_destroy(&g_state.health_lock);
    sqlite3_close(g_state.db);

    fprintf(stderr, "[server] Shut down cleanly\n");
    return 0;
}
