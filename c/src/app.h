#ifndef APP_H
#define APP_H

#include <pthread.h>
#include <stdatomic.h>

#include "sqlite3.h"
#include "config.h"
#include "poller.h"

typedef struct AppState {
    sqlite3              *db;
    pthread_mutex_t       db_mutex;
    DeviceHealth          health[MAX_DEVICES];
    pthread_rwlock_t      health_lock;
    Config                config;
    atomic_bool           shutdown;
    int64_t               started_at;
    atomic_uint_fast64_t  requests_served;
    atomic_int            active_connections;
} AppState;

#endif /* APP_H */
