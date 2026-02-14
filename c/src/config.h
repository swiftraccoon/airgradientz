#ifndef CONFIG_H
#define CONFIG_H

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define MAX_DEVICES 8

typedef struct {
    const char *ip;
    const char *label;
} DeviceConfig;

typedef struct {
    uint16_t      port;
    char          db_path[256];
    DeviceConfig  devices[MAX_DEVICES];
    size_t        device_count;
    uint32_t      poll_interval_ms;
    uint32_t      fetch_timeout_ms;
    uint32_t      max_api_rows;
} Config;

static inline Config config_from_env(void) {
    Config c;
    memset(&c, 0, sizeof(c));

    const char *port_str = getenv("PORT");
    if (port_str) {
        long p = strtol(port_str, NULL, 10);
        if (p > 0 && p <= 65535) {
            c.port = (uint16_t)p;
        } else {
            c.port = 3011;
        }
    } else {
        c.port = 3011;
    }

    const char *db = getenv("DB_PATH");
    if (db) {
        snprintf(c.db_path, sizeof(c.db_path), "%s", db);
    } else {
        snprintf(c.db_path, sizeof(c.db_path), "./airgradientz.db");
    }

    c.devices[0].ip    = "192.168.88.6";
    c.devices[0].label = "outdoor";
    c.devices[1].ip    = "192.168.88.159";
    c.devices[1].label = "indoor";
    c.device_count     = 2;

    c.poll_interval_ms = 15000;
    c.fetch_timeout_ms = 5000;
    c.max_api_rows     = 10000;

    return c;
}

#endif /* CONFIG_H */
