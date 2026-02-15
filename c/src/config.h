#ifndef CONFIG_H
#define CONFIG_H

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "json.h"

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

/* Try to read a config file. Returns malloc'd content or NULL. */
static inline char *config_read_file(const char *path) {
    FILE *f = fopen(path, "r");
    if (!f) return NULL;

    fseek(f, 0, SEEK_END);
    long len = ftell(f);
    if (len <= 0 || len > 1048576) { /* cap at 1MB */
        fclose(f);
        return NULL;
    }
    fseek(f, 0, SEEK_SET);

    char *buf = (char *)malloc((size_t)len + 1);
    if (!buf) {
        fclose(f);
        return NULL;
    }

    size_t n = fread(buf, 1, (size_t)len, f);
    fclose(f);
    buf[n] = '\0';
    return buf;
}

/* Resolve config file path: CONFIG_PATH env, ./airgradientz.json, ../airgradientz.json */
static inline char *config_find_file(void) {
    const char *env_path = getenv("CONFIG_PATH");
    if (env_path) {
        char *content = config_read_file(env_path);
        if (content) {
            fprintf(stderr, "[config] Loaded config from CONFIG_PATH: %s\n", env_path);
            return content;
        }
        fprintf(stderr, "[config] CONFIG_PATH set but unreadable: %s\n", env_path);
    }

    char *content = config_read_file("./airgradientz.json");
    if (content) {
        fprintf(stderr, "[config] Loaded config from ./airgradientz.json\n");
        return content;
    }

    content = config_read_file("../airgradientz.json");
    if (content) {
        fprintf(stderr, "[config] Loaded config from ../airgradientz.json\n");
        return content;
    }

    return NULL;
}

/* Apply parsed JSON config to Config struct. Does not touch port or db_path. */
static inline void config_apply_json(Config *c, const JsonValue *root) {
    if (!json_is_object(root)) return;

    const JsonValue *devices = json_get(root, "devices");
    if (devices && devices->type == JSON_ARRAY && devices->u.array.count > 0) {
        size_t count = devices->u.array.count;
        if (count > MAX_DEVICES) count = MAX_DEVICES;
        c->device_count = count;
        for (size_t i = 0; i < count; i++) {
            const JsonValue *dev = devices->u.array.items[i];
            if (!json_is_object(dev)) continue;
            const char *ip = json_as_str(json_get(dev, "ip"));
            const char *label = json_as_str(json_get(dev, "label"));
            if (ip)    c->devices[i].ip    = strdup(ip);
            if (label) c->devices[i].label = strdup(label);
        }
    }

    bool ok;
    const JsonValue *v;

    v = json_get(root, "pollIntervalMs");
    if (v) {
        int64_t n = json_as_i64(v, &ok);
        if (ok && n > 0) c->poll_interval_ms = (uint32_t)n;
    }

    v = json_get(root, "fetchTimeoutMs");
    if (v) {
        int64_t n = json_as_i64(v, &ok);
        if (ok && n > 0) c->fetch_timeout_ms = (uint32_t)n;
    }

    v = json_get(root, "maxApiRows");
    if (v) {
        int64_t n = json_as_i64(v, &ok);
        if (ok && n > 0) c->max_api_rows = (uint32_t)n;
    }
}

static inline Config config_from_env(void) {
    Config c;
    memset(&c, 0, sizeof(c));

    /* 1. Hardcoded defaults */
    c.port = 3011;
    snprintf(c.db_path, sizeof(c.db_path), "./airgradientz.db");

    c.devices[0].ip    = "192.168.88.6";
    c.devices[0].label = "outdoor";
    c.devices[1].ip    = "192.168.88.159";
    c.devices[1].label = "indoor";
    c.device_count     = 2;

    c.poll_interval_ms = 15000;
    c.fetch_timeout_ms = 5000;
    c.max_api_rows     = 10000;

    /* 2. Config file overrides */
    char *file_content = config_find_file();
    if (file_content) {
        JsonError err;
        JsonValue *root = json_parse(file_content, strlen(file_content), &err);
        if (root) {
            config_apply_json(&c, root);
            json_free(root);
        } else {
            fprintf(stderr, "[config] JSON parse error at position %zu\n", err.pos);
        }
        free(file_content);
    }

    /* 3. Env var overrides (highest priority) */
    const char *port_str = getenv("PORT");
    if (port_str) {
        long p = strtol(port_str, NULL, 10);
        if (p > 0 && p <= 65535) {
            c.port = (uint16_t)p;
        }
    }

    const char *db = getenv("DB_PATH");
    if (db) {
        snprintf(c.db_path, sizeof(c.db_path), "%s", db);
    }

    return c;
}

#endif /* CONFIG_H */
