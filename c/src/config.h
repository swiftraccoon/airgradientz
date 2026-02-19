#ifndef CONFIG_H
#define CONFIG_H

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "json.h"
#include "log.h"

#define MAX_DEVICES 8
#define MAX_DOWNSAMPLE_BUCKETS 16

typedef struct {
    const char *ip;
    const char *label;
} DeviceConfig;

typedef struct {
    char key[8];    /* "5m", "10m", etc. */
    int64_t ms;
} DownsampleBucket;

typedef struct Config {
    uint16_t      port;
    char          db_path[256];
    DeviceConfig  devices[MAX_DEVICES];
    size_t        device_count;
    uint32_t      poll_interval_ms;
    uint32_t      fetch_timeout_ms;
    uint32_t      max_api_rows;
    DownsampleBucket downsample_buckets[MAX_DOWNSAMPLE_BUCKETS];
    size_t        downsample_bucket_count;
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
            log_timestamp();
            fprintf(stderr, "[config] Loaded config from CONFIG_PATH: %s\n", env_path);
            return content;
        }
        log_timestamp();
        fprintf(stderr, "[config] CONFIG_PATH set but unreadable: %s\n", env_path);
    }

    char *content = config_read_file("./airgradientz.json");
    if (content) {
        log_timestamp();
        fprintf(stderr, "[config] Loaded config from ./airgradientz.json\n");
        return content;
    }

    content = config_read_file("../airgradientz.json");
    if (content) {
        log_timestamp();
        fprintf(stderr, "[config] Loaded config from ../airgradientz.json\n");
        return content;
    }

    return NULL;
}

/* Apply parsed JSON config to Config struct. */
static inline void config_apply_json(Config *c, const JsonValue *root) {
    if (!json_is_object(root)) return;

    bool ok;

    /* Devices */
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

    /* downsampleBuckets: {"5m": 300000, "10m": 600000, ...} */
    const JsonValue *buckets = json_get(root, "downsampleBuckets");
    if (buckets && json_is_object(buckets)) {
        size_t bcount = buckets->u.object.count;
        if (bcount > MAX_DOWNSAMPLE_BUCKETS) bcount = MAX_DOWNSAMPLE_BUCKETS;
        c->downsample_bucket_count = 0;
        for (size_t i = 0; i < bcount; i++) {
            const char *bkey = buckets->u.object.pairs[i].key;
            const JsonValue *bval = buckets->u.object.pairs[i].value;
            if (!bkey || strlen(bkey) >= sizeof(c->downsample_buckets[0].key))
                continue;
            int64_t ms = json_as_i64(bval, &ok);
            if (!ok || ms <= 0) continue;
            DownsampleBucket *b = &c->downsample_buckets[c->downsample_bucket_count];
            snprintf(b->key, sizeof(b->key), "%s", bkey);
            b->ms = ms;
            c->downsample_bucket_count++;
        }
    }

    /* Port from ports.c */
    const JsonValue *ports = json_get(root, "ports");
    if (ports && json_is_object(ports)) {
        const JsonValue *my_port = json_get(ports, "c");
        if (my_port) {
            int64_t p = json_as_i64(my_port, &ok);
            if (ok && p > 0 && p <= 65535) c->port = (uint16_t)p;
        }
    }
}

static inline Config config_from_env(void) {
    Config c;
    memset(&c, 0, sizeof(c));

    /* Default db_path only */
    snprintf(c.db_path, sizeof(c.db_path), "./airgradientz.db");

    /* Config file is mandatory */
    char *file_content = config_find_file();
    if (!file_content) {
        fprintf(stderr, "fatal: config file not found\n");
        exit(1);
    }

    JsonError err;
    JsonValue *root = json_parse(file_content, strlen(file_content), &err);
    if (!root) {
        fprintf(stderr, "[config] JSON parse error at position %zu\n", err.pos);
        free(file_content);
        exit(1);
    }

    config_apply_json(&c, root);
    json_free(root);
    free(file_content);

    /* Validate required keys */
    char missing_buf[256];
    size_t off = 0;

    if (c.poll_interval_ms == 0) {
        off += (size_t)snprintf(missing_buf + off, sizeof(missing_buf) - off,
                                "%spollIntervalMs", off ? ", " : "");
    }
    if (c.fetch_timeout_ms == 0) {
        off += (size_t)snprintf(missing_buf + off, sizeof(missing_buf) - off,
                                "%sfetchTimeoutMs", off ? ", " : "");
    }
    if (c.max_api_rows == 0) {
        off += (size_t)snprintf(missing_buf + off, sizeof(missing_buf) - off,
                                "%smaxApiRows", off ? ", " : "");
    }
    if (c.downsample_bucket_count == 0) {
        off += (size_t)snprintf(missing_buf + off, sizeof(missing_buf) - off,
                                "%sdownsampleBuckets", off ? ", " : "");
    }
    if (c.device_count == 0) {
        off += (size_t)snprintf(missing_buf + off, sizeof(missing_buf) - off,
                                "%sdevices", off ? ", " : "");
    }
    if (c.port == 0) {
        off += (size_t)snprintf(missing_buf + off, sizeof(missing_buf) - off,
                                "%sports.c", off ? ", " : "");
    }

    if (off > 0) {
        fprintf(stderr, "fatal: missing required config keys: %s\n", missing_buf);
        exit(1);
    }

    /* Env var overrides (highest priority) */
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
