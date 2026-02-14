#include "api.h"
#include "app.h"
#include "db.h"
#include "poller.h"

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ---- url decode ---- */

static char *url_decode(const char *s, size_t len)
{
    char *out = malloc(len + 1);
    if (!out) return NULL;
    size_t j = 0;
    for (size_t i = 0; i < len; i++) {
        if (s[i] == '%' && i + 2 < len) {
            char hex[3] = { s[i+1], s[i+2], '\0' };
            char *end;
            long v = strtol(hex, &end, 16);
            if (end == hex + 2) {
                out[j++] = (char)v;
                i += 2;
                continue;
            }
        }
        if (s[i] == '+') {
            out[j++] = ' ';
        } else {
            out[j++] = s[i];
        }
    }
    out[j] = '\0';
    return out;
}

char *query_param(const char *query, const char *name)
{
    if (!query || !query[0]) return NULL;
    size_t name_len = strlen(name);
    const char *p = query;

    while (*p) {
        const char *amp = strchr(p, '&');
        size_t pair_len = amp ? (size_t)(amp - p) : strlen(p);

        const char *eq = memchr(p, '=', pair_len);
        if (eq) {
            size_t key_len = (size_t)(eq - p);
            if (key_len == name_len && memcmp(p, name, name_len) == 0) {
                const char *val = eq + 1;
                size_t val_len = pair_len - key_len - 1;
                return url_decode(val, val_len);
            }
        } else {
            if (pair_len == name_len && memcmp(p, name, name_len) == 0) {
                return strdup("");
            }
        }

        if (!amp) break;
        p = amp + 1;
    }

    return NULL;
}

static int64_t parse_i64_param(const char *query, const char *name, int64_t default_val)
{
    char *s = query_param(query, name);
    if (!s) return default_val;
    char *end;
    long long v = strtoll(s, &end, 10);
    int64_t result = (*end == '\0') ? (int64_t)v : default_val;
    free(s);
    return result;
}

/* ---- handlers ---- */

JsonValue *api_handle_readings(struct AppState *state, const HttpReq *req, int *status)
{
    int64_t now = db_now_millis();
    int64_t default_from = now - 24LL * 60 * 60 * 1000;

    int64_t from  = parse_i64_param(req->query, "from",  default_from);
    int64_t to    = parse_i64_param(req->query, "to",    now);

    char *device = query_param(req->query, "device");

    int64_t requested_limit = parse_i64_param(req->query, "limit",
                                              (int64_t)state->config.max_api_rows);
    int64_t max = (int64_t)state->config.max_api_rows;
    int64_t effective_limit = (requested_limit > 0 && requested_limit < max)
                             ? requested_limit : max;

    ReadingQuery q;
    q.device = device ? device : "all";
    q.from   = from;
    q.to     = to;
    q.limit  = effective_limit;

    ReadingList rl;
    pthread_mutex_lock(&state->db_mutex);
    int rc = db_query_readings(state->db, &q, &rl);
    pthread_mutex_unlock(&state->db_mutex);

    free(device);

    if (rc != 0) {
        *status = 500;
        return NULL;
    }

    JsonValue *arr = json_array_new();
    for (size_t i = 0; i < rl.count; i++) {
        json_array_push(arr, reading_to_json(&rl.items[i]));
    }
    reading_list_free(&rl);

    *status = 200;
    return arr;
}

JsonValue *api_handle_readings_latest(struct AppState *state, int *status)
{
    ReadingList rl;
    pthread_mutex_lock(&state->db_mutex);
    int rc = db_get_latest_readings(state->db, &rl);
    pthread_mutex_unlock(&state->db_mutex);

    if (rc != 0) {
        *status = 500;
        return NULL;
    }

    JsonValue *arr = json_array_new();
    for (size_t i = 0; i < rl.count; i++) {
        json_array_push(arr, reading_to_json(&rl.items[i]));
    }
    reading_list_free(&rl);

    *status = 200;
    return arr;
}

JsonValue *api_handle_devices(struct AppState *state, int *status)
{
    DeviceSummaryList dl;
    pthread_mutex_lock(&state->db_mutex);
    int rc = db_get_devices(state->db, &dl);
    pthread_mutex_unlock(&state->db_mutex);

    if (rc != 0) {
        *status = 500;
        return NULL;
    }

    JsonValue *arr = json_array_new();
    for (size_t i = 0; i < dl.count; i++) {
        json_array_push(arr, device_summary_to_json(&dl.items[i]));
    }
    device_summary_list_free(&dl);

    *status = 200;
    return arr;
}

JsonValue *api_handle_health(struct AppState *state, int *status)
{
    *status = 200;
    return poller_get_health_json(state);
}

JsonValue *api_handle_config(struct AppState *state, int *status)
{
    JsonValue *devices = json_array_new();
    for (size_t i = 0; i < state->config.device_count; i++) {
        JsonValue *d = json_object_new();
        json_object_set(d, "ip",    json_string(state->config.devices[i].ip));
        json_object_set(d, "label", json_string(state->config.devices[i].label));
        json_array_push(devices, d);
    }

    JsonValue *cfg = json_object_new();
    json_object_set(cfg, "pollIntervalMs", json_number((double)state->config.poll_interval_ms));
    json_object_set(cfg, "devices", devices);

    *status = 200;
    return cfg;
}
