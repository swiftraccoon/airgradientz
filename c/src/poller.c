#include "poller.h"
#include "app.h"
#include "db.h"
#include "http_client.h"
#include "json.h"

#include <stdatomic.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#define CHECKPOINT_INTERVAL_POLLS 10

static atomic_uint_fast64_t poll_successes = 0;
static atomic_uint_fast64_t poll_failures  = 0;

static void record_success(DeviceHealth *h)
{
    h->status = HEALTH_OK;
    h->last_success = db_now_millis();
    h->last_error_message[0] = '\0';
    h->consecutive_failures = 0;
}

static void record_failure(DeviceHealth *h, const char *msg)
{
    h->status = HEALTH_ERROR;
    h->last_error = db_now_millis();
    snprintf(h->last_error_message, sizeof(h->last_error_message), "%s", msg);
    h->consecutive_failures++;
}

static void format_optional_f64(char *buf, size_t size, bool has, double val)
{
    if (has) {
        snprintf(buf, size, "%.2f", val);
    } else {
        snprintf(buf, size, "N/A");
    }
}

static void format_optional_i64(char *buf, size_t size, bool has, int64_t val)
{
    if (has) {
        snprintf(buf, size, "%lld", (long long)val);
    } else {
        snprintf(buf, size, "N/A");
    }
}

static void fetch_device(AppState *state, size_t idx)
{
    const char *ip    = state->config.devices[idx].ip;
    const char *label = state->config.devices[idx].label;

    char errbuf[256];
    errbuf[0] = '\0';
    char *body = http_get(ip, "/measures/current", state->config.fetch_timeout_ms,
                          errbuf, sizeof(errbuf));

    if (!body) {
        fprintf(stderr, "[poller] %s (%s): fetch failed: %s\n", label, ip, errbuf);
        atomic_fetch_add(&poll_failures, 1);
        pthread_rwlock_wrlock(&state->health_lock);
        record_failure(&state->health[idx], errbuf);
        pthread_rwlock_unlock(&state->health_lock);
        return;
    }

    JsonError jerr;
    JsonValue *data = json_parse(body, strlen(body), &jerr);
    free(body);

    if (!data) {
        char msg[256];
        snprintf(msg, sizeof(msg), "JSON parse error at pos %zu", jerr.pos);
        fprintf(stderr, "[poller] %s (%s): %s\n", label, ip, msg);
        atomic_fetch_add(&poll_failures, 1);
        pthread_rwlock_wrlock(&state->health_lock);
        record_failure(&state->health[idx], msg);
        pthread_rwlock_unlock(&state->health_lock);
        return;
    }

    if (!json_is_object(data)) {
        const char *msg = "unexpected response type: not an object";
        fprintf(stderr, "[poller] %s (%s): %s\n", label, ip, msg);
        atomic_fetch_add(&poll_failures, 1);
        pthread_rwlock_wrlock(&state->health_lock);
        record_failure(&state->health[idx], msg);
        pthread_rwlock_unlock(&state->health_lock);
        json_free(data);
        return;
    }

    /* Insert into DB under mutex */
    pthread_mutex_lock(&state->db_mutex);
    int rc = db_insert_reading(state->db, ip, data);
    pthread_mutex_unlock(&state->db_mutex);

    if (rc != 0) {
        const char *msg = "DB insert failed";
        fprintf(stderr, "[poller] %s (%s): %s\n", label, ip, msg);
        atomic_fetch_add(&poll_failures, 1);
        pthread_rwlock_wrlock(&state->health_lock);
        record_failure(&state->health[idx], msg);
        pthread_rwlock_unlock(&state->health_lock);
        json_free(data);
        return;
    }

    /* Log success */
    bool ok;
    double pm02 = json_as_f64(json_get(data, "pm02"), &ok);
    bool has_pm02 = ok;
    int64_t rco2 = json_as_i64(json_get(data, "rco2"), &ok);
    bool has_rco2 = ok;
    double atmp = json_as_f64(json_get(data, "atmp"), &ok);
    bool has_atmp = ok;

    char pm02_s[32], rco2_s[32], atmp_s[32];
    format_optional_f64(pm02_s, sizeof(pm02_s), has_pm02, pm02);
    format_optional_i64(rco2_s, sizeof(rco2_s), has_rco2, rco2);
    format_optional_f64(atmp_s, sizeof(atmp_s), has_atmp, atmp);

    atomic_fetch_add(&poll_successes, 1);
    pthread_rwlock_wrlock(&state->health_lock);
    record_success(&state->health[idx]);
    pthread_rwlock_unlock(&state->health_lock);

    fprintf(stderr, "[poller] %s (%s): OK — PM2.5=%s, CO2=%s, T=%s°C\n",
            label, ip, pm02_s, rco2_s, atmp_s);

    json_free(data);
}

static void poll_all(AppState *state)
{
    for (size_t i = 0; i < state->config.device_count; i++) {
        fetch_device(state, i);
    }
}

void *poller_run(void *arg)
{
    AppState *state = (AppState *)arg;

    fprintf(stderr, "[poller] Starting — polling %zu devices every %us\n",
            state->config.device_count, state->config.poll_interval_ms / 1000u);

    /* Init health */
    pthread_rwlock_wrlock(&state->health_lock);
    for (size_t i = 0; i < state->config.device_count; i++) {
        state->health[i].ip    = state->config.devices[i].ip;
        state->health[i].label = state->config.devices[i].label;
        state->health[i].status = HEALTH_UNKNOWN;
        state->health[i].last_success = 0;
        state->health[i].last_error = 0;
        state->health[i].last_error_message[0] = '\0';
        state->health[i].consecutive_failures = 0;
    }
    pthread_rwlock_unlock(&state->health_lock);

    /* Initial poll */
    poll_all(state);

    uint32_t poll_count = 0;

    struct timespec sleep_ts;
    sleep_ts.tv_sec  = (time_t)(state->config.poll_interval_ms / 1000u);
    sleep_ts.tv_nsec = (long)((state->config.poll_interval_ms % 1000u) * 1000000u);

    for (;;) {
        nanosleep(&sleep_ts, NULL);

        if (atomic_load(&state->shutdown)) {
            fprintf(stderr, "[poller] Stopped\n");
            break;
        }

        poll_all(state);
        poll_count++;

        if (poll_count % CHECKPOINT_INTERVAL_POLLS == 0) {
            pthread_mutex_lock(&state->db_mutex);
            if (db_checkpoint(state->db) != 0) {
                fprintf(stderr, "[poller] WAL checkpoint failed\n");
            }
            pthread_mutex_unlock(&state->db_mutex);
        }
    }

    return NULL;
}

/* ---- poll counters ---- */

void poller_get_counters(uint64_t *successes, uint64_t *failures)
{
    *successes = atomic_load(&poll_successes);
    *failures  = atomic_load(&poll_failures);
}

/* ---- health JSON ---- */

JsonValue *poller_get_health_json(struct AppState *state)
{
    JsonValue *arr = json_array_new();

    pthread_rwlock_rdlock(&state->health_lock);
    for (size_t i = 0; i < state->config.device_count; i++) {
        const DeviceHealth *h = &state->health[i];

        JsonValue *obj = json_object_new();
        json_object_set(obj, "ip",    json_string(h->ip));
        json_object_set(obj, "label", json_string(h->label));

        const char *status_str = "unknown";
        if (h->status == HEALTH_OK)    status_str = "ok";
        if (h->status == HEALTH_ERROR) status_str = "error";
        json_object_set(obj, "status", json_string(status_str));

        json_object_set(obj, "lastSuccess",
            h->last_success ? json_from_i64(h->last_success) : json_null());
        json_object_set(obj, "lastError",
            h->last_error ? json_from_i64(h->last_error) : json_null());
        json_object_set(obj, "lastErrorMessage",
            h->last_error_message[0] ? json_string(h->last_error_message) : json_null());
        json_object_set(obj, "consecutiveFailures",
            json_number((double)h->consecutive_failures));

        json_array_push(arr, obj);
    }
    pthread_rwlock_unlock(&state->health_lock);

    return arr;
}
