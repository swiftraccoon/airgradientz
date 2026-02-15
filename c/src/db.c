#include "db.h"

#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "config.h"
#include "strbuf.h"

/* ---- pool allocator stats ---- */

static atomic_uint_fast64_t pool_alloc_count;
static atomic_uint_fast64_t pool_bytes_used;

void db_get_pool_stats(uint64_t *alloc_count, uint64_t *bytes_used)
{
    *alloc_count = atomic_load(&pool_alloc_count);
    *bytes_used  = atomic_load(&pool_bytes_used);
}

/* ---- string arena helpers ---- */

#define PTR_IN_ARENA(arena, cap, p) \
    ((arena) && (const char *)(p) >= (arena) && (const char *)(p) < (arena) + (cap))

static char *arena_strdup(char *arena, size_t arena_cap, size_t *arena_used, const char *s)
{
    if (!s) abort();
    size_t len = strlen(s) + 1;

    if (arena && *arena_used + len <= arena_cap) {
        char *p = arena + *arena_used;
        memcpy(p, s, len);
        *arena_used += len;
        return p;
    }

    /* Arena full or missing — fall back to strdup */
    char *p = strdup(s);
    if (!p) abort();
    return p;
}

/* ---- time ---- */

int64_t db_now_millis(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return (int64_t)ts.tv_sec * 1000 + (int64_t)(ts.tv_nsec / 1000000);
}

/* ---- list helpers ---- */

void reading_list_free(ReadingList *rl)
{
    for (size_t i = 0; i < rl->count; i++) {
        if (!PTR_IN_ARENA(rl->str_arena, rl->str_arena_cap, rl->items[i].device_id))
            free(rl->items[i].device_id);
        if (!PTR_IN_ARENA(rl->str_arena, rl->str_arena_cap, rl->items[i].device_type))
            free(rl->items[i].device_type);
        if (!PTR_IN_ARENA(rl->str_arena, rl->str_arena_cap, rl->items[i].device_ip))
            free(rl->items[i].device_ip);
    }
    free(rl->str_arena);
    free(rl->items);
    rl->items = NULL;
    rl->count = rl->cap = 0;
    rl->str_arena = NULL;
    rl->str_arena_cap = rl->str_arena_used = 0;
}

void device_summary_list_free(DeviceSummaryList *dl)
{
    for (size_t i = 0; i < dl->count; i++) {
        if (!PTR_IN_ARENA(dl->str_arena, dl->str_arena_cap, dl->items[i].device_id))
            free(dl->items[i].device_id);
        if (!PTR_IN_ARENA(dl->str_arena, dl->str_arena_cap, dl->items[i].device_type))
            free(dl->items[i].device_type);
        if (!PTR_IN_ARENA(dl->str_arena, dl->str_arena_cap, dl->items[i].device_ip))
            free(dl->items[i].device_ip);
    }
    free(dl->str_arena);
    free(dl->items);
    dl->items = NULL;
    dl->count = dl->cap = 0;
    dl->str_arena = NULL;
    dl->str_arena_cap = dl->str_arena_used = 0;
}

static void reading_list_init_pooled(ReadingList *rl, size_t item_cap, size_t str_cap)
{
    memset(rl, 0, sizeof(*rl));
    if (item_cap > 0) {
        rl->items = malloc(item_cap * sizeof(Reading));
        if (rl->items) rl->cap = item_cap;
    }
    if (str_cap > 0) {
        rl->str_arena = malloc(str_cap);
        if (rl->str_arena) rl->str_arena_cap = str_cap;
    }
    size_t total = item_cap * sizeof(Reading) + str_cap;
    atomic_fetch_add(&pool_alloc_count, 1);
    atomic_fetch_add(&pool_bytes_used, total);
}

static void reading_list_push(ReadingList *rl, const Reading *r)
{
    if (rl->count >= rl->cap) {
        size_t new_cap = rl->cap ? rl->cap * 2 : 32;
        Reading *p = realloc(rl->items, new_cap * sizeof(*p));
        if (!p) { fprintf(stderr, "db: out of memory\n"); abort(); }
        rl->items = p;
        rl->cap = new_cap;
    }
    rl->items[rl->count++] = *r;
}

/* ---- extract helpers ---- */

static bool extract_f64(const JsonValue *data, const char *key, double *out)
{
    const JsonValue *v = json_get(data, key);
    if (!v || v->type != JSON_NUMBER) return false;
    *out = v->u.number;
    return true;
}

static bool extract_i64(const JsonValue *data, const char *key, int64_t *out)
{
    const JsonValue *v = json_get(data, key);
    if (!v || v->type != JSON_NUMBER) return false;
    *out = (int64_t)v->u.number;
    return true;
}

/* ---- initialize ---- */

int db_initialize(sqlite3 *db)
{
    const char *pragmas =
        "PRAGMA journal_mode = WAL;"
        "PRAGMA busy_timeout = 5000;"
        "PRAGMA foreign_keys = ON;";

    char *errmsg = NULL;
    int rc = sqlite3_exec(db, pragmas, NULL, NULL, &errmsg);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "[db] pragma error: %s\n", errmsg ? errmsg : "unknown");
        sqlite3_free(errmsg);
        return -1;
    }

    char *schema = config_read_file("../schema.sql");
    if (!schema) {
        fprintf(stderr, "[db] failed to read ../schema.sql\n");
        return -1;
    }

    rc = sqlite3_exec(db, schema, NULL, NULL, &errmsg);
    free(schema);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "[db] schema error: %s\n", errmsg ? errmsg : "unknown");
        sqlite3_free(errmsg);
        return -1;
    }

    return 0;
}

/* ---- insert ---- */

static void bind_opt_f64(sqlite3_stmt *stmt, int idx, const JsonValue *data, const char *key)
{
    double val;
    if (extract_f64(data, key, &val)) {
        sqlite3_bind_double(stmt, idx, val);
    } else {
        sqlite3_bind_null(stmt, idx);
    }
}

static void bind_opt_i64(sqlite3_stmt *stmt, int idx, const JsonValue *data, const char *key)
{
    int64_t val;
    if (extract_i64(data, key, &val)) {
        sqlite3_bind_int64(stmt, idx, val);
    } else {
        sqlite3_bind_null(stmt, idx);
    }
}

int db_insert_reading(sqlite3 *db, const char *ip, const JsonValue *data)
{
    const char *model_str = json_as_str(json_get(data, "model"));
    const char *device_type = (model_str && model_str[0] == 'I' && model_str[1] == '-')
                              ? "indoor" : "outdoor";

    const char *serial = json_as_str(json_get(data, "serialno"));
    if (!serial) serial = "unknown";

    /* Serialize raw JSON */
    StrBuf raw = strbuf_new();
    json_serialize(data, &raw);

    const char *sql =
        "INSERT INTO readings ("
        "    timestamp, device_id, device_type, device_ip,"
        "    pm01, pm02, pm10, pm02_compensated,"
        "    rco2, atmp, atmp_compensated, rhum, rhum_compensated,"
        "    tvoc_index, nox_index, wifi, raw_json"
        ") VALUES ("
        "    ?1, ?2, ?3, ?4,"
        "    ?5, ?6, ?7, ?8,"
        "    ?9, ?10, ?11, ?12, ?13,"
        "    ?14, ?15, ?16, ?17"
        ")";

    sqlite3_stmt *stmt;
    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "[db] prepare insert error: %s\n", sqlite3_errmsg(db));
        strbuf_free(&raw);
        return -1;
    }

    sqlite3_bind_int64(stmt, 1, db_now_millis());
    sqlite3_bind_text(stmt, 2, serial, -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, 3, device_type, -1, SQLITE_STATIC);
    sqlite3_bind_text(stmt, 4, ip, -1, SQLITE_TRANSIENT);

    bind_opt_f64(stmt, 5,  data, "pm01");
    bind_opt_f64(stmt, 6,  data, "pm02");
    bind_opt_f64(stmt, 7,  data, "pm10");
    bind_opt_f64(stmt, 8,  data, "pm02Compensated");
    bind_opt_i64(stmt, 9,  data, "rco2");
    bind_opt_f64(stmt, 10, data, "atmp");
    bind_opt_f64(stmt, 11, data, "atmpCompensated");
    bind_opt_f64(stmt, 12, data, "rhum");
    bind_opt_f64(stmt, 13, data, "rhumCompensated");
    bind_opt_f64(stmt, 14, data, "tvocIndex");
    bind_opt_f64(stmt, 15, data, "noxIndex");
    bind_opt_i64(stmt, 16, data, "wifi");

    sqlite3_bind_text(stmt, 17, strbuf_cstr(&raw), -1, SQLITE_TRANSIENT);

    rc = sqlite3_step(stmt);
    sqlite3_finalize(stmt);
    strbuf_free(&raw);

    if (rc != SQLITE_DONE) {
        fprintf(stderr, "[db] insert step error: %s\n", sqlite3_errmsg(db));
        return -1;
    }

    return 0;
}

/* ---- query helpers ---- */

#define QUERY_COLS \
    "id, timestamp, device_id, device_type, device_ip, " \
    "pm01, pm02, pm10, pm02_compensated, rco2, " \
    "atmp, atmp_compensated, rhum, rhum_compensated, " \
    "tvoc_index, nox_index, wifi"

static bool col_is_null(sqlite3_stmt *stmt, int col)
{
    return sqlite3_column_type(stmt, col) == SQLITE_NULL;
}

static Reading row_to_reading(sqlite3_stmt *stmt, ReadingList *rl)
{
    Reading r;
    memset(&r, 0, sizeof(r));

    r.id        = sqlite3_column_int64(stmt, 0);
    r.timestamp = sqlite3_column_int64(stmt, 1);
    r.device_id   = arena_strdup(rl->str_arena, rl->str_arena_cap, &rl->str_arena_used,
                                 (const char *)sqlite3_column_text(stmt, 2));
    r.device_type = arena_strdup(rl->str_arena, rl->str_arena_cap, &rl->str_arena_used,
                                 (const char *)sqlite3_column_text(stmt, 3));
    r.device_ip   = arena_strdup(rl->str_arena, rl->str_arena_cap, &rl->str_arena_used,
                                 (const char *)sqlite3_column_text(stmt, 4));

    /* pm01 (5) */
    r.has_pm01 = !col_is_null(stmt, 5);
    if (r.has_pm01) r.pm01 = sqlite3_column_double(stmt, 5);

    r.has_pm02 = !col_is_null(stmt, 6);
    if (r.has_pm02) r.pm02 = sqlite3_column_double(stmt, 6);

    r.has_pm10 = !col_is_null(stmt, 7);
    if (r.has_pm10) r.pm10 = sqlite3_column_double(stmt, 7);

    r.has_pm02_compensated = !col_is_null(stmt, 8);
    if (r.has_pm02_compensated) r.pm02_compensated = sqlite3_column_double(stmt, 8);

    r.has_rco2 = !col_is_null(stmt, 9);
    if (r.has_rco2) r.rco2 = sqlite3_column_int64(stmt, 9);

    r.has_atmp = !col_is_null(stmt, 10);
    if (r.has_atmp) r.atmp = sqlite3_column_double(stmt, 10);

    r.has_atmp_compensated = !col_is_null(stmt, 11);
    if (r.has_atmp_compensated) r.atmp_compensated = sqlite3_column_double(stmt, 11);

    r.has_rhum = !col_is_null(stmt, 12);
    if (r.has_rhum) r.rhum = sqlite3_column_double(stmt, 12);

    r.has_rhum_compensated = !col_is_null(stmt, 13);
    if (r.has_rhum_compensated) r.rhum_compensated = sqlite3_column_double(stmt, 13);

    r.has_tvoc_index = !col_is_null(stmt, 14);
    if (r.has_tvoc_index) r.tvoc_index = sqlite3_column_double(stmt, 14);

    r.has_nox_index = !col_is_null(stmt, 15);
    if (r.has_nox_index) r.nox_index = sqlite3_column_double(stmt, 15);

    r.has_wifi = !col_is_null(stmt, 16);
    if (r.has_wifi) r.wifi = sqlite3_column_int64(stmt, 16);

    return r;
}

/* ---- query readings ---- */

int db_query_readings(sqlite3 *db, const ReadingQuery *q, ReadingList *out)
{
    size_t est_items = (q->limit > 0) ? (size_t)q->limit : 1000;
    reading_list_init_pooled(out, est_items, est_items * 96);

    bool want_device = q->device && strcmp(q->device, "all") != 0;

    StrBuf sql = strbuf_new();
    strbuf_appendf(&sql, "SELECT %s FROM readings WHERE ", QUERY_COLS);

    if (want_device) {
        strbuf_append_cstr(&sql, "device_id = ?3 AND ");
    }

    strbuf_append_cstr(&sql, "timestamp >= ?1 AND timestamp <= ?2 ORDER BY timestamp ASC");

    if (q->limit > 0) {
        if (want_device) {
            strbuf_append_cstr(&sql, " LIMIT ?4");
        } else {
            strbuf_append_cstr(&sql, " LIMIT ?3");
        }
    }

    sqlite3_stmt *stmt;
    int rc = sqlite3_prepare_v2(db, strbuf_cstr(&sql), -1, &stmt, NULL);
    strbuf_free(&sql);

    if (rc != SQLITE_OK) {
        fprintf(stderr, "[db] prepare query error: %s\n", sqlite3_errmsg(db));
        return -1;
    }

    sqlite3_bind_int64(stmt, 1, q->from);
    sqlite3_bind_int64(stmt, 2, q->to);

    if (want_device && q->limit > 0) {
        sqlite3_bind_text(stmt, 3, q->device, -1, SQLITE_TRANSIENT);
        sqlite3_bind_int64(stmt, 4, q->limit);
    } else if (want_device) {
        sqlite3_bind_text(stmt, 3, q->device, -1, SQLITE_TRANSIENT);
    } else if (q->limit > 0) {
        sqlite3_bind_int64(stmt, 3, q->limit);
    }

    while ((rc = sqlite3_step(stmt)) == SQLITE_ROW) {
        Reading r = row_to_reading(stmt, out);
        reading_list_push(out, &r);
    }

    sqlite3_finalize(stmt);
    return (rc == SQLITE_DONE) ? 0 : -1;
}

/* ---- get devices ---- */

int db_get_devices(sqlite3 *db, DeviceSummaryList *out)
{
    memset(out, 0, sizeof(*out));

    /* Pre-allocate for a reasonable number of devices */
    size_t est_devices = 16;
    out->items = malloc(est_devices * sizeof(DeviceSummary));
    if (out->items) out->cap = est_devices;
    out->str_arena = malloc(est_devices * 96);
    if (out->str_arena) out->str_arena_cap = est_devices * 96;
    atomic_fetch_add(&pool_alloc_count, 1);
    atomic_fetch_add(&pool_bytes_used, est_devices * sizeof(DeviceSummary) + est_devices * 96);

    const char *sql =
        "SELECT device_id, device_type, device_ip, "
        "       MAX(timestamp) as last_seen, "
        "       COUNT(*) as reading_count "
        "FROM readings "
        "GROUP BY device_id "
        "ORDER BY device_type";

    sqlite3_stmt *stmt;
    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
    if (rc != SQLITE_OK) return -1;

    while ((rc = sqlite3_step(stmt)) == SQLITE_ROW) {
        if (out->count >= out->cap) {
            size_t new_cap = out->cap ? out->cap * 2 : 8;
            DeviceSummary *p = realloc(out->items, new_cap * sizeof(*p));
            if (!p) abort();
            out->items = p;
            out->cap = new_cap;
        }
        DeviceSummary *d = &out->items[out->count++];
        d->device_id    = arena_strdup(out->str_arena, out->str_arena_cap,
                                       &out->str_arena_used,
                                       (const char *)sqlite3_column_text(stmt, 0));
        d->device_type  = arena_strdup(out->str_arena, out->str_arena_cap,
                                       &out->str_arena_used,
                                       (const char *)sqlite3_column_text(stmt, 1));
        d->device_ip    = arena_strdup(out->str_arena, out->str_arena_cap,
                                       &out->str_arena_used,
                                       (const char *)sqlite3_column_text(stmt, 2));
        d->last_seen    = sqlite3_column_int64(stmt, 3);
        d->reading_count = sqlite3_column_int64(stmt, 4);
    }

    sqlite3_finalize(stmt);
    return (rc == SQLITE_DONE) ? 0 : -1;
}

/* ---- get latest readings ---- */

int db_get_latest_readings(sqlite3 *db, ReadingList *out)
{
    reading_list_init_pooled(out, 16, 16 * 96);

    const char *sql =
        "SELECT r.id, r.timestamp, r.device_id, r.device_type, r.device_ip, "
        "r.pm01, r.pm02, r.pm10, r.pm02_compensated, r.rco2, "
        "r.atmp, r.atmp_compensated, r.rhum, r.rhum_compensated, "
        "r.tvoc_index, r.nox_index, r.wifi "
        "FROM readings r "
        "INNER JOIN ("
        "    SELECT device_id, MAX(id) as max_id "
        "    FROM readings "
        "    GROUP BY device_id"
        ") latest ON r.id = latest.max_id";

    sqlite3_stmt *stmt;
    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
    if (rc != SQLITE_OK) return -1;

    while ((rc = sqlite3_step(stmt)) == SQLITE_ROW) {
        Reading r = row_to_reading(stmt, out);
        reading_list_push(out, &r);
    }

    sqlite3_finalize(stmt);
    return (rc == SQLITE_DONE) ? 0 : -1;
}

/* ---- readings count ---- */

int64_t db_get_readings_count(sqlite3 *db)
{
    const char *sql = "SELECT COUNT(*) FROM readings";
    sqlite3_stmt *stmt;
    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
    if (rc != SQLITE_OK) return -1;

    int64_t count = 0;
    if (sqlite3_step(stmt) == SQLITE_ROW) {
        count = sqlite3_column_int64(stmt, 0);
    }
    sqlite3_finalize(stmt);
    return count;
}

/* ---- checkpoint ---- */

int db_checkpoint(sqlite3 *db)
{
    char *errmsg = NULL;
    int rc = sqlite3_exec(db, "PRAGMA wal_checkpoint(TRUNCATE);", NULL, NULL, &errmsg);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "[db] checkpoint error: %s\n", errmsg ? errmsg : "unknown");
        sqlite3_free(errmsg);
        return -1;
    }
    return 0;
}

/* ---- JSON converters ---- */

static JsonValue *opt_f64_json(bool has, double val)
{
    return has ? json_number(val) : json_null();
}

static JsonValue *opt_i64_json(bool has, int64_t val)
{
    return has ? json_from_i64(val) : json_null();
}

JsonValue *reading_to_json(const Reading *r)
{
    JsonValue *obj = json_object_new();
    json_object_set(obj, "id",                json_from_i64(r->id));
    json_object_set(obj, "timestamp",         json_from_i64(r->timestamp));
    json_object_set(obj, "device_id",         json_string(r->device_id));
    json_object_set(obj, "device_type",       json_string(r->device_type));
    json_object_set(obj, "device_ip",         json_string(r->device_ip));
    json_object_set(obj, "pm01",              opt_f64_json(r->has_pm01, r->pm01));
    json_object_set(obj, "pm02",              opt_f64_json(r->has_pm02, r->pm02));
    json_object_set(obj, "pm10",              opt_f64_json(r->has_pm10, r->pm10));
    json_object_set(obj, "pm02_compensated",  opt_f64_json(r->has_pm02_compensated, r->pm02_compensated));
    json_object_set(obj, "rco2",              opt_i64_json(r->has_rco2, r->rco2));
    json_object_set(obj, "atmp",              opt_f64_json(r->has_atmp, r->atmp));
    json_object_set(obj, "atmp_compensated",  opt_f64_json(r->has_atmp_compensated, r->atmp_compensated));
    json_object_set(obj, "rhum",              opt_f64_json(r->has_rhum, r->rhum));
    json_object_set(obj, "rhum_compensated",  opt_f64_json(r->has_rhum_compensated, r->rhum_compensated));
    json_object_set(obj, "tvoc_index",        opt_f64_json(r->has_tvoc_index, r->tvoc_index));
    json_object_set(obj, "nox_index",         opt_f64_json(r->has_nox_index, r->nox_index));
    json_object_set(obj, "wifi",              opt_i64_json(r->has_wifi, r->wifi));
    return obj;
}

JsonValue *device_summary_to_json(const DeviceSummary *d)
{
    JsonValue *obj = json_object_new();
    json_object_set(obj, "device_id",     json_string(d->device_id));
    json_object_set(obj, "device_type",   json_string(d->device_type));
    json_object_set(obj, "device_ip",     json_string(d->device_ip));
    json_object_set(obj, "last_seen",     json_from_i64(d->last_seen));
    json_object_set(obj, "reading_count", json_from_i64(d->reading_count));
    return obj;
}
