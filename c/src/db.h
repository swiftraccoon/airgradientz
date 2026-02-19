#ifndef DB_H
#define DB_H

#include <stdint.h>
#include <stddef.h>

#include "sqlite3.h"
#include "json.h"

/* ---- Reading ---- */

typedef struct {
    int64_t id;
    int64_t timestamp;
    char   *device_id;
    char   *device_type;
    char   *device_ip;
    double  pm01, pm02, pm10, pm02_compensated;
    int64_t rco2;
    double  atmp, atmp_compensated;
    double  rhum, rhum_compensated;
    double  tvoc_index, nox_index;
    int64_t wifi;
    bool    has_pm01, has_pm02, has_pm10, has_pm02_compensated;
    bool    has_rco2;
    bool    has_atmp, has_atmp_compensated;
    bool    has_rhum, has_rhum_compensated;
    bool    has_tvoc_index, has_nox_index;
    bool    has_wifi;
} Reading;

typedef struct {
    Reading *items;
    size_t   count;
    size_t   cap;
    char    *str_arena;
    size_t   str_arena_cap;
    size_t   str_arena_used;
} ReadingList;

void reading_list_free(ReadingList *rl);

/* ---- DeviceSummary ---- */

typedef struct {
    char   *device_id;
    char   *device_type;
    char   *device_ip;
    int64_t last_seen;
    int64_t reading_count;
} DeviceSummary;

typedef struct {
    DeviceSummary *items;
    size_t         count;
    size_t         cap;
    char          *str_arena;
    size_t         str_arena_cap;
    size_t         str_arena_used;
} DeviceSummaryList;

void device_summary_list_free(DeviceSummaryList *dl);

/* ---- Query params ---- */

typedef struct {
    const char *device;   /* NULL or "all" means no filter */
    int64_t     from;
    int64_t     to;
    int64_t     limit;    /* 0 = no limit */
    int64_t     downsample_ms; /* 0 = no downsampling */
} ReadingQuery;

/* ---- Functions ---- */

int     db_initialize(sqlite3 *db);
int     db_insert_reading(sqlite3 *db, const char *ip, const JsonValue *data);
int     db_query_readings(sqlite3 *db, const ReadingQuery *q, ReadingList *out);
int     db_get_devices(sqlite3 *db, DeviceSummaryList *out);
int     db_get_latest_readings(sqlite3 *db, ReadingList *out);
int     db_checkpoint(sqlite3 *db);
int64_t db_get_readings_count(sqlite3 *db);
int64_t db_get_filtered_count(sqlite3 *db, int64_t from, int64_t to, const char *device);

/* Downsample lookup: maps human-readable bucket names to millisecond durations
   using the Config's downsample_buckets array. Returns 0 if the key is not found. */
typedef struct Config Config;
int64_t downsample_lookup(const Config *cfg, const char *key);

int64_t db_now_millis(void);

/* Pool allocator stats */
void db_get_pool_stats(uint64_t *alloc_count, uint64_t *bytes_used);

/* Convert a Reading to JsonValue (caller frees) */
JsonValue *reading_to_json(const Reading *r);
JsonValue *device_summary_to_json(const DeviceSummary *d);

#endif /* DB_H */
