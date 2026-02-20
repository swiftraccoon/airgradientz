\ AirGradientz - GNU Forth (gforth 0.7.3) implementation
\ Port 3020 (configurable via PORT env var)
\ Uses C FFI for SQLite, HTTP client, JSON parsing, socket I/O

\ ============================================================
\ C FFI Library — all heavy lifting done in C
\ ============================================================

c-library airgrad
s" sqlite3" add-lib
s" curl" add-lib
s" pthread" add-lib

\ ---- Includes and globals ----
\c #ifndef _GNU_SOURCE
\c #define _GNU_SOURCE
\c #endif
\c #include <stdio.h>
\c #include <stdlib.h>
\c #include <string.h>
\c #include <unistd.h>
\c #include <errno.h>
\c #include <time.h>
\c #include <signal.h>
\c #include <fcntl.h>
\c #include <ctype.h>
\c #include <math.h>
\c #include <sys/types.h>
\c #include <sys/stat.h>
\c #include <sys/socket.h>
\c #include <sys/wait.h>
\c #include <netinet/in.h>
\c #include <arpa/inet.h>
\c #include <pthread.h>
\c #include <sqlite3.h>
\c #include <curl/curl.h>
\c
\c /* ---- Global configuration ---- */
\c static int    g_port = 3020;
\c static char   g_db_path[512] = "./airgradientz.db";
\c static int    g_poll_interval_ms = 15000;
\c static int    g_fetch_timeout_ms = 5000;
\c static int    g_max_api_rows = 10000;
\c static long   g_started_at = 0;
\c static long   g_requests_served = 0;
\c static long   g_poll_successes = 0;
\c static long   g_poll_failures = 0;
\c static volatile int g_running = 1;
\c
\c /* ---- Downsample buckets ---- */
\c typedef struct { const char *name; long ms; } ds_bucket_t;
\c static ds_bucket_t g_ds_buckets[] = {
\c     {"5m", 300000}, {"10m", 600000}, {"15m", 900000},
\c     {"30m", 1800000}, {"1h", 3600000}, {"1d", 86400000}, {"1w", 604800000}
\c };
\c #define NUM_DS_BUCKETS 7
\c
\c /* ---- Devices ---- */
\c typedef struct {
\c     char ip[64];
\c     char label[64];
\c     char status[16];
\c     long last_success;
\c     long last_error;
\c     char last_error_msg[256];
\c     int  consecutive_failures;
\c } device_t;
\c static device_t g_devices[16];
\c static int g_num_devices = 0;
\c
\c /* ---- Time helper ---- */
\c static long now_ms(void) {
\c     struct timespec ts;
\c     clock_gettime(CLOCK_REALTIME, &ts);
\c     return (long)(ts.tv_sec * 1000LL + ts.tv_nsec / 1000000LL);
\c }
\c
\c /* ---- Minimal JSON value extractor ---- */
\c static int json_get(const char *json, const char *key, char *out, int outsz) {
\c     if (!json || !key || !out) return -1;
\c     out[0] = 0;
\c     char pattern[300];
\c     snprintf(pattern, sizeof(pattern), "\"%s\"", key);
\c     const char *p = json;
\c     while ((p = strstr(p, pattern)) != NULL) {
\c         if (p > json) {
\c             const char *before = p - 1;
\c             while (before > json && (*before == ' ' || *before == '\t' || *before == '\n' || *before == '\r')) before--;
\c             if (*before != '{' && *before != ',' && *before != '\n') { p++; continue; }
\c         }
\c         p += strlen(pattern);
\c         while (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r') p++;
\c         if (*p != ':') continue;
\c         p++;
\c         while (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r') p++;
\c         if (*p == '"') {
\c             p++;
\c             int i = 0;
\c             while (*p && *p != '"' && i < outsz - 1) {
\c                 if (*p == '\\' && *(p+1)) { out[i++] = *(p+1); p += 2; }
\c                 else { out[i++] = *p; p++; }
\c             }
\c             out[i] = 0;
\c             return i;
\c         } else if (*p == '{' || *p == '[') {
\c             char open = *p, close_ch = (*p == '{') ? '}' : ']';
\c             int depth = 1, i = 0, in_str = 0;
\c             out[i++] = *p; p++;
\c             while (*p && depth > 0 && i < outsz - 1) {
\c                 if (*p == '\\' && in_str) { if (i < outsz - 2) { out[i++] = *p; p++; out[i++] = *p; } p++; continue; }
\c                 if (*p == '"') in_str = !in_str;
\c                 if (!in_str) { if (*p == open) depth++; else if (*p == close_ch) depth--; }
\c                 out[i++] = *p; p++;
\c             }
\c             out[i] = 0;
\c             return i;
\c         } else {
\c             int i = 0;
\c             while (*p && *p != ',' && *p != '}' && *p != ']' && *p != ' ' && *p != '\n' && *p != '\r' && i < outsz - 1) {
\c                 out[i++] = *p; p++;
\c             }
\c             out[i] = 0;
\c             return i;
\c         }
\c     }
\c     return -1;
\c }
\c
\c /* ---- JSON array iterator ---- */
\c static const char *json_array_next(const char *arr, const char *pos, char *out, int outsz) {
\c     if (!arr || !out) return NULL;
\c     if (!pos) {
\c         pos = arr;
\c         while (*pos && *pos != '[') pos++;
\c         if (*pos == '[') pos++;
\c     }
\c     while (*pos && (*pos == ' ' || *pos == '\t' || *pos == '\n' || *pos == '\r' || *pos == ',')) pos++;
\c     if (!*pos || *pos == ']') return NULL;
\c     if (*pos == '{') {
\c         int depth = 0, i = 0, in_str = 0;
\c         while (*pos && i < outsz - 1) {
\c             if (*pos == '\\' && in_str) { out[i++] = *pos++; if (*pos && i < outsz - 1) out[i++] = *pos++; continue; }
\c             if (*pos == '"') in_str = !in_str;
\c             if (!in_str) { if (*pos == '{') depth++; else if (*pos == '}') { depth--; out[i++] = *pos++; if (depth == 0) break; continue; } }
\c             out[i++] = *pos++;
\c         }
\c         out[i] = 0;
\c         return pos;
\c     }
\c     return NULL;
\c }
\c
\c /* ---- Config loading ---- */
\c static char g_config_json[65536];
\c
\c static int load_config_file(const char *path) {
\c     FILE *f = fopen(path, "r");
\c     if (!f) return -1;
\c     int n = fread(g_config_json, 1, sizeof(g_config_json) - 1, f);
\c     fclose(f);
\c     g_config_json[n] = 0;
\c     return 0;
\c }
\c
\c long c_load_config(void) {
\c     const char *cp = getenv("CONFIG_PATH");
\c     int ok = -1;
\c     if (cp && *cp) ok = load_config_file(cp);
\c     if (ok < 0) ok = load_config_file("./airgradientz.json");
\c     if (ok < 0) ok = load_config_file("../airgradientz.json");
\c     if (ok < 0) { fprintf(stderr, "fatal: no config file found\n"); return -1; }
\c
\c     char val[4096];
\c     const char *port_env = getenv("PORT");
\c     if (port_env && *port_env) {
\c         g_port = atoi(port_env);
\c     } else if (json_get(g_config_json, "ports", val, sizeof(val)) > 0) {
\c         char pval[32];
\c         if (json_get(val, "forth", pval, sizeof(pval)) > 0) g_port = atoi(pval);
\c     }
\c
\c     const char *db_env = getenv("DB_PATH");
\c     if (db_env && *db_env) {
\c         snprintf(g_db_path, sizeof(g_db_path), "%s", db_env);
\c     }
\c
\c     if (json_get(g_config_json, "pollIntervalMs", val, sizeof(val)) > 0)
\c         g_poll_interval_ms = atoi(val);
\c     if (json_get(g_config_json, "fetchTimeoutMs", val, sizeof(val)) > 0)
\c         g_fetch_timeout_ms = atoi(val);
\c     if (json_get(g_config_json, "maxApiRows", val, sizeof(val)) > 0)
\c         g_max_api_rows = atoi(val);
\c
\c     char devices_json[8192];
\c     if (json_get(g_config_json, "devices", devices_json, sizeof(devices_json)) > 0) {
\c         char obj[1024];
\c         const char *pos = NULL;
\c         g_num_devices = 0;
\c         while ((pos = json_array_next(devices_json, pos, obj, sizeof(obj))) != NULL && g_num_devices < 16) {
\c             device_t *d = &g_devices[g_num_devices];
\c             memset(d, 0, sizeof(*d));
\c             json_get(obj, "ip", d->ip, sizeof(d->ip));
\c             json_get(obj, "label", d->label, sizeof(d->label));
\c             strcpy(d->status, "unknown");
\c             g_num_devices++;
\c         }
\c     }
\c
\c     g_started_at = now_ms();
\c     fprintf(stderr, "[forth] Config loaded: port=%d db=%s poll=%dms devices=%d\n",
\c             g_port, g_db_path, g_poll_interval_ms, g_num_devices);
\c     return 0;
\c }
\c
\c /* ---- SQLite ---- */
\c static sqlite3 *g_db = NULL;
\c
\c long c_db_init(void) {
\c     int rc = sqlite3_open(g_db_path, &g_db);
\c     if (rc != SQLITE_OK) {
\c         fprintf(stderr, "[forth] sqlite3_open(%s) failed: %s\n", g_db_path, sqlite3_errmsg(g_db));
\c         return -1;
\c     }
\c     sqlite3_exec(g_db, "PRAGMA journal_mode=WAL;", NULL, NULL, NULL);
\c     sqlite3_exec(g_db, "PRAGMA busy_timeout=5000;", NULL, NULL, NULL);
\c     sqlite3_exec(g_db, "PRAGMA foreign_keys=ON;", NULL, NULL, NULL);
\c     const char *schema =
\c         "CREATE TABLE IF NOT EXISTS readings ("
\c         "    id INTEGER PRIMARY KEY AUTOINCREMENT,"
\c         "    timestamp INTEGER NOT NULL,"
\c         "    device_id TEXT NOT NULL,"
\c         "    device_type TEXT NOT NULL CHECK(device_type IN ('indoor', 'outdoor')),"
\c         "    device_ip TEXT NOT NULL,"
\c         "    pm01 REAL, pm02 REAL, pm10 REAL, pm02_compensated REAL,"
\c         "    rco2 INTEGER,"
\c         "    atmp REAL, atmp_compensated REAL,"
\c         "    rhum REAL, rhum_compensated REAL,"
\c         "    tvoc_index REAL, nox_index REAL,"
\c         "    wifi INTEGER,"
\c         "    raw_json TEXT NOT NULL"
\c         ");"
\c         "CREATE INDEX IF NOT EXISTS idx_readings_ts ON readings(timestamp);"
\c         "CREATE INDEX IF NOT EXISTS idx_readings_device ON readings(device_id, timestamp);";
\c     char *err = NULL;
\c     rc = sqlite3_exec(g_db, schema, NULL, NULL, &err);
\c     if (rc != SQLITE_OK) {
\c         fprintf(stderr, "[forth] schema error: %s\n", err ? err : "unknown");
\c         if (err) sqlite3_free(err);
\c     }
\c     fprintf(stderr, "[forth] Database initialized: %s\n", g_db_path);
\c     return 0;
\c }
\c
\c /* ---- Response buffer ---- */
\c #define RESP_MAX (16 * 1024 * 1024)
\c static char *g_resp = NULL;
\c static int   g_resp_len = 0;
\c
\c static void resp_init(void) {
\c     if (!g_resp) g_resp = (char*)malloc(RESP_MAX);
\c     g_resp_len = 0;
\c     g_resp[0] = 0;
\c }
\c
\c static void resp_append(const char *s, int len) {
\c     if (g_resp_len + len >= RESP_MAX) return;
\c     memcpy(g_resp + g_resp_len, s, len);
\c     g_resp_len += len;
\c     g_resp[g_resp_len] = 0;
\c }
\c
\c static void resp_str(const char *s) {
\c     resp_append(s, strlen(s));
\c }
\c
\c /* ---- JSON output helpers ---- */
\c static void json_out_str(const char *key, const char *val, int comma) {
\c     char buf[2048];
\c     int n;
\c     if (key) {
\c         char escaped[1024];
\c         int ei = 0;
\c         for (int i = 0; val && val[i] && ei < (int)sizeof(escaped) - 6; i++) {
\c             char c = val[i];
\c             if (c == '"') { escaped[ei++] = '\\'; escaped[ei++] = '"'; }
\c             else if (c == '\\') { escaped[ei++] = '\\'; escaped[ei++] = '\\'; }
\c             else if (c == '\n') { escaped[ei++] = '\\'; escaped[ei++] = 'n'; }
\c             else if (c == '\r') { escaped[ei++] = '\\'; escaped[ei++] = 'r'; }
\c             else if (c == '\t') { escaped[ei++] = '\\'; escaped[ei++] = 't'; }
\c             else if ((unsigned char)c < 0x20) {
\c                 ei += snprintf(escaped + ei, sizeof(escaped) - ei, "\\u%04x", (unsigned char)c);
\c             }
\c             else escaped[ei++] = c;
\c         }
\c         escaped[ei] = 0;
\c         n = snprintf(buf, sizeof(buf), "\"%s\":\"%s\"%s", key, escaped, comma ? "," : "");
\c     } else {
\c         n = snprintf(buf, sizeof(buf), "\"%s\"%s", val ? val : "", comma ? "," : "");
\c     }
\c     resp_append(buf, n);
\c }
\c
\c static void json_out_int(const char *key, long val, int comma) {
\c     char buf[256];
\c     int n = snprintf(buf, sizeof(buf), "\"%s\":%ld%s", key, val, comma ? "," : "");
\c     resp_append(buf, n);
\c }
\c
\c static void json_out_null(const char *key, int comma) {
\c     char buf[256];
\c     int n = snprintf(buf, sizeof(buf), "\"%s\":null%s", key, comma ? "," : "");
\c     resp_append(buf, n);
\c }
\c
\c static void json_out_real_or_null(const char *key, int is_null, double val, int comma) {
\c     char buf[256];
\c     int n;
\c     if (is_null) {
\c         n = snprintf(buf, sizeof(buf), "\"%s\":null%s", key, comma ? "," : "");
\c     } else {
\c         if (val == (long)val && !isinf(val) && !isnan(val) && val >= -1e15 && val <= 1e15) {
\c             n = snprintf(buf, sizeof(buf), "\"%s\":%ld%s", key, (long)val, comma ? "," : "");
\c         } else {
\c             n = snprintf(buf, sizeof(buf), "\"%s\":%.10g%s", key, val, comma ? "," : "");
\c         }
\c     }
\c     resp_append(buf, n);
\c }
\c
\c static void json_out_int_or_null(const char *key, int is_null, long val, int comma) {
\c     char buf[256];
\c     int n;
\c     if (is_null) {
\c         n = snprintf(buf, sizeof(buf), "\"%s\":null%s", key, comma ? "," : "");
\c     } else {
\c         n = snprintf(buf, sizeof(buf), "\"%s\":%ld%s", key, val, comma ? "," : "");
\c     }
\c     resp_append(buf, n);
\c }
\c
\c /* ---- Query readings to JSON ---- */
\c static int query_readings_to_json(const char *sql, int include_id) {
\c     resp_init();
\c     resp_str("[");
\c     sqlite3_stmt *stmt = NULL;
\c     int rc = sqlite3_prepare_v2(g_db, sql, -1, &stmt, NULL);
\c     if (rc != SQLITE_OK) {
\c         fprintf(stderr, "[forth] SQL error: %s\nQuery: %s\n", sqlite3_errmsg(g_db), sql);
\c         resp_init();
\c         resp_str("[]");
\c         return -1;
\c     }
\c     int first = 1;
\c     while (sqlite3_step(stmt) == SQLITE_ROW) {
\c         if (!first) resp_str(",");
\c         first = 0;
\c         resp_str("{");
\c         int col = 0;
\c         if (include_id) {
\c             json_out_int("id", sqlite3_column_int64(stmt, col), 1);
\c             col++;
\c         }
\c         json_out_int("timestamp", sqlite3_column_int64(stmt, col), 1); col++;
\c         json_out_str("device_id", (const char*)sqlite3_column_text(stmt, col), 1); col++;
\c         json_out_str("device_type", (const char*)sqlite3_column_text(stmt, col), 1); col++;
\c         json_out_str("device_ip", (const char*)sqlite3_column_text(stmt, col), 1); col++;
\c         const char *rf1[] = {"pm01", "pm02", "pm10", "pm02_compensated", NULL};
\c         for (int i = 0; rf1[i]; i++) {
\c             int is_null = (sqlite3_column_type(stmt, col) == SQLITE_NULL);
\c             json_out_real_or_null(rf1[i], is_null, sqlite3_column_double(stmt, col), 1);
\c             col++;
\c         }
\c         {
\c             int is_null = (sqlite3_column_type(stmt, col) == SQLITE_NULL);
\c             json_out_int_or_null("rco2", is_null, sqlite3_column_int64(stmt, col), 1);
\c             col++;
\c         }
\c         const char *rf2[] = {"atmp", "atmp_compensated", "rhum", "rhum_compensated", NULL};
\c         for (int i = 0; rf2[i]; i++) {
\c             int is_null = (sqlite3_column_type(stmt, col) == SQLITE_NULL);
\c             json_out_real_or_null(rf2[i], is_null, sqlite3_column_double(stmt, col), 1);
\c             col++;
\c         }
\c         {
\c             int is_null = (sqlite3_column_type(stmt, col) == SQLITE_NULL);
\c             json_out_real_or_null("tvoc_index", is_null, sqlite3_column_double(stmt, col), 1);
\c             col++;
\c         }
\c         {
\c             int is_null = (sqlite3_column_type(stmt, col) == SQLITE_NULL);
\c             json_out_real_or_null("nox_index", is_null, sqlite3_column_double(stmt, col), 1);
\c             col++;
\c         }
\c         {
\c             int is_null = (sqlite3_column_type(stmt, col) == SQLITE_NULL);
\c             json_out_int_or_null("wifi", is_null, sqlite3_column_int64(stmt, col), 0);
\c             col++;
\c         }
\c         resp_str("}");
\c     }
\c     sqlite3_finalize(stmt);
\c     resp_str("]");
\c     return 0;
\c }
\c
\c /* ---- API: readings ---- */
\c long c_api_readings(const char *from_s, const char *to_s, const char *device,
\c                     const char *limit_s, const char *downsample) {
\c     long now = now_ms();
\c     long from_ts = (from_s && *from_s) ? atol(from_s) : (now - 86400000L);
\c     long to_ts = (to_s && *to_s) ? atol(to_s) : now;
\c     int requested_limit = (limit_s && *limit_s) ? atoi(limit_s) : 0;
\c     int limit = g_max_api_rows;
\c     if (requested_limit > 0 && requested_limit < g_max_api_rows)
\c         limit = requested_limit;
\c
\c     int filter_device = 0;
\c     if (device && *device && strcmp(device, "all") != 0)
\c         filter_device = 1;
\c
\c     long bucket_ms = 0;
\c     if (downsample && *downsample) {
\c         int found = 0;
\c         for (int i = 0; i < NUM_DS_BUCKETS; i++) {
\c             if (strcmp(downsample, g_ds_buckets[i].name) == 0) {
\c                 bucket_ms = g_ds_buckets[i].ms;
\c                 found = 1;
\c                 break;
\c             }
\c         }
\c         if (!found) return -400;
\c     }
\c
\c     char sql[2048];
\c     if (bucket_ms > 0) {
\c         if (filter_device) {
\c             snprintf(sql, sizeof(sql),
\c                 "SELECT (timestamp / %ld) * %ld AS timestamp,"
\c                 " device_id, device_type, device_ip,"
\c                 " AVG(pm01) AS pm01, AVG(pm02) AS pm02, AVG(pm10) AS pm10,"
\c                 " AVG(pm02_compensated) AS pm02_compensated,"
\c                 " CAST(AVG(rco2) AS INTEGER) AS rco2,"
\c                 " AVG(atmp) AS atmp, AVG(atmp_compensated) AS atmp_compensated,"
\c                 " AVG(rhum) AS rhum, AVG(rhum_compensated) AS rhum_compensated,"
\c                 " AVG(tvoc_index) AS tvoc_index, AVG(nox_index) AS nox_index,"
\c                 " CAST(AVG(wifi) AS INTEGER) AS wifi"
\c                 " FROM readings"
\c                 " WHERE device_id = '%s' AND timestamp >= %ld AND timestamp <= %ld"
\c                 " GROUP BY (timestamp / %ld), device_id"
\c                 " ORDER BY timestamp ASC LIMIT %d",
\c                 bucket_ms, bucket_ms, device, from_ts, to_ts, bucket_ms, limit);
\c         } else {
\c             snprintf(sql, sizeof(sql),
\c                 "SELECT (timestamp / %ld) * %ld AS timestamp,"
\c                 " device_id, device_type, device_ip,"
\c                 " AVG(pm01) AS pm01, AVG(pm02) AS pm02, AVG(pm10) AS pm10,"
\c                 " AVG(pm02_compensated) AS pm02_compensated,"
\c                 " CAST(AVG(rco2) AS INTEGER) AS rco2,"
\c                 " AVG(atmp) AS atmp, AVG(atmp_compensated) AS atmp_compensated,"
\c                 " AVG(rhum) AS rhum, AVG(rhum_compensated) AS rhum_compensated,"
\c                 " AVG(tvoc_index) AS tvoc_index, AVG(nox_index) AS nox_index,"
\c                 " CAST(AVG(wifi) AS INTEGER) AS wifi"
\c                 " FROM readings"
\c                 " WHERE timestamp >= %ld AND timestamp <= %ld"
\c                 " GROUP BY (timestamp / %ld), device_id"
\c                 " ORDER BY timestamp ASC LIMIT %d",
\c                 bucket_ms, bucket_ms, from_ts, to_ts, bucket_ms, limit);
\c         }
\c         return query_readings_to_json(sql, 0);
\c     } else {
\c         if (filter_device) {
\c             snprintf(sql, sizeof(sql),
\c                 "SELECT id, timestamp, device_id, device_type, device_ip,"
\c                 " pm01, pm02, pm10, pm02_compensated, rco2,"
\c                 " atmp, atmp_compensated, rhum, rhum_compensated,"
\c                 " tvoc_index, nox_index, wifi"
\c                 " FROM readings"
\c                 " WHERE device_id = '%s' AND timestamp >= %ld AND timestamp <= %ld"
\c                 " ORDER BY timestamp ASC LIMIT %d",
\c                 device, from_ts, to_ts, limit);
\c         } else {
\c             snprintf(sql, sizeof(sql),
\c                 "SELECT id, timestamp, device_id, device_type, device_ip,"
\c                 " pm01, pm02, pm10, pm02_compensated, rco2,"
\c                 " atmp, atmp_compensated, rhum, rhum_compensated,"
\c                 " tvoc_index, nox_index, wifi"
\c                 " FROM readings"
\c                 " WHERE timestamp >= %ld AND timestamp <= %ld"
\c                 " ORDER BY timestamp ASC LIMIT %d",
\c                 from_ts, to_ts, limit);
\c         }
\c         return query_readings_to_json(sql, 1);
\c     }
\c }
\c
\c /* ---- API: readings/latest ---- */
\c long c_api_readings_latest(void) {
\c     const char *sql =
\c         "SELECT r.id, r.timestamp, r.device_id, r.device_type, r.device_ip,"
\c         " r.pm01, r.pm02, r.pm10, r.pm02_compensated, r.rco2,"
\c         " r.atmp, r.atmp_compensated, r.rhum, r.rhum_compensated,"
\c         " r.tvoc_index, r.nox_index, r.wifi"
\c         " FROM readings r"
\c         " INNER JOIN ("
\c         "     SELECT device_id, MAX(id) AS max_id"
\c         "     FROM readings GROUP BY device_id"
\c         " ) latest ON r.id = latest.max_id";
\c     return query_readings_to_json(sql, 1);
\c }
\c
\c /* ---- API: readings/count ---- */
\c long c_api_readings_count(const char *from_s, const char *to_s, const char *device) {
\c     long now = now_ms();
\c     long from_ts = (from_s && *from_s) ? atol(from_s) : 0;
\c     long to_ts = (to_s && *to_s) ? atol(to_s) : now;
\c
\c     int filter_device = 0;
\c     if (device && *device && strcmp(device, "all") != 0)
\c         filter_device = 1;
\c
\c     char sql[1024];
\c     if (filter_device) {
\c         snprintf(sql, sizeof(sql),
\c             "SELECT COUNT(*) FROM readings WHERE device_id = '%s' AND timestamp >= %ld AND timestamp <= %ld",
\c             device, from_ts, to_ts);
\c     } else {
\c         snprintf(sql, sizeof(sql),
\c             "SELECT COUNT(*) FROM readings WHERE timestamp >= %ld AND timestamp <= %ld",
\c             from_ts, to_ts);
\c     }
\c
\c     resp_init();
\c     sqlite3_stmt *stmt = NULL;
\c     int rc = sqlite3_prepare_v2(g_db, sql, -1, &stmt, NULL);
\c     if (rc != SQLITE_OK) {
\c         resp_str("{\"count\":0}");
\c         return -1;
\c     }
\c     long count = 0;
\c     if (sqlite3_step(stmt) == SQLITE_ROW) {
\c         count = sqlite3_column_int64(stmt, 0);
\c     }
\c     sqlite3_finalize(stmt);
\c     char buf[128];
\c     snprintf(buf, sizeof(buf), "{\"count\":%ld}", count);
\c     resp_str(buf);
\c     return 0;
\c }
\c
\c /* ---- API: devices ---- */
\c long c_api_devices(void) {
\c     const char *sql =
\c         "SELECT device_id, device_type, device_ip,"
\c         " MAX(timestamp) AS last_seen, COUNT(*) AS reading_count"
\c         " FROM readings GROUP BY device_id ORDER BY device_type";
\c     resp_init();
\c     resp_str("[");
\c     sqlite3_stmt *stmt = NULL;
\c     int rc = sqlite3_prepare_v2(g_db, sql, -1, &stmt, NULL);
\c     if (rc != SQLITE_OK) { resp_str("]"); return -1; }
\c     int first = 1;
\c     while (sqlite3_step(stmt) == SQLITE_ROW) {
\c         if (!first) resp_str(",");
\c         first = 0;
\c         resp_str("{");
\c         json_out_str("device_id", (const char*)sqlite3_column_text(stmt, 0), 1);
\c         json_out_str("device_type", (const char*)sqlite3_column_text(stmt, 1), 1);
\c         json_out_str("device_ip", (const char*)sqlite3_column_text(stmt, 2), 1);
\c         json_out_int("last_seen", sqlite3_column_int64(stmt, 3), 1);
\c         json_out_int("reading_count", sqlite3_column_int64(stmt, 4), 0);
\c         resp_str("}");
\c     }
\c     sqlite3_finalize(stmt);
\c     resp_str("]");
\c     return 0;
\c }
\c
\c /* ---- API: health ---- */
\c long c_api_health(void) {
\c     resp_init();
\c     resp_str("[");
\c     for (int i = 0; i < g_num_devices; i++) {
\c         if (i > 0) resp_str(",");
\c         device_t *d = &g_devices[i];
\c         resp_str("{");
\c         json_out_str("ip", d->ip, 1);
\c         json_out_str("label", d->label, 1);
\c         json_out_str("status", d->status, 1);
\c         if (d->last_success > 0) json_out_int("lastSuccess", d->last_success, 1);
\c         else json_out_null("lastSuccess", 1);
\c         if (d->last_error > 0) json_out_int("lastError", d->last_error, 1);
\c         else json_out_null("lastError", 1);
\c         if (d->last_error_msg[0]) json_out_str("lastErrorMessage", d->last_error_msg, 1);
\c         else json_out_null("lastErrorMessage", 1);
\c         json_out_int("consecutiveFailures", d->consecutive_failures, 0);
\c         resp_str("}");
\c     }
\c     resp_str("]");
\c     return 0;
\c }
\c
\c /* ---- API: config ---- */
\c long c_api_config(void) {
\c     resp_init();
\c     resp_str("{");
\c     json_out_int("pollIntervalMs", g_poll_interval_ms, 1);
\c     resp_str("\"downsampleBuckets\":{");
\c     for (int i = 0; i < NUM_DS_BUCKETS; i++) {
\c         char buf[128];
\c         int n = snprintf(buf, sizeof(buf), "\"%s\":%ld%s",
\c             g_ds_buckets[i].name, g_ds_buckets[i].ms, (i < NUM_DS_BUCKETS - 1) ? "," : "");
\c         resp_append(buf, n);
\c     }
\c     resp_str("},");
\c     resp_str("\"devices\":[");
\c     for (int i = 0; i < g_num_devices; i++) {
\c         if (i > 0) resp_str(",");
\c         resp_str("{");
\c         json_out_str("ip", g_devices[i].ip, 1);
\c         json_out_str("label", g_devices[i].label, 0);
\c         resp_str("}");
\c     }
\c     resp_str("]}");
\c     return 0;
\c }
\c
\c /* ---- API: stats ---- */
\c long c_api_stats(void) {
\c     resp_init();
\c     long now = now_ms();
\c     long uptime = now - g_started_at;
\c     long pid = (long)getpid();
\c
\c     long rss_bytes = 0;
\c     {
\c         char path[64];
\c         snprintf(path, sizeof(path), "/proc/%ld/statm", pid);
\c         FILE *f = fopen(path, "r");
\c         if (f) {
\c             long total, rss_pages;
\c             if (fscanf(f, "%ld %ld", &total, &rss_pages) == 2) {
\c                 rss_bytes = rss_pages * 4096;
\c             }
\c             fclose(f);
\c         }
\c     }
\c
\c     long db_size = 0;
\c     {
\c         struct stat st;
\c         if (stat(g_db_path, &st) == 0) {
\c             db_size = (long)st.st_size;
\c         }
\c     }
\c
\c     long readings_count = 0;
\c     {
\c         sqlite3_stmt *stmt = NULL;
\c         if (sqlite3_prepare_v2(g_db, "SELECT COUNT(*) FROM readings", -1, &stmt, NULL) == SQLITE_OK) {
\c             if (sqlite3_step(stmt) == SQLITE_ROW) readings_count = sqlite3_column_int64(stmt, 0);
\c             sqlite3_finalize(stmt);
\c         }
\c     }
\c
\c     resp_str("{");
\c     json_out_str("implementation", "forth", 1);
\c     json_out_int("pid", pid, 1);
\c     json_out_int("uptime_ms", uptime, 1);
\c     json_out_int("memory_rss_bytes", rss_bytes, 1);
\c     json_out_int("db_size_bytes", db_size, 1);
\c     json_out_int("readings_count", readings_count, 1);
\c     json_out_int("requests_served", g_requests_served, 1);
\c     json_out_int("active_connections", 0, 1);
\c     json_out_int("poll_successes", g_poll_successes, 1);
\c     json_out_int("poll_failures", g_poll_failures, 1);
\c     json_out_int("pool_alloc_count", 0, 1);
\c     json_out_int("pool_bytes_used", 0, 1);
\c     json_out_int("started_at", g_started_at, 0);
\c     resp_str("}");
\c     return 0;
\c }
\c
\c /* ---- Network server ---- */
\c static int g_server_fd = -1;
\c
\c long c_server_listen(void) {
\c     g_server_fd = socket(AF_INET, SOCK_STREAM, 0);
\c     if (g_server_fd < 0) return -1;
\c     int opt = 1;
\c     setsockopt(g_server_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
\c     struct sockaddr_in addr;
\c     memset(&addr, 0, sizeof(addr));
\c     addr.sin_family = AF_INET;
\c     addr.sin_addr.s_addr = INADDR_ANY;
\c     addr.sin_port = htons((uint16_t)g_port);
\c     if (bind(g_server_fd, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
\c         fprintf(stderr, "[forth] bind(%d) failed: %s\n", g_port, strerror(errno));
\c         close(g_server_fd);
\c         return -2;
\c     }
\c     if (listen(g_server_fd, 128) < 0) {
\c         close(g_server_fd);
\c         return -3;
\c     }
\c     fprintf(stderr, "[forth] Listening on port %d\n", g_port);
\c     return 0;
\c }
\c
\c long c_server_accept(void) {
\c     struct sockaddr_in addr;
\c     socklen_t len = sizeof(addr);
\c     return (long)accept(g_server_fd, (struct sockaddr*)&addr, &len);
\c }
\c
\c /* ---- Request parsing ---- */
\c static char g_req_buf[65536];
\c static int  g_req_len = 0;
\c static char g_method[16];
\c static char g_path[4096];
\c static char g_query[4096];
\c
\c long c_recv_request(long fd) {
\c     g_req_len = 0;
\c     g_method[0] = 0;
\c     g_path[0] = 0;
\c     g_query[0] = 0;
\c
\c     struct timeval tv;
\c     tv.tv_sec = 5;
\c     tv.tv_usec = 0;
\c     setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
\c
\c     int n = recv(fd, g_req_buf, sizeof(g_req_buf) - 1, 0);
\c     if (n <= 0) return -1;
\c     g_req_buf[n] = 0;
\c     g_req_len = n;
\c
\c     char *p = g_req_buf;
\c     char *sp1 = strchr(p, ' ');
\c     if (!sp1) return -1;
\c     int mlen = sp1 - p;
\c     if (mlen >= (int)sizeof(g_method)) mlen = sizeof(g_method) - 1;
\c     memcpy(g_method, p, mlen);
\c     g_method[mlen] = 0;
\c
\c     p = sp1 + 1;
\c     char *sp2 = strchr(p, ' ');
\c     if (!sp2) sp2 = p + strlen(p);
\c     int ulen = sp2 - p;
\c
\c     char *qmark = memchr(p, '?', ulen);
\c     if (qmark) {
\c         int plen = qmark - p;
\c         if (plen >= (int)sizeof(g_path)) plen = sizeof(g_path) - 1;
\c         memcpy(g_path, p, plen);
\c         g_path[plen] = 0;
\c         int qlen = sp2 - qmark - 1;
\c         if (qlen >= (int)sizeof(g_query)) qlen = sizeof(g_query) - 1;
\c         if (qlen > 0) memcpy(g_query, qmark + 1, qlen);
\c         g_query[qlen > 0 ? qlen : 0] = 0;
\c     } else {
\c         int plen = ulen;
\c         if (plen >= (int)sizeof(g_path)) plen = sizeof(g_path) - 1;
\c         memcpy(g_path, p, plen);
\c         g_path[plen] = 0;
\c         g_query[0] = 0;
\c     }
\c     return 0;
\c }
\c
\c long c_resp_ptr(void) { return (long)g_resp; }
\c long c_resp_len(void) { return (long)g_resp_len; }
\c
\c /* ---- Query string parameter extraction ---- */
\c static char g_param_buf[4096];
\c
\c long c_query_param(const char *query, const char *name) {
\c     g_param_buf[0] = 0;
\c     if (!query || !name || !*query) return 0;
\c     int nlen = strlen(name);
\c     const char *p = query;
\c     while (p && *p) {
\c         if (strncmp(p, name, nlen) == 0 && p[nlen] == '=') {
\c             const char *val = p + nlen + 1;
\c             const char *end = strchr(val, '&');
\c             int vlen = end ? (int)(end - val) : (int)strlen(val);
\c             if (vlen >= (int)sizeof(g_param_buf)) vlen = sizeof(g_param_buf) - 1;
\c             memcpy(g_param_buf, val, vlen);
\c             g_param_buf[vlen] = 0;
\c             return (long)vlen;
\c         }
\c         p = strchr(p, '&');
\c         if (p) p++;
\c     }
\c     return 0;
\c }
\c
\c /* ---- URL decoding ---- */
\c static char g_decoded_path[4096];
\c
\c static int hex_val(char c) {
\c     if (c >= '0' && c <= '9') return c - '0';
\c     if (c >= 'a' && c <= 'f') return c - 'a' + 10;
\c     if (c >= 'A' && c <= 'F') return c - 'A' + 10;
\c     return -1;
\c }
\c
\c long c_url_decode_path(void) {
\c     const char *src = g_path;
\c     int di = 0;
\c     while (*src && di < (int)sizeof(g_decoded_path) - 1) {
\c         if (*src == '%' && src[1] && src[2]) {
\c             int h = hex_val(src[1]);
\c             int l = hex_val(src[2]);
\c             if (h >= 0 && l >= 0) {
\c                 char ch = (char)((h << 4) | l);
\c                 if (ch == 0) return -1;
\c                 g_decoded_path[di++] = ch;
\c                 src += 3;
\c                 continue;
\c             }
\c         }
\c         g_decoded_path[di++] = *src++;
\c     }
\c     g_decoded_path[di] = 0;
\c     if (strstr(g_decoded_path, "..")) return -2;
\c     return 0;
\c }
\c
\c /* ---- HTTP response sending ---- */
\c static void send_all(int fd, const char *buf, int len) {
\c     int sent = 0;
\c     while (sent < len) {
\c         int n = send(fd, buf + sent, len - sent, MSG_NOSIGNAL);
\c         if (n <= 0) break;
\c         sent += n;
\c     }
\c }
\c
\c long c_send_response(long fd, long status, const char *status_text,
\c                      const char *content_type, const char *body, long body_len,
\c                      int is_static) {
\c     char header[4096];
\c     int hlen = snprintf(header, sizeof(header),
\c         "HTTP/1.1 %ld %s\r\n"
\c         "Content-Type: %s\r\n"
\c         "Content-Length: %ld\r\n"
\c         "X-Content-Type-Options: nosniff\r\n"
\c         "X-Frame-Options: DENY\r\n"
\c         "Connection: close\r\n"
\c         "%s"
\c         "\r\n",
\c         status, status_text, content_type, body_len,
\c         is_static ? "Cache-Control: public, max-age=600\r\n" : "");
\c     send_all(fd, header, hlen);
\c     if (body && body_len > 0) send_all(fd, body, body_len);
\c     return 0;
\c }
\c
\c long c_send_json_response(long fd, long status, const char *status_text) {
\c     return c_send_response(fd, status, status_text, "application/json",
\c                            g_resp, g_resp_len, 0);
\c }
\c
\c long c_send_error(long fd, long status, const char *status_text, const char *msg) {
\c     resp_init();
\c     resp_str("{\"error\":\"");
\c     for (const char *p = msg; *p; p++) {
\c         if (*p == '"') { resp_str("\\\""); }
\c         else if (*p == '\\') { resp_str("\\\\"); }
\c         else { resp_append(p, 1); }
\c     }
\c     resp_str("\"}");
\c     return c_send_json_response(fd, status, status_text);
\c }
\c
\c /* ---- Static file serving ---- */
\c #define MAX_FILE_SIZE (16 * 1024 * 1024)
\c
\c static const char *content_type_for(const char *path) {
\c     const char *ext = strrchr(path, '.');
\c     if (!ext) return "application/octet-stream";
\c     if (strcmp(ext, ".html") == 0) return "text/html";
\c     if (strcmp(ext, ".css") == 0) return "text/css";
\c     if (strcmp(ext, ".js") == 0) return "application/javascript";
\c     if (strcmp(ext, ".json") == 0) return "application/json";
\c     if (strcmp(ext, ".png") == 0) return "image/png";
\c     if (strcmp(ext, ".jpg") == 0 || strcmp(ext, ".jpeg") == 0) return "image/jpeg";
\c     if (strcmp(ext, ".gif") == 0) return "image/gif";
\c     if (strcmp(ext, ".svg") == 0) return "image/svg+xml";
\c     if (strcmp(ext, ".ico") == 0) return "image/x-icon";
\c     if (strcmp(ext, ".woff2") == 0) return "font/woff2";
\c     if (strcmp(ext, ".woff") == 0) return "font/woff";
\c     return "application/octet-stream";
\c }
\c
\c long c_serve_static(long fd) {
\c     int rc = c_url_decode_path();
\c     if (rc == -1) {
\c         return c_send_error(fd, 400, "Bad Request", "Bad request");
\c     }
\c     if (rc == -2) {
\c         return c_send_error(fd, 403, "Forbidden", "Forbidden");
\c     }
\c
\c     char filepath[4096];
\c     const char *rel = g_decoded_path;
\c     while (*rel == '/') rel++;
\c     if (*rel == 0) {
\c         snprintf(filepath, sizeof(filepath), "public/index.html");
\c     } else {
\c         snprintf(filepath, sizeof(filepath), "public/%s", rel);
\c     }
\c
\c     char resolved[4096], public_resolved[4096];
\c     if (!realpath(filepath, resolved)) {
\c         return c_send_error(fd, 404, "Not Found", "Not found");
\c     }
\c     if (!realpath("public", public_resolved)) {
\c         return c_send_error(fd, 404, "Not Found", "Not found");
\c     }
\c     int pub_len = strlen(public_resolved);
\c     if (strncmp(resolved, public_resolved, pub_len) != 0 ||
\c         (resolved[pub_len] != '/' && resolved[pub_len] != 0)) {
\c         return c_send_error(fd, 404, "Not Found", "Not found");
\c     }
\c
\c     struct stat st;
\c     if (stat(resolved, &st) != 0 || !S_ISREG(st.st_mode)) {
\c         return c_send_error(fd, 404, "Not Found", "Not found");
\c     }
\c     if (st.st_size > MAX_FILE_SIZE) {
\c         return c_send_error(fd, 404, "Not Found", "Not found");
\c     }
\c
\c     FILE *f = fopen(resolved, "rb");
\c     if (!f) return c_send_error(fd, 404, "Not Found", "Not found");
\c     char *buf = (char*)malloc(st.st_size);
\c     if (!buf) { fclose(f); return c_send_error(fd, 500, "Internal Server Error", "Out of memory"); }
\c     long nread = fread(buf, 1, st.st_size, f);
\c     fclose(f);
\c
\c     const char *ct = content_type_for(resolved);
\c     c_send_response(fd, 200, "OK", ct, buf, nread, 1);
\c     free(buf);
\c     return 0;
\c }
\c
\c /* ---- Close connection ---- */
\c void c_close(long fd) { close((int)fd); }
\c
\c /* ---- curl write callback ---- */
\c static char g_curl_buf[1048576];
\c static int  g_curl_len = 0;
\c
\c static size_t curl_write_cb(char *data, size_t sz, size_t nmemb, void *u) {
\c     (void)u;
\c     size_t total = sz * nmemb;
\c     if (g_curl_len + (int)total >= (int)sizeof(g_curl_buf)) return 0;
\c     memcpy(g_curl_buf + g_curl_len, data, total);
\c     g_curl_len += total;
\c     return total;
\c }
\c
\c /* ---- Device polling ---- */
\c static int poll_count = 0;
\c
\c static void poll_one_device(int idx) {
\c     device_t *d = &g_devices[idx];
\c     char url[256];
\c     snprintf(url, sizeof(url), "http://%s/measures/current", d->ip);
\c
\c     g_curl_len = 0;
\c     CURL *c = curl_easy_init();
\c     if (!c) {
\c         d->last_error = now_ms();
\c         snprintf(d->last_error_msg, sizeof(d->last_error_msg), "curl_easy_init failed");
\c         strcpy(d->status, "error");
\c         d->consecutive_failures++;
\c         g_poll_failures++;
\c         return;
\c     }
\c     curl_easy_setopt(c, CURLOPT_URL, url);
\c     curl_easy_setopt(c, CURLOPT_WRITEFUNCTION, curl_write_cb);
\c     curl_easy_setopt(c, CURLOPT_TIMEOUT_MS, (long)g_fetch_timeout_ms);
\c     curl_easy_setopt(c, CURLOPT_NOSIGNAL, 1L);
\c     curl_easy_setopt(c, CURLOPT_CONNECTTIMEOUT_MS, (long)g_fetch_timeout_ms);
\c
\c     CURLcode res = curl_easy_perform(c);
\c     long http_code = 0;
\c     curl_easy_getinfo(c, CURLINFO_RESPONSE_CODE, &http_code);
\c     curl_easy_cleanup(c);
\c     g_curl_buf[g_curl_len] = 0;
\c
\c     if (res != CURLE_OK) {
\c         d->last_error = now_ms();
\c         snprintf(d->last_error_msg, sizeof(d->last_error_msg), "%s", curl_easy_strerror(res));
\c         strcpy(d->status, "error");
\c         d->consecutive_failures++;
\c         g_poll_failures++;
\c         fprintf(stderr, "[forth] Poll %s: curl error: %s\n", d->ip, curl_easy_strerror(res));
\c         return;
\c     }
\c
\c     if (http_code != 200) {
\c         d->last_error = now_ms();
\c         snprintf(d->last_error_msg, sizeof(d->last_error_msg), "HTTP %ld", http_code);
\c         strcpy(d->status, "error");
\c         d->consecutive_failures++;
\c         g_poll_failures++;
\c         fprintf(stderr, "[forth] Poll %s: HTTP %ld\n", d->ip, http_code);
\c         return;
\c     }
\c
\c     const char *json = g_curl_buf;
\c     while (*json == ' ' || *json == '\n' || *json == '\r' || *json == '\t') json++;
\c     if (*json != '{') {
\c         d->last_error = now_ms();
\c         snprintf(d->last_error_msg, sizeof(d->last_error_msg), "Invalid JSON response");
\c         strcpy(d->status, "error");
\c         d->consecutive_failures++;
\c         g_poll_failures++;
\c         fprintf(stderr, "[forth] Poll %s: invalid JSON\n", d->ip);
\c         return;
\c     }
\c
\c     char val[512];
\c     char serial[128] = "unknown";
\c     char model[128] = "";
\c     if (json_get(json, "serialno", val, sizeof(val)) > 0) strncpy(serial, val, sizeof(serial)-1);
\c     json_get(json, "model", model, sizeof(model));
\c
\c     const char *dev_type = "outdoor";
\c     if (model[0] == 'I' && model[1] == '-') dev_type = "indoor";
\c
\c     long ts = now_ms();
\c
\c     sqlite3_stmt *stmt = NULL;
\c     const char *insert_sql =
\c         "INSERT INTO readings (timestamp, device_id, device_type, device_ip,"
\c         " pm01, pm02, pm10, pm02_compensated, rco2,"
\c         " atmp, atmp_compensated, rhum, rhum_compensated,"
\c         " tvoc_index, nox_index, wifi, raw_json)"
\c         " VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
\c     int rc = sqlite3_prepare_v2(g_db, insert_sql, -1, &stmt, NULL);
\c     if (rc != SQLITE_OK) {
\c         d->last_error = now_ms();
\c         snprintf(d->last_error_msg, sizeof(d->last_error_msg), "DB prepare: %s", sqlite3_errmsg(g_db));
\c         strcpy(d->status, "error");
\c         d->consecutive_failures++;
\c         g_poll_failures++;
\c         return;
\c     }
\c
\c     sqlite3_bind_int64(stmt, 1, ts);
\c     sqlite3_bind_text(stmt, 2, serial, -1, SQLITE_TRANSIENT);
\c     sqlite3_bind_text(stmt, 3, dev_type, -1, SQLITE_TRANSIENT);
\c     sqlite3_bind_text(stmt, 4, d->ip, -1, SQLITE_TRANSIENT);
\c
\c     struct { const char *json_key; int col; } real_fields[] = {
\c         {"pm01", 5}, {"pm02", 6}, {"pm10", 7}, {"pm02Compensated", 8},
\c         {"atmp", 10}, {"atmpCompensated", 11}, {"rhum", 12}, {"rhumCompensated", 13},
\c         {"tvocIndex", 14}, {"noxIndex", 15},
\c     };
\c     for (int i = 0; i < 10; i++) {
\c         if (json_get(json, real_fields[i].json_key, val, sizeof(val)) > 0 && strcmp(val, "null") != 0) {
\c             sqlite3_bind_double(stmt, real_fields[i].col, atof(val));
\c         } else {
\c             sqlite3_bind_null(stmt, real_fields[i].col);
\c         }
\c     }
\c     if (json_get(json, "rco2", val, sizeof(val)) > 0 && strcmp(val, "null") != 0) {
\c         sqlite3_bind_int(stmt, 9, atoi(val));
\c     } else {
\c         sqlite3_bind_null(stmt, 9);
\c     }
\c     if (json_get(json, "wifi", val, sizeof(val)) > 0 && strcmp(val, "null") != 0) {
\c         sqlite3_bind_int(stmt, 16, atoi(val));
\c     } else {
\c         sqlite3_bind_null(stmt, 16);
\c     }
\c     sqlite3_bind_text(stmt, 17, json, -1, SQLITE_TRANSIENT);
\c
\c     rc = sqlite3_step(stmt);
\c     sqlite3_finalize(stmt);
\c
\c     if (rc != SQLITE_DONE) {
\c         d->last_error = now_ms();
\c         snprintf(d->last_error_msg, sizeof(d->last_error_msg), "DB insert: %s", sqlite3_errmsg(g_db));
\c         strcpy(d->status, "error");
\c         d->consecutive_failures++;
\c         g_poll_failures++;
\c         fprintf(stderr, "[forth] Poll %s: DB error: %s\n", d->ip, sqlite3_errmsg(g_db));
\c         return;
\c     }
\c
\c     strcpy(d->status, "ok");
\c     d->last_success = ts;
\c     d->last_error_msg[0] = 0;
\c     d->consecutive_failures = 0;
\c     g_poll_successes++;
\c     fprintf(stderr, "[forth] Poll %s: OK (serial=%s type=%s)\n", d->ip, serial, dev_type);
\c }
\c
\c void c_poll_all_devices(void) {
\c     for (int i = 0; i < g_num_devices; i++) {
\c         poll_one_device(i);
\c     }
\c     poll_count++;
\c     if (poll_count % 10 == 0) {
\c         sqlite3_wal_checkpoint(g_db, NULL);
\c     }
\c }
\c
\c /* ---- Poller thread ---- */
\c static pthread_t g_poller_thread;
\c static pthread_mutex_t g_db_mutex = PTHREAD_MUTEX_INITIALIZER;
\c
\c static void *poller_thread_fn(void *arg) {
\c     (void)arg;
\c     fprintf(stderr, "[forth] Poller thread started (interval=%dms)\n", g_poll_interval_ms);
\c     while (g_running) {
\c         pthread_mutex_lock(&g_db_mutex);
\c         c_poll_all_devices();
\c         pthread_mutex_unlock(&g_db_mutex);
\c
\c         int remaining = g_poll_interval_ms;
\c         while (remaining > 0 && g_running) {
\c             int sleep_ms = remaining > 500 ? 500 : remaining;
\c             usleep(sleep_ms * 1000);
\c             remaining -= sleep_ms;
\c         }
\c     }
\c     return NULL;
\c }
\c
\c long c_start_poller(void) {
\c     curl_global_init(CURL_GLOBAL_ALL);
\c     int rc = pthread_create(&g_poller_thread, NULL, poller_thread_fn, NULL);
\c     if (rc != 0) {
\c         fprintf(stderr, "[forth] pthread_create failed: %d\n", rc);
\c         return -1;
\c     }
\c     pthread_detach(g_poller_thread);
\c     return 0;
\c }
\c
\c /* ---- Handle one HTTP connection ---- */
\c long c_handle_connection(long fd) {
\c     if (c_recv_request(fd) < 0) {
\c         c_send_error(fd, 400, "Bad Request", "Bad request");
\c         return -1;
\c     }
\c
\c     g_requests_served++;
\c
\c     if (strcmp(g_method, "GET") != 0) {
\c         c_send_error(fd, 405, "Method Not Allowed", "Method not allowed");
\c         return 0;
\c     }
\c
\c     if (strcmp(g_path, "/api/readings/count") == 0) {
\c         char from_s[32], to_s[32], device_s[256];
\c         from_s[0] = to_s[0] = device_s[0] = 0;
\c         c_query_param(g_query, "from"); strcpy(from_s, g_param_buf);
\c         c_query_param(g_query, "to"); strcpy(to_s, g_param_buf);
\c         c_query_param(g_query, "device"); strcpy(device_s, g_param_buf);
\c         pthread_mutex_lock(&g_db_mutex);
\c         c_api_readings_count(from_s, to_s, device_s);
\c         pthread_mutex_unlock(&g_db_mutex);
\c         c_send_json_response(fd, 200, "OK");
\c         return 0;
\c     }
\c
\c     if (strcmp(g_path, "/api/readings/latest") == 0) {
\c         pthread_mutex_lock(&g_db_mutex);
\c         c_api_readings_latest();
\c         pthread_mutex_unlock(&g_db_mutex);
\c         c_send_json_response(fd, 200, "OK");
\c         return 0;
\c     }
\c
\c     if (strcmp(g_path, "/api/readings") == 0) {
\c         char from_s[32], to_s[32], device_s[256], limit_s[32], ds_s[16];
\c         from_s[0] = to_s[0] = device_s[0] = limit_s[0] = ds_s[0] = 0;
\c         c_query_param(g_query, "from"); strcpy(from_s, g_param_buf);
\c         c_query_param(g_query, "to"); strcpy(to_s, g_param_buf);
\c         c_query_param(g_query, "device"); strcpy(device_s, g_param_buf);
\c         c_query_param(g_query, "limit"); strcpy(limit_s, g_param_buf);
\c         c_query_param(g_query, "downsample"); strcpy(ds_s, g_param_buf);
\c
\c         pthread_mutex_lock(&g_db_mutex);
\c         long rc = c_api_readings(from_s, to_s, device_s, limit_s, ds_s);
\c         pthread_mutex_unlock(&g_db_mutex);
\c
\c         if (rc == -400) {
\c             c_send_error(fd, 400, "Bad Request", "invalid downsample value");
\c         } else {
\c             c_send_json_response(fd, 200, "OK");
\c         }
\c         return 0;
\c     }
\c
\c     if (strcmp(g_path, "/api/devices") == 0) {
\c         pthread_mutex_lock(&g_db_mutex);
\c         c_api_devices();
\c         pthread_mutex_unlock(&g_db_mutex);
\c         c_send_json_response(fd, 200, "OK");
\c         return 0;
\c     }
\c
\c     if (strcmp(g_path, "/api/health") == 0) {
\c         c_api_health();
\c         c_send_json_response(fd, 200, "OK");
\c         return 0;
\c     }
\c
\c     if (strcmp(g_path, "/api/config") == 0) {
\c         c_api_config();
\c         c_send_json_response(fd, 200, "OK");
\c         return 0;
\c     }
\c
\c     if (strcmp(g_path, "/api/stats") == 0) {
\c         pthread_mutex_lock(&g_db_mutex);
\c         c_api_stats();
\c         pthread_mutex_unlock(&g_db_mutex);
\c         c_send_json_response(fd, 200, "OK");
\c         return 0;
\c     }
\c
\c     if (strncmp(g_path, "/api/", 5) == 0) {
\c         c_send_error(fd, 404, "Not Found", "Not found");
\c         return 0;
\c     }
\c
\c     c_serve_static(fd);
\c     return 0;
\c }
\c
\c /* ---- Signal handling ---- */
\c void c_setup_signals(void) {
\c     signal(SIGPIPE, SIG_IGN);
\c     signal(SIGCHLD, SIG_IGN);
\c }
\c
\c long c_get_port(void) { return (long)g_port; }

\ ---- C function declarations for Forth ----
c-function c-load-config c_load_config -- n
c-function c-db-init c_db_init -- n
c-function c-server-listen c_server_listen -- n
c-function c-server-accept c_server_accept -- n
c-function c-handle-connection c_handle_connection n -- n
c-function c-close c_close n -- void
c-function c-start-poller c_start_poller -- n
c-function c-setup-signals c_setup_signals -- void
c-function c-get-port c_get_port -- n

end-c-library

\ ============================================================
\ Forth main program
\ ============================================================

: main ( -- )
  \ Setup signal handlers
  c-setup-signals

  \ Load configuration
  c-load-config 0< if
    1 (bye)
  then

  \ Initialize database
  c-db-init 0< if
    1 (bye)
  then

  \ Start listening
  c-server-listen 0< if
    1 (bye)
  then

  \ Start background poller thread
  c-start-poller drop

  \ Main accept loop
  begin
    c-server-accept dup 0>= while
      dup c-handle-connection drop
      c-close
  repeat
  drop
;

main
bye
