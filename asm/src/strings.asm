; strings.asm — All .rodata constants: SQL, HTTP, format strings, paths
; Every other module uses `extern` to reference these.
default rel

section .rodata

; ── SQL ──────────────────────────────────────────────────────────────────────

global sql_schema
sql_schema:
    db `CREATE TABLE IF NOT EXISTS readings (`
    db `id INTEGER PRIMARY KEY AUTOINCREMENT,`
    db `timestamp INTEGER NOT NULL,`
    db `device_id TEXT NOT NULL,`
    db `device_type TEXT NOT NULL CHECK(device_type IN ('indoor','outdoor')),`
    db `device_ip TEXT NOT NULL,`
    db `pm01 REAL, pm02 REAL, pm10 REAL, pm02_compensated REAL,`
    db `rco2 INTEGER,`
    db `atmp REAL, atmp_compensated REAL,`
    db `rhum REAL, rhum_compensated REAL,`
    db `tvoc_index REAL, nox_index REAL,`
    db `wifi INTEGER,`
    db `raw_json TEXT NOT NULL);`
    db `CREATE INDEX IF NOT EXISTS idx_readings_ts ON readings(timestamp);`
    db `CREATE INDEX IF NOT EXISTS idx_readings_device ON readings(device_id,timestamp);`, 0

global sql_pragma
sql_pragma:
    db `PRAGMA journal_mode=WAL;PRAGMA busy_timeout=5000;PRAGMA foreign_keys=ON;`, 0

global sql_insert_reading
sql_insert_reading:
    db `INSERT INTO readings (`
    db `timestamp,device_id,device_type,device_ip,`
    db `pm01,pm02,pm10,pm02_compensated,`
    db `rco2,atmp,atmp_compensated,rhum,rhum_compensated,`
    db `tvoc_index,nox_index,wifi,raw_json`
    db `) VALUES (?,?,?,?,`
    db `?,?,?,?,`
    db `?,?,?,?,?,`
    db `?,?,?,?);`, 0

; IIF(col=CAST(col AS INTEGER),CAST(col AS INTEGER),col) converts whole-number
; floats to integers — e.g. 1.0 → 1, 20.78 → 20.78, NULL → NULL
global sql_readings_json
sql_readings_json:
    db `SELECT COALESCE(json_group_array(json_object(`
    db `'id',id,'timestamp',timestamp,`
    db `'device_id',device_id,'device_type',device_type,'device_ip',device_ip,`
    db `'pm01',IIF(pm01=CAST(pm01 AS INTEGER),CAST(pm01 AS INTEGER),pm01),`
    db `'pm02',IIF(pm02=CAST(pm02 AS INTEGER),CAST(pm02 AS INTEGER),pm02),`
    db `'pm10',IIF(pm10=CAST(pm10 AS INTEGER),CAST(pm10 AS INTEGER),pm10),`
    db `'pm02_compensated',IIF(pm02_compensated=CAST(pm02_compensated AS INTEGER),CAST(pm02_compensated AS INTEGER),pm02_compensated),`
    db `'rco2',rco2,`
    db `'atmp',IIF(atmp=CAST(atmp AS INTEGER),CAST(atmp AS INTEGER),atmp),`
    db `'atmp_compensated',IIF(atmp_compensated=CAST(atmp_compensated AS INTEGER),CAST(atmp_compensated AS INTEGER),atmp_compensated),`
    db `'rhum',IIF(rhum=CAST(rhum AS INTEGER),CAST(rhum AS INTEGER),rhum),`
    db `'rhum_compensated',IIF(rhum_compensated=CAST(rhum_compensated AS INTEGER),CAST(rhum_compensated AS INTEGER),rhum_compensated),`
    db `'tvoc_index',IIF(tvoc_index=CAST(tvoc_index AS INTEGER),CAST(tvoc_index AS INTEGER),tvoc_index),`
    db `'nox_index',IIF(nox_index=CAST(nox_index AS INTEGER),CAST(nox_index AS INTEGER),nox_index),`
    db `'wifi',wifi`
    db `)),'[]') FROM (SELECT * FROM readings`, 0

; Suffix fragments for building query
global sql_where_ts
sql_where_ts:
    db ` WHERE timestamp >= %lld AND timestamp <= %lld`, 0

global sql_and_device
sql_and_device:
    db ` AND device_id = ?`, 0

global sql_where_device
sql_where_device:
    db ` WHERE device_id = ?`, 0

global sql_order_limit
sql_order_limit:
    db ` ORDER BY timestamp ASC LIMIT %d)`, 0

global sql_order_limit_close
sql_order_limit_close:
    db ` ORDER BY timestamp ASC LIMIT %d);`, 0

global sql_latest_json
sql_latest_json:
    db `SELECT COALESCE(json_group_array(json_object(`
    db `'id',id,'timestamp',timestamp,`
    db `'device_id',device_id,'device_type',device_type,'device_ip',device_ip,`
    db `'pm01',IIF(pm01=CAST(pm01 AS INTEGER),CAST(pm01 AS INTEGER),pm01),`
    db `'pm02',IIF(pm02=CAST(pm02 AS INTEGER),CAST(pm02 AS INTEGER),pm02),`
    db `'pm10',IIF(pm10=CAST(pm10 AS INTEGER),CAST(pm10 AS INTEGER),pm10),`
    db `'pm02_compensated',IIF(pm02_compensated=CAST(pm02_compensated AS INTEGER),CAST(pm02_compensated AS INTEGER),pm02_compensated),`
    db `'rco2',rco2,`
    db `'atmp',IIF(atmp=CAST(atmp AS INTEGER),CAST(atmp AS INTEGER),atmp),`
    db `'atmp_compensated',IIF(atmp_compensated=CAST(atmp_compensated AS INTEGER),CAST(atmp_compensated AS INTEGER),atmp_compensated),`
    db `'rhum',IIF(rhum=CAST(rhum AS INTEGER),CAST(rhum AS INTEGER),rhum),`
    db `'rhum_compensated',IIF(rhum_compensated=CAST(rhum_compensated AS INTEGER),CAST(rhum_compensated AS INTEGER),rhum_compensated),`
    db `'tvoc_index',IIF(tvoc_index=CAST(tvoc_index AS INTEGER),CAST(tvoc_index AS INTEGER),tvoc_index),`
    db `'nox_index',IIF(nox_index=CAST(nox_index AS INTEGER),CAST(nox_index AS INTEGER),nox_index),`
    db `'wifi',wifi`
    db `)),'[]') FROM (`
    db `SELECT r.* FROM readings r `
    db `INNER JOIN (SELECT device_id, MAX(id) AS max_id FROM readings GROUP BY device_id) g `
    db `ON r.device_id = g.device_id AND r.id = g.max_id);`, 0

global sql_devices_json
sql_devices_json:
    db `SELECT COALESCE(json_group_array(json_object(`
    db `'device_id',device_id,'device_type',device_type,'device_ip',device_ip,`
    db `'reading_count',cnt,'last_seen',last_ts`
    db `)),'[]') FROM (`
    db `SELECT device_id,device_type,device_ip,`
    db `COUNT(*) AS cnt,MAX(timestamp) AS last_ts `
    db `FROM readings GROUP BY device_id);`, 0

global sql_downsample_json
sql_downsample_json:
    db `SELECT COALESCE(json_group_array(json_object(`
    db `'timestamp',timestamp,`
    db `'device_id',device_id,'device_type',device_type,'device_ip',device_ip,`
    db `'pm01',IIF(pm01=CAST(pm01 AS INTEGER),CAST(pm01 AS INTEGER),pm01),`
    db `'pm02',IIF(pm02=CAST(pm02 AS INTEGER),CAST(pm02 AS INTEGER),pm02),`
    db `'pm10',IIF(pm10=CAST(pm10 AS INTEGER),CAST(pm10 AS INTEGER),pm10),`
    db `'pm02_compensated',IIF(pm02_compensated=CAST(pm02_compensated AS INTEGER),CAST(pm02_compensated AS INTEGER),pm02_compensated),`
    db `'rco2',rco2,`
    db `'atmp',IIF(atmp=CAST(atmp AS INTEGER),CAST(atmp AS INTEGER),atmp),`
    db `'atmp_compensated',IIF(atmp_compensated=CAST(atmp_compensated AS INTEGER),CAST(atmp_compensated AS INTEGER),atmp_compensated),`
    db `'rhum',IIF(rhum=CAST(rhum AS INTEGER),CAST(rhum AS INTEGER),rhum),`
    db `'rhum_compensated',IIF(rhum_compensated=CAST(rhum_compensated AS INTEGER),CAST(rhum_compensated AS INTEGER),rhum_compensated),`
    db `'tvoc_index',IIF(tvoc_index=CAST(tvoc_index AS INTEGER),CAST(tvoc_index AS INTEGER),tvoc_index),`
    db `'nox_index',IIF(nox_index=CAST(nox_index AS INTEGER),CAST(nox_index AS INTEGER),nox_index),`
    db `'wifi',wifi`
    db `)),'[]') FROM (SELECT (timestamp / %lld) * %lld AS timestamp,`
    db `device_id,device_type,device_ip,`
    db `AVG(pm01) AS pm01,AVG(pm02) AS pm02,AVG(pm10) AS pm10,`
    db `AVG(pm02_compensated) AS pm02_compensated,`
    db `CAST(AVG(rco2) AS INTEGER) AS rco2,`
    db `AVG(atmp) AS atmp,AVG(atmp_compensated) AS atmp_compensated,`
    db `AVG(rhum) AS rhum,AVG(rhum_compensated) AS rhum_compensated,`
    db `AVG(tvoc_index) AS tvoc_index,AVG(nox_index) AS nox_index,`
    db `CAST(AVG(wifi) AS INTEGER) AS wifi`
    db ` FROM readings`, 0

global sql_ds_group_by
sql_ds_group_by:
    db ` GROUP BY (timestamp / %lld), device_id ORDER BY timestamp ASC LIMIT %d);`, 0

global sql_count_filtered
sql_count_filtered:
    db `SELECT json_object('count', COUNT(*)) FROM readings`, 0

global sql_count_where_ts
sql_count_where_ts:
    db ` WHERE timestamp >= %lld AND timestamp <= %lld`, 0

global sql_count_and_device
sql_count_and_device:
    db ` AND device_id = ?`, 0

global sql_count_readings
sql_count_readings:
    db `SELECT COUNT(*) FROM readings;`, 0

global sql_checkpoint
sql_checkpoint:
    db `PRAGMA wal_checkpoint(PASSIVE);`, 0

; ── HTTP response templates ──────────────────────────────────────────────────

global http_200_json_hdr
http_200_json_hdr:
    db `HTTP/1.1 200 OK\r\n`
    db `Content-Type: application/json\r\n`
    db `Content-Length: %zu\r\n`
    db `X-Content-Type-Options: nosniff\r\n`
    db `X-Frame-Options: DENY\r\n`
    db `Connection: close\r\n`
    db `\r\n`, 0

global http_200_file_hdr
http_200_file_hdr:
    db `HTTP/1.1 200 OK\r\n`
    db `Content-Type: %s\r\n`
    db `Content-Length: %zu\r\n`
    db `X-Content-Type-Options: nosniff\r\n`
    db `X-Frame-Options: DENY\r\n`
    db `Connection: close\r\n`
    db `\r\n`, 0

global http_403_response
http_403_response:
    db `HTTP/1.1 403 Forbidden\r\n`
    db `Content-Type: application/json\r\n`
    db `Content-Length: 21\r\n`
    db `X-Content-Type-Options: nosniff\r\n`
    db `X-Frame-Options: DENY\r\n`
    db `Connection: close\r\n`
    db `\r\n{"error":"Forbidden"}`, 0
global http_403_response_len
http_403_response_len equ $ - http_403_response - 1

global http_404_response
http_404_response:
    db `HTTP/1.1 404 Not Found\r\n`
    db `Content-Type: application/json\r\n`
    db `Content-Length: 21\r\n`
    db `X-Content-Type-Options: nosniff\r\n`
    db `X-Frame-Options: DENY\r\n`
    db `Connection: close\r\n`
    db `\r\n{"error":"Not found"}`, 0
global http_404_response_len
http_404_response_len equ $ - http_404_response - 1

global http_405_response
http_405_response:
    db `HTTP/1.1 405 Method Not Allowed\r\n`
    db `Content-Type: application/json\r\n`
    db `Content-Length: 30\r\n`
    db `X-Content-Type-Options: nosniff\r\n`
    db `X-Frame-Options: DENY\r\n`
    db `Connection: close\r\n`
    db `\r\n{"error":"Method not allowed"}`, 0
global http_405_response_len
http_405_response_len equ $ - http_405_response - 1

global http_400_response
http_400_response:
    db `HTTP/1.1 400 Bad Request\r\n`
    db `Content-Type: application/json\r\n`
    db `Content-Length: 39\r\n`
    db `X-Content-Type-Options: nosniff\r\n`
    db `X-Frame-Options: DENY\r\n`
    db `Connection: close\r\n`
    db `\r\n{"error":"Invalid downsample interval"}`, 0
global http_400_response_len
http_400_response_len equ $ - http_400_response - 1

global http_500_response
http_500_response:
    db `HTTP/1.1 500 Internal Server Error\r\n`
    db `Content-Type: text/plain\r\n`
    db `Content-Length: 21\r\n`
    db `X-Content-Type-Options: nosniff\r\n`
    db `X-Frame-Options: DENY\r\n`
    db `Connection: close\r\n`
    db `\r\nInternal Server Error`, 0
global http_500_response_len
http_500_response_len equ $ - http_500_response - 1

; ── JSON format strings ──────────────────────────────────────────────────────

global fmt_stats_json
fmt_stats_json:
    db `{"implementation":"asm","uptime_ms":%lld,"memory_rss_bytes":%lld,`
    db `"started_at":%lld,"requests_served":%lld,`
    db `"active_connections":%lld,`
    db `"poll_successes":%lld,"poll_failures":%lld}`, 0

global fmt_config_json
fmt_config_json:
    db `{"pollIntervalMs":%d,"downsampleBuckets":{`, 0

global fmt_config_devices_hdr
fmt_config_devices_hdr:
    db `},"devices":[`, 0

global fmt_config_device
fmt_config_device:
    db `{"ip":"%s","label":"%s"}`, 0

global fmt_health_entry
fmt_health_entry:
    db `{"ip":"%s","label":"%s","status":"%s",`
    db `"lastSuccess":%s,"lastError":%s,`
    db `"lastErrorMessage":%s,`
    db `"consecutiveFailures":%d}`, 0

; ── HTTP client format strings ───────────────────────────────────────────────

global fmt_http_get
fmt_http_get:
    db `GET /measures/current HTTP/1.1\r\nHost: %s\r\nConnection: close\r\n\r\n`, 0

; ── Path / config strings ────────────────────────────────────────────────────

global str_public_dir
str_public_dir:
    db `public`, 0

global str_index_html
str_index_html:
    db `index.html`, 0

global str_dotdot
str_dotdot:
    db `..`, 0

global str_null_json
str_null_json:
    db `null`, 0

global str_empty_array
str_empty_array:
    db `[]`, 0

global str_port_env
str_port_env:
    db `PORT`, 0

global str_db_path_env
str_db_path_env:
    db `DB_PATH`, 0

global str_config_path_env
str_config_path_env:
    db `CONFIG_PATH`, 0

global str_default_db
str_default_db:
    db `airgradientz.db`, 0

global str_config_local
str_config_local:
    db `airgradientz.json`, 0

global str_config_parent
str_config_parent:
    db `../airgradientz.json`, 0

; ── Config keys for strstr ───────────────────────────────────────────────────

global str_key_poll_interval
str_key_poll_interval:
    db `"pollIntervalMs"`, 0

global str_key_fetch_timeout
str_key_fetch_timeout:
    db `"fetchTimeoutMs"`, 0

global str_key_max_api_rows
str_key_max_api_rows:
    db `"maxApiRows"`, 0

global str_key_devices
str_key_devices:
    db `"devices"`, 0

global str_key_ip
str_key_ip:
    db `"ip"`, 0

global str_key_label
str_key_label:
    db `"label"`, 0

global str_key_ports
str_key_ports:
    db `"ports"`, 0

global str_key_asm
str_key_asm:
    db `"asm"`, 0

global str_key_downsample_buckets
str_key_downsample_buckets:
    db `"downsampleBuckets"`, 0

; ── AirGradient response keys ────────────────────────────────────────────────

global str_ag_pm01
str_ag_pm01:
    db `"pm01"`, 0

global str_ag_pm02
str_ag_pm02:
    db `"pm02"`, 0

global str_ag_pm10
str_ag_pm10:
    db `"pm10"`, 0

global str_ag_pm02comp
str_ag_pm02comp:
    db `"pm02Compensated"`, 0

global str_ag_rco2
str_ag_rco2:
    db `"rco2"`, 0

global str_ag_atmp
str_ag_atmp:
    db `"atmp"`, 0

global str_ag_atmpcomp
str_ag_atmpcomp:
    db `"atmpCompensated"`, 0

global str_ag_rhum
str_ag_rhum:
    db `"rhum"`, 0

global str_ag_rhumcomp
str_ag_rhumcomp:
    db `"rhumCompensated"`, 0

global str_ag_tvoc
str_ag_tvoc:
    db `"tvocIndex"`, 0

global str_ag_nox
str_ag_nox:
    db `"noxIndex"`, 0

global str_ag_wifi
str_ag_wifi:
    db `"wifi"`, 0

global str_ag_model
str_ag_model:
    db `"model"`, 0

global str_ag_serial
str_ag_serial:
    db `"serialno"`, 0

; ── Content types ────────────────────────────────────────────────────────────

global ct_html
ct_html:
    db `text/html; charset=utf-8`, 0

global ct_css
ct_css:
    db `text/css; charset=utf-8`, 0

global ct_js
ct_js:
    db `application/javascript; charset=utf-8`, 0

global ct_json
ct_json:
    db `application/json; charset=utf-8`, 0

global ct_png
ct_png:
    db `image/png`, 0

global ct_jpeg
ct_jpeg:
    db `image/jpeg`, 0

global ct_svg
ct_svg:
    db `image/svg+xml`, 0

global ct_ico
ct_ico:
    db `image/x-icon`, 0

global ct_octet
ct_octet:
    db `application/octet-stream`, 0

; ── File extensions ──────────────────────────────────────────────────────────

global ext_html
ext_html:
    db `.html`, 0

global ext_css
ext_css:
    db `.css`, 0

global ext_js
ext_js:
    db `.js`, 0

global ext_json
ext_json:
    db `.json`, 0

global ext_png
ext_png:
    db `.png`, 0

global ext_jpg
ext_jpg:
    db `.jpg`, 0

global ext_jpeg
ext_jpeg:
    db `.jpeg`, 0

global ext_svg
ext_svg:
    db `.svg`, 0

global ext_ico
ext_ico:
    db `.ico`, 0

; ── Route paths ──────────────────────────────────────────────────────────────

global route_api_readings_latest
route_api_readings_latest:
    db `/api/readings/latest`, 0

global route_api_readings_count
route_api_readings_count:
    db `/api/readings/count`, 0

global route_api_readings
route_api_readings:
    db `/api/readings`, 0

global route_api_devices
route_api_devices:
    db `/api/devices`, 0

global route_api_health
route_api_health:
    db `/api/health`, 0

global route_api_config
route_api_config:
    db `/api/config`, 0

global route_api_stats
route_api_stats:
    db `/api/stats`, 0

global str_get
str_get:
    db `GET`, 0

global str_slash
str_slash:
    db `/`, 0

; ── Log format strings ───────────────────────────────────────────────────────

global log_server_start
log_server_start:
    db `[server] Listening on port %d`, 10, 0

global log_server_stop
log_server_stop:
    db `[server] Shutting down`, 10, 0

global log_config_loaded
log_config_loaded:
    db `[config] Loaded %d devices, poll=%dms, timeout=%dms, maxRows=%d`, 10, 0

global log_db_opened
log_db_opened:
    db `[db] Opened %s`, 10, 0

global log_poll_success
log_poll_success:
    db `[poller] Got reading from %s (%s)`, 10, 0

global log_poll_error
log_poll_error:
    db `[poller] Error polling %s (%s): %s`, 10, 0

global log_signal
log_signal:
    db `[server] Received signal %d, shutting down`, 10, 0

; ── Timestamp logging ────────────────────────────────────────────────────────

global log_ts_datefmt
log_ts_datefmt:
    db `%Y-%m-%d %H:%M:%S`, 0

global log_ts_prefix
log_ts_prefix:
    db `[%s] `, 0

; ── Query param keys ─────────────────────────────────────────────────────────

global str_param_from
str_param_from:
    db `from`, 0

global str_param_to
str_param_to:
    db `to`, 0

global str_param_device
str_param_device:
    db `device`, 0

global str_all
str_all:
    db `all`, 0

global str_param_limit
str_param_limit:
    db `limit`, 0

global str_param_downsample
str_param_downsample:
    db `downsample`, 0

; Downsample bucket format for config response
global fmt_ds_bucket_first
fmt_ds_bucket_first:
    db `"%s":%lld`, 0
global fmt_ds_bucket_rest
fmt_ds_bucket_rest:
    db `,"%s":%lld`, 0

; ── Misc format strings ──────────────────────────────────────────────────────

global fmt_quoted_str
fmt_quoted_str:
    db `"%s"`, 0

global fmt_int
fmt_int:
    db `%d`, 0

global fmt_lld
fmt_lld:
    db `%lld`, 0

global fmt_str
fmt_str:
    db `%s`, 0

; ── Health status strings ────────────────────────────────────────────────────

global str_status_ok
str_status_ok:
    db `ok`, 0

global str_status_error
str_status_error:
    db `error`, 0

global str_status_unknown
str_status_unknown:
    db `unknown`, 0

; ── Device type strings for determining indoor/outdoor ───────────────────────

global str_indoor
str_indoor:
    db `indoor`, 0

global str_outdoor
str_outdoor:
    db `outdoor`, 0

; ── /proc/self/statm for RSS ─────────────────────────────────────────────────

global str_proc_statm
str_proc_statm:
    db `/proc/self/statm`, 0

global fmt_sscanf_statm
fmt_sscanf_statm:
    db `%*d %ld`, 0

; ── snprintf helpers ─────────────────────────────────────────────────────────

global fmt_path_join
fmt_path_join:
    db `%s/%s`, 0

global fmt_colon_port
fmt_colon_port:
    db `%d`, 0

; ── Device poll response parsing ─────────────────────────────────────────────

global fmt_extract_int
fmt_extract_int:
    db `%d`, 0

global fmt_extract_float
fmt_extract_float:
    db `%lf`, 0

; Sizes for BSS
global MAX_CONNS
MAX_CONNS equ 4096

global CONN_BUF_SIZE
CONN_BUF_SIZE equ 8192

global MAX_DEVICES
MAX_DEVICES equ 8

global MAX_RESPONSE_SIZE
MAX_RESPONSE_SIZE equ 2097152  ; 2MB

global HEALTH_ENTRY_SIZE
HEALTH_ENTRY_SIZE equ 296

global MAX_CONFIG_SIZE
MAX_CONFIG_SIZE equ 8192

global MAX_QUERY_SIZE
MAX_QUERY_SIZE equ 4096

global STALE_CONN_TIMEOUT
STALE_CONN_TIMEOUT equ 30

; Connection phases
global CONN_FREE
CONN_FREE equ 0
global CONN_READING
CONN_READING equ 1
global CONN_WRITING
CONN_WRITING equ 2

; Epoll constants
global EPOLLIN
EPOLLIN equ 0x001
global EPOLLOUT
EPOLLOUT equ 0x004
global EPOLLET
EPOLLET equ 0x80000000
global EPOLL_CTL_ADD
EPOLL_CTL_ADD equ 1
global EPOLL_CTL_MOD
EPOLL_CTL_MOD equ 3
global EPOLL_CTL_DEL
EPOLL_CTL_DEL equ 2

; Socket constants
global AF_INET
AF_INET equ 2
global SOCK_STREAM
SOCK_STREAM equ 1
global SOL_SOCKET
SOL_SOCKET equ 1
global SO_REUSEADDR
SO_REUSEADDR equ 2
global SO_REUSEPORT
SO_REUSEPORT equ 15
global IPPROTO_TCP
IPPROTO_TCP equ 6

; Fcntl
global F_GETFL
F_GETFL equ 3
global F_SETFL
F_SETFL equ 4
global O_NONBLOCK
O_NONBLOCK equ 2048
global O_RDONLY
O_RDONLY equ 0

; Signals
global SIGINT
SIGINT equ 2
global SIGTERM
SIGTERM equ 15
global SIGPIPE
SIGPIPE equ 13
global SIG_IGN
SIG_IGN equ 1

; Errno
global EAGAIN
EAGAIN equ 11
