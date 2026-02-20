# AirGradientz API Specification

All implementations expose identical JSON endpoints on their respective ports. All endpoints accept only GET requests (405 for other methods) and return `Content-Type: application/json`. All timestamps are Unix epoch milliseconds.

## GET /api/readings

Historical sensor readings.

### Query Parameters

| Param    | Type    | Default | Description                              |
|----------|---------|---------|------------------------------------------|
| `from`   | integer | now - 24h | Start time, epoch ms (inclusive)       |
| `to`     | integer | now     | End time, epoch ms (inclusive)           |
| `device` | string  | —       | Filter by device_id                      |
| `limit`  | integer | —       | Max rows returned (0, negative, or missing = no explicit limit; always capped by `maxApiRows`) |
| `downsample` | string | —   | Aggregate into time buckets: `5m`,`10m`,`15m`,`30m`,`1h`,`1d`,`1w` |

Unrecognized `downsample` values return 400. When `downsample` is set, the server aggregates on the fly using `GROUP BY (timestamp / bucket_ms), device_id` with `AVG()` for sensor fields and `CAST(AVG(...) AS INTEGER)` for integer fields (rco2, wifi).

### Response

JSON array of reading objects, ordered by `timestamp` ASC.

| Field               | Type              | Description                        |
|---------------------|-------------------|------------------------------------|
| `id`                | integer           | Autoincrement primary key          |
| `timestamp`         | integer           | Server-generated, epoch ms         |
| `device_id`         | string            | Device serial number               |
| `device_type`       | string            | `"indoor"` or `"outdoor"`          |
| `device_ip`         | string            | IPv4 address                       |
| `pm01`              | number \| null    | PM1.0 concentration                |
| `pm02`              | number \| null    | PM2.5 concentration                |
| `pm10`              | number \| null    | PM10 concentration                 |
| `pm02_compensated`  | number \| null    | PM2.5 compensated                  |
| `rco2`              | integer \| null   | CO2 concentration (ppm)            |
| `atmp`              | number \| null    | Temperature                        |
| `atmp_compensated`  | number \| null    | Temperature compensated            |
| `rhum`              | number \| null    | Relative humidity                  |
| `rhum_compensated`  | number \| null    | Relative humidity compensated      |
| `tvoc_index`        | number \| null    | TVOC index                         |
| `nox_index`         | number \| null    | NOx index                          |
| `wifi`              | integer \| null   | WiFi RSSI (dBm)                    |

The `raw_json` column exists in the database but is never included in API responses. Null sensor fields indicate the sensor is not available on that device model or the device just booted.

When `downsample` is set, the `id` field is omitted (no single row identity after aggregation). All other fields have the same shape.

## GET /api/readings/count

Total reading count for a time range.

### Query Parameters

| Param    | Type    | Default | Description                  |
|----------|---------|---------|------------------------------|
| `from`   | integer | 0       | Start time, epoch ms         |
| `to`     | integer | now     | End time, epoch ms           |
| `device` | string  | —       | Optional device_id filter    |

### Response

```json
{"count": 345219}
```

## GET /api/readings/latest

Most recent reading per device. Returns a JSON array of reading objects with the same shape as `/api/readings`. One entry per device, selected by highest `id`.

## GET /api/devices

Device summaries. Returns a JSON array ordered by `device_type`.

| Field           | Type    | Description                |
|-----------------|---------|----------------------------|
| `device_id`     | string  | Device serial number       |
| `device_type`   | string  | `"indoor"` or `"outdoor"`  |
| `device_ip`     | string  | IPv4 address               |
| `last_seen`     | integer | Epoch ms of last reading   |
| `reading_count` | integer | Total readings stored      |

## GET /api/health

Per-device poll health. Returns a JSON array. Note: field names are camelCase, matching the in-memory health model.

| Field                  | Type              | Description                          |
|------------------------|-------------------|--------------------------------------|
| `ip`                   | string            | Device IPv4 address                  |
| `label`                | string            | Device label from config             |
| `status`               | string            | `"ok"`, `"error"`, or `"unknown"`    |
| `lastSuccess`          | integer \| null   | Epoch ms of last successful poll     |
| `lastError`            | integer \| null   | Epoch ms of last failed poll         |
| `lastErrorMessage`     | string \| null    | Error description from last failure  |
| `consecutiveFailures`  | integer           | Consecutive failed polls             |

## GET /api/config

Active configuration. Returns a JSON object.

| Field            | Type   | Description                                  |
|------------------|--------|----------------------------------------------|
| `pollIntervalMs`      | integer| Polling interval in milliseconds             |
| `downsampleBuckets`   | object | Map of label to milliseconds (e.g. `{"5m": 300000, "1h": 3600000}`) |
| `devices`             | array  | Array of `{ip: string, label: string}` objects|

## GET /api/stats

Runtime introspection. Returns a JSON object.

| Field                | Type    | Description                                      |
|----------------------|---------|--------------------------------------------------|
| `implementation`     | string  | Implementation name (e.g. `"c"`, `"rust"`)       |
| `pid`                | integer | OS process ID                                    |
| `uptime_ms`          | integer | Milliseconds since server start                  |
| `memory_rss_bytes`   | integer | Resident set size in bytes                       |
| `db_size_bytes`      | integer | SQLite database file size in bytes               |
| `readings_count`     | integer | Total readings in database                       |
| `requests_served`    | integer | Total HTTP requests handled                      |
| `active_connections` | integer | Current open connections                         |
| `poll_successes`     | integer | Total successful device polls                    |
| `poll_failures`      | integer | Total failed device polls                        |
| `pool_alloc_count`   | integer | Pool allocator allocations (C-specific, 0 elsewhere)|
| `pool_bytes_used`    | integer | Pool allocator bytes used (C-specific, 0 elsewhere) |
| `started_at`         | integer | Server start time, epoch ms                      |

## Error Responses

All errors return a JSON object with a single `error` field:

```json
{"error": "<message>"}
```

| Status | Meaning                                    |
|--------|--------------------------------------------|
| 400    | Bad request (invalid query parameters)     |
| 403    | Forbidden (path traversal attempt)         |
| 404    | Not found (unknown endpoint or resource)   |
| 405    | Method not allowed (non-GET request)       |
| 500    | Internal server error (DB failure, etc.)   |

## Query Parameter Defaults & Edge Cases

| Param | Edge Case | Behavior |
|-------|-----------|----------|
| `from` | missing or empty | Defaults to `now - 24 hours` |
| `to` | missing or empty | Defaults to current time |
| `limit` | missing | No explicit limit; result capped by `maxApiRows` |
| `limit` | `0` | Treated as no explicit limit; capped by `maxApiRows` |
| `limit` | negative | Treated as no explicit limit; capped by `maxApiRows` |
| `limit` | exceeds `maxApiRows` | Capped to `maxApiRows` |
| `device` | missing or empty | No device filter (returns all devices) |
| `device` | `"all"` | No device filter (returns all devices) |
| `device` | nonexistent serial | Returns empty `[]` (not an error) |
| `from` | greater than `to` | Returns empty `[]` (not an error) |
| `downsample` | unrecognized value | Returns 400 with `{"error": "..."}` |

All `/api/readings` results are ordered by `timestamp ASC`. The `/api/readings/latest` endpoint selects the reading with the highest `id` per device (not highest timestamp).

When `downsample` is set, the `id` field is omitted from response objects. Error responses always have the shape `{"error": "<message>"}` with no extra fields.
