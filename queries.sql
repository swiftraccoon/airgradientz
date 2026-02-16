-- queries.sql: Named queries for all implementations.
-- Canonical column lists and query patterns. Each implementation
-- adapts placeholder style (:name -> ?, ?1, @name) as needed.
--
-- Format: "-- name: <query_name>" delimiter, then SQL.
-- Implementations parse this file to extract queries by name.
-- Brackets [...] denote optional clauses built dynamically.

-- name: insert_reading
INSERT INTO readings (
    timestamp, device_id, device_type, device_ip,
    pm01, pm02, pm10, pm02_compensated,
    rco2, atmp, atmp_compensated, rhum, rhum_compensated,
    tvoc_index, nox_index, wifi, raw_json
) VALUES (
    :timestamp, :device_id, :device_type, :device_ip,
    :pm01, :pm02, :pm10, :pm02_compensated,
    :rco2, :atmp, :atmp_compensated, :rhum, :rhum_compensated,
    :tvoc_index, :nox_index, :wifi, :raw_json
);

-- name: reading_columns
-- Not a query. Canonical column list for SELECT queries.
-- Implementations interpolate this into select_readings and select_latest.
id, timestamp, device_id, device_type, device_ip,
pm01, pm02, pm10, pm02_compensated, rco2,
atmp, atmp_compensated, rhum, rhum_compensated,
tvoc_index, nox_index, wifi

-- name: select_readings
-- Template: implementations build WHERE clause dynamically.
-- Required: timestamp >= :from AND timestamp <= :to
-- Optional: device_id = :device (prepended to WHERE)
-- Optional: LIMIT :limit (appended)
SELECT {reading_columns}
FROM readings
WHERE [device_id = :device AND] timestamp >= :from AND timestamp <= :to
ORDER BY timestamp ASC
[LIMIT :limit];

-- name: select_latest
SELECT r.id, r.timestamp, r.device_id, r.device_type, r.device_ip,
    r.pm01, r.pm02, r.pm10, r.pm02_compensated, r.rco2,
    r.atmp, r.atmp_compensated, r.rhum, r.rhum_compensated,
    r.tvoc_index, r.nox_index, r.wifi
FROM readings r
INNER JOIN (
    SELECT device_id, MAX(id) AS max_id
    FROM readings
    GROUP BY device_id
) latest ON r.id = latest.max_id;

-- name: select_devices
SELECT device_id, device_type, device_ip,
       MAX(timestamp) AS last_seen,
       COUNT(*) AS reading_count
FROM readings
GROUP BY device_id
ORDER BY device_type;

-- name: count_readings
SELECT COUNT(*) FROM readings;
