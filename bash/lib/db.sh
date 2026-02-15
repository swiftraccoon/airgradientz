#!/usr/bin/env bash
# SQLite query functions for AirGradientz bash handler.
# shellcheck disable=SC2154  # AGTZ_DB_PATH set by config.sh/exported env

readonly QUERY_COLS='id, timestamp, device_id, device_type, device_ip,
    pm01, pm02, pm10, pm02_compensated, rco2,
    atmp, atmp_compensated, rhum, rhum_compensated,
    tvoc_index, nox_index, wifi'

db_exec() {
    sqlite3 -bail "${AGTZ_DB_PATH}" "$@"
}

# Normalize JSON numbers: SQLite REAL columns output 12.0 for integer values;
# other implementations output 12. Pipe through jq to match.
normalize_json() {
    jq -c 'walk(if type == "number" and . == floor then floor else . end)'
}

init_db() {
    local schema
    schema=$(< "../schema.sql")
    db_exec >/dev/null <<EOF
PRAGMA journal_mode=WAL;
PRAGMA busy_timeout=5000;
PRAGMA foreign_keys=ON;
${schema}
EOF
}

# Output readings as a JSON array using sqlite3 json_group_array.
query_readings() {
    local device="$1" from_ts="$2" to_ts="$3" limit="$4"

    local where_clause="timestamp >= ${from_ts} AND timestamp <= ${to_ts}"
    if [[ -n "${device}" ]] && [[ "${device}" != "all" ]]; then
        # Validate device_id: alphanumeric, dash, underscore only
        if [[ ! "${device}" =~ ^[a-zA-Z0-9_-]+$ ]]; then
            printf '[]'
            return
        fi
        where_clause="device_id = '${device}' AND ${where_clause}"
    fi

    local sql="SELECT json_group_array(json_object(
        'id', id,
        'timestamp', timestamp,
        'device_id', device_id,
        'device_type', device_type,
        'device_ip', device_ip,
        'pm01', pm01,
        'pm02', pm02,
        'pm10', pm10,
        'pm02_compensated', pm02_compensated,
        'rco2', rco2,
        'atmp', atmp,
        'atmp_compensated', atmp_compensated,
        'rhum', rhum,
        'rhum_compensated', rhum_compensated,
        'tvoc_index', tvoc_index,
        'nox_index', nox_index,
        'wifi', wifi
    )) FROM (
        SELECT ${QUERY_COLS} FROM readings
        WHERE ${where_clause}
        ORDER BY timestamp ASC
        LIMIT ${limit}
    )"

    local result
    result=$(db_exec "${sql}" 2>/dev/null) || { printf '[]'; return; }
    printf '%s' "${result}" | normalize_json
}

get_latest_readings() {
    local sql="SELECT json_group_array(json_object(
        'id', r.id,
        'timestamp', r.timestamp,
        'device_id', r.device_id,
        'device_type', r.device_type,
        'device_ip', r.device_ip,
        'pm01', r.pm01,
        'pm02', r.pm02,
        'pm10', r.pm10,
        'pm02_compensated', r.pm02_compensated,
        'rco2', r.rco2,
        'atmp', r.atmp,
        'atmp_compensated', r.atmp_compensated,
        'rhum', r.rhum,
        'rhum_compensated', r.rhum_compensated,
        'tvoc_index', r.tvoc_index,
        'nox_index', r.nox_index,
        'wifi', r.wifi
    )) FROM readings r
    INNER JOIN (
        SELECT device_id, MAX(id) as max_id
        FROM readings GROUP BY device_id
    ) latest ON r.id = latest.max_id"

    local result
    result=$(db_exec "${sql}" 2>/dev/null) || { printf '[]'; return; }
    printf '%s' "${result}" | normalize_json
}

get_devices() {
    local sql="SELECT json_group_array(json_object(
        'device_id', device_id,
        'device_type', device_type,
        'device_ip', device_ip,
        'last_seen', last_seen,
        'reading_count', reading_count
    )) FROM (
        SELECT device_id, device_type, device_ip,
            MAX(timestamp) as last_seen, COUNT(*) as reading_count
        FROM readings GROUP BY device_id ORDER BY device_type
    )"

    local result
    result=$(db_exec "${sql}" 2>/dev/null) || { printf '[]'; return; }
    printf '%s' "${result}" | normalize_json
}

get_readings_count() {
    local result
    result=$(db_exec "SELECT COUNT(*) FROM readings" 2>/dev/null) || { printf '0'; return; }
    printf '%s' "${result}"
}

get_db_size() {
    local size
    size=$(stat -c '%s' "${AGTZ_DB_PATH}" 2>/dev/null) || { printf '0'; return; }
    printf '%s' "${size}"
}

db_checkpoint() {
    db_exec "PRAGMA wal_checkpoint(TRUNCATE);" >/dev/null 2>/dev/null || true
}
