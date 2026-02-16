#!/usr/bin/env bash
# Test suite for AirGradientz x86_64 assembly implementation.
# Integration tests using curl against the running server.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
TEST_DIR="$(mktemp -d)"
PORT=13018  # Use non-standard port to avoid conflicts
BASE="http://localhost:${PORT}"
DB_PATH="${TEST_DIR}/test.db"

# Track results
PASS=0
FAIL=0
ERRORS=()

# --- Test framework ---

assert_eq() {
    local label="$1" expected="$2" actual="$3"
    if [[ "${expected}" == "${actual}" ]]; then
        PASS=$(( PASS + 1 ))
    else
        FAIL=$(( FAIL + 1 ))
        ERRORS+=("FAIL: ${label}: expected '${expected}', got '${actual}'")
        echo "  FAIL: ${label}" >&2
    fi
}

assert_contains() {
    local label="$1" haystack="$2" needle="$3"
    if [[ "${haystack}" == *"${needle}"* ]]; then
        PASS=$(( PASS + 1 ))
    else
        FAIL=$(( FAIL + 1 ))
        ERRORS+=("FAIL: ${label}: output does not contain '${needle}'")
        echo "  FAIL: ${label}" >&2
    fi
}

assert_not_contains() {
    local label="$1" haystack="$2" needle="$3"
    if [[ "${haystack}" != *"${needle}"* ]]; then
        PASS=$(( PASS + 1 ))
    else
        FAIL=$(( FAIL + 1 ))
        ERRORS+=("FAIL: ${label}: output should not contain '${needle}'")
        echo "  FAIL: ${label}" >&2
    fi
}

assert_json_field() {
    local label="$1" json="$2" field="$3" expected="$4"
    local actual
    actual=$(jq -r "${field}" <<< "${json}" 2>/dev/null) || actual="(jq error)"
    assert_eq "${label}" "${expected}" "${actual}"
}

assert_json_length() {
    local label="$1" json="$2" expected="$3"
    local actual
    actual=$(jq 'length' <<< "${json}" 2>/dev/null) || actual="(jq error)"
    assert_eq "${label}" "${expected}" "${actual}"
}

assert_json_not_null() {
    local label="$1" json="$2" field="$3"
    local actual
    actual=$(jq -r "${field}" <<< "${json}" 2>/dev/null) || actual="(jq error)"
    if [[ "${actual}" != "null" ]] && [[ -n "${actual}" ]]; then
        PASS=$(( PASS + 1 ))
    else
        FAIL=$(( FAIL + 1 ))
        ERRORS+=("FAIL: ${label}: ${field} should not be null")
        echo "  FAIL: ${label}" >&2
    fi
}

assert_http_status() {
    local label="$1" url="$2" method="$3" expected="$4"
    local code
    code=$(curl -s -o /dev/null -w "%{http_code}" -X "${method}" "${url}" 2>/dev/null) || code="000"
    assert_eq "${label}" "${expected}" "${code}"
}

assert_header() {
    local label="$1" url="$2" header="$3" expected="$4"
    local actual
    actual=$(curl -si "${url}" 2>/dev/null | grep -i "^${header}:" | sed 's/^[^:]*: //' | tr -d '\r\n')
    assert_contains "${label}" "${actual}" "${expected}"
}

# --- Helpers ---

now_ms() {
    date +%s%3N
}

insert_reading() {
    local device_id="$1" device_type="$2" device_ip="$3"
    local pm02="${4:-null}" rco2="${5:-null}" atmp="${6:-null}"
    local ts
    ts=$(now_ms)
    sqlite3 "${DB_PATH}" "INSERT INTO readings (
        timestamp, device_id, device_type, device_ip,
        pm01, pm02, pm10, pm02_compensated,
        rco2, atmp, atmp_compensated, rhum, rhum_compensated,
        tvoc_index, nox_index, wifi, raw_json
    ) VALUES (
        ${ts}, '${device_id}', '${device_type}', '${device_ip}',
        null, ${pm02}, null, null,
        ${rco2}, ${atmp}, null, null, null,
        null, null, null, '{}'
    );"
}

# --- Setup ---

cleanup() {
    if [[ -n "${SERVER_PID:-}" ]]; then
        kill "${SERVER_PID}" 2>/dev/null || true
        wait "${SERVER_PID}" 2>/dev/null || true
    fi
    rm -rf "${TEST_DIR}"
}
trap cleanup EXIT

# Write minimal config for testing
cat > "${TEST_DIR}/airgradientz.json" <<'CONF'
{
    "devices": [
        {"ip": "192.168.1.100", "label": "test-device"}
    ],
    "pollIntervalMs": 600000,
    "fetchTimeoutMs": 1000,
    "maxApiRows": 10000
}
CONF

# Set up public dir symlink
ln -sf "${SCRIPT_DIR}/public" "${TEST_DIR}/public"

# Start server
PORT="${PORT}" DB_PATH="${DB_PATH}" CONFIG_PATH="${TEST_DIR}/airgradientz.json" \
    "${SCRIPT_DIR}/airgradientz" 2>"${TEST_DIR}/stderr.log" &
SERVER_PID=$!
sleep 2

if ! kill -0 "${SERVER_PID}" 2>/dev/null; then
    echo "FAIL: Server died on startup"
    cat "${TEST_DIR}/stderr.log"
    exit 1
fi

echo "Running asm tests..."
echo ""

# Load shared test fixtures
FIXTURE_FILE="../test-fixtures.json"
if [[ ! -f "${FIXTURE_FILE}" ]]; then
    FIXTURE_FILE="../../test-fixtures.json"
fi
INDOOR_SERIAL=$(jq -r '.indoorFull.serialno' "${FIXTURE_FILE}")
INDOOR_PM02=$(jq -r '.indoorFull.pm02' "${FIXTURE_FILE}")
INDOOR_RCO2=$(jq -r '.indoorFull.rco2' "${FIXTURE_FILE}")
INDOOR_ATMP=$(jq -r '.indoorFull.atmp' "${FIXTURE_FILE}")
OUTDOOR_SERIAL=$(jq -r '.outdoorFull.serialno' "${FIXTURE_FILE}")
OUTDOOR_PM02=$(jq -r '.outdoorFull.pm02' "${FIXTURE_FILE}")
OUTDOOR_RCO2=$(jq -r '.outdoorFull.rco2' "${FIXTURE_FILE}")
OUTDOOR_ATMP=$(jq -r '.outdoorFull.atmp' "${FIXTURE_FILE}")

# ═══════════════════════════════════════════════════════════════════
# API STATS
# ═══════════════════════════════════════════════════════════════════

echo "--- Stats endpoint ---"

body=$(curl -sf "${BASE}/api/stats")
assert_json_field "stats implementation" "${body}" '.implementation' "asm"
assert_json_not_null "stats uptime_ms" "${body}" '.uptime_ms'
assert_json_not_null "stats memory_rss_bytes" "${body}" '.memory_rss_bytes'
assert_json_not_null "stats started_at" "${body}" '.started_at'
assert_json_field "stats requests_served" "${body}" '.requests_served' "0"
assert_json_field "stats poll_failures" "${body}" '.poll_failures' "0"

echo ""

# ═══════════════════════════════════════════════════════════════════
# API CONFIG
# ═══════════════════════════════════════════════════════════════════

echo "--- Config endpoint ---"

body=$(curl -sf "${BASE}/api/config")
assert_json_field "config pollIntervalMs" "${body}" '.pollIntervalMs' "600000"
assert_json_field "config fetchTimeoutMs" "${body}" '.fetchTimeoutMs' "1000"
assert_json_field "config maxApiRows" "${body}" '.maxApiRows' "10000"
config_devices=$(jq '.devices | length' <<< "${body}")
assert_eq "config has 1 device" "1" "${config_devices}"
assert_json_field "config device ip" "${body}" '.devices[0].ip' "192.168.1.100"
assert_json_field "config device label" "${body}" '.devices[0].label' "test-device"

echo ""

# ═══════════════════════════════════════════════════════════════════
# EMPTY DATABASE QUERIES
# ═══════════════════════════════════════════════════════════════════

echo "--- Empty database ---"

body=$(curl -sf "${BASE}/api/readings")
assert_json_length "readings empty" "${body}" "0"

body=$(curl -sf "${BASE}/api/readings/latest")
assert_json_length "latest empty" "${body}" "0"

body=$(curl -sf "${BASE}/api/devices")
assert_json_length "devices empty" "${body}" "0"

echo ""

# ═══════════════════════════════════════════════════════════════════
# INSERT TEST DATA AND QUERY
# ═══════════════════════════════════════════════════════════════════

echo "--- Readings with data ---"

insert_reading "${INDOOR_SERIAL}" "indoor" "192.168.1.1" "${INDOOR_PM02}" "${INDOOR_RCO2}" "${INDOOR_ATMP}"
insert_reading "${INDOOR_SERIAL}" "indoor" "192.168.1.1" 15 460 22.8
insert_reading "${OUTDOOR_SERIAL}" "outdoor" "192.168.1.2" "${OUTDOOR_PM02}" "${OUTDOOR_RCO2}" "${OUTDOOR_ATMP}"
sleep 0.1

body=$(curl -sf "${BASE}/api/readings")
count=$(jq 'length' <<< "${body}")
assert_eq "readings returns 3" "3" "${count}"

# Readings are ORDER BY timestamp DESC, so last inserted is first
assert_json_field "reading has device_id" "${body}" '.[0].device_id' "${OUTDOOR_SERIAL}"
assert_json_not_null "reading has timestamp" "${body}" '.[0].timestamp'
assert_json_not_null "reading has id" "${body}" '.[0].id'

echo ""

# ═══════════════════════════════════════════════════════════════════
# QUERY PARAMETERS
# ═══════════════════════════════════════════════════════════════════

echo "--- Query parameters ---"

# Device filter
body=$(curl -sf "${BASE}/api/readings?device=${INDOOR_SERIAL}")
count=$(jq 'length' <<< "${body}")
assert_eq "filter indoor device" "2" "${count}"

body=$(curl -sf "${BASE}/api/readings?device=${OUTDOOR_SERIAL}")
count=$(jq 'length' <<< "${body}")
assert_eq "filter outdoor device" "1" "${count}"

body=$(curl -sf "${BASE}/api/readings?device=nonexistent")
assert_json_length "filter nonexistent device" "${body}" "0"

# Limit
body=$(curl -sf "${BASE}/api/readings?limit=1")
assert_json_length "limit=1" "${body}" "1"

body=$(curl -sf "${BASE}/api/readings?limit=2")
assert_json_length "limit=2" "${body}" "2"

# Time range
far_future=$(($(now_ms) + 86400000))
body=$(curl -sf "${BASE}/api/readings?from=${far_future}")
assert_json_length "from=future returns 0" "${body}" "0"

echo ""

# ═══════════════════════════════════════════════════════════════════
# LATEST AND DEVICES
# ═══════════════════════════════════════════════════════════════════

echo "--- Latest and devices ---"

body=$(curl -sf "${BASE}/api/readings/latest")
count=$(jq 'length' <<< "${body}")
assert_eq "latest returns 2 devices" "2" "${count}"

body=$(curl -sf "${BASE}/api/devices")
count=$(jq 'length' <<< "${body}")
assert_eq "devices returns 2" "2" "${count}"

# Check device fields
assert_json_not_null "device has device_id" "${body}" '.[0].device_id'
assert_json_not_null "device has reading_count" "${body}" '.[0].reading_count'
assert_json_not_null "device has first_seen" "${body}" '.[0].first_seen'
assert_json_not_null "device has last_seen" "${body}" '.[0].last_seen'

echo ""

# ═══════════════════════════════════════════════════════════════════
# HEALTH ENDPOINT
# ═══════════════════════════════════════════════════════════════════

echo "--- Health endpoint ---"

body=$(curl -sf "${BASE}/api/health")
assert_json_length "health returns 1 device" "${body}" "1"
assert_json_field "health device ip" "${body}" '.[0].ip' "192.168.1.100"
assert_json_field "health device label" "${body}" '.[0].label' "test-device"
assert_json_not_null "health has status" "${body}" '.[0].status'
assert_json_not_null "health has consecutiveFailures" "${body}" '.[0].consecutiveFailures'

echo ""

# ═══════════════════════════════════════════════════════════════════
# STATIC FILE SERVING
# ═══════════════════════════════════════════════════════════════════

echo "--- Static files ---"

# Index page
body=$(curl -sf "${BASE}/")
assert_contains "index.html served" "${body}" "<html"

# CSS file
body=$(curl -sf "${BASE}/style.css")
assert_contains "style.css served" "${body}" "box-sizing"

# Content-Type headers (use -si for GET with headers, not -sI which sends HEAD)
ct=$(curl -si "${BASE}/" 2>/dev/null | grep -i "^Content-Type:" | tr -d '\r\n')
assert_contains "index content-type" "${ct}" "text/html"

ct=$(curl -si "${BASE}/style.css" 2>/dev/null | grep -i "^Content-Type:" | tr -d '\r\n')
assert_contains "css content-type" "${ct}" "text/css"

echo ""

# ═══════════════════════════════════════════════════════════════════
# ERROR HANDLING
# ═══════════════════════════════════════════════════════════════════

echo "--- Error handling ---"

assert_http_status "404 nonexistent path" "${BASE}/nonexistent" "GET" "404"
assert_http_status "405 POST method" "${BASE}/api/stats" "POST" "405"
assert_http_status "405 PUT method" "${BASE}/api/readings" "PUT" "405"
assert_http_status "404 unknown API" "${BASE}/api/unknown" "GET" "404"

echo ""

# ═══════════════════════════════════════════════════════════════════
# SECURITY
# ═══════════════════════════════════════════════════════════════════

echo "--- Security ---"

assert_http_status "path traversal .." "${BASE}/../airgradientz.json" "GET" "404"
assert_http_status "path traversal %2e%2e" "${BASE}/%2e%2e/airgradientz.json" "GET" "404"
assert_http_status "path traversal encoded" "${BASE}/%2e%2e/%2e%2e/etc/passwd" "GET" "404"

# Security headers on API response
headers=$(curl -si "${BASE}/api/stats" 2>/dev/null)
xcto=$(echo "${headers}" | grep -i "x-content-type-options" | tr -d '\r\n')
assert_contains "X-Content-Type-Options present" "${xcto}" "nosniff"

echo ""

# ═══════════════════════════════════════════════════════════════════
# RESPONSE HEADERS
# ═══════════════════════════════════════════════════════════════════

echo "--- Response headers ---"

headers=$(curl -si "${BASE}/api/stats" 2>/dev/null)
assert_contains "has Content-Length" "${headers}" "Content-Length"
ct=$(echo "${headers}" | grep -i "^Content-Type:" | tr -d '\r\n')
assert_contains "API content-type is json" "${ct}" "application/json"

echo ""

# ═══════════════════════════════════════════════════════════════════
# ADDITIONAL DATA TESTS
# ═══════════════════════════════════════════════════════════════════

echo "--- Additional data tests ---"

# Insert more data for limit testing
for i in $(seq 1 5); do
    insert_reading "limitdev" "indoor" "192.168.1.3" "${i}" 400 20
done
sleep 0.1

body=$(curl -sf "${BASE}/api/readings?device=limitdev&limit=3")
assert_json_length "limit with device filter" "${body}" "3"

body=$(curl -sf "${BASE}/api/readings?device=limitdev")
assert_json_length "limitdev has 5 readings" "${body}" "5"

# Latest should now show 3 unique devices
body=$(curl -sf "${BASE}/api/readings/latest")
count=$(jq 'length' <<< "${body}")
assert_eq "latest now returns 3" "3" "${count}"

# Stats should show request count > 0
body=$(curl -sf "${BASE}/api/stats")
requests=$(jq '.requests_served' <<< "${body}")
if (( requests > 0 )); then
    PASS=$(( PASS + 1 ))
else
    FAIL=$(( FAIL + 1 ))
    ERRORS+=("FAIL: requests_served should be > 0, got ${requests}")
    echo "  FAIL: requests_served > 0" >&2
fi

echo ""

# ═══════════════════════════════════════════════════════════════════
# SQL DRIFT VALIDATION (source-level checks against queries.sql)
# ═══════════════════════════════════════════════════════════════════

echo "--- SQL drift tests ---"

QUERIES_FILE="${SCRIPT_DIR}/../queries.sql"
ASM_STRINGS="${SCRIPT_DIR}/src/strings.asm"

# Extract a named query from queries.sql: collects all non-comment, non-empty
# lines between "-- name: <name>" and the next "-- name:" (or EOF), strips
# leading/trailing whitespace, joins into one line, and removes trailing ";".
extract_query() {
    local name="$1" file="$2"
    local in_block=false
    local result=""
    while IFS= read -r line; do
        if [[ "${line}" == "-- name: ${name}" ]]; then
            in_block=true
            continue
        elif [[ "${line}" == "-- name: "* ]]; then
            if ${in_block}; then break; fi
            continue
        fi
        if ${in_block}; then
            [[ "${line}" == "--"* ]] && continue
            local trimmed
            trimmed=$(echo "${line}" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
            [[ -z "${trimmed}" ]] && continue
            if [[ -n "${result}" ]]; then
                result="${result} ${trimmed}"
            else
                result="${trimmed}"
            fi
        fi
    done < "${file}"
    result="${result%%;}"
    printf '%s' "${result}"
}

# Concatenate all db lines from strings.asm for a given label into one string.
# NASM syntax: db `...`, or db `...`, 0
extract_asm_sql() {
    local label="$1" file="$2"
    local in_block=false
    local result=""
    while IFS= read -r line; do
        if [[ "${line}" =~ ^"${label}": ]]; then
            in_block=true
            continue
        fi
        if ${in_block}; then
            # Stop at next label or non-db line (but allow blank lines between db lines)
            if [[ "${line}" =~ ^[a-zA-Z_\.] ]] && [[ "${line}" != *"db "* ]]; then
                break
            fi
            if [[ "${line}" == *"db "* ]]; then
                # Extract content between backticks
                local content
                content=$(echo "${line}" | sed "s/.*db \`//;s/\`.*//")
                result="${result}${content}"
            fi
        fi
    done < "${file}"
    printf '%s' "${result}"
}

# --- Validate INSERT column list ---

canonical_insert=$(extract_query "insert_reading" "${QUERIES_FILE}")
# Extract just the column names from canonical INSERT (between "(" and ")")
canonical_insert_cols=$(echo "${canonical_insert}" | sed 's/.*INSERT INTO readings (//;s/).*//' | tr -s '[:space:],' ' ' | sed 's/^ //;s/ $//')

asm_insert=$(extract_asm_sql "sql_insert_reading" "${ASM_STRINGS}")
asm_insert_cols=$(echo "${asm_insert}" | sed 's/.*INSERT INTO readings (//;s/).*//' | tr -s '[:space:],' ' ' | sed 's/^ //;s/ $//')

assert_eq "insert columns match queries.sql" "${canonical_insert_cols}" "${asm_insert_cols}"

# --- Validate reading columns in json_object keys ---

canonical_cols=$(extract_query "reading_columns" "${QUERIES_FILE}")
# Normalize to space-separated, no commas
canonical_col_list=$(echo "${canonical_cols}" | tr ',' '\n' | sed 's/^[[:space:]]*//;s/[[:space:]]*$//' | sort)

# Extract json_object keys from sql_readings_json (the column names used in SELECT)
asm_readings=$(extract_asm_sql "sql_readings_json" "${ASM_STRINGS}")
# json_object uses 'key',column pairs — extract the column names (unquoted ones)
asm_reading_cols=$(echo "${asm_readings}" | grep -oP "'[^']+'" | tr -d "'" | sort)

# Compare: canonical should have id + the 16 data columns; ASM json_object has the same set
for col in ${canonical_col_list}; do
    assert_contains "readings json_object has column '${col}'" "${asm_reading_cols}" "${col}"
done

# --- Validate latest query uses JOIN with GROUP BY device_id ---

asm_latest=$(extract_asm_sql "sql_latest_json" "${ASM_STRINGS}")
assert_contains "latest query INNER JOIN" "${asm_latest}" "INNER JOIN"
assert_contains "latest query GROUP BY device_id" "${asm_latest}" "GROUP BY device_id"

# --- Validate devices query structure ---

asm_devices=$(extract_asm_sql "sql_devices_json" "${ASM_STRINGS}")
assert_contains "devices query GROUP BY device_id" "${asm_devices}" "GROUP BY device_id"
assert_contains "devices query COUNT(*)" "${asm_devices}" "COUNT(*)"
assert_contains "devices query MIN(timestamp)" "${asm_devices}" "MIN(timestamp)"
assert_contains "devices query MAX(timestamp)" "${asm_devices}" "MAX(timestamp)"

# --- Validate count query ---

canonical_count=$(extract_query "count_readings" "${QUERIES_FILE}")
asm_count=$(extract_asm_sql "sql_count_readings" "${ASM_STRINGS}")
# Strip trailing semicolons for comparison
asm_count_norm="${asm_count%%;}"
assert_eq "count query matches queries.sql" "${canonical_count}" "${asm_count_norm}"

# --- Validate devices query has same columns as queries.sql ---

canonical_devices=$(extract_query "select_devices" "${QUERIES_FILE}")
# Check key columns from canonical: device_id, device_type, device_ip, last_seen, reading_count
assert_contains "devices query has device_id" "${asm_devices}" "device_id"
assert_contains "devices query has device_type" "${asm_devices}" "device_type"
assert_contains "devices query has device_ip" "${asm_devices}" "device_ip"

echo ""

# ═══════════════════════════════════════════════════════════════════
# SUMMARY
# ═══════════════════════════════════════════════════════════════════

echo "═══════════════════════════════════════════"
echo "Results: ${PASS} passed, ${FAIL} failed"
echo "═══════════════════════════════════════════"

if [[ ${#ERRORS[@]} -gt 0 ]]; then
    echo ""
    echo "Failures:"
    for err in "${ERRORS[@]}"; do
        echo "  ${err}"
    done
fi

if [[ ${FAIL} -gt 0 ]]; then
    exit 1
fi
