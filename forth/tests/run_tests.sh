#!/usr/bin/env bash
# Test suite for AirGradientz GNU Forth (gforth) implementation.
# Integration tests using curl against the running server.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
TEST_DIR="$(mktemp -d)"
PORT=13020  # Use non-standard port to avoid conflicts
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
    code=$(curl -s -o /dev/null -w "%{http_code}" --path-as-is -X "${method}" "${url}" 2>/dev/null) || code="000"
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
    "ports": {"forth": 3020},
    "devices": [
        {"ip": "192.168.1.100", "label": "test-device"}
    ],
    "pollIntervalMs": 600000,
    "fetchTimeoutMs": 1000,
    "maxApiRows": 10000,
    "downsampleBuckets": {
        "5m": 300000,
        "10m": 600000,
        "15m": 900000,
        "30m": 1800000,
        "1h": 3600000,
        "1d": 86400000,
        "1w": 604800000
    }
}
CONF

# Set up public dir symlink
ln -sf "${SCRIPT_DIR}/public" "${TEST_DIR}/public"

# Start server
PORT="${PORT}" DB_PATH="${DB_PATH}" CONFIG_PATH="${TEST_DIR}/airgradientz.json" \
    gforth "${SCRIPT_DIR}/src/main.fs" 2>"${TEST_DIR}/stderr.log" &
SERVER_PID=$!

# Wait for server to be ready (up to 30s for gforth FFI compilation)
ready=false
for _ in $(seq 1 60); do
    if curl -sf -o /dev/null "http://localhost:${PORT}/api/health" 2>/dev/null; then
        ready=true
        break
    fi
    if ! kill -0 "${SERVER_PID}" 2>/dev/null; then
        echo "FAIL: Server died on startup"
        cat "${TEST_DIR}/stderr.log"
        exit 1
    fi
    sleep 0.5
done

if ! ${ready}; then
    echo "FAIL: Server did not become ready within 30s"
    cat "${TEST_DIR}/stderr.log"
    kill "${SERVER_PID}" 2>/dev/null || true
    exit 1
fi

echo "Running forth tests..."
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
assert_json_field "stats implementation" "${body}" '.implementation' "forth"
assert_json_not_null "stats uptime_ms" "${body}" '.uptime_ms'
assert_json_not_null "stats memory_rss_bytes" "${body}" '.memory_rss_bytes'
assert_json_not_null "stats started_at" "${body}" '.started_at'
assert_json_not_null "stats pid" "${body}" '.pid'
assert_json_not_null "stats db_size_bytes" "${body}" '.db_size_bytes'
assert_json_not_null "stats readings_count" "${body}" '.readings_count'
assert_json_not_null "stats requests_served" "${body}" '.requests_served'
assert_json_not_null "stats poll_failures" "${body}" '.poll_failures'

echo ""

# ═══════════════════════════════════════════════════════════════════
# API CONFIG
# ═══════════════════════════════════════════════════════════════════

echo "--- Config endpoint ---"

body=$(curl -sf "${BASE}/api/config")
assert_json_field "config pollIntervalMs" "${body}" '.pollIntervalMs' "600000"
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

# Readings are ORDER BY timestamp ASC, so first inserted is first
assert_json_field "reading has device_id" "${body}" '.[0].device_id' "${INDOOR_SERIAL}"
assert_json_not_null "reading has timestamp" "${body}" '.[0].timestamp'
assert_json_not_null "reading has id" "${body}" '.[0].id'

# Verify ASC ordering: first reading should have lower id than last
first_id=$(jq '.[0].id' <<< "${body}")
last_id=$(jq '.[-1].id' <<< "${body}")
if (( first_id < last_id )); then
    PASS=$(( PASS + 1 ))
else
    FAIL=$(( FAIL + 1 ))
    ERRORS+=("FAIL: readings ASC order: first id (${first_id}) should be < last id (${last_id})")
    echo "  FAIL: readings ASC order" >&2
fi

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
assert_json_not_null "device has last_seen" "${body}" '.[0].last_seen'

# Verify devices response does NOT contain first_seen (not in API spec)
has_first_seen=$(jq '.[0] | has("first_seen")' <<< "${body}")
assert_eq "device does NOT have first_seen" "false" "${has_first_seen}"

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

# Content-Type headers
ct=$(curl -si "${BASE}/" 2>/dev/null | grep -i "^Content-Type:" | tr -d '\r\n')
assert_contains "index content-type" "${ct}" "text/html"

ct=$(curl -si "${BASE}/style.css" 2>/dev/null | grep -i "^Content-Type:" | tr -d '\r\n')
assert_contains "css content-type" "${ct}" "text/css"

# Dashboard structure verification
body=$(curl -sf "${BASE}/")
assert_contains "HTML has charts section" "${body}" 'id="charts"'
assert_contains "HTML has device-status" "${body}" 'id="device-status"'
assert_contains "HTML has time-range nav" "${body}" 'id="time-range"'
assert_contains "HTML has current-values" "${body}" 'id="current-values"'

body=$(curl -sf "${BASE}/app.js")
assert_contains "app.js exists and has fetchReadings" "${body}" "fetchReadings"
assert_contains "app.js references /api/readings" "${body}" "/api/readings"

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

assert_http_status "path traversal .." "${BASE}/../airgradientz.json" "GET" "403"
assert_http_status "path traversal %2e%2e" "${BASE}/%2e%2e/airgradientz.json" "GET" "403"
assert_http_status "path traversal encoded" "${BASE}/%2e%2e/%2e%2e/etc/passwd" "GET" "403"

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
# DOWNSAMPLE
# ═══════════════════════════════════════════════════════════════════

echo "--- Downsample ---"

body=$(curl -sf "${BASE}/api/readings?downsample=1h")
ds_count=$(jq 'length' <<< "${body}")
if (( ds_count > 0 )); then
    PASS=$(( PASS + 1 ))
    # Downsampled results should NOT have 'id' field
    first_has_id=$(jq '.[0] | has("id")' <<< "${body}")
    assert_eq "downsample omits id field" "false" "${first_has_id}"
    # Should still have standard fields
    assert_json_not_null "downsample has timestamp" "${body}" '.[0].timestamp'
    assert_json_not_null "downsample has device_id" "${body}" '.[0].device_id'
else
    FAIL=$(( FAIL + 1 ))
    ERRORS+=("FAIL: downsample=1h should return data, got 0")
    echo "  FAIL: downsample=1h returns data" >&2
fi

# Invalid downsample returns 400
code=$(curl -s -o /dev/null -w "%{http_code}" "${BASE}/api/readings?downsample=invalid" 2>/dev/null) || code="000"
assert_eq "downsample=invalid returns 400" "400" "${code}"

echo ""

# ═══════════════════════════════════════════════════════════════════
# READINGS COUNT
# ═══════════════════════════════════════════════════════════════════

echo "--- Readings count ---"

body=$(curl -sf "${BASE}/api/readings/count")
total_count=$(jq '.count' <<< "${body}")
if (( total_count > 0 )); then
    PASS=$(( PASS + 1 ))
else
    FAIL=$(( FAIL + 1 ))
    ERRORS+=("FAIL: readings/count should be > 0, got ${total_count}")
    echo "  FAIL: readings/count > 0" >&2
fi

# Count response should have ONLY "count" key (no extra fields)
count_keys=$(jq 'keys | length' <<< "${body}")
assert_eq "readings/count has exactly 1 key" "1" "${count_keys}"
count_has_count=$(jq 'has("count")' <<< "${body}")
assert_eq "readings/count has count key" "true" "${count_has_count}"

# Count with device filter
body=$(curl -sf "${BASE}/api/readings/count?device=${INDOOR_SERIAL}")
device_count=$(jq '.count' <<< "${body}")
assert_eq "readings/count for indoor device is 2" "2" "${device_count}"

echo ""

# ═══════════════════════════════════════════════════════════════════
# CONFIG DOWNSAMPLE BUCKETS
# ═══════════════════════════════════════════════════════════════════

echo "--- Config downsampleBuckets ---"

body=$(curl -sf "${BASE}/api/config")
assert_json_field "config has downsampleBuckets.5m" "${body}" '.downsampleBuckets["5m"]' "300000"
assert_json_field "config has downsampleBuckets.1h" "${body}" '.downsampleBuckets["1h"]' "3600000"
assert_json_field "config has downsampleBuckets.1w" "${body}" '.downsampleBuckets["1w"]' "604800000"
bucket_count=$(jq '.downsampleBuckets | length' <<< "${body}")
assert_eq "config downsampleBuckets has 7 entries" "7" "${bucket_count}"

echo ""

# ═══════════════════════════════════════════════════════════════════
# LATEST PICKS BY MAX(id), NOT MAX(timestamp)
# ═══════════════════════════════════════════════════════════════════

echo "--- Latest picks by MAX(id), not MAX(timestamp) ---"

# Insert two readings for a new device with the SAME timestamp but different values
test_ts=$(now_ms)
sqlite3 "${DB_PATH}" "INSERT INTO readings (
    timestamp, device_id, device_type, device_ip,
    pm01, pm02, pm10, pm02_compensated,
    rco2, atmp, atmp_compensated, rhum, rhum_compensated,
    tvoc_index, nox_index, wifi, raw_json
) VALUES (
    ${test_ts}, 'latest-test', 'indoor', '192.168.1.99',
    null, 100, null, null,
    null, null, null, null, null,
    null, null, null, '{}'
);"
sqlite3 "${DB_PATH}" "INSERT INTO readings (
    timestamp, device_id, device_type, device_ip,
    pm01, pm02, pm10, pm02_compensated,
    rco2, atmp, atmp_compensated, rhum, rhum_compensated,
    tvoc_index, nox_index, wifi, raw_json
) VALUES (
    ${test_ts}, 'latest-test', 'indoor', '192.168.1.99',
    null, 999, null, null,
    null, null, null, null, null,
    null, null, null, '{}'
);"
sleep 0.1

body=$(curl -sf "${BASE}/api/readings/latest")
latest_pm02=$(jq '[.[] | select(.device_id == "latest-test")][0].pm02 | floor' <<< "${body}")
# The second insert (pm02=999) has the higher id, so latest should pick it
assert_eq "latest picks higher id (pm02=999)" "999" "${latest_pm02}"

echo ""

# ═══════════════════════════════════════════════════════════════════
# TEST-SPEC: DOWNSAMPLE BUCKET VERIFICATION
# ═══════════════════════════════════════════════════════════════════

echo "--- test-spec: downsample bucket verification ---"

# All 7 valid downsample bucket values must return 200
for bucket in 5m 10m 15m 30m 1h 1d 1w; do
    code=$(curl -s -o /dev/null -w "%{http_code}" "${BASE}/api/readings?downsample=${bucket}" 2>/dev/null) || code="000"
    assert_eq "downsample=${bucket} returns 200" "200" "${code}"
done

# Invalid bucket returns 400
code=$(curl -s -o /dev/null -w "%{http_code}" "${BASE}/api/readings?downsample=2h" 2>/dev/null) || code="000"
assert_eq "downsample=2h (invalid) returns 400" "400" "${code}"

echo ""

# ═══════════════════════════════════════════════════════════════════
# TEST-SPEC: QUERY EDGE CASES
# ═══════════════════════════════════════════════════════════════════

echo "--- test-spec: query edge cases ---"

# from > to returns empty array
body=$(curl -sf "${BASE}/api/readings?from=9999999999999&to=1")
assert_json_length "from > to returns empty array" "${body}" "0"

# nonexistent device returns empty array
body=$(curl -sf "${BASE}/api/readings?device=nonexistent-serial-xyz")
assert_json_length "nonexistent device returns empty array" "${body}" "0"

# limit=1 returns exactly 1 result
body=$(curl -sf "${BASE}/api/readings?limit=1")
assert_json_length "limit=1 returns exactly 1" "${body}" "1"

# count with from > to returns zero
body=$(curl -sf "${BASE}/api/readings/count?from=9999999999999&to=1")
count_val=$(jq '.count' <<< "${body}")
assert_eq "count from > to returns 0" "0" "${count_val}"

# count with nonexistent device returns zero
body=$(curl -sf "${BASE}/api/readings/count?device=nonexistent-serial-xyz")
count_val=$(jq '.count' <<< "${body}")
assert_eq "count nonexistent device returns 0" "0" "${count_val}"

echo ""

# ═══════════════════════════════════════════════════════════════════
# TEST-SPEC: RESPONSE SHAPE VALIDATION
# ═══════════════════════════════════════════════════════════════════

echo "--- test-spec: response shape validation ---"

# Readings response must NOT contain "raw_json" field
body=$(curl -sf "${BASE}/api/readings?limit=1")
count_items=$(jq 'length' <<< "${body}")
if (( count_items > 0 )); then
    has_raw_json=$(jq '.[0] | has("raw_json")' <<< "${body}")
    assert_eq "readings response has no raw_json" "false" "${has_raw_json}"

    # Readings must have required fields
    for field in id timestamp device_id device_type device_ip pm01 pm02 pm10 pm02_compensated rco2 atmp atmp_compensated rhum rhum_compensated tvoc_index nox_index wifi; do
        has_field=$(jq ".[0] | has(\"${field}\")" <<< "${body}")
        assert_eq "readings has required field '${field}'" "true" "${has_field}"
    done
else
    FAIL=$(( FAIL + 1 ))
    ERRORS+=("FAIL: readings should have data for shape validation")
    echo "  FAIL: readings should have data" >&2
fi

# Downsampled response must NOT contain "raw_json" or "id"
body=$(curl -sf "${BASE}/api/readings?downsample=1h")
ds_items=$(jq 'length' <<< "${body}")
if (( ds_items > 0 )); then
    has_raw_json=$(jq '.[0] | has("raw_json")' <<< "${body}")
    assert_eq "downsampled response has no raw_json" "false" "${has_raw_json}"
    has_id=$(jq '.[0] | has("id")' <<< "${body}")
    assert_eq "downsampled response has no id" "false" "${has_id}"
else
    FAIL=$(( FAIL + 1 ))
    ERRORS+=("FAIL: downsampled readings should have data for shape validation")
    echo "  FAIL: downsampled should have data" >&2
fi

# Devices response must NOT contain "first_seen"
body=$(curl -sf "${BASE}/api/devices")
dev_items=$(jq 'length' <<< "${body}")
if (( dev_items > 0 )); then
    has_first_seen=$(jq '.[0] | has("first_seen")' <<< "${body}")
    assert_eq "devices response has no first_seen" "false" "${has_first_seen}"

    # Devices must have required fields
    for field in device_id device_type device_ip last_seen reading_count; do
        has_field=$(jq ".[0] | has(\"${field}\")" <<< "${body}")
        assert_eq "devices has required field '${field}'" "true" "${has_field}"
    done
else
    FAIL=$(( FAIL + 1 ))
    ERRORS+=("FAIL: devices should have data for shape validation")
    echo "  FAIL: devices should have data" >&2
fi

# Count response has exactly {"count": N} — no extra fields
body=$(curl -sf "${BASE}/api/readings/count")
count_keys=$(jq -c 'keys' <<< "${body}")
assert_eq "count response has exactly {\"count\"} key" '["count"]' "${count_keys}"

# Config response shape
body=$(curl -sf "${BASE}/api/config")
for field in pollIntervalMs downsampleBuckets devices; do
    has_field=$(jq "has(\"${field}\")" <<< "${body}")
    assert_eq "config has required field '${field}'" "true" "${has_field}"
done
has_forbidden=$(jq 'has("downsampleThreshold")' <<< "${body}")
assert_eq "config has no downsampleThreshold" "false" "${has_forbidden}"

echo ""

# ═══════════════════════════════════════════════════════════════════
# TEST-SPEC: SECURITY
# ═══════════════════════════════════════════════════════════════════

echo "--- test-spec: security ---"

# Path traversal returns 403
assert_http_status "path traversal /../../../etc/passwd" "${BASE}/../../../etc/passwd" "GET" "403"
assert_http_status "encoded path traversal /%2e%2e/%2e%2e/etc/passwd" "${BASE}/%2e%2e/%2e%2e/etc/passwd" "GET" "403"

# Unknown API path returns 404
assert_http_status "unknown API /api/nonexistent" "${BASE}/api/nonexistent" "GET" "404"

# POST on readings returns 405
assert_http_status "POST /api/readings returns 405" "${BASE}/api/readings" "POST" "405"

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
