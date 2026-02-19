#!/usr/bin/env bash
# Test suite for AirGradientz bash implementation.
# Mirrors test coverage from Go's db_test.go and server_test.go.
# shellcheck disable=SC1091  # source paths resolved at runtime, not statically
# shellcheck disable=SC2312  # $(func) in test assertions is intentional
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
TEST_DIR="$(mktemp -d)"

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
        ERRORS+=("FAIL: ${label}: '${haystack}' does not contain '${needle}'")
        echo "  FAIL: ${label}" >&2
    fi
}

assert_not_contains() {
    local label="$1" haystack="$2" needle="$3"
    if [[ "${haystack}" != *"${needle}"* ]]; then
        PASS=$(( PASS + 1 ))
    else
        FAIL=$(( FAIL + 1 ))
        ERRORS+=("FAIL: ${label}: '${haystack}' should not contain '${needle}'")
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

assert_json_null() {
    local label="$1" json="$2" field="$3"
    local actual
    actual=$(jq -r "${field}" <<< "${json}" 2>/dev/null) || actual="(jq error)"
    assert_eq "${label}" "null" "${actual}"
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

# --- Setup test environment ---

setup_test_env() {
    export AGTZ_SCRIPT_DIR="${SCRIPT_DIR}"
    export AGTZ_PORT=3017
    export AGTZ_DB_PATH="${TEST_DIR}/test.db"
    export AGTZ_POLL_INTERVAL_MS=15000
    export AGTZ_FETCH_TIMEOUT_MS=5000
    export AGTZ_MAX_API_ROWS=10000
    export AGTZ_DOWNSAMPLE_BUCKETS='{"5m":300000,"10m":600000,"15m":900000,"30m":1800000,"1h":3600000,"1d":86400000,"1w":604800000}'
    export AGTZ_DEVICES_JSON='[{"ip":"192.168.1.1","label":"test-indoor"}]'

    # Create run directory
    mkdir -p "${TEST_DIR}/run"
    printf '0' > "${TEST_DIR}/run/requests"
    printf '0' > "${TEST_DIR}/run/poll_successes"
    printf '0' > "${TEST_DIR}/run/poll_failures"
    printf '%s' "$(now_ms)" > "${TEST_DIR}/run/started_at"
    printf '%d' "$$" > "${TEST_DIR}/run/server_pid"

    # Source libs
    # shellcheck source=../lib/config.sh
    source "${SCRIPT_DIR}/lib/config.sh"
    # shellcheck source=../lib/db.sh
    source "${SCRIPT_DIR}/lib/db.sh"
    # shellcheck source=../lib/http.sh
    source "${SCRIPT_DIR}/lib/http.sh"

    # Initialize DB
    init_db
}

# Helper: insert a test reading directly via SQL
insert_test_reading() {
    local device_id="$1" device_type="$2" device_ip="$3"
    local pm02="${4:-null}" rco2="${5:-null}" atmp="${6:-null}"
    local ts
    ts=$(now_ms)
    db_exec "INSERT INTO readings (
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

# Helper: run handler.sh with piped HTTP request, capture response
do_request() {
    local method="$1" path="$2"
    printf '%s %s HTTP/1.1\r\nHost: localhost\r\n\r\n' "${method}" "${path}" \
        | AGTZ_SCRIPT_DIR="${SCRIPT_DIR}" \
          AGTZ_DB_PATH="${AGTZ_DB_PATH}" \
          AGTZ_PORT="${AGTZ_PORT}" \
          AGTZ_POLL_INTERVAL_MS="${AGTZ_POLL_INTERVAL_MS}" \
          AGTZ_FETCH_TIMEOUT_MS="${AGTZ_FETCH_TIMEOUT_MS}" \
          AGTZ_MAX_API_ROWS="${AGTZ_MAX_API_ROWS}" \
          AGTZ_DOWNSAMPLE_BUCKETS="${AGTZ_DOWNSAMPLE_BUCKETS}" \
          AGTZ_DEVICES_JSON="${AGTZ_DEVICES_JSON}" \
          bash "${SCRIPT_DIR}/handler.sh" 2>/dev/null
}

get_status_code() {
    local response="$1"
    local first_line
    first_line=$(head -1 <<< "${response}")
    # "HTTP/1.1 200 OK" → "200"
    local code
    code=$(awk '{print $2}' <<< "${first_line}")
    printf '%s' "${code}"
}

get_body() {
    local response="$1"
    # Body starts after the blank line
    sed -n '/^\r*$/,$ p' <<< "${response}" | tail -n +2
}

get_header() {
    local response="$1" header_name="$2"
    grep -i "^${header_name}:" <<< "${response}" | sed 's/^[^:]*: //' | tr -d '\r'
}

cleanup() {
    rm -rf "${TEST_DIR}"
}
trap cleanup EXIT

# --- Source and setup ---

# shellcheck source=../lib/config.sh
source "${SCRIPT_DIR}/lib/config.sh"
setup_test_env

echo "Running bash tests..."
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
# DB TESTS
# ═══════════════════════════════════════════════════════════════════

echo "--- DB tests ---"

# TestInitDBCreatesTable
table_name=$(db_exec "SELECT name FROM sqlite_master WHERE type='table' AND name='readings'")
assert_eq "init_db creates readings table" "readings" "${table_name}"

# TestSchemaFileExists
if [[ -f "../schema.sql" ]]; then
    assert_eq "schema.sql exists" "yes" "yes"
else
    assert_eq "schema.sql exists" "yes" "no"
fi

# TestGetReadingsCount (empty)
count=$(get_readings_count)
assert_eq "readings count empty db" "0" "${count}"

# TestInsertAndQuery
insert_test_reading "${INDOOR_SERIAL}" "indoor" "192.168.1.1" "${INDOOR_PM02}" "${INDOOR_RCO2}" "${INDOOR_ATMP}"
result=$(query_readings "all" 0 "$(( $(now_ms) + 1000 ))" 100)
assert_json_length "query returns 1 reading" "${result}" "1"
assert_json_field "device_id is indoor serial" "${result}" '.[0].device_id' "${INDOOR_SERIAL}"
assert_json_field "device_type is indoor" "${result}" '.[0].device_type' "indoor"
assert_json_field "device_ip is correct" "${result}" '.[0].device_ip' "192.168.1.1"
assert_json_field "pm02 is indoor fixture" "${result}" '.[0].pm02' "${INDOOR_PM02}"
assert_json_field "rco2 is indoor fixture" "${result}" '.[0].rco2' "${INDOOR_RCO2}"
assert_json_field "atmp is indoor fixture" "${result}" '.[0].atmp' "${INDOOR_ATMP}"

# TestGetReadingsCount (after insert)
count=$(get_readings_count)
assert_eq "readings count after insert" "1" "${count}"

# TestNullFields
insert_test_reading "boot1" "indoor" "192.168.1.1"
result=$(query_readings "boot1" 0 "$(( $(now_ms) + 1000 ))" 100)
assert_json_null "null pm01" "${result}" '.[0].pm01'
assert_json_null "null pm02 for boot" "${result}" '.[0].pm02'
assert_json_null "null rco2 for boot" "${result}" '.[0].rco2'
assert_json_null "null atmp for boot" "${result}" '.[0].atmp'
assert_json_null "null wifi for boot" "${result}" '.[0].wifi'
assert_json_null "null tvoc_index for boot" "${result}" '.[0].tvoc_index'

# TestZeroCompensatedValues
insert_test_reading "zero1" "indoor" "192.168.1.1" 0 0 0
result=$(query_readings "zero1" 0 "$(( $(now_ms) + 1000 ))" 100)
assert_json_field "zero pm02 is 0 not null" "${result}" '.[0].pm02' "0"
assert_json_field "zero rco2 is 0 not null" "${result}" '.[0].rco2' "0"
assert_json_field "zero atmp is 0 not null" "${result}" '.[0].atmp' "0"

# TestDeviceFiltering
insert_test_reading "${OUTDOOR_SERIAL}" "outdoor" "192.168.1.2" "${OUTDOOR_PM02}" "${OUTDOOR_RCO2}" "${OUTDOOR_ATMP}"

result=$(query_readings "${INDOOR_SERIAL}" 0 "$(( $(now_ms) + 1000 ))" 100)
assert_json_length "filter by indoor device" "${result}" "1"

result=$(query_readings "all" 0 "$(( $(now_ms) + 1000 ))" 100)
count=$(jq 'length' <<< "${result}")
assert_eq "all devices returns all" "4" "${count}"

result=$(query_readings "" 0 "$(( $(now_ms) + 1000 ))" 100)
count=$(jq 'length' <<< "${result}")
assert_eq "empty device means all" "4" "${count}"

result=$(query_readings "nonexistent" 0 "$(( $(now_ms) + 1000 ))" 100)
assert_json_length "nonexistent device returns 0" "${result}" "0"

# TestQueryLimit
for i in $(seq 1 5); do
    insert_test_reading "limitdev" "indoor" "192.168.1.1" "${i}" 0 0
done
result=$(query_readings "limitdev" 0 "$(( $(now_ms) + 1000 ))" 3)
assert_json_length "query limit 3" "${result}" "3"

# TestGetLatestReadings
result=$(get_latest_readings)
count=$(jq 'length' <<< "${result}")
# We have indoor, boot1, zero1, outdoor, limitdev → 5 unique devices
assert_eq "latest readings one per device" "5" "${count}"

# TestGetDevices
result=$(get_devices)
count=$(jq 'length' <<< "${result}")
assert_eq "devices returns unique devices" "5" "${count}"

# Check reading_count for limitdev (5 inserts)
limitdev_count=$(jq -r '.[] | select(.device_id == "limitdev") | .reading_count' <<< "${result}")
assert_eq "limitdev reading_count" "5" "${limitdev_count}"

# TestCheckpoint
db_checkpoint
assert_eq "checkpoint succeeds" "0" "$?"

echo ""

# ═══════════════════════════════════════════════════════════════════
# CONFIG TESTS
# ═══════════════════════════════════════════════════════════════════

echo "--- Config tests ---"

assert_eq "default port" "3017" "${AGTZ_PORT}"
assert_eq "poll interval" "15000" "${AGTZ_POLL_INTERVAL_MS}"
assert_eq "fetch timeout" "5000" "${AGTZ_FETCH_TIMEOUT_MS}"
assert_eq "max api rows" "10000" "${AGTZ_MAX_API_ROWS}"

# TestNowMillis
ts=$(now_ms)
if (( ts > 1700000000000 )); then
    assert_eq "now_ms is reasonable" "yes" "yes"
else
    assert_eq "now_ms is reasonable" "yes" "no (${ts})"
fi

echo ""

# ═══════════════════════════════════════════════════════════════════
# HTTP / URL TESTS
# ═══════════════════════════════════════════════════════════════════

echo "--- HTTP parsing tests ---"

# TestURLDecode
assert_eq "url_decode %2e%2e" ".." "$(url_decode '%2e%2e')"
assert_eq "url_decode hello+world" "hello world" "$(url_decode 'hello+world')"
assert_eq "url_decode plain" "plain" "$(url_decode 'plain')"
assert_eq "url_decode %2F" "/" "$(url_decode '%2F')"

# TestParseQueryParam
assert_eq "parse param present" "42" "$(parse_query_param 'from=42&to=100' 'from' '')"
assert_eq "parse param missing" "99" "$(parse_query_param 'other=1' 'from' '99')"
assert_eq "parse param empty query" "default" "$(parse_query_param '' 'from' 'default')"

# TestContentTypeFor
assert_eq "ct html" "text/html; charset=utf-8" "$(content_type_for 'file.html')"
assert_eq "ct css" "text/css; charset=utf-8" "$(content_type_for 'file.css')"
assert_eq "ct js" "application/javascript; charset=utf-8" "$(content_type_for 'file.js')"
assert_eq "ct json" "application/json; charset=utf-8" "$(content_type_for 'file.json')"
assert_eq "ct png" "image/png" "$(content_type_for 'file.png')"
assert_eq "ct jpg" "image/jpeg" "$(content_type_for 'file.jpg')"
assert_eq "ct jpeg" "image/jpeg" "$(content_type_for 'file.jpeg')"
assert_eq "ct svg" "image/svg+xml" "$(content_type_for 'file.svg')"
assert_eq "ct ico" "image/x-icon" "$(content_type_for 'file.ico')"
assert_eq "ct unknown" "application/octet-stream" "$(content_type_for 'file.xyz')"

# TestCounters
printf '0' > "${TEST_DIR}/run/test_counter"
increment_counter "${TEST_DIR}/run/test_counter"
increment_counter "${TEST_DIR}/run/test_counter"
increment_counter "${TEST_DIR}/run/test_counter"
val=$(read_counter "${TEST_DIR}/run/test_counter")
assert_eq "counter increments to 3" "3" "${val}"

echo ""

# ═══════════════════════════════════════════════════════════════════
# HANDLER / ENDPOINT TESTS
# ═══════════════════════════════════════════════════════════════════

echo "--- Handler tests ---"

# Need to point handler at test run dir. handler.sh uses AGTZ_SCRIPT_DIR/run,
# so we create a temporary wrapper that uses our test dirs.
# Actually, handler.sh uses SCRIPT_DIR = AGTZ_SCRIPT_DIR, RUN_DIR = SCRIPT_DIR/run
# So we need to make AGTZ_SCRIPT_DIR point to a dir that has our test run/ and public symlink

HANDLER_TEST_DIR="${TEST_DIR}/handler_env"
mkdir -p "${HANDLER_TEST_DIR}/run" "${HANDLER_TEST_DIR}/lib"
ln -sf "${SCRIPT_DIR}/lib/config.sh" "${HANDLER_TEST_DIR}/lib/config.sh"
ln -sf "${SCRIPT_DIR}/lib/http.sh" "${HANDLER_TEST_DIR}/lib/http.sh"
ln -sf "${SCRIPT_DIR}/lib/db.sh" "${HANDLER_TEST_DIR}/lib/db.sh"
ln -sf "${SCRIPT_DIR}/public" "${HANDLER_TEST_DIR}/public"
ln -sf "${SCRIPT_DIR}/handler.sh" "${HANDLER_TEST_DIR}/handler.sh"
printf '0' > "${HANDLER_TEST_DIR}/run/requests"
printf '0' > "${HANDLER_TEST_DIR}/run/poll_successes"
printf '0' > "${HANDLER_TEST_DIR}/run/poll_failures"
printf '%s' "$(now_ms)" > "${HANDLER_TEST_DIR}/run/started_at"
printf '%d' "$$" > "${HANDLER_TEST_DIR}/run/server_pid"

# Initialize health.json for handler tests
printf '[{"ip":"192.168.1.1","label":"test-indoor","status":"unknown","lastSuccess":null,"lastError":null,"lastErrorMessage":null,"consecutiveFailures":0}]' \
    > "${HANDLER_TEST_DIR}/run/health.json"

do_handler_request() {
    local method="$1" path="$2"
    printf '%s %s HTTP/1.1\r\nHost: localhost\r\n\r\n' "${method}" "${path}" \
        | AGTZ_SCRIPT_DIR="${HANDLER_TEST_DIR}" \
          AGTZ_DB_PATH="${AGTZ_DB_PATH}" \
          AGTZ_PORT="${AGTZ_PORT}" \
          AGTZ_POLL_INTERVAL_MS="${AGTZ_POLL_INTERVAL_MS}" \
          AGTZ_FETCH_TIMEOUT_MS="${AGTZ_FETCH_TIMEOUT_MS}" \
          AGTZ_MAX_API_ROWS="${AGTZ_MAX_API_ROWS}" \
          AGTZ_DOWNSAMPLE_BUCKETS="${AGTZ_DOWNSAMPLE_BUCKETS}" \
          AGTZ_DEVICES_JSON="${AGTZ_DEVICES_JSON}" \
          bash "${HANDLER_TEST_DIR}/handler.sh" 2>/dev/null || true
}

# TestMethodNotAllowed
response=$(do_handler_request POST "/api/readings")
status=$(get_status_code "${response}")
assert_eq "POST returns 405" "405" "${status}"

# TestReadingsEmpty
response=$(do_handler_request GET "/api/readings?from=99999999999999&to=99999999999999")
status=$(get_status_code "${response}")
body=$(get_body "${response}")
assert_eq "readings empty status 200" "200" "${status}"
assert_json_length "readings empty returns []" "${body}" "0"

# TestReadingsEndpoint
response=$(do_handler_request GET "/api/readings")
status=$(get_status_code "${response}")
body=$(get_body "${response}")
assert_eq "readings status 200" "200" "${status}"
count=$(jq 'length' <<< "${body}")
# Should have the readings we inserted earlier
if (( count > 0 )); then
    assert_eq "readings returns data" "yes" "yes"
else
    assert_eq "readings returns data" "yes" "no (got ${count})"
fi

# TestReadingsWithDeviceFilter
response=$(do_handler_request GET "/api/readings?device=${INDOOR_SERIAL}")
body=$(get_body "${response}")
filtered_count=$(jq 'length' <<< "${body}")
assert_eq "device filter returns 1" "1" "${filtered_count}"

# TestReadingsWithLimit
response=$(do_handler_request GET "/api/readings?limit=2")
body=$(get_body "${response}")
limited_count=$(jq 'length' <<< "${body}")
assert_eq "limit=2 returns 2" "2" "${limited_count}"

# TestReadingsLatest
response=$(do_handler_request GET "/api/readings/latest")
status=$(get_status_code "${response}")
body=$(get_body "${response}")
assert_eq "latest status 200" "200" "${status}"
latest_count=$(jq 'length' <<< "${body}")
assert_eq "latest returns 5 devices" "5" "${latest_count}"

# TestDevicesEndpoint
response=$(do_handler_request GET "/api/devices")
status=$(get_status_code "${response}")
body=$(get_body "${response}")
assert_eq "devices status 200" "200" "${status}"
devices_count=$(jq 'length' <<< "${body}")
assert_eq "devices returns 5" "5" "${devices_count}"

# TestHealthEndpoint
response=$(do_handler_request GET "/api/health")
status=$(get_status_code "${response}")
body=$(get_body "${response}")
assert_eq "health status 200" "200" "${status}"
assert_json_length "health returns 1 device" "${body}" "1"
assert_json_field "health initial status" "${body}" '.[0].status' "unknown"
assert_json_field "health ip" "${body}" '.[0].ip' "192.168.1.1"

# TestConfigEndpoint
response=$(do_handler_request GET "/api/config")
status=$(get_status_code "${response}")
body=$(get_body "${response}")
assert_eq "config status 200" "200" "${status}"
assert_json_field "config pollIntervalMs" "${body}" '.pollIntervalMs' "15000"
config_devices=$(jq '.devices | length' <<< "${body}")
assert_eq "config has 1 device" "1" "${config_devices}"

# TestStatsEndpoint
response=$(do_handler_request GET "/api/stats")
status=$(get_status_code "${response}")
body=$(get_body "${response}")
assert_eq "stats status 200" "200" "${status}"
assert_json_field "stats implementation" "${body}" '.implementation' "bash"
assert_json_not_null "stats has uptime_ms" "${body}" '.uptime_ms'
assert_json_not_null "stats has memory_rss_bytes" "${body}" '.memory_rss_bytes'
assert_json_not_null "stats has started_at" "${body}" '.started_at'

# TestSecurityHeaders
response=$(do_handler_request GET "/api/health")
ct_header=$(get_header "${response}" "Content-Type")
xcto_header=$(get_header "${response}" "X-Content-Type-Options")
xfo_header=$(get_header "${response}" "X-Frame-Options")
conn_header=$(get_header "${response}" "Connection")
assert_eq "Content-Type is application/json" "application/json" "${ct_header}"
assert_eq "X-Content-Type-Options is nosniff" "nosniff" "${xcto_header}"
assert_eq "X-Frame-Options is DENY" "DENY" "${xfo_header}"
assert_eq "Connection is close" "close" "${conn_header}"

# TestStaticServing (index.html)
response=$(do_handler_request GET "/")
status=$(get_status_code "${response}")
body=$(get_body "${response}")
assert_eq "/ returns 200" "200" "${status}"
assert_contains "/ serves html" "${body}" "<!DOCTYPE html>"

# Dashboard structure verification
assert_contains "HTML has charts section" "${body}" 'id="charts"'
assert_contains "HTML has device-status" "${body}" 'id="device-status"'
assert_contains "HTML has time-range nav" "${body}" 'id="time-range"'
assert_contains "HTML has current-values" "${body}" 'id="current-values"'

# CSS file
response=$(do_handler_request GET "/style.css")
status=$(get_status_code "${response}")
css_body=$(get_body "${response}")
assert_eq "/style.css returns 200" "200" "${status}"
assert_contains "CSS has chart-card class" "${css_body}" ".chart-card"

# JS file
response=$(do_handler_request GET "/app.js")
status=$(get_status_code "${response}")
js_body=$(get_body "${response}")
assert_eq "/app.js returns 200" "200" "${status}"
assert_contains "app.js has fetchReadings" "${js_body}" "fetchReadings"
assert_contains "app.js references /api/readings" "${js_body}" "/api/readings"

# TestNotFoundForMissing
response=$(do_handler_request GET "/nonexistent.html")
status=$(get_status_code "${response}")
assert_eq "missing file returns 404" "404" "${status}"

# TestPathTraversal
response=$(do_handler_request GET "/../../etc/passwd")
status=$(get_status_code "${response}")
assert_eq "dotdot returns 403" "403" "${status}"

response=$(do_handler_request GET "/%2e%2e/etc/passwd")
status=$(get_status_code "${response}")
assert_eq "encoded dotdot returns 403" "403" "${status}"

response=$(do_handler_request GET "/foo/../../../etc/passwd")
status=$(get_status_code "${response}")
assert_eq "dotdot in middle returns 403" "403" "${status}"

# TestRawJSONNotInAPIResponse
response=$(do_handler_request GET "/api/readings?limit=1")
body=$(get_body "${response}")
assert_not_contains "raw_json not in API response" "${body}" "raw_json"

# TestReadingsDownsample1h
response=$(do_handler_request GET "/api/readings?downsample=1h")
status=$(get_status_code "${response}")
body=$(get_body "${response}")
assert_eq "downsample=1h status 200" "200" "${status}"
ds_count=$(jq 'length' <<< "${body}")
if (( ds_count > 0 )); then
    assert_eq "downsample returns data" "yes" "yes"
    # Downsampled results should NOT have 'id' field
    first_has_id=$(jq '.[0] | has("id")' <<< "${body}")
    assert_eq "downsample omits id field" "false" "${first_has_id}"
    # Should still have standard fields
    assert_json_not_null "downsample has timestamp" "${body}" '.[0].timestamp'
    assert_json_not_null "downsample has device_id" "${body}" '.[0].device_id'
else
    assert_eq "downsample returns data" "yes" "no (got ${ds_count})"
fi

# TestReadingsDownsampleInvalid
response=$(do_handler_request GET "/api/readings?downsample=invalid")
status=$(get_status_code "${response}")
assert_eq "downsample=invalid returns 400" "400" "${status}"

# TestReadingsCount
response=$(do_handler_request GET "/api/readings/count")
status=$(get_status_code "${response}")
body=$(get_body "${response}")
assert_eq "readings/count status 200" "200" "${status}"
total_count=$(jq '.count' <<< "${body}")
if (( total_count > 0 )); then
    assert_eq "readings/count returns positive count" "yes" "yes"
else
    assert_eq "readings/count returns positive count" "yes" "no (got ${total_count})"
fi

# TestReadingsCountWithDevice
response=$(do_handler_request GET "/api/readings/count?device=${INDOOR_SERIAL}")
status=$(get_status_code "${response}")
body=$(get_body "${response}")
assert_eq "readings/count?device status 200" "200" "${status}"
device_count=$(jq '.count' <<< "${body}")
assert_eq "readings/count for indoor device is 1" "1" "${device_count}"

# TestConfigHasDownsampleBuckets
response=$(do_handler_request GET "/api/config")
body=$(get_body "${response}")
bucket_count=$(jq '.downsampleBuckets | length' <<< "${body}")
assert_eq "config has downsampleBuckets" "7" "${bucket_count}"
assert_json_field "config downsampleBuckets.1h" "${body}" '.downsampleBuckets["1h"]' "3600000"

echo ""

# ═══════════════════════════════════════════════════════════════════
# REGRESSION TESTS
# ═══════════════════════════════════════════════════════════════════

echo "--- Regression tests ---"

# TestReadingsOrderedASC
# Insert readings with different timestamps and verify ASC order via handler
response=$(do_handler_request GET "/api/readings?device=${INDOOR_SERIAL}")
body=$(get_body "${response}")
count=$(jq 'length' <<< "${body}")
if (( count > 1 )); then
    first_ts=$(jq '.[0].timestamp' <<< "${body}")
    last_ts=$(jq '.[-1].timestamp' <<< "${body}")
    if (( first_ts <= last_ts )); then
        assert_eq "readings ASC order (first <= last)" "yes" "yes"
    else
        assert_eq "readings ASC order (first <= last)" "yes" "no (${first_ts} > ${last_ts})"
    fi
else
    # Only 1 reading, test with all readings instead
    response=$(do_handler_request GET "/api/readings?limit=10")
    body=$(get_body "${response}")
    first_ts=$(jq '.[0].timestamp' <<< "${body}")
    last_ts=$(jq '.[-1].timestamp' <<< "${body}")
    if (( first_ts <= last_ts )); then
        assert_eq "readings ASC order (first <= last)" "yes" "yes"
    else
        assert_eq "readings ASC order (first <= last)" "yes" "no"
    fi
fi

# TestLatestByMaxID: Insert 2 readings with same timestamp, verify latest returns higher id
ts_same=$(now_ms)
db_exec "INSERT INTO readings (timestamp, device_id, device_type, device_ip, pm02, rco2, raw_json) VALUES (${ts_same}, 'maxid_dev', 'indoor', '10.0.0.50', 10.0, 400, '{}');"
db_exec "INSERT INTO readings (timestamp, device_id, device_type, device_ip, pm02, rco2, raw_json) VALUES (${ts_same}, 'maxid_dev', 'indoor', '10.0.0.50', 99.0, 999, '{}');"

response=$(do_handler_request GET "/api/readings/latest")
body=$(get_body "${response}")
maxid_pm02=$(jq -r '.[] | select(.device_id == "maxid_dev") | .pm02' <<< "${body}")
maxid_rco2=$(jq -r '.[] | select(.device_id == "maxid_dev") | .rco2' <<< "${body}")
assert_eq "latest by MAX(id) pm02=99" "99" "${maxid_pm02}"
assert_eq "latest by MAX(id) rco2=999" "999" "${maxid_rco2}"

# TestDevicesNoFirstSeen
response=$(do_handler_request GET "/api/devices")
body=$(get_body "${response}")
has_first_seen=$(jq 'any(.[]; has("first_seen"))' <<< "${body}")
assert_eq "devices has no first_seen" "false" "${has_first_seen}"
# But must have expected fields
has_device_id=$(jq 'all(.[]; has("device_id"))' <<< "${body}")
has_last_seen=$(jq 'all(.[]; has("last_seen"))' <<< "${body}")
has_reading_count=$(jq 'all(.[]; has("reading_count"))' <<< "${body}")
assert_eq "devices has device_id" "true" "${has_device_id}"
assert_eq "devices has last_seen" "true" "${has_last_seen}"
assert_eq "devices has reading_count" "true" "${has_reading_count}"

# TestCountResponseShape: Only {"count": N}, no extra fields
response=$(do_handler_request GET "/api/readings/count")
body=$(get_body "${response}")
count_keys=$(jq 'keys | length' <<< "${body}")
assert_eq "count response has exactly 1 key" "1" "${count_keys}"
count_key=$(jq -r 'keys[0]' <<< "${body}")
assert_eq "count response key is 'count'" "count" "${count_key}"
count_val=$(jq '.count' <<< "${body}")
if (( count_val >= 0 )); then
    assert_eq "count value is non-negative" "yes" "yes"
else
    assert_eq "count value is non-negative" "yes" "no (${count_val})"
fi

echo ""

# ═══════════════════════════════════════════════════════════════════
# SQL DRIFT TESTS
# ═══════════════════════════════════════════════════════════════════

echo "--- SQL drift tests ---"

# Extract a named query from queries.sql
# Usage: extract_query "query_name" < queries.sql
extract_query() {
    local name="$1"
    local in_block=false
    local result=""
    while IFS= read -r line; do
        if [[ "${line}" == "-- name: ${name}" ]]; then
            in_block=true
            continue
        elif [[ "${line}" == "-- name: "* ]]; then
            if ${in_block}; then
                break
            fi
            continue
        fi
        if ${in_block}; then
            # Skip comments
            [[ "${line}" == "--"* ]] && continue
            # Skip empty lines
            local trimmed
            trimmed=$(echo "${line}" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
            [[ -z "${trimmed}" ]] && continue
            if [[ -n "${result}" ]]; then
                result="${result} ${trimmed}"
            else
                result="${trimmed}"
            fi
        fi
    done
    # Strip trailing semicolons
    result="${result%%;}"
    printf '%s' "${result}"
}

QUERIES_FILE="${SCRIPT_DIR}/../queries.sql"
DB_SH_FILE="${SCRIPT_DIR}/lib/db.sh"
DB_SH_CONTENTS=$(< "${DB_SH_FILE}")

# 1. reading_columns match
CANONICAL_COLS=$(extract_query "reading_columns" < "${QUERIES_FILE}")
CANONICAL_COLS=$(echo "${CANONICAL_COLS}" | tr -s '[:space:]' ' ' | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')

DB_COLS=$(grep -A3 "^readonly QUERY_COLS=" "${DB_SH_FILE}" \
    | tr -d "'" \
    | sed "s/readonly QUERY_COLS=//" \
    | tr '\n' ' ' \
    | tr -s '[:space:]' ' ' \
    | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')

assert_eq "reading_columns matches queries.sql" "${CANONICAL_COLS}" "${DB_COLS}"

# 2. INSERT column list match
CANONICAL_INSERT=$(extract_query "insert_reading" < "${QUERIES_FILE}")
# Extract just the column list from "INSERT INTO readings ( cols ) VALUES ..."
CANONICAL_INSERT_COLS=$(echo "${CANONICAL_INSERT}" | sed 's/INSERT INTO readings (//;s/) VALUES.*//' | tr -s '[:space:]' ' ' | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')

# Verify poller.sh has INSERT INTO readings (the actual insert happens there)
POLLER_FILE="${SCRIPT_DIR}/lib/poller.sh"
POLLER_CONTENTS=$(< "${POLLER_FILE}")
assert_contains "poller has INSERT INTO readings" "${POLLER_CONTENTS}" "INSERT INTO readings"

# Extract INSERT columns from poller.sh and normalize
POLLER_INSERT_COLS=$(echo "${POLLER_CONTENTS}" \
    | sed -n '/INSERT INTO readings/,/) VALUES/p' \
    | sed 's/.*INSERT INTO readings (//;s/) VALUES.*//' \
    | tr '\n' ' ' \
    | tr -s '[:space:]' ' ' \
    | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')

assert_eq "poller insert columns match queries.sql" "${CANONICAL_INSERT_COLS}" "${POLLER_INSERT_COLS}"

# 3. SELECT devices query match
assert_contains "devices query has GROUP BY device_id" "${DB_SH_CONTENTS}" "GROUP BY device_id"
assert_contains "devices query has ORDER BY device_type" "${DB_SH_CONTENTS}" "ORDER BY device_type"
assert_contains "devices query has MAX(timestamp)" "${DB_SH_CONTENTS}" "MAX(timestamp)"
assert_contains "devices query has COUNT(*)" "${DB_SH_CONTENTS}" "COUNT(*)"

# 4. SELECT latest pattern match — use full function body from db.sh
LATEST_FUNC=$(sed -n '/^get_latest_readings/,/^}/p' "${DB_SH_FILE}")
assert_contains "latest query has MAX(id) as max_id" "${LATEST_FUNC}" "MAX(id) as max_id"
assert_contains "latest query has INNER JOIN" "${LATEST_FUNC}" "INNER JOIN"
assert_contains "latest query has GROUP BY device_id" "${LATEST_FUNC}" "GROUP BY device_id"
assert_contains "latest query joins on r.id = latest.max_id" "${LATEST_FUNC}" "r.id = latest.max_id"

# 5. COUNT query match
assert_contains "count query uses SELECT COUNT(*) FROM readings" "${DB_SH_CONTENTS}" "SELECT COUNT(*) FROM readings"

echo ""

# ═══════════════════════════════════════════════════════════════════
# TEST-SPEC.JSON INTEGRATION TESTS
# ═══════════════════════════════════════════════════════════════════

echo "--- test-spec.json integration tests ---"

SPEC_FILE="${SCRIPT_DIR}/../test-spec.json"
if [[ ! -f "${SPEC_FILE}" ]]; then
    SPEC_FILE="${SCRIPT_DIR}/../../test-spec.json"
fi

# --- Response shape: reading required fields ---
response=$(do_handler_request GET "/api/readings?limit=1")
body=$(get_body "${response}")
reading_count=$(jq 'length' <<< "${body}")
if (( reading_count > 0 )); then
    first_reading=$(jq '.[0]' <<< "${body}")
    # Check requiredFields are present
    while IFS= read -r field; do
        has_field=$(jq --arg f "${field}" 'has($f)' <<< "${first_reading}")
        assert_eq "reading has required field '${field}'" "true" "${has_field}"
    done < <(jq -r '.responseShapes.reading.requiredFields[]' "${SPEC_FILE}")
    # Check forbiddenFields are absent
    while IFS= read -r field; do
        has_field=$(jq --arg f "${field}" 'has($f)' <<< "${first_reading}")
        assert_eq "reading lacks forbidden field '${field}'" "false" "${has_field}"
    done < <(jq -r '.responseShapes.reading.forbiddenFields[]' "${SPEC_FILE}")
fi

# --- Response shape: device fields ---
response=$(do_handler_request GET "/api/devices")
body=$(get_body "${response}")
device_count=$(jq 'length' <<< "${body}")
if (( device_count > 0 )); then
    first_device=$(jq '.[0]' <<< "${body}")
    # Check requiredFields are present
    while IFS= read -r field; do
        has_field=$(jq --arg f "${field}" 'has($f)' <<< "${first_device}")
        assert_eq "device has required field '${field}'" "true" "${has_field}"
    done < <(jq -r '.responseShapes.device.requiredFields[]' "${SPEC_FILE}")
    # Check forbiddenFields are absent
    while IFS= read -r field; do
        has_field=$(jq --arg f "${field}" 'has($f)' <<< "${first_device}")
        assert_eq "device lacks forbidden field '${field}'" "false" "${has_field}"
    done < <(jq -r '.responseShapes.device.forbiddenFields[]' "${SPEC_FILE}")
fi

# --- Downsample bucket verification ---
while IFS= read -r bucket_json; do
    param=$(jq -r '.param' <<< "${bucket_json}")
    expected_ms=$(jq -r '.expectMs' <<< "${bucket_json}")
    actual_ms=$(jq -r --arg key "${param}" '.[$key] // empty' <<< "${AGTZ_DOWNSAMPLE_BUCKETS}")
    assert_eq "downsample bucket '${param}' resolves to ${expected_ms}ms" "${expected_ms}" "${actual_ms}"
done < <(jq -c '.downsampleBuckets[]' "${SPEC_FILE}")

# --- Query edge cases: from>to returns empty ---
response=$(do_handler_request GET "/api/readings?from=9999999999999&to=1")
status=$(get_status_code "${response}")
body=$(get_body "${response}")
assert_eq "from>to status 200" "200" "${status}"
assert_json_length "from>to returns empty array" "${body}" "0"

# --- Query edge cases: nonexistent device returns empty ---
response=$(do_handler_request GET "/api/readings?device=nonexistent-serial-xyz")
status=$(get_status_code "${response}")
body=$(get_body "${response}")
assert_eq "nonexistent device status 200" "200" "${status}"
assert_json_length "nonexistent device returns empty array" "${body}" "0"

echo ""

# ═══════════════════════════════════════════════════════════════════
# SUMMARY
# ═══════════════════════════════════════════════════════════════════

echo "=== Results ==="
echo "  PASSED: ${PASS}"
echo "  FAILED: ${FAIL}"

if [[ ${#ERRORS[@]} -gt 0 ]]; then
    echo ""
    echo "Failures:"
    for err in "${ERRORS[@]}"; do
        echo "  ${err}"
    done
fi

echo ""
echo "Total: $(( PASS + FAIL )) tests"

if (( FAIL > 0 )); then
    exit 1
fi
