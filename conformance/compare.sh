#!/usr/bin/env bash
set -euo pipefail

# ── Conformance comparator ───────────────────────────────────────────────────
# Compares API responses across multiple implementations.
# Usage: ./conformance/compare.sh name1:port1 name2:port2 [name3:port3 ...]
# Requires at least 2 implementations to compare.

CURL_TIMEOUT=10
RETRY_COUNT=3
RETRY_DELAY=1
PASS_COUNT=0
FAIL_COUNT=0
WARN_COUNT=0

log() { echo "[compare] $*" >&2; }

# ── Parse arguments ──────────────────────────────────────────────────────────

if [[ $# -lt 2 ]]; then
    echo "Usage: $0 name1:port1 name2:port2 [name3:port3 ...]" >&2
    exit 1
fi

declare -a IMPL_NAMES=()
declare -a IMPL_PORTS=()

for arg in "$@"; do
    name="${arg%%:*}"
    port="${arg##*:}"
    if [[ -z "${name}" || -z "${port}" || "${name}" == "${port}" ]]; then
        echo "ERROR: Invalid argument '${arg}', expected name:port" >&2
        exit 1
    fi
    IMPL_NAMES+=("${name}")
    IMPL_PORTS+=("${port}")
done

IMPL_COUNT="${#IMPL_NAMES[@]}"
REF_NAME="${IMPL_NAMES[0]}"

log "Comparing ${IMPL_COUNT} implementations (reference: ${REF_NAME})"

# ── Helper functions ─────────────────────────────────────────────────────────

# Fetch an endpoint, return "status_code<TAB>body"
# Retries RETRY_COUNT times on curl failure, logging stderr each time.
fetch() {
    local port="$1" path="$2"
    local url="http://localhost:${port}${path}"
    local tmpfile http_code="" attempt curl_exit
    tmpfile="$(mktemp)"

    for (( attempt=1; attempt<=RETRY_COUNT; attempt++ )); do
        http_code="$(curl -s -o "${tmpfile}" -w '%{http_code}' \
            --max-time "${CURL_TIMEOUT}" "${url}" 2>"${tmpfile}.err")" && break
        curl_exit=$?
        if [[ ${attempt} -lt ${RETRY_COUNT} ]]; then
            log "WARN: fetch ${url} attempt ${attempt}/${RETRY_COUNT} failed (curl exit ${curl_exit}): $(cat "${tmpfile}.err" 2>/dev/null)"
            sleep "${RETRY_DELAY}"
        else
            log "ERROR: fetch ${url} failed after ${RETRY_COUNT} attempts (curl exit ${curl_exit}): $(cat "${tmpfile}.err" 2>/dev/null)"
        fi
        http_code=""
    done

    rm -f "${tmpfile}.err"

    if [[ -z "${http_code}" || "${http_code}" == "000" ]]; then
        rm -f "${tmpfile}"
        printf '000\t'
        return
    fi

    local body
    body="$(cat "${tmpfile}")"
    rm -f "${tmpfile}"
    printf '%s\t%s' "${http_code}" "${body}"
}

# Get HTTP status code with retry. Extra curl args passed through.
# Usage: curl_status_with_retry PORT PATH [extra_curl_args...]
curl_status_with_retry() {
    local port="$1" path="$2"
    shift 2
    local url="http://localhost:${port}${path}"
    local code="" attempt curl_exit

    for (( attempt=1; attempt<=RETRY_COUNT; attempt++ )); do
        code="$(curl -s -o /dev/null -w '%{http_code}' --max-time "${CURL_TIMEOUT}" \
            "$@" "${url}" 2>/dev/null)" && break
        curl_exit=$?
        if [[ ${attempt} -lt ${RETRY_COUNT} ]]; then
            log "WARN: curl ${url} attempt ${attempt}/${RETRY_COUNT} failed (exit ${curl_exit})"
            sleep "${RETRY_DELAY}"
        else
            log "ERROR: curl ${url} failed after ${RETRY_COUNT} attempts (exit ${curl_exit})"
        fi
        code=""
    done

    printf '%s' "${code:-000}"
}

# Compare status codes across all implementations
# Returns 0 if all match, 1 if mismatch
check_status_codes() {
    local -n codes_ref=$1
    local ref_code="${codes_ref[0]}"
    local idx
    for (( idx=1; idx<IMPL_COUNT; idx++ )); do
        if [[ "${codes_ref[${idx}]}" != "${ref_code}" ]]; then
            return 1
        fi
    done
    return 0
}

report_pass() {
    local test_name="$1"
    echo "  PASS  ${test_name}"
    PASS_COUNT=$((PASS_COUNT + 1))
}

report_fail() {
    local test_name="$1"
    shift
    echo "  FAIL  ${test_name}"
    for detail in "$@"; do
        echo "        ${detail}"
    done
    FAIL_COUNT=$((FAIL_COUNT + 1))
}

report_warn() {
    local test_name="$1"
    shift
    echo "  WARN  ${test_name}"
    for detail in "$@"; do
        echo "        ${detail}"
    done
    WARN_COUNT=$((WARN_COUNT + 1))
}

# ── Normalization filters ────────────────────────────────────────────────────
# These functions are invoked indirectly via string variable in run_test().
# shellcheck disable=SC2329
normalize_readings() {
    jq -S '[.[] | del(.id) | .timestamp = 0] | sort_by(.device_id, .pm02)' 2>/dev/null || echo "null"
}

# shellcheck disable=SC2329
normalize_latest() {
    jq -S '[.[] | del(.id) | .timestamp = 0] | sort_by(.device_id)' 2>/dev/null || echo "null"
}

# shellcheck disable=SC2329
normalize_devices() {
    jq -S '[.[] | .last_seen = 0] | sort_by(.device_id)' 2>/dev/null || echo "null"
}

# shellcheck disable=SC2329
normalize_health() {
    jq -S '[.[] | .lastSuccess = 0 | .lastError = 0 | .lastAttempt = 0 | .consecutiveFailures = 0]' 2>/dev/null || echo "null"
}

normalize_health_no_message() {
    # Strip error messages and volatile fields for structural comparison
    jq -S '[.[] | .lastSuccess = 0 | .lastError = 0 | .lastAttempt = 0 | .consecutiveFailures = 0 | del(.lastErrorMessage)]' 2>/dev/null || echo "null"
}

# shellcheck disable=SC2329
normalize_config() {
    jq -S 'del(.port)' 2>/dev/null || echo "null"
}

# shellcheck disable=SC2329
normalize_identity() {
    jq -S '.' 2>/dev/null || echo "null"
}

# ── Test runner ──────────────────────────────────────────────────────────────

# run_test TEST_NAME ENDPOINT EXPECTED_STATUS NORMALIZE_FN [COMPARE_MODE]
# COMPARE_MODE: "full" (default), "status_only", "warn_message"
run_test() {
    local test_name="$1"
    local endpoint="$2"
    local expected_status="$3"
    local normalize_fn="$4"
    local compare_mode="${5:-full}"

    # Fetch from all implementations
    declare -a statuses=()
    declare -a bodies=()
    local idx
    for (( idx=0; idx<IMPL_COUNT; idx++ )); do
        local result
        result="$(fetch "${IMPL_PORTS[${idx}]}" "${endpoint}")"
        statuses+=("${result%%$'\t'*}")
        bodies+=("${result#*$'\t'}")
    done

    # Check expected status on reference
    if [[ "${expected_status}" != "*" && "${statuses[0]}" != "${expected_status}" ]]; then
        report_fail "${test_name}" \
            "Reference ${REF_NAME} returned ${statuses[0]}, expected ${expected_status}"
        return
    fi

    # Check status codes match across all impls
    local codes_match=true
    # shellcheck disable=SC2310
    check_status_codes statuses || codes_match=false
    if [[ "${codes_match}" == "false" ]]; then
        local details=()
        for (( idx=0; idx<IMPL_COUNT; idx++ )); do
            details+=("${IMPL_NAMES[${idx}]}: HTTP ${statuses[${idx}]}")
        done
        report_fail "${test_name}" "Status code mismatch:" "${details[@]}"
        return
    fi

    # For status_only mode, we just check status codes match
    if [[ "${compare_mode}" == "status_only" ]]; then
        report_pass "${test_name}"
        return
    fi

    # Normalize and compare bodies
    local ref_normalized
    ref_normalized="$(echo "${bodies[0]}" | ${normalize_fn})"

    local all_match=true
    declare -a diff_details=()

    for (( idx=1; idx<IMPL_COUNT; idx++ )); do
        local impl_normalized
        impl_normalized="$(echo "${bodies[${idx}]}" | ${normalize_fn})"

        if [[ "${ref_normalized}" != "${impl_normalized}" ]]; then
            all_match=false
            diff_details+=("${IMPL_NAMES[${idx}]} differs from ${REF_NAME}")

            # Show first few lines of diff
            local diff_output
            diff_output="$(diff <(echo "${ref_normalized}") <(echo "${impl_normalized}") | head -20)" || true
            if [[ -n "${diff_output}" ]]; then
                diff_details+=("${diff_output}")
            fi
        fi
    done

    # For warn_message mode, check if differences are only in error messages
    if [[ "${compare_mode}" == "warn_message" && "${all_match}" == "false" ]]; then
        local ref_no_msg all_structural_match
        ref_no_msg="$(echo "${bodies[0]}" | normalize_health_no_message)"
        all_structural_match=true
        for (( idx=1; idx<IMPL_COUNT; idx++ )); do
            local impl_no_msg
            impl_no_msg="$(echo "${bodies[${idx}]}" | normalize_health_no_message)"
            if [[ "${ref_no_msg}" != "${impl_no_msg}" ]]; then
                all_structural_match=false
                break
            fi
        done
        if [[ "${all_structural_match}" == "true" ]]; then
            report_warn "${test_name}" "Structural match but error message wording differs"
            return
        fi
    fi

    if [[ "${all_match}" == "true" ]]; then
        report_pass "${test_name}"
    else
        report_fail "${test_name}" "${diff_details[@]}"
    fi
}

# ── Test cases ───────────────────────────────────────────────────────────────

echo ""
echo "=== Conformance Test Results ==="
echo ""

# 1. GET /api/readings/latest
run_test \
    "GET /api/readings/latest" \
    "/api/readings/latest" \
    "200" \
    "normalize_latest" \
    "full"

# 2. GET /api/devices
run_test \
    "GET /api/devices" \
    "/api/devices" \
    "200" \
    "normalize_devices" \
    "full"

# 3. GET /api/health
run_test \
    "GET /api/health" \
    "/api/health" \
    "200" \
    "normalize_health" \
    "warn_message"

# 4. GET /api/config
run_test \
    "GET /api/config" \
    "/api/config" \
    "200" \
    "normalize_config" \
    "full"

# 5. GET /api/readings (all, wide time range)
run_test \
    "GET /api/readings?from=0&to=9999999999999" \
    "/api/readings?from=0&to=9999999999999" \
    "200" \
    "normalize_readings" \
    "full"

# 6. GET /api/readings with device filter
run_test \
    "GET /api/readings?device=indoor (device filter)" \
    "/api/readings?from=0&to=9999999999999&device=indoor" \
    "200" \
    "normalize_readings" \
    "full"

# 7. GET /api/readings/count
run_test \
    "GET /api/readings/count?from=0&to=9999999999999" \
    "/api/readings/count?from=0&to=9999999999999" \
    "200" \
    "normalize_identity" \
    "full"

# 8. GET /api/readings with downsample
run_test \
    "GET /api/readings?downsample=1h" \
    "/api/readings?from=0&to=9999999999999&downsample=1h" \
    "200" \
    "normalize_readings" \
    "full"

# 9. GET /api/readings with invalid downsample (400)
run_test \
    "GET /api/readings?downsample=invalid (400)" \
    "/api/readings?downsample=invalid" \
    "400" \
    "normalize_identity" \
    "status_only"

# 10. POST /api/readings (405)
# Uses curl -X POST directly since fetch() does GET
run_post_test() {
    local test_name="POST /api/readings (405)"
    declare -a statuses=()
    local idx

    for (( idx=0; idx<IMPL_COUNT; idx++ )); do
        local code
        code="$(curl_status_with_retry "${IMPL_PORTS[${idx}]}" "/api/readings" -X POST)"
        statuses+=("${code}")
    done

    if [[ "${statuses[0]}" != "405" ]]; then
        report_fail "${test_name}" \
            "Reference ${REF_NAME} returned ${statuses[0]}, expected 405"
        return
    fi

    local codes_match=true
    # shellcheck disable=SC2310
    check_status_codes statuses || codes_match=false
    if [[ "${codes_match}" == "false" ]]; then
        local details=()
        for (( idx=0; idx<IMPL_COUNT; idx++ )); do
            details+=("${IMPL_NAMES[${idx}]}: HTTP ${statuses[${idx}]}")
        done
        report_fail "${test_name}" "Status code mismatch:" "${details[@]}"
        return
    fi

    report_pass "${test_name}"
}
run_post_test

# 11. GET /nonexistent (404)
run_test \
    "GET /nonexistent (404)" \
    "/nonexistent" \
    "404" \
    "normalize_identity" \
    "status_only"

# 12. GET /api/stats (200, content volatile)
run_test \
    "GET /api/stats (status only)" \
    "/api/stats" \
    "200" \
    "normalize_identity" \
    "status_only"

# ── Downsample coverage ──────────────────────────────────────────────────────

# All remaining downsample buckets return 200
for bucket in 5m 10m 15m 30m 1d 1w; do
    run_test \
        "GET /api/readings?downsample=${bucket}" \
        "/api/readings?from=0&to=9999999999999&downsample=${bucket}" \
        "200" \
        "normalize_readings" \
        "full"
done

# Downsample with device filter
run_test \
    "GET /api/readings?downsample=1h&device=indoor" \
    "/api/readings?from=0&to=9999999999999&downsample=1h&device=indoor" \
    "200" \
    "normalize_readings" \
    "full"

# Downsample on empty time range
run_test \
    "GET /api/readings?downsample=1h (empty range)" \
    "/api/readings?from=9999999999999&to=9999999999999&downsample=1h" \
    "200" \
    "normalize_identity" \
    "full"

# ── Query parameter edge cases ──────────────────────────────────────────────

# limit=1 returns exactly 1 result
run_test \
    "GET /api/readings?limit=1 (exactly 1)" \
    "/api/readings?from=0&to=9999999999999&limit=1" \
    "200" \
    "normalize_readings" \
    "full"

# limit=0 returns results (not empty)
run_limit_zero_test() {
    local test_name="GET /api/readings?limit=0 (returns data)"
    declare -a statuses=()
    declare -a lengths=()
    local idx

    for (( idx=0; idx<IMPL_COUNT; idx++ )); do
        local result
        result="$(fetch "${IMPL_PORTS[${idx}]}" "/api/readings?from=0&to=9999999999999&limit=0")"
        statuses+=("${result%%$'\t'*}")
        local body="${result#*$'\t'}"
        lengths+=("$(echo "${body}" | jq 'length' 2>/dev/null || echo "0")")
    done

    local all_ok=true
    declare -a details=()
    for (( idx=0; idx<IMPL_COUNT; idx++ )); do
        if [[ "${statuses[${idx}]}" != "200" ]]; then
            all_ok=false
            details+=("${IMPL_NAMES[${idx}]}: HTTP ${statuses[${idx}]}")
        elif [[ "${lengths[${idx}]}" == "0" ]]; then
            all_ok=false
            details+=("${IMPL_NAMES[${idx}]}: returned 0 results (limit=0 should not mean empty)")
        fi
    done

    if [[ "${all_ok}" == "true" ]]; then
        report_pass "${test_name}"
    else
        report_fail "${test_name}" "${details[@]}"
    fi
}
run_limit_zero_test

# from > to returns empty
run_test \
    "GET /api/readings?from>to (empty)" \
    "/api/readings?from=9999999999999&to=1" \
    "200" \
    "normalize_identity" \
    "full"

# device=all returns same as no device param
run_test \
    "GET /api/readings?device=all" \
    "/api/readings?from=0&to=9999999999999&device=all" \
    "200" \
    "normalize_readings" \
    "full"

# nonexistent device returns empty
run_test \
    "GET /api/readings?device=nonexistent" \
    "/api/readings?from=0&to=9999999999999&device=nonexistent-xyz" \
    "200" \
    "normalize_identity" \
    "full"

# count with nonexistent device returns 0
run_test \
    "GET /api/readings/count?device=nonexistent" \
    "/api/readings/count?from=0&to=9999999999999&device=nonexistent-xyz" \
    "200" \
    "normalize_identity" \
    "full"

# count with from > to returns 0
run_test \
    "GET /api/readings/count?from>to" \
    "/api/readings/count?from=9999999999999&to=1" \
    "200" \
    "normalize_identity" \
    "full"

# ── Response shape validation ───────────────────────────────────────────────

# readings have no raw_json field
run_field_absence_test() {
    local test_name="$1" endpoint="$2" forbidden_field="$3"
    local idx

    for (( idx=0; idx<IMPL_COUNT; idx++ )); do
        local result
        result="$(fetch "${IMPL_PORTS[${idx}]}" "${endpoint}")"
        local body="${result#*$'\t'}"
        local has_field
        has_field="$(echo "${body}" | jq --arg f "${forbidden_field}" '[.[] | has($f)] | any' 2>/dev/null || echo "true")"
        if [[ "${has_field}" == "true" ]]; then
            report_fail "${test_name}" "${IMPL_NAMES[${idx}]}: response contains forbidden field '${forbidden_field}'"
            return
        fi
    done
    report_pass "${test_name}"
}

run_field_absence_test \
    "readings have no raw_json" \
    "/api/readings?from=0&to=9999999999999" \
    "raw_json"

run_field_absence_test \
    "devices have no first_seen" \
    "/api/devices" \
    "first_seen"

# /api/config has downsampleBuckets key (no deprecated downsampleThreshold)
run_config_shape_test() {
    local test_name="config has downsampleBuckets"
    local idx

    for (( idx=0; idx<IMPL_COUNT; idx++ )); do
        local result
        result="$(fetch "${IMPL_PORTS[${idx}]}" "/api/config")"
        local body="${result#*$'\t'}"
        local has_key
        has_key="$(echo "${body}" | jq 'has("downsampleBuckets")' 2>/dev/null || echo "false")"
        if [[ "${has_key}" != "true" ]]; then
            report_fail "${test_name}" "${IMPL_NAMES[${idx}]}: missing downsampleBuckets"
            return
        fi
        local has_old
        has_old="$(echo "${body}" | jq 'has("downsampleThreshold")' 2>/dev/null || echo "false")"
        if [[ "${has_old}" == "true" ]]; then
            report_fail "${test_name}" "${IMPL_NAMES[${idx}]}: still has deprecated downsampleThreshold"
            return
        fi
    done
    report_pass "${test_name}"
}
run_config_shape_test

# Error responses are {"error": "..."} format
run_error_shape_test() {
    local test_name="$1" endpoint="$2" expected_status="$3"
    local idx

    for (( idx=0; idx<IMPL_COUNT; idx++ )); do
        local result
        result="$(fetch "${IMPL_PORTS[${idx}]}" "${endpoint}")"
        local status="${result%%$'\t'*}"
        local body="${result#*$'\t'}"

        if [[ "${status}" != "${expected_status}" ]]; then
            report_fail "${test_name}" "${IMPL_NAMES[${idx}]}: HTTP ${status}, expected ${expected_status}"
            return
        fi

        local has_error_key
        has_error_key="$(echo "${body}" | jq 'has("error")' 2>/dev/null || echo "false")"
        if [[ "${has_error_key}" != "true" ]]; then
            report_fail "${test_name}" "${IMPL_NAMES[${idx}]}: response missing 'error' key"
            return
        fi

        local key_count
        key_count="$(echo "${body}" | jq 'keys | length' 2>/dev/null || echo "0")"
        if [[ "${key_count}" != "1" ]]; then
            report_fail "${test_name}" "${IMPL_NAMES[${idx}]}: error response has ${key_count} keys, expected 1"
            return
        fi
    done
    report_pass "${test_name}"
}

run_error_shape_test "400 error shape" "/api/readings?downsample=bogus" "400"
run_error_shape_test "404 error shape" "/api/nonexistent" "404"

# JSON responses have Content-Type: application/json
run_content_type_test() {
    local test_name="Content-Type: application/json"
    local idx

    for (( idx=0; idx<IMPL_COUNT; idx++ )); do
        local ct
        ct=""
        for (( _attempt=1; _attempt<=RETRY_COUNT; _attempt++ )); do
            ct="$(curl -s -o /dev/null -w '%{content_type}' --max-time "${CURL_TIMEOUT}" \
                "http://localhost:${IMPL_PORTS[${idx}]}/api/config" 2>/dev/null)" && break
            [[ ${_attempt} -lt ${RETRY_COUNT} ]] && sleep "${RETRY_DELAY}"
            ct=""
        done
        if [[ "${ct}" != *"application/json"* ]]; then
            report_fail "${test_name}" "${IMPL_NAMES[${idx}]}: Content-Type='${ct}'"
            return
        fi
    done
    report_pass "${test_name}"
}
run_content_type_test

# ── Security tests ──────────────────────────────────────────────────────────

# Path traversal returns 403
run_security_status_test() {
    local test_name="$1" method="$2" path="$3" expected_status="$4"
    local idx

    for (( idx=0; idx<IMPL_COUNT; idx++ )); do
        local code
        code="$(curl_status_with_retry "${IMPL_PORTS[${idx}]}" "${path}" --path-as-is -X "${method}")"

        if [[ "${code}" != "${expected_status}" ]]; then
            report_fail "${test_name}" "${IMPL_NAMES[${idx}]}: HTTP ${code}, expected ${expected_status}"
            return
        fi
    done
    report_pass "${test_name}"
}

run_security_status_test "path traversal blocked (403)" "GET" "/../../../etc/passwd" "403"
run_security_status_test "encoded traversal blocked (403)" "GET" "/%2e%2e/%2e%2e/etc/passwd" "403"

# No X-Powered-By header
run_no_powered_by_test() {
    local test_name="no X-Powered-By header"
    local idx

    for (( idx=0; idx<IMPL_COUNT; idx++ )); do
        local headers
        headers=""
        for (( _attempt=1; _attempt<=RETRY_COUNT; _attempt++ )); do
            headers="$(curl -sI --max-time "${CURL_TIMEOUT}" \
                "http://localhost:${IMPL_PORTS[${idx}]}/api/config" 2>/dev/null)" && break
            [[ ${_attempt} -lt ${RETRY_COUNT} ]] && sleep "${RETRY_DELAY}"
            headers=""
        done
        if echo "${headers}" | grep -qi "x-powered-by"; then
            report_fail "${test_name}" "${IMPL_NAMES[${idx}]}: sends X-Powered-By header"
            return
        fi
    done
    report_pass "${test_name}"
}
run_no_powered_by_test

# ── Ordering and selection tests ────────────────────────────────────────────

# Readings sorted ASC (first timestamp <= last timestamp)
run_sort_order_test() {
    local test_name="readings sorted ASC"
    local idx

    for (( idx=0; idx<IMPL_COUNT; idx++ )); do
        local result
        result="$(fetch "${IMPL_PORTS[${idx}]}" "/api/readings?from=0&to=9999999999999")"
        local body="${result#*$'\t'}"
        local count
        count="$(echo "${body}" | jq 'length' 2>/dev/null || echo "0")"
        if [[ "${count}" -lt 2 ]]; then
            continue
        fi
        local first_ts last_ts
        first_ts="$(echo "${body}" | jq '.[0].timestamp' 2>/dev/null || echo "0")"
        last_ts="$(echo "${body}" | jq '.[-1].timestamp' 2>/dev/null || echo "0")"
        if [[ "${first_ts}" -gt "${last_ts}" ]]; then
            report_fail "${test_name}" "${IMPL_NAMES[${idx}]}: first=${first_ts} > last=${last_ts}"
            return
        fi
    done
    report_pass "${test_name}"
}
run_sort_order_test

# Downsampled readings omit id field
run_downsample_no_id_test() {
    local test_name="downsampled readings omit id"
    local idx

    for (( idx=0; idx<IMPL_COUNT; idx++ )); do
        local result
        result="$(fetch "${IMPL_PORTS[${idx}]}" "/api/readings?from=0&to=9999999999999&downsample=1h")"
        local body="${result#*$'\t'}"
        local arr_len
        arr_len="$(echo "${body}" | jq 'length' 2>/dev/null || echo "0")"
        if [[ "${arr_len}" == "0" ]]; then
            continue
        fi
        local has_id
        has_id="$(echo "${body}" | jq '[.[] | has("id")] | any' 2>/dev/null || echo "true")"
        if [[ "${has_id}" == "true" ]]; then
            report_fail "${test_name}" "${IMPL_NAMES[${idx}]}: downsampled response contains 'id' field"
            return
        fi
    done
    report_pass "${test_name}"
}
run_downsample_no_id_test

# count response is exactly {"count": N}
run_count_shape_test() {
    local test_name="count response exact shape"
    local idx

    for (( idx=0; idx<IMPL_COUNT; idx++ )); do
        local result
        result="$(fetch "${IMPL_PORTS[${idx}]}" "/api/readings/count?from=0&to=9999999999999")"
        local body="${result#*$'\t'}"
        local key_count
        key_count="$(echo "${body}" | jq 'keys | length' 2>/dev/null || echo "0")"
        if [[ "${key_count}" != "1" ]]; then
            report_fail "${test_name}" "${IMPL_NAMES[${idx}]}: count response has ${key_count} keys, expected 1"
            return
        fi
        local has_count
        has_count="$(echo "${body}" | jq 'has("count")' 2>/dev/null || echo "false")"
        if [[ "${has_count}" != "true" ]]; then
            report_fail "${test_name}" "${IMPL_NAMES[${idx}]}: count response missing 'count' key"
            return
        fi
    done
    report_pass "${test_name}"
}
run_count_shape_test

# ── Web page smoke tests ─────────────────────────────────────────────────────
# Verify that static files are served correctly and contain expected structure

run_web_test() {
    local test_name="$1"
    local path="$2"
    local expected_status="$3"
    local required_content="$4"

    declare -a statuses=()
    declare -a bodies=()
    local idx
    for (( idx=0; idx<IMPL_COUNT; idx++ )); do
        local result
        result="$(fetch "${IMPL_PORTS[${idx}]}" "${path}")"
        statuses+=("${result%%$'\t'*}")
        bodies+=("${result#*$'\t'}")
    done

    # Check expected status on all impls
    local all_ok=true
    declare -a details=()
    for (( idx=0; idx<IMPL_COUNT; idx++ )); do
        if [[ "${statuses[${idx}]}" != "${expected_status}" ]]; then
            all_ok=false
            details+=("${IMPL_NAMES[${idx}]}: HTTP ${statuses[${idx}]}")
        fi
    done
    if [[ "${all_ok}" == "false" ]]; then
        report_fail "${test_name}" "Status code mismatch (expected ${expected_status}):" "${details[@]}"
        return
    fi

    # Check that required content exists in all responses
    local content_ok=true
    declare -a content_details=()
    for (( idx=0; idx<IMPL_COUNT; idx++ )); do
        if [[ "${bodies[${idx}]}" != *"${required_content}"* ]]; then
            content_ok=false
            content_details+=("${IMPL_NAMES[${idx}]}: missing '${required_content}'")
        fi
    done
    if [[ "${content_ok}" == "false" ]]; then
        report_fail "${test_name}" "${content_details[@]}"
        return
    fi

    report_pass "${test_name}"
}

# 13. GET / (index.html)
run_web_test \
    "GET / (index.html)" \
    "/" \
    "200" \
    "AirGradient Dashboard"

# 14. GET /style.css
run_web_test \
    "GET /style.css" \
    "/style.css" \
    "200" \
    "box-sizing"

# 15. GET /app.js
run_web_test \
    "GET /app.js" \
    "/app.js" \
    "200" \
    "fetchReadings"

# 16. HTML structure: has chart containers
run_web_test \
    "HTML has chart section" \
    "/" \
    "200" \
    "id=\"charts\""

# 17. HTML structure: has device-status area
run_web_test \
    "HTML has device-status" \
    "/" \
    "200" \
    "id=\"device-status\""

# 18. HTML structure: has time-range nav
run_web_test \
    "HTML has time-range nav" \
    "/" \
    "200" \
    "id=\"time-range\""

# 19. HTML structure: has current-values section
run_web_test \
    "HTML has current-values" \
    "/" \
    "200" \
    "id=\"current-values\""

# 20. CSS has chart-card class
run_web_test \
    "CSS has chart-card class" \
    "/style.css" \
    "200" \
    ".chart-card"

# 21. JS references API endpoints
run_web_test \
    "JS calls /api/readings" \
    "/app.js" \
    "200" \
    "/api/readings"

# ── Summary ──────────────────────────────────────────────────────────────────

echo ""
echo "=== Summary ==="
echo "  Passed:   ${PASS_COUNT}"
echo "  Failed:   ${FAIL_COUNT}"
echo "  Warnings: ${WARN_COUNT}"
echo ""

if [[ ${FAIL_COUNT} -gt 0 ]]; then
    exit 1
fi
exit 0
