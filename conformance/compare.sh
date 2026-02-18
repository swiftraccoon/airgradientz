#!/usr/bin/env bash
set -euo pipefail

# ── Conformance comparator ───────────────────────────────────────────────────
# Compares API responses across multiple implementations.
# Usage: ./conformance/compare.sh name1:port1 name2:port2 [name3:port3 ...]
# Requires at least 2 implementations to compare.

CURL_TIMEOUT=10
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
fetch() {
    local port="$1" path="$2"
    local url="http://localhost:${port}${path}"
    local tmpfile
    tmpfile="$(mktemp)"
    local http_code
    http_code="$(curl -s -o "${tmpfile}" -w '%{http_code}' --max-time "${CURL_TIMEOUT}" "${url}" 2>/dev/null)" || {
        rm -f "${tmpfile}"
        printf '000\t'
        return
    }
    local body
    body="$(cat "${tmpfile}")"
    rm -f "${tmpfile}"
    printf '%s\t%s' "${http_code}" "${body}"
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
    jq -S '[.[] | del(.id) | .timestamp = 0]' 2>/dev/null || echo "null"
}

# shellcheck disable=SC2329
normalize_latest() {
    jq -S '[.[] | del(.id) | .timestamp = 0]' 2>/dev/null || echo "null"
}

# shellcheck disable=SC2329
normalize_devices() {
    jq -S '[.[] | .last_seen = 0] | sort_by(.device_id)' 2>/dev/null || echo "null"
}

# shellcheck disable=SC2329
normalize_health() {
    jq -S '[.[] | .lastSuccess = 0 | .lastError = 0 | .lastAttempt = 0]' 2>/dev/null || echo "null"
}

normalize_health_no_message() {
    # Strip error messages for structural comparison
    jq -S '[.[] | .lastSuccess = 0 | .lastError = 0 | .lastAttempt = 0 | del(.lastErrorMessage)]' 2>/dev/null || echo "null"
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
    local message_diff=false
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

        # For warn_message mode, also check without messages
        if [[ "${compare_mode}" == "warn_message" && "${all_match}" == "false" ]]; then
            local ref_no_msg impl_no_msg
            ref_no_msg="$(echo "${bodies[0]}" | normalize_health_no_message)"
            impl_no_msg="$(echo "${bodies[${idx}]}" | normalize_health_no_message)"
            if [[ "${ref_no_msg}" == "${impl_no_msg}" ]]; then
                message_diff=true
                all_match=true  # structural match, only message differs
                diff_details=()
            fi
        fi
    done

    if [[ "${all_match}" == "true" && "${message_diff}" == "true" ]]; then
        report_warn "${test_name}" "Structural match but error message wording differs"
        return
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
        code="$(curl -s -o /dev/null -w '%{http_code}' --max-time "${CURL_TIMEOUT}" \
            -X POST "http://localhost:${IMPL_PORTS[${idx}]}/api/readings" 2>/dev/null)" || code="000"
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
