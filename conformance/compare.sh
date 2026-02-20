#!/usr/bin/env bash
set -euo pipefail

# ── Cross-implementation conformance comparator ──────────────────────────────
# Phase 1: Run runner.sh per implementation for spec compliance.
# Phase 2: Cross-impl response diffing for volatile/relative data.
#
# Usage: ./conformance/compare.sh name1:port1 name2:port2 [name3:port3 ...]
# Requires at least 2 implementations to compare.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
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

# ── Phase 1: Spec compliance per implementation ─────────────────────────────

echo ""
echo "=== Phase 1: Spec Compliance ==="

runner_failures=0
for (( idx=0; idx<IMPL_COUNT; idx++ )); do
    echo ""
    echo "--- ${IMPL_NAMES[${idx}]} (port ${IMPL_PORTS[${idx}]}) ---"
    if ! bash "${SCRIPT_DIR}/runner.sh" "localhost:${IMPL_PORTS[${idx}]}"; then
        runner_failures=$((runner_failures + 1))
        log "FAIL: ${IMPL_NAMES[${idx}]} failed spec compliance"
    fi
done

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
            log "WARN: fetch ${url} attempt ${attempt}/${RETRY_COUNT} failed (curl exit ${curl_exit}): $(cat "${tmpfile}.err" 2>/dev/null || true)"
            sleep "${RETRY_DELAY}"
        else
            log "ERROR: fetch ${url} failed after ${RETRY_COUNT} attempts (curl exit ${curl_exit}): $(cat "${tmpfile}.err" 2>/dev/null || true)"
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
    jq -S '[.[] | .lastSuccess = 0 | .lastError = 0 | .consecutiveFailures = 0]' 2>/dev/null || echo "null"
}

normalize_health_no_message() {
    # Strip error messages and volatile fields for structural comparison
    jq -S '[.[] | .lastSuccess = 0 | .lastError = 0 | .consecutiveFailures = 0 | del(.lastErrorMessage)]' 2>/dev/null || echo "null"
}

# shellcheck disable=SC2329
normalize_config() {
    jq -S 'del(.port)' 2>/dev/null || echo "null"
}

# shellcheck disable=SC2329
normalize_identity() {
    jq -S '.' 2>/dev/null || echo "null"
}

# ── Phase 2: Cross-implementation response diffing ──────────────────────────

# run_cross_test TEST_NAME ENDPOINT NORMALIZE_FN [COMPARE_MODE]
# COMPARE_MODE: "full" (default), "status_only", "warn_message"
run_cross_test() {
    local test_name="$1"
    local endpoint="$2"
    local normalize_fn="$3"
    local compare_mode="${4:-full}"

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

    # Check status codes match across all impls
    local ref_status="${statuses[0]}"
    local codes_match=true
    for (( idx=1; idx<IMPL_COUNT; idx++ )); do
        if [[ "${statuses[${idx}]}" != "${ref_status}" ]]; then
            codes_match=false
            break
        fi
    done

    if [[ "${codes_match}" == "false" ]]; then
        local details=()
        for (( idx=0; idx<IMPL_COUNT; idx++ )); do
            details+=("${IMPL_NAMES[${idx}]}: HTTP ${statuses[${idx}]}")
        done
        report_fail "${test_name}" "Status code mismatch:" "${details[@]}"
        return
    fi

    # For status_only mode, just check status codes match
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

echo ""
echo "=== Phase 2: Cross-Implementation Comparison ==="
echo ""

run_cross_test "readings match"     "/api/readings?from=0&to=9999999999999"                "normalize_readings"
run_cross_test "latest match"       "/api/readings/latest"                                  "normalize_latest"
run_cross_test "devices match"      "/api/devices"                                          "normalize_devices"
run_cross_test "health match"       "/api/health"                                           "normalize_health" "warn_message"
run_cross_test "config match"       "/api/config"                                           "normalize_config"
run_cross_test "count match"        "/api/readings/count?from=0&to=9999999999999"           "normalize_identity"
run_cross_test "downsample match"   "/api/readings?from=0&to=9999999999999&downsample=1h"   "normalize_readings"
run_cross_test "device filter"      "/api/readings?from=0&to=9999999999999&device=indoor"   "normalize_readings"
run_cross_test "empty range"        "/api/readings?from=9999999999999&to=1"                 "normalize_identity"
run_cross_test "nonexistent device" "/api/readings?from=0&to=9999999999999&device=nonexistent-xyz" "normalize_identity"
run_cross_test "count from>to"      "/api/readings/count?from=9999999999999&to=1"           "normalize_identity"
run_cross_test "stats (status)"     "/api/stats"                                            "normalize_identity" "status_only"

# ── Summary ──────────────────────────────────────────────────────────────────

echo ""
echo "=== Summary ==="
echo "  Runner failures: ${runner_failures}"
echo "  Cross-impl passed:   ${PASS_COUNT}"
echo "  Cross-impl failed:   ${FAIL_COUNT}"
echo "  Cross-impl warnings: ${WARN_COUNT}"
echo ""

if [[ ${runner_failures} -gt 0 || ${FAIL_COUNT} -gt 0 ]]; then
    exit 1
fi
exit 0
