#!/usr/bin/env bash
set -euo pipefail

# ── JSON-driven HTTP test runner ─────────────────────────────────────────────
# Reads httpTests from test-spec.json, runs each test via curl against a
# given host:port, checks all assertions, and reports pass/fail.
#
# Usage: ./conformance/runner.sh HOST:PORT [--spec PATH]
#        e.g. ./conformance/runner.sh localhost:3011
#             ./conformance/runner.sh localhost:3011 --spec ./test-spec.json

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

CURL_TIMEOUT=10
PASS_COUNT=0
FAIL_COUNT=0
TOTAL_COUNT=0

# ── Argument parsing ────────────────────────────────────────────────────────

SPEC_PATH="${REPO_ROOT}/test-spec.json"
HOST_PORT=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --spec)
            SPEC_PATH="$2"
            shift 2
            ;;
        --spec=*)
            SPEC_PATH="${1#*=}"
            shift
            ;;
        -*)
            echo "Usage: $0 HOST:PORT [--spec PATH]" >&2
            exit 1
            ;;
        *)
            if [[ -z "${HOST_PORT}" ]]; then
                HOST_PORT="$1"
            else
                echo "ERROR: Unexpected argument: $1" >&2
                echo "Usage: $0 HOST:PORT [--spec PATH]" >&2
                exit 1
            fi
            shift
            ;;
    esac
done

if [[ -z "${HOST_PORT}" ]]; then
    echo "Usage: $0 HOST:PORT [--spec PATH]" >&2
    exit 1
fi

if [[ ! -f "${SPEC_PATH}" ]]; then
    echo "ERROR: Spec file not found: ${SPEC_PATH}" >&2
    exit 1
fi

# Dependency check
for cmd in curl jq; do
    if ! command -v "${cmd}" &>/dev/null; then
        echo "ERROR: Missing required dependency: ${cmd}" >&2
        exit 1
    fi
done

# ── Load spec data ──────────────────────────────────────────────────────────

SPEC_JSON="$(< "${SPEC_PATH}")"
TEST_COUNT="$(echo "${SPEC_JSON}" | jq '.httpTests | length')"
RESPONSE_SHAPES="$(echo "${SPEC_JSON}" | jq -c '.responseShapes')"

if [[ "${TEST_COUNT}" -eq 0 ]]; then
    echo "ERROR: No httpTests found in ${SPEC_PATH}" >&2
    exit 1
fi

# ── Temp files ──────────────────────────────────────────────────────────────

TMPBODY="$(mktemp)"
TMPHEADERS="$(mktemp)"
# shellcheck disable=SC2329  # invoked via trap
cleanup() {
    rm -f "${TMPBODY}" "${TMPHEADERS}"
}
trap cleanup EXIT INT TERM

# ── Reporting ───────────────────────────────────────────────────────────────

report_pass() {
    echo "  PASS  $1"
    PASS_COUNT=$((PASS_COUNT + 1))
}

report_fail() {
    local test_name="$1"
    shift
    echo "  FAIL  ${test_name} -- $*"
    FAIL_COUNT=$((FAIL_COUNT + 1))
}

# ── Header lookup (case-insensitive) ────────────────────────────────────────
# Reads TMPHEADERS, finds header by name (case-insensitive), prints value.
# Returns 1 if header not found.

get_header_value() {
    local name="$1"
    local line
    # grep case-insensitive for "Header-Name:" then extract value
    line="$(grep -i "^${name}:" "${TMPHEADERS}" | head -1)" || return 1
    # Strip header name, leading/trailing whitespace, and trailing CR
    local value="${line#*:}"
    value="${value#"${value%%[![:space:]]*}"}"
    value="${value%$'\r'}"
    value="${value%"${value##*[![:space:]]}"}"
    printf '%s' "${value}"
}

# ── Body shape validation ───────────────────────────────────────────────────
# Validates a JSON object against a responseShapes entry.
# Arguments: shape_name body_json
# Prints error message on failure, empty string on success.

validate_shape() {
    local shape_name="$1"
    local body="$2"

    local shape
    shape="$(echo "${RESPONSE_SHAPES}" | jq -c --arg s "${shape_name}" '.[$s] // empty')"
    if [[ -z "${shape}" ]]; then
        echo "unknown shape '${shape_name}'"
        return
    fi

    # Determine the object to validate:
    # - If body is an array, validate first element (empty arrays pass)
    # - If body is an object, validate the object directly
    local obj_to_check
    local body_type
    body_type="$(echo "${body}" | jq -r 'type' 2>/dev/null)" || body_type="unknown"

    if [[ "${body_type}" == "array" ]]; then
        local arr_len
        arr_len="$(echo "${body}" | jq 'length')"
        if [[ "${arr_len}" -eq 0 ]]; then
            # Empty arrays pass shape validation
            return
        fi
        obj_to_check="$(echo "${body}" | jq -c '.[0]')"
    elif [[ "${body_type}" == "object" ]]; then
        obj_to_check="$(echo "${body}" | jq -c '.')"
    else
        echo "body is ${body_type}, expected array or object"
        return
    fi

    # Check requiredFields
    local required
    required="$(echo "${shape}" | jq -c '.requiredFields // []')"
    local req_len
    req_len="$(echo "${required}" | jq 'length')"
    local i field has_it
    for (( i=0; i<req_len; i++ )); do
        field="$(echo "${required}" | jq -r ".[${i}]")"
        has_it="$(echo "${obj_to_check}" | jq --arg f "${field}" 'has($f)')"
        if [[ "${has_it}" != "true" ]]; then
            echo "missing required field '${field}'"
            return
        fi
    done

    # Check forbiddenFields
    local forbidden
    forbidden="$(echo "${shape}" | jq -c '.forbiddenFields // []')"
    local forb_len
    forb_len="$(echo "${forbidden}" | jq 'length')"
    for (( i=0; i<forb_len; i++ )); do
        field="$(echo "${forbidden}" | jq -r ".[${i}]")"
        has_it="$(echo "${obj_to_check}" | jq --arg f "${field}" 'has($f)')"
        if [[ "${has_it}" == "true" ]]; then
            echo "forbidden field '${field}' present"
            return
        fi
    done

    # Check exactFields + noExtraFields
    local no_extra
    no_extra="$(echo "${shape}" | jq -r '.noExtraFields // false')"
    local exact_fields
    exact_fields="$(echo "${shape}" | jq -c '.exactFields // []')"
    local exact_len
    exact_len="$(echo "${exact_fields}" | jq 'length')"

    if [[ "${no_extra}" == "true" && "${exact_len}" -gt 0 ]]; then
        # Check that all exactFields are present
        for (( i=0; i<exact_len; i++ )); do
            field="$(echo "${exact_fields}" | jq -r ".[${i}]")"
            has_it="$(echo "${obj_to_check}" | jq --arg f "${field}" 'has($f)')"
            if [[ "${has_it}" != "true" ]]; then
                echo "missing exact field '${field}'"
                return
            fi
        done

        # Check that there are no extra fields
        local actual_keys expected_keys
        actual_keys="$(echo "${obj_to_check}" | jq -c '[keys[]] | sort')"
        expected_keys="$(echo "${exact_fields}" | jq -c 'sort')"
        if [[ "${actual_keys}" != "${expected_keys}" ]]; then
            echo "extra fields: expected ${expected_keys}, got ${actual_keys}"
            return
        fi
    fi
}

# ── Run a single test ───────────────────────────────────────────────────────

run_test() {
    local test_json="$1"
    local test_name method path expect

    test_name="$(echo "${test_json}" | jq -r '.name')"
    method="$(echo "${test_json}" | jq -r '.method')"
    path="$(echo "${test_json}" | jq -r '.path')"
    expect="$(echo "${test_json}" | jq -c '.expect')"

    TOTAL_COUNT=$((TOTAL_COUNT + 1))

    local url="http://${HOST_PORT}${path}"

    # Execute curl: capture status code, headers, and body
    local http_code
    http_code="$(curl -s -o "${TMPBODY}" -D "${TMPHEADERS}" -w '%{http_code}' \
        --max-time "${CURL_TIMEOUT}" -X "${method}" --path-as-is "${url}" 2>/dev/null)" || http_code="000"

    local body
    body="$(< "${TMPBODY}")"

    # ── Assert: status ──────────────────────────────────────────────────
    local expected_status
    expected_status="$(echo "${expect}" | jq -r '.status // empty')"
    if [[ -n "${expected_status}" && "${http_code}" != "${expected_status}" ]]; then
        report_fail "${test_name}" "status: got ${http_code}, expected ${expected_status}"
        return
    fi

    # ── Assert: headers (substring match, case-insensitive lookup) ──────
    local headers_obj
    headers_obj="$(echo "${expect}" | jq -c '.headers // empty')"
    if [[ -n "${headers_obj}" && "${headers_obj}" != "null" ]]; then
        local header_names
        header_names="$(echo "${headers_obj}" | jq -r 'keys[]')"
        local hdr_name expected_val actual_val
        while IFS= read -r hdr_name; do
            [[ -z "${hdr_name}" ]] && continue
            expected_val="$(echo "${headers_obj}" | jq -r --arg h "${hdr_name}" '.[$h]')"
            # shellcheck disable=SC2310  # set -e disabled in || is intentional
            actual_val="$(get_header_value "${hdr_name}")" || actual_val=""

            if [[ -z "${actual_val}" ]]; then
                report_fail "${test_name}" "header '${hdr_name}': not present"
                return
            fi

            # Empty expected_val means just check existence
            if [[ -n "${expected_val}" ]]; then
                if [[ "${actual_val}" != *"${expected_val}"* ]]; then
                    report_fail "${test_name}" "header '${hdr_name}': got '${actual_val}', expected '${expected_val}'"
                    return
                fi
            fi
        done <<< "${header_names}"
    fi

    # ── Assert: headersAbsent ───────────────────────────────────────────
    local absent_arr
    absent_arr="$(echo "${expect}" | jq -c '.headersAbsent // empty')"
    if [[ -n "${absent_arr}" && "${absent_arr}" != "null" ]]; then
        local absent_len absent_hdr
        absent_len="$(echo "${absent_arr}" | jq 'length')"
        local ai
        for (( ai=0; ai<absent_len; ai++ )); do
            absent_hdr="$(echo "${absent_arr}" | jq -r ".[${ai}]")"
            # shellcheck disable=SC2310  # set -e disabled in if is intentional
            if get_header_value "${absent_hdr}" >/dev/null 2>&1; then
                report_fail "${test_name}" "header '${absent_hdr}' should be absent but is present"
                return
            fi
        done
    fi

    # ── Assert: bodyContains ────────────────────────────────────────────
    local body_contains
    body_contains="$(echo "${expect}" | jq -r '.bodyContains // empty')"
    if [[ -n "${body_contains}" ]]; then
        if [[ "${body}" != *"${body_contains}"* ]]; then
            report_fail "${test_name}" "body does not contain '${body_contains}'"
            return
        fi
    fi

    # ── Assert: bodyIsEmptyArray ────────────────────────────────────────
    local body_empty_arr
    body_empty_arr="$(echo "${expect}" | jq -r '.bodyIsEmptyArray // empty')"
    if [[ "${body_empty_arr}" == "true" ]]; then
        local trimmed="${body//[[:space:]]/}"
        if [[ "${trimmed}" != "[]" ]]; then
            local preview
            preview="$(echo "${body}" | head -c 80 || true)"
            report_fail "${test_name}" "expected empty array [], got: ${preview}"
            return
        fi
    fi

    # ── Assert: bodyHasResults ──────────────────────────────────────────
    local body_has_results
    body_has_results="$(echo "${expect}" | jq -r '.bodyHasResults // empty')"
    if [[ "${body_has_results}" == "true" ]]; then
        local arr_len
        arr_len="$(echo "${body}" | jq 'type == "array" and length > 0' 2>/dev/null)" || arr_len="false"
        if [[ "${arr_len}" != "true" ]]; then
            report_fail "${test_name}" "expected non-empty JSON array"
            return
        fi
    fi

    # ── Assert: bodyLength ──────────────────────────────────────────────
    local body_length
    body_length="$(echo "${expect}" | jq -r '.bodyLength // empty')"
    if [[ -n "${body_length}" ]]; then
        local actual_len
        actual_len="$(echo "${body}" | jq 'length' 2>/dev/null)" || actual_len="-1"
        if [[ "${actual_len}" != "${body_length}" ]]; then
            report_fail "${test_name}" "body length: got ${actual_len}, expected ${body_length}"
            return
        fi
    fi

    # ── Assert: bodyExact ───────────────────────────────────────────────
    local body_exact
    body_exact="$(echo "${expect}" | jq -c '.bodyExact // empty')"
    if [[ -n "${body_exact}" && "${body_exact}" != "null" ]]; then
        local expected_sorted actual_sorted
        expected_sorted="$(echo "${body_exact}" | jq -cS '.')"
        actual_sorted="$(echo "${body}" | jq -cS '.' 2>/dev/null)" || actual_sorted=""
        if [[ "${actual_sorted}" != "${expected_sorted}" ]]; then
            report_fail "${test_name}" "body mismatch: got ${actual_sorted}, expected ${expected_sorted}"
            return
        fi
    fi

    # ── Assert: bodyShape ───────────────────────────────────────────────
    local body_shape
    body_shape="$(echo "${expect}" | jq -r '.bodyShape // empty')"
    if [[ -n "${body_shape}" ]]; then
        local shape_err
        shape_err="$(validate_shape "${body_shape}" "${body}")"
        if [[ -n "${shape_err}" ]]; then
            report_fail "${test_name}" "shape '${body_shape}': ${shape_err}"
            return
        fi
    fi

    # ── Assert: bodyFieldAbsent ─────────────────────────────────────────
    local body_field_absent
    body_field_absent="$(echo "${expect}" | jq -r '.bodyFieldAbsent // empty')"
    if [[ -n "${body_field_absent}" ]]; then
        local has_field
        has_field="$(echo "${body}" | jq --arg f "${body_field_absent}" \
            'if type == "array" then [.[] | has($f)] | any else has($f) end' 2>/dev/null)" || has_field="false"
        if [[ "${has_field}" == "true" ]]; then
            report_fail "${test_name}" "field '${body_field_absent}' should be absent"
            return
        fi
    fi

    # ── Assert: noIdField ───────────────────────────────────────────────
    local no_id_field
    no_id_field="$(echo "${expect}" | jq -r '.noIdField // empty')"
    if [[ "${no_id_field}" == "true" ]]; then
        local has_id
        has_id="$(echo "${body}" | jq \
            'if type == "array" then [.[] | has("id")] | any else has("id") end' 2>/dev/null)" || has_id="false"
        if [[ "${has_id}" == "true" ]]; then
            report_fail "${test_name}" "response contains 'id' field (should be absent)"
            return
        fi
    fi

    # ── Assert: arraySortedAsc ──────────────────────────────────────────
    local sorted_field
    sorted_field="$(echo "${expect}" | jq -r '.arraySortedAsc // empty')"
    if [[ -n "${sorted_field}" ]]; then
        local is_sorted
        is_sorted="$(echo "${body}" | jq --arg f "${sorted_field}" '
            if type != "array" or length < 2 then true
            else
                [.[] | .[$f]] as $vals |
                ($vals | sort) as $sorted |
                $vals == $sorted
            end
        ' 2>/dev/null)" || is_sorted="true"
        if [[ "${is_sorted}" != "true" ]]; then
            report_fail "${test_name}" "array not sorted ascending by '${sorted_field}'"
            return
        fi
    fi

    # All assertions passed
    report_pass "${test_name}"
}

# ── Main ────────────────────────────────────────────────────────────────────

echo ""
echo "=== HTTP Test Results (${HOST_PORT}) ==="
echo ""

for (( idx=0; idx<TEST_COUNT; idx++ )); do
    test_json="$(echo "${SPEC_JSON}" | jq -c ".httpTests[${idx}]")"
    run_test "${test_json}"
done

echo ""
echo "Summary: ${PASS_COUNT}/${TOTAL_COUNT} passed, ${FAIL_COUNT} failed"

if [[ "${FAIL_COUNT}" -gt 0 ]]; then
    exit 1
fi
exit 0
