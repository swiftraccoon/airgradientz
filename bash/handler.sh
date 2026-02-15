#!/usr/bin/env bash
# Per-connection HTTP handler, invoked by ncat -c.
# stdin/stdout are wired to the TCP socket.
set -euo pipefail

# AGTZ_SCRIPT_DIR is exported by server.sh and inherited via ncat
SCRIPT_DIR="${AGTZ_SCRIPT_DIR:?AGTZ_SCRIPT_DIR not set}"

# shellcheck source=lib/config.sh
source "${SCRIPT_DIR}/lib/config.sh"
# shellcheck source=lib/http.sh
source "${SCRIPT_DIR}/lib/http.sh"
# shellcheck source=lib/db.sh
source "${SCRIPT_DIR}/lib/db.sh"

RUN_DIR="${SCRIPT_DIR}/run"
PUBLIC_DIR="${SCRIPT_DIR}/public"

# --- Route handlers ---

handle_readings() {
    local now_ts default_from from_ts to_ts device limit requested_limit
    now_ts=$(now_ms)
    default_from=$(( now_ts - 24 * 60 * 60 * 1000 ))

    from_ts=$(parse_query_param "${QUERY_STRING}" "from" "${default_from}")
    to_ts=$(parse_query_param "${QUERY_STRING}" "to" "${now_ts}")
    device=$(parse_query_param "${QUERY_STRING}" "device" "")
    requested_limit=$(parse_query_param "${QUERY_STRING}" "limit" "${AGTZ_MAX_API_ROWS}")

    # Validate integers
    [[ "${from_ts}" =~ ^-?[0-9]+$ ]] || from_ts="${default_from}"
    [[ "${to_ts}" =~ ^-?[0-9]+$ ]] || to_ts="${now_ts}"
    [[ "${requested_limit}" =~ ^[0-9]+$ ]] || requested_limit="${AGTZ_MAX_API_ROWS}"

    limit="${AGTZ_MAX_API_ROWS}"
    if (( requested_limit > 0 && requested_limit < AGTZ_MAX_API_ROWS )); then
        limit="${requested_limit}"
    fi

    [[ -z "${device}" ]] && device="all"

    local result
    result=$(query_readings "${device}" "${from_ts}" "${to_ts}" "${limit}")
    send_json "200 OK" "${result}"
}

handle_readings_latest() {
    local result
    result=$(get_latest_readings)
    send_json "200 OK" "${result}"
}

handle_devices() {
    local result
    result=$(get_devices)
    send_json "200 OK" "${result}"
}

handle_health() {
    local health
    if [[ -f "${RUN_DIR}/health.json" ]]; then
        health=$(< "${RUN_DIR}/health.json")
    else
        health="[]"
    fi
    send_json "200 OK" "${health}"
}

handle_config() {
    local devices_arr
    devices_arr=$(jq -c '[.[] | {ip, label}]' <<< "${AGTZ_DEVICES_JSON}")
    local result
    result=$(jq -cn --argjson interval "${AGTZ_POLL_INTERVAL_MS}" \
                    --argjson devices "${devices_arr}" \
                    '{"pollIntervalMs": $interval, "devices": $devices}')
    send_json "200 OK" "${result}"
}

handle_stats() {
    local now_ts started_at uptime_ms pid rss_bytes db_size readings_count
    local requests_served poll_successes poll_failures

    now_ts=$(now_ms)
    if [[ -f "${RUN_DIR}/started_at" ]]; then
        started_at=$(< "${RUN_DIR}/started_at")
    else
        started_at="${now_ts}"
    fi
    uptime_ms=$(( now_ts - started_at ))
    if [[ -f "${RUN_DIR}/server_pid" ]]; then
        pid=$(< "${RUN_DIR}/server_pid")
    else
        pid=$$
    fi

    # Read RSS from /proc
    rss_bytes=0
    if [[ -f "/proc/${pid}/statm" ]]; then
        local statm pages
        statm=$(< "/proc/${pid}/statm")
        pages=$(cut -d' ' -f2 <<< "${statm}")
        rss_bytes=$(( pages * 4096 ))
    fi

    db_size=$(get_db_size)
    readings_count=$(get_readings_count)
    requests_served=$(read_counter "${RUN_DIR}/requests")
    poll_successes=$(read_counter "${RUN_DIR}/poll_successes")
    poll_failures=$(read_counter "${RUN_DIR}/poll_failures")

    local result
    result=$(jq -cn \
        --arg impl "bash" \
        --argjson pid "${pid}" \
        --argjson uptime "${uptime_ms}" \
        --argjson rss "${rss_bytes}" \
        --argjson dbsize "${db_size}" \
        --argjson readings "${readings_count}" \
        --argjson requests "${requests_served}" \
        --argjson conns 0 \
        --argjson psuc "${poll_successes}" \
        --argjson pfail "${poll_failures}" \
        --argjson started "${started_at}" \
        '{
            "implementation": $impl,
            "pid": $pid,
            "uptime_ms": $uptime,
            "memory_rss_bytes": $rss,
            "db_size_bytes": $dbsize,
            "readings_count": $readings,
            "requests_served": $requests,
            "active_connections": $conns,
            "poll_successes": $psuc,
            "poll_failures": $pfail,
            "pool_alloc_count": 0,
            "pool_bytes_used": 0,
            "started_at": $started
        }')
    send_json "200 OK" "${result}"
}

handle_static() {
    local decoded_path
    decoded_path=$(url_decode "${REQUEST_PATH}")

    # Reject paths containing ..
    if [[ "${decoded_path}" == *'..'* ]]; then
        send_error "404 Not Found" "Not found"
        return
    fi

    # Reject control characters
    if [[ "${decoded_path}" =~ [[:cntrl:]] ]]; then
        send_error "404 Not Found" "Not found"
        return
    fi

    local relative="${decoded_path#/}"
    local file_path
    if [[ -z "${relative}" ]]; then
        file_path="${PUBLIC_DIR}/index.html"
    else
        file_path="${PUBLIC_DIR}/${relative}"
    fi

    # Resolve symlinks and verify under public/
    local resolved public_resolved
    resolved=$(realpath "${file_path}" 2>/dev/null) || { send_error "404 Not Found" "Not found"; return; }
    public_resolved=$(realpath "${PUBLIC_DIR}" 2>/dev/null) || { send_error "404 Not Found" "Not found"; return; }

    if [[ "${resolved}" != "${public_resolved}" ]] && [[ "${resolved}" != "${public_resolved}"/* ]]; then
        send_error "404 Not Found" "Not found"
        return
    fi

    # Must be a regular file
    if [[ ! -f "${resolved}" ]]; then
        send_error "404 Not Found" "Not found"
        return
    fi

    local ct
    ct=$(content_type_for "${resolved}")

    local is_binary=0
    # shellcheck disable=SC2310  # set -e disabled in && is intentional
    is_binary_type "${ct}" && is_binary=1 || true

    if (( is_binary )); then
        send_file_binary "${resolved}" "${ct}"
    else
        send_file "${resolved}" "${ct}"
    fi
}

# --- Main: read request, route, respond ---

read_ok=0
# shellcheck disable=SC2310  # set -e disabled in && is intentional
read_request && read_ok=1 || true
if (( ! read_ok )); then
    send_error "400 Bad Request" "Bad request"
    exit 0
fi

# Only GET is allowed
if [[ "${REQUEST_METHOD}" != "GET" ]]; then
    send_error "405 Method Not Allowed" "Method not allowed"
    exit 0
fi

# Increment request counter
increment_counter "${RUN_DIR}/requests"

# Route the request
case "${REQUEST_PATH}" in
    /api/readings/latest)
        handle_readings_latest
        ;;
    /api/readings)
        handle_readings
        ;;
    /api/devices)
        handle_devices
        ;;
    /api/health)
        handle_health
        ;;
    /api/config)
        handle_config
        ;;
    /api/stats)
        handle_stats
        ;;
    *)
        handle_static
        ;;
esac
