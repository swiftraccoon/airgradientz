#!/usr/bin/env bash
# HTTP parsing and response building for AirGradientz bash handler.
# Variables REQUEST_METHOD, REQUEST_PATH, QUERY_STRING are set by read_request
# and consumed by the sourcing script (handler.sh).

# shellcheck disable=SC2034  # REQUEST_METHOD/PATH/QUERY_STRING used by caller
read_request() {
    local line
    if ! IFS= read -r -t 10 line; then
        return 1
    fi
    # Strip trailing \r
    line="${line%$'\r'}"

    # Parse "GET /path?query HTTP/1.x"
    REQUEST_METHOD="${line%% *}"
    local rest="${line#* }"
    local uri="${rest%% *}"
    REQUEST_PATH="${uri%%\?*}"
    if [[ "${uri}" == *'?'* ]]; then
        QUERY_STRING="${uri#*\?}"
    else
        QUERY_STRING=""
    fi

    # Consume remaining headers (we don't need them)
    while IFS= read -r -t 5 line; do
        line="${line%$'\r'}"
        [[ -z "${line}" ]] && break
    done
    return 0
}

url_decode() {
    local encoded="$1"
    # Replace + with space, then decode %XX sequences
    encoded="${encoded//+/ }"
    printf '%b' "${encoded//%/\\x}"
}

parse_query_param() {
    local query="$1" name="$2" default="${3:-}"
    local IFS='&'
    local pair
    for pair in ${query}; do
        local key="${pair%%=*}"
        local val="${pair#*=}"
        if [[ "${key}" == "${name}" ]]; then
            printf '%s' "${val}"
            return 0
        fi
    done
    printf '%s' "${default}"
}

send_response() {
    local status="$1" content_type="$2" body="$3"
    local length=${#body}
    printf 'HTTP/1.1 %s\r\n' "${status}"
    printf 'Content-Type: %s\r\n' "${content_type}"
    printf 'Content-Length: %d\r\n' "${length}"
    printf 'Connection: close\r\n'
    printf 'X-Content-Type-Options: nosniff\r\n'
    printf 'X-Frame-Options: DENY\r\n'
    printf '\r\n'
    printf '%s' "${body}"
}

send_json() {
    local status="$1" body="$2"
    send_response "${status}" "application/json" "${body}"
}

send_error() {
    local status="$1" message="$2"
    local body
    body=$(jq -cn --arg msg "${message}" '{"error": $msg}')
    send_json "${status}" "${body}"
}

send_file() {
    local file_path="$1" content_type="$2"
    local size body
    size=$(stat -c '%s' "${file_path}" 2>/dev/null) || { send_error "404 Not Found" "Not found"; return; }

    if (( size > 16 * 1024 * 1024 )); then
        send_error "413 Request Entity Too Large" "File too large"
        return
    fi

    body=$(< "${file_path}")
    local length=${#body}
    printf 'HTTP/1.1 200 OK\r\n'
    printf 'Content-Type: %s\r\n' "${content_type}"
    printf 'Content-Length: %d\r\n' "${length}"
    printf 'Connection: close\r\n'
    printf 'X-Content-Type-Options: nosniff\r\n'
    printf 'X-Frame-Options: DENY\r\n'
    printf 'Cache-Control: public, max-age=600\r\n'
    printf '\r\n'
    printf '%s' "${body}"
}

send_file_binary() {
    local file_path="$1" content_type="$2"
    local size
    size=$(stat -c '%s' "${file_path}" 2>/dev/null) || { send_error "404 Not Found" "Not found"; return; }

    if (( size > 16 * 1024 * 1024 )); then
        send_error "413 Request Entity Too Large" "File too large"
        return
    fi

    printf 'HTTP/1.1 200 OK\r\n'
    printf 'Content-Type: %s\r\n' "${content_type}"
    printf 'Content-Length: %d\r\n' "${size}"
    printf 'Connection: close\r\n'
    printf 'X-Content-Type-Options: nosniff\r\n'
    printf 'X-Frame-Options: DENY\r\n'
    printf 'Cache-Control: public, max-age=600\r\n'
    printf '\r\n'
    cat "${file_path}"
}

content_type_for() {
    local path="$1"
    case "${path##*.}" in
        html) printf 'text/html; charset=utf-8' ;;
        css)  printf 'text/css; charset=utf-8' ;;
        js)   printf 'application/javascript; charset=utf-8' ;;
        json) printf 'application/json; charset=utf-8' ;;
        png)  printf 'image/png' ;;
        jpg|jpeg) printf 'image/jpeg' ;;
        svg)  printf 'image/svg+xml' ;;
        ico)  printf 'image/x-icon' ;;
        *)    printf 'application/octet-stream' ;;
    esac
}

is_binary_type() {
    local ct="$1"
    case "${ct}" in
        image/*|application/octet-stream) return 0 ;;
        *) return 1 ;;
    esac
}

# increment_counter and read_counter are defined in config.sh (shared with poller)
