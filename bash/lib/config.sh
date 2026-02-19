#!/usr/bin/env bash
# Config loading for AirGradientz bash implementation.
# Sources shared config from airgradientz.json, exports AGTZ_* env vars.

find_config_file() {
    if [[ -n "${CONFIG_PATH:-}" ]] && [[ -f "${CONFIG_PATH}" ]]; then
        printf '%s' "${CONFIG_PATH}"
        return 0
    fi
    if [[ -f "./airgradientz.json" ]]; then
        printf '%s' "./airgradientz.json"
        return 0
    fi
    if [[ -f "../airgradientz.json" ]]; then
        printf '%s' "../airgradientz.json"
        return 0
    fi
    return 1
}

load_config() {
    local config_file
    if ! config_file=$(find_config_file); then
        printf 'fatal: no config file found (tried CONFIG_PATH, ./airgradientz.json, ../airgradientz.json)\n' >&2
        exit 1
    fi

    local config
    config=$(< "${config_file}")

    AGTZ_POLL_INTERVAL_MS=$(jq -r '.pollIntervalMs // empty' <<< "${config}")
    AGTZ_FETCH_TIMEOUT_MS=$(jq -r '.fetchTimeoutMs // empty' <<< "${config}")
    AGTZ_MAX_API_ROWS=$(jq -r '.maxApiRows // empty' <<< "${config}")
    AGTZ_DOWNSAMPLE_BUCKETS=$(jq -c '.downsampleBuckets // empty' <<< "${config}")
    AGTZ_DEVICES_JSON=$(jq -c '.devices // empty' <<< "${config}")
    AGTZ_PORT=$(jq -r '.ports.bash // empty' <<< "${config}")

    log "Loaded config from ${config_file}"

    # Validate required keys
    local missing=()
    [[ -z "${AGTZ_POLL_INTERVAL_MS}" ]] && missing+=("pollIntervalMs")
    [[ -z "${AGTZ_FETCH_TIMEOUT_MS}" ]] && missing+=("fetchTimeoutMs")
    [[ -z "${AGTZ_MAX_API_ROWS}" ]] && missing+=("maxApiRows")
    if [[ -z "${AGTZ_DOWNSAMPLE_BUCKETS}" ]] || [[ "${AGTZ_DOWNSAMPLE_BUCKETS}" == "null" ]] || [[ "${AGTZ_DOWNSAMPLE_BUCKETS}" == "{}" ]]; then
        missing+=("downsampleBuckets")
    fi
    if [[ -z "${AGTZ_DEVICES_JSON}" ]] || [[ "${AGTZ_DEVICES_JSON}" == "[]" ]] || [[ "${AGTZ_DEVICES_JSON}" == "null" ]]; then
        missing+=("devices")
    fi
    [[ -z "${AGTZ_PORT}" ]] && missing+=("ports.bash")

    if [[ ${#missing[@]} -gt 0 ]]; then
        local joined
        joined=$(printf '%s, ' "${missing[@]}")
        printf 'fatal: missing required config keys: %s\n' "${joined%, }" >&2
        exit 1
    fi

    # PORT env var overrides config; DB_PATH defaults to ./airgradientz.db
    AGTZ_PORT="${PORT:-${AGTZ_PORT}}"
    AGTZ_DB_PATH="${DB_PATH:-./airgradientz.db}"

    export AGTZ_PORT AGTZ_DB_PATH AGTZ_POLL_INTERVAL_MS AGTZ_FETCH_TIMEOUT_MS
    export AGTZ_MAX_API_ROWS AGTZ_DOWNSAMPLE_BUCKETS AGTZ_DEVICES_JSON
    export AGTZ_SCRIPT_DIR
}

log() {
    local ts
    ts=$(date '+%Y-%m-%d %H:%M:%S') || true
    printf '[%s] %s\n' "${ts}" "$*" >&2
}

now_ms() {
    local sec nsec
    sec=$(date +%s)
    nsec=$(date +%N)
    # %N may return literal "N" on some systems (macOS)
    if [[ "${nsec}" == "N" ]]; then
        printf '%s000' "${sec}"
    else
        printf '%s%03d' "${sec}" "$(( 10#${nsec} / 1000000 ))"
    fi
}

# Atomic counter operations using flock. Shared by handler and poller.
# Note: $(< file 2>/dev/null) is broken in bash — the 2> redirect
# interferes with the builtin < redirect. Use [[ -f ]] guard instead.
increment_counter() {
    local file="$1"
    (
        flock -x 9
        local val=0
        [[ -f "${file}" ]] && val=$(< "${file}")
        printf '%d' $(( val + 1 )) > "${file}"
    ) 9>> "${file}.lock"
}

read_counter() {
    local file="$1"
    local val=0
    [[ -f "${file}" ]] && val=$(< "${file}")
    [[ -z "${val}" ]] && val=0
    printf '%d' "${val}"
}
