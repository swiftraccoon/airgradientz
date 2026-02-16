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
    if config_file=$(find_config_file); then
        local config
        config=$(< "${config_file}")

        AGTZ_PORT=$(jq -r '.ports.bash // empty' <<< "${config}")
        AGTZ_POLL_INTERVAL_MS=$(jq -r '.pollIntervalMs // .defaults.pollIntervalMs // empty' <<< "${config}")
        AGTZ_FETCH_TIMEOUT_MS=$(jq -r '.fetchTimeoutMs // .defaults.fetchTimeoutMs // empty' <<< "${config}")
        AGTZ_MAX_API_ROWS=$(jq -r '.maxApiRows // .defaults.maxApiRows // empty' <<< "${config}")
        AGTZ_DEVICES_JSON=$(jq -c '.devices // .defaults.devices // []' <<< "${config}")

        log "Loaded config from ${config_file}"
    else
        log "No config file found, using defaults"
        AGTZ_DEVICES_JSON='[]'
    fi

    # Env vars override config file; config file overrides defaults
    AGTZ_PORT="${PORT:-${AGTZ_PORT:-3017}}"
    AGTZ_DB_PATH="${DB_PATH:-./airgradientz.db}"
    AGTZ_POLL_INTERVAL_MS="${AGTZ_POLL_INTERVAL_MS:-15000}"
    AGTZ_FETCH_TIMEOUT_MS="${AGTZ_FETCH_TIMEOUT_MS:-5000}"
    AGTZ_MAX_API_ROWS="${AGTZ_MAX_API_ROWS:-10000}"

    export AGTZ_PORT AGTZ_DB_PATH AGTZ_POLL_INTERVAL_MS AGTZ_FETCH_TIMEOUT_MS
    export AGTZ_MAX_API_ROWS AGTZ_DEVICES_JSON
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
