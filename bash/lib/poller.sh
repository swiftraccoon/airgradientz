#!/usr/bin/env bash
# Poller loop: fetches device data, inserts into SQLite, updates health.
# shellcheck disable=SC2154  # AGTZ_* vars set by config.sh/exported env

poller_main() {
    local run_dir="${AGTZ_SCRIPT_DIR}/run"
    local poll_count=0
    local interval_s
    interval_s=$(( AGTZ_POLL_INTERVAL_MS / 1000 ))
    local timeout_s
    timeout_s=$(( AGTZ_FETCH_TIMEOUT_MS / 1000 ))
    [[ "${timeout_s}" -lt 1 ]] && timeout_s=1

    local device_count
    device_count=$(jq -r 'length' <<< "${AGTZ_DEVICES_JSON}")

    if [[ "${device_count}" -eq 0 ]]; then
        log "[poller] No devices configured, poller sleeping"
        while true; do sleep 60; done
    fi

    # Initialize health.json
    init_health "${run_dir}" "${device_count}"

    log "[poller] Polling ${device_count} device(s) every ${interval_s}s"

    while true; do
        poll_all_devices "${run_dir}" "${device_count}" "${timeout_s}"
        poll_count=$(( poll_count + 1 ))

        # Checkpoint every 10 polls
        if (( poll_count % 10 == 0 )); then
            db_checkpoint
        fi

        sleep "${interval_s}"
    done
}

init_health() {
    local run_dir="$1" device_count="$2"
    local health="[]"
    local i
    for (( i = 0; i < device_count; i++ )); do
        local ip label
        ip=$(jq -r ".[${i}].ip" <<< "${AGTZ_DEVICES_JSON}")
        label=$(jq -r ".[${i}].label" <<< "${AGTZ_DEVICES_JSON}")
        health=$(jq -c --arg ip "${ip}" --arg label "${label}" \
            '. + [{"ip": $ip, "label": $label, "status": "unknown",
                   "lastSuccess": null, "lastError": null,
                   "lastErrorMessage": null, "consecutiveFailures": 0}]' \
            <<< "${health}")
    done
    printf '%s' "${health}" > "${run_dir}/health.json"
}

poll_all_devices() {
    local run_dir="$1" device_count="$2" timeout_s="$3"
    local i
    for (( i = 0; i < device_count; i++ )); do
        local ip label
        ip=$(jq -r ".[${i}].ip" <<< "${AGTZ_DEVICES_JSON}")
        label=$(jq -r ".[${i}].label" <<< "${AGTZ_DEVICES_JSON}")
        poll_device "${run_dir}" "${i}" "${ip}" "${label}" "${timeout_s}"
    done
}

poll_device() {
    local run_dir="$1" idx="$2" ip="$3" label="$4" timeout_s="$5"
    local response now_ts

    now_ts=$(now_ms)

    if ! response=$(curl -s --max-time "${timeout_s}" --max-filesize 1048576 \
            "http://${ip}/measures/current" 2>/dev/null); then
        update_health_error "${run_dir}" "${idx}" "${now_ts}" "fetch failed: connection error"
        increment_counter "${run_dir}/poll_failures"
        log "[poller] ${label} (${ip}): FAIL — connection error"
        return
    fi

    # Validate JSON
    if ! jq -e '.' <<< "${response}" >/dev/null 2>&1; then
        update_health_error "${run_dir}" "${idx}" "${now_ts}" "invalid JSON response"
        increment_counter "${run_dir}/poll_failures"
        log "[poller] ${label} (${ip}): FAIL — invalid JSON"
        return
    fi

    # Extract fields and insert
    local serial device_type model
    serial=$(jq -r '.serialno // "unknown"' <<< "${response}")
    [[ -z "${serial}" ]] && serial="unknown"
    model=$(jq -r '.model // ""' <<< "${response}")

    # Classify: model starts with "I-" → indoor, else outdoor
    if [[ "${model}" == I-* ]]; then
        device_type="indoor"
    else
        device_type="outdoor"
    fi

    # Escape single quotes in raw JSON for SQL
    local raw_json_escaped
    raw_json_escaped="${response//\'/\'\'}"

    # Extract numeric fields (jq outputs "null" for missing → SQL NULL)
    local pm01 pm02 pm10 pm02c rco2 atmp atmpc rhum rhumc tvoc nox wifi
    pm01=$(jq '.pm01 // null' <<< "${response}")
    pm02=$(jq '.pm02 // null' <<< "${response}")
    pm10=$(jq '.pm10 // null' <<< "${response}")
    pm02c=$(jq '.pm02Compensated // null' <<< "${response}")
    rco2=$(jq '.rco2 // null' <<< "${response}")
    atmp=$(jq '.atmp // null' <<< "${response}")
    atmpc=$(jq '.atmpCompensated // null' <<< "${response}")
    rhum=$(jq '.rhum // null' <<< "${response}")
    rhumc=$(jq '.rhumCompensated // null' <<< "${response}")
    tvoc=$(jq '.tvocIndex // null' <<< "${response}")
    nox=$(jq '.noxIndex // null' <<< "${response}")
    wifi=$(jq '.wifi // null' <<< "${response}")

    local insert_sql="INSERT INTO readings (
        timestamp, device_id, device_type, device_ip,
        pm01, pm02, pm10, pm02_compensated,
        rco2, atmp, atmp_compensated, rhum, rhum_compensated,
        tvoc_index, nox_index, wifi, raw_json
    ) VALUES (
        ${now_ts}, '${serial}', '${device_type}', '${ip}',
        ${pm01}, ${pm02}, ${pm10}, ${pm02c},
        ${rco2}, ${atmp}, ${atmpc}, ${rhum}, ${rhumc},
        ${tvoc}, ${nox}, ${wifi}, '${raw_json_escaped}'
    );"

    if ! db_exec "${insert_sql}" 2>/dev/null; then
        update_health_error "${run_dir}" "${idx}" "${now_ts}" "database insert failed"
        increment_counter "${run_dir}/poll_failures"
        log "[poller] ${label} (${ip}): FAIL — DB insert"
        return
    fi

    update_health_success "${run_dir}" "${idx}" "${now_ts}"
    increment_counter "${run_dir}/poll_successes"

    # Log summary
    local pm02_display="N/A" rco2_display="N/A" atmp_display="N/A"
    [[ "${pm02}" != "null" ]] && pm02_display="${pm02}"
    [[ "${rco2}" != "null" ]] && rco2_display="${rco2}"
    [[ "${atmp}" != "null" ]] && atmp_display="${atmp}"
    log "[poller] ${label} (${ip}): OK — PM2.5=${pm02_display}, CO2=${rco2_display}, T=${atmp_display}°C"
}

update_health_success() {
    local run_dir="$1" idx="$2" now_ts="$3"
    local health_file="${run_dir}/health.json"
    local tmp="${health_file}.tmp"
    (
        flock -x 9
        jq -c --argjson idx "${idx}" --argjson ts "${now_ts}" \
            '.[$idx].status = "ok" | .[$idx].lastSuccess = $ts |
             .[$idx].lastErrorMessage = null | .[$idx].consecutiveFailures = 0' \
            "${health_file}" > "${tmp}"
        mv "${tmp}" "${health_file}"
    ) 9>> "${health_file}.lock"
}

update_health_error() {
    local run_dir="$1" idx="$2" now_ts="$3" message="$4"
    local health_file="${run_dir}/health.json"
    local tmp="${health_file}.tmp"
    (
        flock -x 9
        jq -c --argjson idx "${idx}" --argjson ts "${now_ts}" --arg msg "${message}" \
            '.[$idx].status = "error" | .[$idx].lastError = $ts |
             .[$idx].lastErrorMessage = $msg |
             .[$idx].consecutiveFailures = (.[$idx].consecutiveFailures + 1)' \
            "${health_file}" > "${tmp}"
        mv "${tmp}" "${health_file}"
    ) 9>> "${health_file}.lock"
}
