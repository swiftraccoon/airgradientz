#!/usr/bin/env bash
# AirGradientz Bash server — main entry point.
# Uses ncat for TCP, sqlite3 for DB, jq for JSON, curl for polling.
set -euo pipefail

AGTZ_SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
export AGTZ_SCRIPT_DIR

# shellcheck source=lib/config.sh
source "${AGTZ_SCRIPT_DIR}/lib/config.sh"
# shellcheck source=lib/db.sh
source "${AGTZ_SCRIPT_DIR}/lib/db.sh"
# shellcheck source=lib/poller.sh
source "${AGTZ_SCRIPT_DIR}/lib/poller.sh"

# Load configuration
load_config

# Create run directory for shared state
RUN_DIR="${AGTZ_SCRIPT_DIR}/run"
rm -rf "${RUN_DIR}"
mkdir -p "${RUN_DIR}"

# Initialize counters
printf '0' > "${RUN_DIR}/requests"
printf '0' > "${RUN_DIR}/poll_successes"
printf '0' > "${RUN_DIR}/poll_failures"

# Record start time and PID
now_ms > "${RUN_DIR}/started_at"
printf '%d' "$$" > "${RUN_DIR}/server_pid"

# Initialize database
log "Initializing database at ${AGTZ_DB_PATH}"
init_db

# Cleanup on exit
POLLER_PID=""
NCAT_PID=""

cleanup() {
    log "Shutting down..."
    [[ -n "${POLLER_PID}" ]] && kill "${POLLER_PID}" 2>/dev/null || true
    [[ -n "${NCAT_PID}" ]] && kill "${NCAT_PID}" 2>/dev/null || true
    wait 2>/dev/null || true
    rm -rf "${RUN_DIR}"
    log "Stopped"
}
trap cleanup EXIT INT TERM

# Start poller in background
poller_main &
POLLER_PID=$!
log "Poller started (PID ${POLLER_PID})"

# Start HTTP server via ncat
log "Listening on http://localhost:${AGTZ_PORT}"
ncat -l -k -p "${AGTZ_PORT}" -m 20 -i 30s -c "${AGTZ_SCRIPT_DIR}/handler.sh" &
NCAT_PID=$!

# Wait for ncat to exit (or signal)
wait "${NCAT_PID}" || true
