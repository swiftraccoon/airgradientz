#!/usr/bin/env bash
set -euo pipefail

# ── Mock AirGradient device server ───────────────────────────────────────────
# Serves the indoorFull fixture from test-fixtures.json on /measures/current.
# Usage: ./conformance/mock-device.sh [port]

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
FIXTURES="${REPO_ROOT}/test-fixtures.json"
PORT="${1:-9999}"

if [[ ! -f "${FIXTURES}" ]]; then
    echo "ERROR: test-fixtures.json not found at ${FIXTURES}" >&2
    exit 1
fi

for cmd in ncat jq; do
    if ! command -v "${cmd}" &>/dev/null; then
        echo "ERROR: ${cmd} is required but not found" >&2
        exit 1
    fi
done

# Extract the indoorFull fixture once at startup
PAYLOAD="$(jq -c '.indoorFull' "${FIXTURES}")"
CONTENT_LENGTH="${#PAYLOAD}"

log() { echo "[mock-device] $*" >&2; }

# Write the handler script to a temp file so ncat -c can use it
HANDLER="$(mktemp)"

cleanup() {
    log "Shutting down mock device on port ${PORT}"
    rm -f "${HANDLER}"
    # Kill any lingering ncat processes we spawned
    if [[ -n "${NCAT_PID:-}" ]] && kill -0 "${NCAT_PID}" 2>/dev/null; then
        kill "${NCAT_PID}" 2>/dev/null || true
        wait "${NCAT_PID}" 2>/dev/null || true
    fi
}
trap cleanup EXIT INT TERM

# The handler reads the HTTP request line, discards headers, then responds.
# It checks the path and returns 200 for /measures/current, 404 otherwise.
cat > "${HANDLER}" << 'HANDLER_EOF'
#!/usr/bin/env bash
read -r method path _version || true
# Strip carriage return
path="${path%$'\r'}"
# Consume remaining headers
while IFS= read -r header; do
    header="${header%$'\r'}"
    [[ -z "${header}" ]] && break
done

if [[ "${method}" == "GET" && "${path}" == "/health" ]]; then
    printf 'HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok'
elif [[ "${method}" == "GET" && "${path}" == "/measures/current" ]]; then
    printf 'HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %s\r\nConnection: close\r\n\r\n%s' \
        "${MOCK_CONTENT_LENGTH}" "${MOCK_PAYLOAD}"
else
    body='{"error":"not found"}'
    printf 'HTTP/1.1 404 Not Found\r\nContent-Type: application/json\r\nContent-Length: %s\r\nConnection: close\r\n\r\n%s' \
        "${#body}" "${body}"
fi
HANDLER_EOF
chmod +x "${HANDLER}"

log "Serving indoorFull fixture on port ${PORT} (${CONTENT_LENGTH} bytes)"
log "Endpoint: GET /measures/current"

# Export payload for the handler subprocess
export MOCK_PAYLOAD="${PAYLOAD}"
export MOCK_CONTENT_LENGTH="${CONTENT_LENGTH}"

# Run ncat in listen+keep-open mode, forking handler per connection
ncat -l -k -p "${PORT}" -c "${HANDLER}" &
NCAT_PID=$!

# Wait for ncat to exit (or be killed by trap)
wait "${NCAT_PID}" 2>/dev/null || true
