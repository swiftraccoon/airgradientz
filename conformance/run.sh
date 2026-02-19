#!/usr/bin/env bash
set -euo pipefail

# ── Conformance test orchestrator ────────────────────────────────────────────
# Starts a mock device, builds & launches implementations, waits for them to
# poll, then runs compare.sh to verify all produce identical API responses.
#
# Usage: ./conformance/run.sh [impl...]
#        e.g. ./conformance/run.sh c nodejs go
#        ./conformance/run.sh          # all implementations

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

HEALTH_TIMEOUT=30   # seconds to wait for impl to become ready
HEALTH_INTERVAL=0.5 # seconds between health checks
POLL_WAIT=20        # seconds to wait for at least one poll cycle
BASE_PORT=19000     # starting port for impl allocation

log() { echo "[conformance] $*" >&2; }

# ── Dependency check ─────────────────────────────────────────────────────────

check_deps() {
    local missing=()
    for cmd in bash curl jq ncat bc; do
        if ! command -v "${cmd}" &>/dev/null; then
            missing+=("${cmd}")
        fi
    done
    if [[ ${#missing[@]} -gt 0 ]]; then
        echo "ERROR: Missing required dependencies: ${missing[*]}" >&2
        exit 1
    fi
}

# ── Implementation registry ─────────────────────────────────────────────────
# Format: name|directory|build_cmd|start_cmd
# Ports are assigned dynamically from BASE_PORT.

IMPL_REGISTRY=(
    "c|c|cd c && make clean && make|cd c && ./airgradientz"
    "nodejs|nodejs|cd nodejs && npm install --silent|cd nodejs && node server.js"
    "rust|rust|cd rust && cargo build --release 2>/dev/null|cd rust && ./target/release/airgradientz"
    "zig|zig|cd zig && zig build -Doptimize=ReleaseFast|cd zig && ./zig-out/bin/airgradientz"
    "d|d|cd d && source ~/dlang/ldc-*/activate 2>/dev/null; dub build -b release|cd d && ./airgradientz"
    "elixir|elixir|cd elixir && mix deps.get --quiet && mix compile|cd elixir && mix run --no-halt"
    "nim|nim|cd nim && export PATH=\$HOME/.nimble/bin:\$PATH && nim c -d:release --threads:on --mm:orc --path:src -o:airgradientz src/airgradientz.nim 2>/dev/null|cd nim && ./airgradientz"
    "go|go|cd go && go build -o airgradientz .|cd go && ./airgradientz"
    "bash|bash|cd bash && bash build.sh|cd bash && bash server.sh"
    "asm|asm|cd asm && make clean && make|cd asm && ./airgradientz"
    "haskell|haskell|cd haskell && export PATH=\$HOME/.ghcup/bin:\$PATH && cabal build 2>/dev/null|cd haskell && export PATH=\$HOME/.ghcup/bin:\$PATH && cabal run airgradientz 2>/dev/null"
)

# ── Helper functions ─────────────────────────────────────────────────────────

get_field() {
    local entry="$1" field="$2"
    echo "${entry}" | cut -d'|' -f"${field}"
}

# Find a free port by binding to port 0
find_free_port() {
    python3 -c 'import socket; s=socket.socket(); s.bind(("",0)); print(s.getsockname()[1]); s.close()' 2>/dev/null \
        || echo "$((BASE_PORT + RANDOM % 1000))"
}

# ── Process tracking ─────────────────────────────────────────────────────────

declare -a CHILD_PIDS=()
TMPDIR_PATH=""

# shellcheck disable=SC2329  # invoked via trap
cleanup() {
    log "Cleaning up..."
    for pid in "${CHILD_PIDS[@]}"; do
        if kill -0 "${pid}" 2>/dev/null; then
            kill "${pid}" 2>/dev/null || true
        fi
    done
    # Wait briefly for graceful shutdown, then force
    sleep 1
    for pid in "${CHILD_PIDS[@]}"; do
        if kill -0 "${pid}" 2>/dev/null; then
            kill -9 "${pid}" 2>/dev/null || true
        fi
        wait "${pid}" 2>/dev/null || true
    done
    if [[ -n "${TMPDIR_PATH}" && -d "${TMPDIR_PATH}" ]]; then
        rm -rf "${TMPDIR_PATH}"
    fi
    log "Cleanup complete."
}
trap cleanup EXIT INT TERM

# ── Resolve requested implementations ────────────────────────────────────────

resolve_impls() {
    local requested=("$@")
    local resolved=()

    if [[ ${#requested[@]} -eq 0 ]]; then
        for entry in "${IMPL_REGISTRY[@]}"; do
            resolved+=("${entry}")
        done
    else
        for req in "${requested[@]}"; do
            local found=false
            for entry in "${IMPL_REGISTRY[@]}"; do
                local name
                name="$(get_field "${entry}" 1)"
                if [[ "${name}" == "${req}" ]]; then
                    resolved+=("${entry}")
                    found=true
                    break
                fi
            done
            if [[ "${found}" == "false" ]]; then
                log "WARNING: Unknown implementation '${req}', skipping."
            fi
        done
    fi

    printf '%s\n' "${resolved[@]}"
}

wait_ready() {
    local name="$1" port="$2"
    local url="http://localhost:${port}/api/health"
    local elapsed=0

    # shellcheck disable=SC2312
    while (( $(echo "${elapsed} < ${HEALTH_TIMEOUT}" | bc -l) )); do
        if curl -sf "${url}" -o /dev/null 2>/dev/null; then
            log "${name} is ready (${elapsed}s)"
            return 0
        fi
        sleep "${HEALTH_INTERVAL}"
        elapsed="$(echo "${elapsed} + ${HEALTH_INTERVAL}" | bc -l)"
    done

    log "WARNING: ${name} did not become ready within ${HEALTH_TIMEOUT}s"
    return 1
}

# ── Main ─────────────────────────────────────────────────────────────────────

check_deps

# Parse arguments
IMPLS=()
# shellcheck disable=SC2312
while IFS= read -r line; do
    [[ -n "${line}" ]] && IMPLS+=("${line}")
done < <(resolve_impls "$@")

if [[ ${#IMPLS[@]} -lt 2 ]]; then
    log "ERROR: Need at least 2 implementations to compare."
    log "Usage: $0 [impl1 impl2 ...]"
    exit 1
fi

log "Testing ${#IMPLS[@]} implementation(s)..."

# ── Step 1: Create temp directory and config ─────────────────────────────────

TMPDIR_PATH="$(mktemp -d)"
log "Temp directory: ${TMPDIR_PATH}"

# Find a free port for mock device
MOCK_PORT="$(find_free_port)"
log "Mock device port: ${MOCK_PORT}"

# ── Step 2: Start mock device ────────────────────────────────────────────────

log "Starting mock device server..."
bash "${SCRIPT_DIR}/mock-device.sh" "${MOCK_PORT}" &
MOCK_PID=$!
CHILD_PIDS+=("${MOCK_PID}")

# Wait for mock device to be ready
sleep 1
if ! kill -0 "${MOCK_PID}" 2>/dev/null; then
    log "ERROR: Mock device failed to start"
    exit 1
fi

# Verify mock device responds
mock_ready=false
for _ in 1 2 3; do
    if curl -sf "http://localhost:${MOCK_PORT}/measures/current" -o /dev/null 2>/dev/null; then
        mock_ready=true
        break
    fi
    sleep 1
done
if [[ "${mock_ready}" == "false" ]]; then
    log "ERROR: Mock device not responding on port ${MOCK_PORT}"
    exit 1
fi
log "Mock device is ready"

# ── Step 3: Write temp config ────────────────────────────────────────────────

CONFIG_PATH="${TMPDIR_PATH}/airgradientz.json"

# Build ports object dynamically
port_offset=0
declare -A IMPL_PORT_MAP=()
PORTS_JSON="{"
first=true
for entry in "${IMPLS[@]}"; do
    name="$(get_field "${entry}" 1)"
    assigned_port=$((BASE_PORT + port_offset))
    IMPL_PORT_MAP[${name}]=${assigned_port}
    if [[ "${first}" == "true" ]]; then
        first=false
    else
        PORTS_JSON+=","
    fi
    PORTS_JSON+="\"${name}\":${assigned_port}"
    port_offset=$((port_offset + 1))
done
PORTS_JSON+="}"

# Write config with device pointing to mock server
jq -n \
    --argjson ports "${PORTS_JSON}" \
    --arg mock_ip "localhost:${MOCK_PORT}" \
    '{
        pollIntervalMs: 10000,
        fetchTimeoutMs: 5000,
        maxApiRows: 10000,
        downsampleBuckets: {"5m": 300000, "10m": 600000, "15m": 900000, "30m": 1800000, "1h": 3600000, "1d": 86400000, "1w": 604800000},
        ports: $ports,
        devices: [
            { ip: $mock_ip, label: "indoor" }
        ]
    }' > "${CONFIG_PATH}"

log "Config written to ${CONFIG_PATH}"

# ── Step 4: Build and start implementations ──────────────────────────────────

declare -a COMPARE_ARGS=()
declare -a STARTED_IMPLS=()

for entry in "${IMPLS[@]}"; do
    name="$(get_field "${entry}" 1)"
    dir="$(get_field "${entry}" 2)"
    build_cmd="$(get_field "${entry}" 3)"
    start_cmd="$(get_field "${entry}" 4)"
    port="${IMPL_PORT_MAP[${name}]}"
    db_path="${TMPDIR_PATH}/${name}.db"

    # Check directory exists
    if [[ ! -d "${REPO_ROOT}/${dir}" ]]; then
        log "WARNING: Directory ${dir} does not exist, skipping ${name}"
        continue
    fi

    # Build
    log "Building ${name}..."
    if ! (cd "${REPO_ROOT}" && eval "${build_cmd}") >/dev/null 2>&1; then
        log "WARNING: Build failed for ${name}, skipping"
        continue
    fi
    log "Build succeeded for ${name}"

    # Start with env vars
    log "Starting ${name} on port ${port}..."
    (
        cd "${REPO_ROOT}"
        export PORT="${port}"
        export DB_PATH="${db_path}"
        export CONFIG_PATH="${CONFIG_PATH}"
        eval "${start_cmd}"
    ) >/dev/null 2>&1 &
    impl_pid=$!
    CHILD_PIDS+=("${impl_pid}")

    STARTED_IMPLS+=("${name}|${port}|${impl_pid}")
done

if [[ ${#STARTED_IMPLS[@]} -lt 2 ]]; then
    log "ERROR: Fewer than 2 implementations started successfully."
    exit 1
fi

# ── Step 5: Wait for all implementations to be ready ─────────────────────────

for impl_info in "${STARTED_IMPLS[@]}"; do
    name="$(echo "${impl_info}" | cut -d'|' -f1)"
    port="$(echo "${impl_info}" | cut -d'|' -f2)"

    ready=false
    # shellcheck disable=SC2310
    wait_ready "${name}" "${port}" && ready=true
    if [[ "${ready}" == "false" ]]; then
        log "WARNING: ${name} failed health check, excluding from comparison"
        continue
    fi
    COMPARE_ARGS+=("${name}:${port}")
done

if [[ ${#COMPARE_ARGS[@]} -lt 2 ]]; then
    log "ERROR: Fewer than 2 implementations passed health check."
    exit 1
fi

# ── Step 6: Wait for poll cycle ──────────────────────────────────────────────

log "Waiting ${POLL_WAIT}s for implementations to poll mock device..."
sleep "${POLL_WAIT}"

# Verify at least the reference impl has data
ref_port="${COMPARE_ARGS[0]##*:}"
reading_count="$(curl -sf "http://localhost:${ref_port}/api/readings/count?from=0&to=9999999999999" 2>/dev/null | jq -r '.count // 0')" || reading_count=0
if [[ "${reading_count}" -eq 0 ]]; then
    log "WARNING: Reference implementation has 0 readings after poll wait."
    log "Extending wait by 15 seconds..."
    sleep 15
    reading_count="$(curl -sf "http://localhost:${ref_port}/api/readings/count?from=0&to=9999999999999" 2>/dev/null | jq -r '.count // 0')" || reading_count=0
    if [[ "${reading_count}" -eq 0 ]]; then
        log "ERROR: Reference implementation still has 0 readings. Aborting."
        exit 1
    fi
fi
log "Reference implementation has ${reading_count} reading(s)"

# ── Step 7: Run comparator ───────────────────────────────────────────────────

log "Running conformance comparator..."
log "Implementations: ${COMPARE_ARGS[*]}"
echo ""

# Run compare.sh and capture exit code
exit_code=0
bash "${SCRIPT_DIR}/compare.sh" "${COMPARE_ARGS[@]}" || exit_code=$?

# ── Report ───────────────────────────────────────────────────────────────────

if [[ ${exit_code} -eq 0 ]]; then
    log "All conformance tests passed!"
else
    log "Some conformance tests failed (exit code: ${exit_code})"
fi

exit "${exit_code}"
