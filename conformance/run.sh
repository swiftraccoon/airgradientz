#!/usr/bin/env bash
set -euo pipefail

# ── Conformance test orchestrator ────────────────────────────────────────────
# Starts a mock device, builds & launches implementations, waits for them to
# poll, then runs compare.sh to verify all produce identical API responses.
#
# Usage: ./conformance/run.sh [--batch-size N] [--ref IMPL] [impl...]
#        ./conformance/run.sh --solo IMPL
#        e.g. ./conformance/run.sh c nodejs go
#        ./conformance/run.sh --batch-size 3 --ref c
#        ./conformance/run.sh --solo haskell  # test single impl (~30s)
#        ./conformance/run.sh                 # all implementations at once

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

HEALTH_TIMEOUT=60   # seconds to wait for impl to become ready
POLL_WAIT=20        # seconds to wait for at least one poll cycle
BASE_PORT=19000     # starting port for impl allocation
BATCH_SIZE=0        # max impls per batch (0 = all at once)
REF_IMPL="c"        # reference implementation name
SOLO_IMPL=""         # single-impl mode (empty = disabled)

log() { echo "[conformance] $*" >&2; }

# ── Dependency check ─────────────────────────────────────────────────────────

check_deps() {
    local missing=()
    for cmd in bash curl jq ncat; do
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
    "forth|forth|cd forth && bash build.sh|cd forth && gforth src/main.fs"
)

# ── Helper functions ─────────────────────────────────────────────────────────

get_field() {
    local entry="$1" field="$2"
    echo "${entry}" | cut -d'|' -f"${field}"
}

find_free_port() {
    python3 -c 'import socket; s=socket.socket(); s.bind(("",0)); print(s.getsockname()[1]); s.close()' 2>/dev/null \
        || echo "$((BASE_PORT + RANDOM % 1000))"
}

# ── Process tracking ─────────────────────────────────────────────────────────

declare -a CHILD_PIDS=()
declare -a ALLOCATED_PORTS=()
TMPDIR_PATH=""

# Kill any process listening on a specific port.
kill_port() {
    local port="$1"
    fuser -k "${port}/tcp" >/dev/null 2>&1 || true
}

# shellcheck disable=SC2329  # invoked via trap
cleanup() {
    log "Cleaning up..."
    # Kill tracked PIDs first
    for pid in "${CHILD_PIDS[@]}"; do
        kill "${pid}" 2>/dev/null || true
    done
    sleep 1
    # Force-kill any survivors by port (catches orphaned child processes
    # from cabal run, mix run, node, etc.)
    for port in "${ALLOCATED_PORTS[@]}"; do
        kill_port "${port}"
    done
    for pid in "${CHILD_PIDS[@]}"; do
        kill -9 "${pid}" 2>/dev/null || true
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

# Wait for a server to respond to health checks.
# Uses integer loop with 0.5s sleep — no bc dependency.
# Prints diagnostic curl output on failure.
wait_ready() {
    local name="$1" port="$2"
    local url="http://localhost:${port}/api/health"
    local max_attempts=$(( HEALTH_TIMEOUT * 2 ))  # 0.5s per attempt
    local attempt

    for (( attempt=1; attempt<=max_attempts; attempt++ )); do
        if curl -sf "${url}" -o /dev/null 2>/dev/null; then
            log "${name} ready (${attempt} attempts, ~$(( attempt / 2 ))s)"
            return 0
        fi
        sleep 0.5
    done

    # Diagnostic on failure — show what curl actually sees
    log "ERROR: ${name} not ready after ${HEALTH_TIMEOUT}s on port ${port}"
    log "  Diagnostic curl -v output:"
    curl -v "http://localhost:${port}/api/health" 2>&1 | head -15 | while IFS= read -r line; do
        log "    ${line}"
    done
    return 1
}

# Stop specific PIDs and their ports. Uses port-based killing to ensure
# child processes (cabal run, mix run, node, etc.) are also terminated.
stop_pids_and_ports() {
    local -a pids_to_stop=()
    local -a ports_to_stop=()
    # Parse "pid:port" pairs
    local pair
    for pair in "$@"; do
        pids_to_stop+=("${pair%%:*}")
        ports_to_stop+=("${pair##*:}")
    done
    # SIGTERM tracked PIDs
    for pid in "${pids_to_stop[@]}"; do
        kill "${pid}" 2>/dev/null || true
    done
    sleep 1
    # Force-kill by port to catch orphaned children
    for port in "${ports_to_stop[@]}"; do
        kill_port "${port}"
    done
    for pid in "${pids_to_stop[@]}"; do
        kill -9 "${pid}" 2>/dev/null || true
        wait "${pid}" 2>/dev/null || true
    done
}

# Start an implementation server. Prints its PID to stdout.
# NOTE: This is called in $() command substitution, so array modifications
# here are lost. Callers must track PIDs/ports in the parent shell.
start_impl() {
    local entry="$1"
    local name start_cmd port db_path
    name="$(get_field "${entry}" 1)"
    start_cmd="$(get_field "${entry}" 4)"
    port="${IMPL_PORT_MAP[${name}]}"
    db_path="${IMPL_DB_MAP[${name}]}"

    # Kill anything that might be lingering on this port from a previous run
    kill_port "${port}"

    log "  Starting ${name} on port ${port}..."
    (
        cd "${REPO_ROOT}"
        export PORT="${port}"
        export DB_PATH="${db_path}"
        export CONFIG_PATH="${CONFIG_PATH}"
        eval "${start_cmd}"
    ) >/dev/null 2>&1 &
    echo "$!"
}

# ── Main ─────────────────────────────────────────────────────────────────────

check_deps

# ── Solo mode: test a single implementation ─────────────────────────────────

solo_main() {
    local impl_name="$1"

    # Find impl in registry
    local impl_entry=""
    for entry in "${IMPL_REGISTRY[@]}"; do
        local name
        name="$(get_field "${entry}" 1)"
        if [[ "${name}" == "${impl_name}" ]]; then
            impl_entry="${entry}"
            break
        fi
    done

    if [[ -z "${impl_entry}" ]]; then
        log "ERROR: Unknown implementation '${impl_name}'"
        local avail=""
        for entry in "${IMPL_REGISTRY[@]}"; do
            avail+="$(get_field "${entry}" 1) "
        done
        log "Available: ${avail}"
        exit 1
    fi

    local dir
    dir="$(get_field "${impl_entry}" 2)"
    if [[ ! -d "${REPO_ROOT}/${dir}" ]]; then
        log "ERROR: Directory ${dir} does not exist for ${impl_name}"
        exit 1
    fi

    log "Solo mode: testing ${impl_name}"

    # Create temp directory
    TMPDIR_PATH="$(mktemp -d)"
    log "Temp directory: ${TMPDIR_PATH}"

    # Start mock device on ephemeral port
    local mock_port
    mock_port="$(find_free_port)"
    log "Mock device port: ${mock_port}"

    bash "${SCRIPT_DIR}/mock-device.sh" "${mock_port}" &
    local mock_pid=$!
    CHILD_PIDS+=("${mock_pid}")
    ALLOCATED_PORTS+=("${mock_port}")

    local mock_ready=false
    for _ in $(seq 1 10); do
        if curl -sf "http://localhost:${mock_port}/health" -o /dev/null 2>/dev/null; then
            mock_ready=true
            break
        fi
        sleep 0.5
    done
    if [[ "${mock_ready}" == "false" ]]; then
        log "ERROR: Mock device not responding on port ${mock_port}"
        exit 1
    fi
    log "Mock device is ready"

    # Assign ephemeral port for impl
    local impl_port
    impl_port="$(find_free_port)"

    # Create config JSON
    CONFIG_PATH="${TMPDIR_PATH}/airgradientz.json"
    export CONFIG_PATH
    jq -n \
        --arg impl "${impl_name}" \
        --argjson port "${impl_port}" \
        --arg mock_ip "127.0.0.1:${mock_port}" \
        '{
            pollIntervalMs: 600000,
            fetchTimeoutMs: 5000,
            maxApiRows: 10000,
            downsampleBuckets: {"5m": 300000, "10m": 600000, "15m": 900000, "30m": 1800000, "1h": 3600000, "1d": 86400000, "1w": 604800000},
            ports: { ($impl): $port },
            devices: [
                { ip: $mock_ip, label: "indoor" }
            ]
        }' > "${CONFIG_PATH}"
    log "Config written to ${CONFIG_PATH}"

    # Set up maps needed by start_impl
    declare -A IMPL_PORT_MAP=()
    declare -A IMPL_DB_MAP=()
    IMPL_PORT_MAP[${impl_name}]="${impl_port}"
    IMPL_DB_MAP[${impl_name}]="${TMPDIR_PATH}/${impl_name}.db"

    # Build
    local build_cmd
    build_cmd="$(get_field "${impl_entry}" 3)"
    log "Building ${impl_name}..."
    if ! (cd "${REPO_ROOT}" && eval "${build_cmd}") >/dev/null 2>&1; then
        log "ERROR: Build failed for ${impl_name}"
        exit 1
    fi
    log "Build succeeded: ${impl_name}"

    # Start
    log "Starting ${impl_name} on port ${impl_port}..."
    local impl_pid
    impl_pid="$(start_impl "${impl_entry}")"
    CHILD_PIDS+=("${impl_pid}")
    ALLOCATED_PORTS+=("${impl_port}")

    # Wait for health
    # shellcheck disable=SC2310
    if ! wait_ready "${impl_name}" "${impl_port}"; then
        log "ERROR: ${impl_name} failed health check"
        exit 1
    fi

    # Wait for poll cycle
    log "Waiting ${POLL_WAIT}s for ${impl_name} to poll mock device..."
    sleep "${POLL_WAIT}"

    # Verify readings exist
    local reading_count
    reading_count="$(curl -sf "http://localhost:${impl_port}/api/readings/count?from=0&to=9999999999999" 2>/dev/null | jq -r '.count // 0')" || reading_count=0
    if [[ "${reading_count}" -eq 0 ]]; then
        log "WARNING: ${impl_name} has 0 readings, extending wait by 15s..."
        sleep 15
        reading_count="$(curl -sf "http://localhost:${impl_port}/api/readings/count?from=0&to=9999999999999" 2>/dev/null | jq -r '.count // 0')" || reading_count=0
        if [[ "${reading_count}" -eq 0 ]]; then
            log "ERROR: ${impl_name} still has 0 readings. Aborting."
            exit 1
        fi
    fi
    log "${impl_name} has ${reading_count} reading(s)"

    # Run test runner
    log "Running test runner against ${impl_name}..."
    local runner_exit=0
    bash "${SCRIPT_DIR}/runner.sh" "localhost:${impl_port}" || runner_exit=$?

    echo ""
    if [[ ${runner_exit} -eq 0 ]]; then
        log "Solo test passed: ${impl_name}"
    else
        log "Solo test failed: ${impl_name}"
    fi

    exit "${runner_exit}"
}

# Parse --flags before positional args
POSITIONAL_ARGS=()
while [[ $# -gt 0 ]]; do
    case "$1" in
        --batch-size)
            BATCH_SIZE="$2"
            shift 2
            ;;
        --batch-size=*)
            BATCH_SIZE="${1#*=}"
            shift
            ;;
        --ref)
            REF_IMPL="$2"
            shift 2
            ;;
        --ref=*)
            REF_IMPL="${1#*=}"
            shift
            ;;
        --solo)
            SOLO_IMPL="$2"
            shift 2
            ;;
        --solo=*)
            SOLO_IMPL="${1#*=}"
            shift
            ;;
        -*)
            log "ERROR: Unknown option: $1"
            log "Usage: $0 [--batch-size N] [--ref IMPL] [--solo IMPL] [impl...]"
            exit 1
            ;;
        *)
            POSITIONAL_ARGS+=("$1")
            shift
            ;;
    esac
done

if [[ -n "${SOLO_IMPL}" ]]; then
    solo_main "${SOLO_IMPL}"
fi

# Resolve implementations
IMPLS=()
# shellcheck disable=SC2312
while IFS= read -r line; do
    [[ -n "${line}" ]] && IMPLS+=("${line}")
done < <(resolve_impls "${POSITIONAL_ARGS[@]}")

if [[ ${#IMPLS[@]} -lt 2 ]]; then
    log "ERROR: Need at least 2 implementations to compare."
    log "Usage: $0 [--batch-size N] [--ref IMPL] [impl1 impl2 ...]"
    exit 1
fi

# Separate reference impl from the rest
REF_ENTRY=""
NON_REF_IMPLS=()
for entry in "${IMPLS[@]}"; do
    name="$(get_field "${entry}" 1)"
    if [[ "${name}" == "${REF_IMPL}" ]]; then
        REF_ENTRY="${entry}"
    else
        NON_REF_IMPLS+=("${entry}")
    fi
done

if [[ -z "${REF_ENTRY}" ]]; then
    log "ERROR: Reference implementation '${REF_IMPL}' not found in requested implementations."
    exit 1
fi

# Effective batch size: 0 means all at once
if [[ "${BATCH_SIZE}" -eq 0 ]]; then
    BATCH_SIZE=${#NON_REF_IMPLS[@]}
fi

log "Testing ${#IMPLS[@]} implementations (ref: ${REF_IMPL}, batch size: ${BATCH_SIZE})"

# ── Step 1: Create temp directory ─────────────────────────────────────────────

TMPDIR_PATH="$(mktemp -d)"
log "Temp directory: ${TMPDIR_PATH}"

MOCK_PORT="$(find_free_port)"
log "Mock device port: ${MOCK_PORT}"

# ── Step 2: Start mock device ────────────────────────────────────────────────

log "Starting mock device server..."
bash "${SCRIPT_DIR}/mock-device.sh" "${MOCK_PORT}" &
MOCK_PID=$!
CHILD_PIDS+=("${MOCK_PID}")

# Wait for mock device using /health endpoint
mock_ready=false
for _ in $(seq 1 10); do
    if curl -sf "http://localhost:${MOCK_PORT}/health" -o /dev/null 2>/dev/null; then
        mock_ready=true
        break
    fi
    sleep 0.5
done
if [[ "${mock_ready}" == "false" ]]; then
    log "ERROR: Mock device not responding on port ${MOCK_PORT}"
    exit 1
fi
log "Mock device is ready"

# ── Step 3: Assign ports for all implementations ─────────────────────────────

port_offset=0
declare -A IMPL_PORT_MAP=()
declare -A IMPL_DB_MAP=()
PORTS_JSON="{"
first=true

ALL_IMPLS=("${REF_ENTRY}" "${NON_REF_IMPLS[@]}")
for entry in "${ALL_IMPLS[@]}"; do
    name="$(get_field "${entry}" 1)"
    assigned_port=$((BASE_PORT + port_offset))
    IMPL_PORT_MAP[${name}]=${assigned_port}
    IMPL_DB_MAP[${name}]="${TMPDIR_PATH}/${name}.db"
    if [[ "${first}" == "true" ]]; then
        first=false
    else
        PORTS_JSON+=","
    fi
    PORTS_JSON+="\"${name}\":${assigned_port}"
    port_offset=$((port_offset + 1))
done
PORTS_JSON+="}"

CONFIG_PATH="${TMPDIR_PATH}/airgradientz.json"
jq -n \
    --argjson ports "${PORTS_JSON}" \
    --arg mock_ip "127.0.0.1:${MOCK_PORT}" \
    '{
        pollIntervalMs: 600000,
        fetchTimeoutMs: 5000,
        maxApiRows: 10000,
        downsampleBuckets: {"5m": 300000, "10m": 600000, "15m": 900000, "30m": 1800000, "1h": 3600000, "1d": 86400000, "1w": 604800000},
        ports: $ports,
        devices: [
            { ip: $mock_ip, label: "indoor" }
        ]
    }' > "${CONFIG_PATH}"
log "Config written to ${CONFIG_PATH}"

# ── Step 4: Build all implementations upfront ─────────────────────────────────

log "Building all implementations..."
declare -A IMPL_BUILT=()
for entry in "${ALL_IMPLS[@]}"; do
    name="$(get_field "${entry}" 1)"
    dir="$(get_field "${entry}" 2)"
    build_cmd="$(get_field "${entry}" 3)"

    if [[ ! -d "${REPO_ROOT}/${dir}" ]]; then
        log "WARNING: Directory ${dir} does not exist, skipping ${name}"
        continue
    fi

    log "  Building ${name}..."
    if ! (cd "${REPO_ROOT}" && eval "${build_cmd}") >/dev/null 2>&1; then
        log "  WARNING: Build failed for ${name}, skipping"
        continue
    fi
    IMPL_BUILT[${name}]=1
    log "  Build succeeded: ${name}"
done

if [[ -z "${IMPL_BUILT[${REF_IMPL}]+x}" ]]; then
    log "ERROR: Reference implementation '${REF_IMPL}' failed to build."
    exit 1
fi

# ── Step 5: Start reference implementation ────────────────────────────────────

log "Starting reference implementation: ${REF_IMPL}"
ref_pid="$(start_impl "${REF_ENTRY}")"
CHILD_PIDS+=("${ref_pid}")
ALLOCATED_PORTS+=("${IMPL_PORT_MAP[${REF_IMPL}]}")

# shellcheck disable=SC2310
if ! wait_ready "${REF_IMPL}" "${IMPL_PORT_MAP[${REF_IMPL}]}"; then
    log "ERROR: Reference implementation failed health check."
    exit 1
fi

log "Waiting ${POLL_WAIT}s for ${REF_IMPL} to poll mock device..."
sleep "${POLL_WAIT}"

ref_port="${IMPL_PORT_MAP[${REF_IMPL}]}"
reading_count="$(curl -sf "http://localhost:${ref_port}/api/readings/count?from=0&to=9999999999999" 2>/dev/null | jq -r '.count // 0')" || reading_count=0
if [[ "${reading_count}" -eq 0 ]]; then
    log "WARNING: Reference has 0 readings, extending wait by 15s..."
    sleep 15
    reading_count="$(curl -sf "http://localhost:${ref_port}/api/readings/count?from=0&to=9999999999999" 2>/dev/null | jq -r '.count // 0')" || reading_count=0
    if [[ "${reading_count}" -eq 0 ]]; then
        log "ERROR: Reference still has 0 readings. Aborting."
        exit 1
    fi
fi
log "Reference has ${reading_count} reading(s)"

# ── Step 6: Test non-ref implementations in batches ──────────────────────────

total_exit_code=0
batch_num=0
tested_count=0

TESTABLE_IMPLS=()
for entry in "${NON_REF_IMPLS[@]}"; do
    name="$(get_field "${entry}" 1)"
    if [[ -n "${IMPL_BUILT[${name}]+x}" ]]; then
        TESTABLE_IMPLS+=("${entry}")
    fi
done

if [[ ${#TESTABLE_IMPLS[@]} -eq 0 ]]; then
    log "ERROR: No non-reference implementations built successfully."
    exit 1
fi

total_batches=$(( (${#TESTABLE_IMPLS[@]} + BATCH_SIZE - 1) / BATCH_SIZE ))
log "Running ${#TESTABLE_IMPLS[@]} implementations in ${total_batches} batch(es)"

idx=0
while [[ ${idx} -lt ${#TESTABLE_IMPLS[@]} ]]; do
    batch_num=$((batch_num + 1))

    batch_end=$((idx + BATCH_SIZE))
    if [[ ${batch_end} -gt ${#TESTABLE_IMPLS[@]} ]]; then
        batch_end=${#TESTABLE_IMPLS[@]}
    fi
    BATCH=("${TESTABLE_IMPLS[@]:${idx}:${BATCH_SIZE}}")
    batch_names=()
    for entry in "${BATCH[@]}"; do
        batch_names+=("$(get_field "${entry}" 1)")
    done

    log ""
    log "── Batch ${batch_num}/${total_batches}: ${batch_names[*]} ──"

    # Start batch servers with staggered startup
    declare -a batch_pid_ports=()
    declare -a batch_compare_args=("${REF_IMPL}:${ref_port}")
    for entry in "${BATCH[@]}"; do
        name="$(get_field "${entry}" 1)"
        port="${IMPL_PORT_MAP[${name}]}"

        pid="$(start_impl "${entry}")"
        CHILD_PIDS+=("${pid}")
        ALLOCATED_PORTS+=("${port}")
        batch_pid_ports+=("${pid}:${port}")

        # Staggered: wait for this server to be ready before starting next
        ready=false
        # shellcheck disable=SC2310
        wait_ready "${name}" "${port}" && ready=true
        if [[ "${ready}" == "false" ]]; then
            log "WARNING: ${name} failed health check, excluding from batch"
        else
            batch_compare_args+=("${name}:${port}")
            tested_count=$((tested_count + 1))
        fi

        sleep 1
    done

    # Wait for batch servers to poll
    if [[ ${#batch_compare_args[@]} -ge 2 ]]; then
        log "Waiting ${POLL_WAIT}s for batch to poll..."
        sleep "${POLL_WAIT}"

        log "Running comparator: ${batch_compare_args[*]}"
        echo ""
        batch_exit=0
        bash "${SCRIPT_DIR}/compare.sh" "${batch_compare_args[@]}" || batch_exit=$?

        if [[ ${batch_exit} -ne 0 ]]; then
            total_exit_code=1
        fi
    else
        log "WARNING: Batch ${batch_num} has fewer than 2 ready implementations, skipping."
        total_exit_code=1
    fi

    # Stop batch servers (reference stays running)
    log "Stopping batch ${batch_num} servers..."
    stop_pids_and_ports "${batch_pid_ports[@]}"
    unset batch_pid_ports

    idx=${batch_end}
done

# ── Report ───────────────────────────────────────────────────────────────────

echo ""
if [[ ${total_exit_code} -eq 0 ]]; then
    log "All conformance tests passed! (${tested_count} implementations tested against ${REF_IMPL})"
else
    log "Some conformance tests failed (${tested_count} implementations tested against ${REF_IMPL})"
fi

exit "${total_exit_code}"
