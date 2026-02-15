#!/usr/bin/env bash
set -euo pipefail

# ── Benchmark harness for airgradientz implementations ──────────────────────
# Usage: ./bench/run.sh [--concurrent N] [impl...]
#        e.g. ./bench/run.sh --concurrent 50 c rust zig
#        ./bench/run.sh             default: all implementations, sequential only

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
DATE="$(date +%Y-%m-%d)"
RESULTS_DIR="$REPO_ROOT/bench/results"
RESULTS_FILE="$RESULTS_DIR/$DATE.md"
REQUESTS_PER_ENDPOINT=100
CONCURRENT=0
HEALTH_TIMEOUT=15   # seconds
HEALTH_INTERVAL=0.5 # seconds

# ── Dependency check ────────────────────────────────────────────────────────

check_deps() {
    local missing=()
    for cmd in bash curl jq bc; do
        if ! command -v "$cmd" &>/dev/null; then
            missing+=("$cmd")
        fi
    done
    if [[ ${#missing[@]} -gt 0 ]]; then
        echo "ERROR: Missing required dependencies: ${missing[*]}" >&2
        echo "Install them with your package manager, e.g.:" >&2
        echo "  sudo apt install ${missing[*]}" >&2
        echo "  brew install ${missing[*]}" >&2
        exit 1
    fi
}

# ── Logging ──────────────────────────────────────────────────────────────────

log() { echo "[bench] $*" >&2; }

# ── Implementation registry ─────────────────────────────────────────────────
# Format: name|port|directory|build_cmd|start_cmd|binary_path

# Read ports from airgradientz.json (fall back to defaults if jq fails)
get_port() {
    local impl="$1"
    local default="$2"
    local port
    port=$(jq -r ".ports.${impl} // empty" "$REPO_ROOT/airgradientz.json" 2>/dev/null) || true
    echo "${port:-$default}"
}

PORT_C=$(get_port c 3011)
PORT_NODEJS=$(get_port nodejs 3010)
PORT_RUST=$(get_port rust 3009)
PORT_ZIG=$(get_port zig 3012)
PORT_D=$(get_port d 3014)
PORT_ELIXIR=$(get_port elixir 3013)
PORT_NIM=$(get_port nim 3015)
PORT_GO=$(get_port go 3016)
PORT_BASH=$(get_port bash 3017)

IMPL_REGISTRY=(
    "c|${PORT_C}|c|cd c && make clean && make|cd c && ./airgradientz|c/airgradientz"
    "nodejs|${PORT_NODEJS}|nodejs|cd nodejs && npm install --silent|cd nodejs && node --env-file=.env server.js|n/a"
    "rust|${PORT_RUST}|rust|cd rust && cargo build --release 2>/dev/null|cd rust && ./target/release/airgradientz|rust/target/release/airgradientz"
    "zig|${PORT_ZIG}|zig|cd zig && zig build -Doptimize=ReleaseFast|cd zig && ./zig-out/bin/airgradientz|zig/zig-out/bin/airgradientz"
    "d|${PORT_D}|d|cd d && source ~/dlang/ldc-*/activate 2>/dev/null; dub build -b release|cd d && ./airgradientz|d/airgradientz"
    "elixir|${PORT_ELIXIR}|elixir|cd elixir && mix deps.get --quiet && mix compile|cd elixir && mix run --no-halt|n/a"
    "nim|${PORT_NIM}|nim|cd nim && export PATH=\$HOME/.nimble/bin:\$PATH && nim c -d:release --threads:on --mm:orc --path:src -o:airgradientz src/airgradientz.nim 2>/dev/null|cd nim && ./airgradientz|nim/airgradientz"
    "go|${PORT_GO}|go|cd go && go build -o airgradientz .|cd go && ./airgradientz|go/airgradientz"
    "bash|${PORT_BASH}|bash|cd bash && bash build.sh|cd bash && bash server.sh|n/a"
)

# Display names for the report header
declare -A DISPLAY_NAMES=(
    [c]="C"
    [nodejs]="Node.js"
    [rust]="Rust"
    [zig]="Zig"
    [d]="D"
    [elixir]="Elixir"
    [nim]="Nim"
    [go]="Go"
    [bash]="Bash"
)

# Canonical order for table columns
IMPL_ORDER=(c nodejs rust zig d elixir nim go bash)

# ── Process tracking for cleanup ─────────────────────────────────────────────

ACTIVE_PID=""

cleanup() {
    if [[ -n "$ACTIVE_PID" ]] && kill -0 "$ACTIVE_PID" 2>/dev/null; then
        log "Cleaning up process $ACTIVE_PID..."
        kill_impl "$ACTIVE_PID"
    fi
}
trap cleanup EXIT INT TERM

# ── Functions ────────────────────────────────────────────────────────────────

get_field() {
    local entry="$1" field="$2"
    echo "$entry" | cut -d'|' -f"$field"
}

build_impl() {
    local name="$1" build_cmd="$2"
    log "Building $name..."
    if ! (cd "$REPO_ROOT" && eval "$build_cmd") >/dev/null 2>&1; then
        log "WARNING: Build failed for $name, skipping."
        return 1
    fi
    log "Build succeeded for $name."
    return 0
}

start_impl() {
    local name="$1" start_cmd="$2"
    log "Starting $name..."
    (cd "$REPO_ROOT" && eval "$start_cmd") >/dev/null 2>&1 &
    local pid=$!
    echo "$pid"
}

wait_ready() {
    local name="$1" port="$2"
    local url="http://localhost:$port/api/health"
    local elapsed=0

    log "Waiting for $name to be ready on port $port..."
    while (( $(echo "$elapsed < $HEALTH_TIMEOUT" | bc -l) )); do
        if curl -sf "$url" -o /dev/null 2>/dev/null; then
            log "$name is ready (${elapsed}s)."
            return 0
        fi
        sleep "$HEALTH_INTERVAL"
        elapsed="$(echo "$elapsed + $HEALTH_INTERVAL" | bc -l)"
    done

    log "WARNING: $name did not become ready within ${HEALTH_TIMEOUT}s."
    return 1
}

measure_startup() {
    local port="$1"
    local url="http://localhost:$port/api/health"
    local start_ns end_ns elapsed_ms

    start_ns="$(date +%s%N)"

    # The server is already started; we measure from now until health responds.
    # Caller should call this immediately after start_impl.
    local elapsed=0
    while (( $(echo "$elapsed < $HEALTH_TIMEOUT" | bc -l) )); do
        if curl -sf "$url" -o /dev/null 2>/dev/null; then
            end_ns="$(date +%s%N)"
            elapsed_ms="$(echo "($end_ns - $start_ns) / 1000000" | bc)"
            echo "$elapsed_ms"
            return 0
        fi
        sleep "$HEALTH_INTERVAL"
        elapsed="$(echo "$elapsed + $HEALTH_INTERVAL" | bc -l)"
    done

    echo "-"
    return 1
}

get_stats() {
    local port="$1"
    curl -sf "http://localhost:$port/api/stats" 2>/dev/null || echo "{}"
}

get_rss_from_stats() {
    local stats_json="$1"
    echo "$stats_json" | jq -r '.memory_rss_bytes // empty' 2>/dev/null || echo ""
}

get_requests_from_stats() {
    local stats_json="$1"
    echo "$stats_json" | jq -r '.requests_served // empty' 2>/dev/null || echo ""
}

bytes_to_mb() {
    local bytes="$1"
    if [[ -z "$bytes" || "$bytes" == "-" || "$bytes" == "null" ]]; then
        echo "-"
        return
    fi
    # bc may omit leading zero; printf fixes it
    local val
    val="$(echo "scale=2; $bytes / 1048576" | bc -l)"
    printf '%.2f' "$val"
}

run_load() {
    local port="$1" endpoint="$2"
    local url="http://localhost:$port$endpoint"
    local total=0
    local count=0

    for (( i=1; i<=REQUESTS_PER_ENDPOINT; i++ )); do
        local time_s
        time_s="$(curl -sf -o /dev/null -w '%{time_total}' "$url" 2>/dev/null || echo "")"
        if [[ -n "$time_s" ]]; then
            total="$(echo "$total + $time_s" | bc -l)"
            count=$((count + 1))
        fi
    done

    if [[ $count -eq 0 ]]; then
        echo "-"
        return
    fi

    # Average in milliseconds
    local avg
    avg="$(echo "scale=2; ($total / $count) * 1000" | bc -l)"
    printf '%.2f' "$avg"
}

run_concurrent_load() {
    local port="$1" endpoint="$2" concurrency="$3"
    local url="http://localhost:$port$endpoint"
    local tmpdir
    tmpdir="$(mktemp -d)"

    # Spawn N concurrent curl processes
    for (( i=1; i<=concurrency; i++ )); do
        curl -sf -o /dev/null -w '%{time_total}\n' "$url" > "$tmpdir/$i.txt" 2>/dev/null &
    done
    wait

    # Collect times
    local total=0 count=0 max=0
    for (( i=1; i<=concurrency; i++ )); do
        local t
        t="$(cat "$tmpdir/$i.txt" 2>/dev/null || echo "")"
        if [[ -n "$t" ]]; then
            total="$(echo "$total + $t" | bc -l)"
            count=$((count + 1))
            if (( $(echo "$t > $max" | bc -l) )); then
                max="$t"
            fi
        fi
    done
    rm -rf "$tmpdir"

    if [[ $count -eq 0 ]]; then
        echo "- -"
        return
    fi

    local avg_ms max_ms
    avg_ms="$(printf '%.2f' "$(echo "($total / $count) * 1000" | bc -l)")"
    max_ms="$(printf '%.2f' "$(echo "$max * 1000" | bc -l)")"
    echo "$avg_ms $max_ms"
}

get_binary_size() {
    local binary_path="$1"
    if [[ "$binary_path" == "n/a" ]]; then
        echo "-"
        return
    fi

    local full_path="$REPO_ROOT/$binary_path"
    if [[ -f "$full_path" ]]; then
        local size_bytes
        size_bytes="$(stat --printf='%s' "$full_path" 2>/dev/null || wc -c < "$full_path" 2>/dev/null || echo "")"
        if [[ -n "$size_bytes" ]]; then
            bytes_to_mb "$size_bytes"
            return
        fi
    fi
    echo "-"
}

kill_impl() {
    local pid="$1"
    if kill -0 "$pid" 2>/dev/null; then
        kill "$pid" 2>/dev/null || true
        # Wait up to 3 seconds for graceful shutdown
        local waited=0
        while kill -0 "$pid" 2>/dev/null && [[ $waited -lt 6 ]]; do
            sleep 0.5
            waited=$((waited + 1))
        done
        # Force kill if still alive
        if kill -0 "$pid" 2>/dev/null; then
            kill -9 "$pid" 2>/dev/null || true
            wait "$pid" 2>/dev/null || true
        fi
    fi
    wait "$pid" 2>/dev/null || true
}

# ── Parse arguments ─────────────────────────────────────────────────────────

resolve_impls() {
    local requested=("$@")
    local resolved=()

    if [[ ${#requested[@]} -eq 0 ]]; then
        # Default: all implementations
        for entry in "${IMPL_REGISTRY[@]}"; do
            resolved+=("$entry")
        done
    else
        for req in "${requested[@]}"; do
            local found=false
            for entry in "${IMPL_REGISTRY[@]}"; do
                local name
                name="$(get_field "$entry" 1)"
                if [[ "$name" == "$req" ]]; then
                    resolved+=("$entry")
                    found=true
                    break
                fi
            done
            if [[ "$found" == "false" ]]; then
                log "WARNING: Unknown implementation '$req', skipping."
            fi
        done
    fi

    printf '%s\n' "${resolved[@]}"
}

# ── Metric storage ──────────────────────────────────────────────────────────
# We store results in associative arrays keyed by impl name.

declare -A R_STARTUP
declare -A R_RSS_BASELINE
declare -A R_RSS_AFTER
declare -A R_AVG_READINGS
declare -A R_AVG_LATEST
declare -A R_AVG_DEVICES
declare -A R_AVG_STATS
declare -A R_REQUESTS
declare -A R_BINARY_SIZE
declare -A R_CONC_AVG
declare -A R_CONC_MAX

# Initialize all impls to "-"
for impl_name in "${IMPL_ORDER[@]}"; do
    R_STARTUP[$impl_name]="-"
    R_RSS_BASELINE[$impl_name]="-"
    R_RSS_AFTER[$impl_name]="-"
    R_AVG_READINGS[$impl_name]="-"
    R_AVG_LATEST[$impl_name]="-"
    R_AVG_DEVICES[$impl_name]="-"
    R_AVG_STATS[$impl_name]="-"
    R_REQUESTS[$impl_name]="-"
    R_BINARY_SIZE[$impl_name]="-"
    R_CONC_AVG[$impl_name]="-"
    R_CONC_MAX[$impl_name]="-"
done

# ── Main ─────────────────────────────────────────────────────────────────────

check_deps

# Parse --concurrent flag
POSITIONAL=()
while [[ $# -gt 0 ]]; do
    case "$1" in
        --concurrent)
            CONCURRENT="$2"
            shift 2
            ;;
        *)
            POSITIONAL+=("$1")
            shift
            ;;
    esac
done

IMPLS=()
while IFS= read -r line; do
    [[ -n "$line" ]] && IMPLS+=("$line")
done < <(resolve_impls "${POSITIONAL[@]}")

if [[ ${#IMPLS[@]} -eq 0 ]]; then
    log "No implementations to benchmark."
    exit 1
fi

log "Benchmarking ${#IMPLS[@]} implementation(s)..."
log ""

for entry in "${IMPLS[@]}"; do
    name="$(get_field "$entry" 1)"
    port="$(get_field "$entry" 2)"
    dir="$(get_field "$entry" 3)"
    build_cmd="$(get_field "$entry" 4)"
    start_cmd="$(get_field "$entry" 5)"
    binary_path="$(get_field "$entry" 6)"

    log "============================================"
    log "Testing $name (port $port)"
    log "============================================"

    # Check directory exists
    if [[ ! -d "$REPO_ROOT/$dir" ]]; then
        log "WARNING: Directory $dir does not exist, skipping $name."
        continue
    fi

    # Build
    if ! build_impl "$name" "$build_cmd"; then
        continue
    fi

    # Start
    pid="$(start_impl "$name" "$start_cmd")"
    ACTIVE_PID="$pid"

    # Measure startup time (polls health endpoint)
    startup_ms="$(measure_startup "$port")" || true
    if [[ "$startup_ms" == "-" ]]; then
        log "WARNING: $name failed to start, skipping."
        kill_impl "$pid"
        ACTIVE_PID=""
        continue
    fi
    R_STARTUP[$name]="$startup_ms"

    # Baseline stats
    log "Collecting baseline stats..."
    baseline_stats="$(get_stats "$port")"
    baseline_rss="$(get_rss_from_stats "$baseline_stats")"
    R_RSS_BASELINE[$name]="$(bytes_to_mb "$baseline_rss")"

    # Load test
    log "Running load test ($REQUESTS_PER_ENDPOINT requests per endpoint)..."

    log "  /api/readings..."
    R_AVG_READINGS[$name]="$(run_load "$port" '/api/readings?from=0&to=9999999999999')"

    log "  /api/readings/latest..."
    R_AVG_LATEST[$name]="$(run_load "$port" '/api/readings/latest')"

    log "  /api/devices..."
    R_AVG_DEVICES[$name]="$(run_load "$port" '/api/devices')"

    log "  /api/stats..."
    R_AVG_STATS[$name]="$(run_load "$port" '/api/stats')"

    # Concurrent load test
    if [[ $CONCURRENT -gt 0 ]]; then
        log "  Concurrent load ($CONCURRENT connections to /api/readings/latest)..."
        read -r conc_avg conc_max <<< "$(run_concurrent_load "$port" '/api/readings/latest' "$CONCURRENT")"
        R_CONC_AVG[$name]="$conc_avg"
        R_CONC_MAX[$name]="$conc_max"
    fi

    # Post-load stats
    log "Collecting post-load stats..."
    post_stats="$(get_stats "$port")"
    post_rss="$(get_rss_from_stats "$post_stats")"
    R_RSS_AFTER[$name]="$(bytes_to_mb "$post_rss")"

    requests_served="$(get_requests_from_stats "$post_stats")"
    R_REQUESTS[$name]="${requests_served:-"-"}"

    # Binary size
    R_BINARY_SIZE[$name]="$(get_binary_size "$binary_path")"

    # Kill
    log "Stopping $name..."
    kill_impl "$pid"
    ACTIVE_PID=""

    log "$name done."
    log ""
done

# ── Build results table ─────────────────────────────────────────────────────

# Determine which impls to include in the table (those that were requested or all)
requested_names=()
for entry in "${IMPLS[@]}"; do
    requested_names+=("$(get_field "$entry" 1)")
done

# Build column list preserving canonical order
cols=()
for impl_name in "${IMPL_ORDER[@]}"; do
    for req in "${requested_names[@]}"; do
        if [[ "$impl_name" == "$req" ]]; then
            cols+=("$impl_name")
            break
        fi
    done
done

# Generate markdown table
generate_table() {
    local header="| Metric |"
    local separator="|--------|"

    for col in "${cols[@]}"; do
        header+=" ${DISPLAY_NAMES[$col]} |"
        separator+="------|"
    done

    echo "$header"
    echo "$separator"

    # Row helper
    print_row() {
        local label="$1"
        shift
        local -n arr=$1
        local row="| $label |"
        for col in "${cols[@]}"; do
            row+=" ${arr[$col]} |"
        done
        echo "$row"
    }

    print_row "Startup (ms)" R_STARTUP
    print_row "RSS baseline (MB)" R_RSS_BASELINE
    print_row "RSS after load (MB)" R_RSS_AFTER
    print_row "/api/readings avg (ms)" R_AVG_READINGS
    print_row "/api/readings/latest avg (ms)" R_AVG_LATEST
    print_row "/api/devices avg (ms)" R_AVG_DEVICES
    print_row "/api/stats avg (ms)" R_AVG_STATS
    print_row "Requests served" R_REQUESTS
    print_row "Binary size (MB)" R_BINARY_SIZE

    if [[ $CONCURRENT -gt 0 ]]; then
        print_row "Concurrent avg (ms) [N=$CONCURRENT]" R_CONC_AVG
        print_row "Concurrent max (ms) [N=$CONCURRENT]" R_CONC_MAX
    fi
}

# Write report
mkdir -p "$RESULTS_DIR"

footer_text() {
    if [[ $CONCURRENT -gt 0 ]]; then
        echo "*${REQUESTS_PER_ENDPOINT} sequential requests per endpoint, plus ${CONCURRENT} concurrent connections. Times in milliseconds.*"
    else
        echo "*${REQUESTS_PER_ENDPOINT} sequential requests per endpoint. Times in milliseconds.*"
    fi
}

{
    echo "# Benchmark Results -- $DATE"
    echo ""
    generate_table
    echo ""
    footer_text
} > "$RESULTS_FILE"

log "============================================"
log "Results written to $RESULTS_FILE"
log "============================================"
echo ""
echo "# Benchmark Results -- $DATE"
echo ""
generate_table
echo ""
footer_text
