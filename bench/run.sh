#!/usr/bin/env bash
set -euo pipefail

# ── Benchmark harness for airgradientz implementations ──────────────────────
# Usage: ./bench/run.sh [--concurrent N] [--requests N] [--seed-rows N] [impl...]
#        e.g. ./bench/run.sh --concurrent 50 c rust zig
#        ./bench/run.sh c nodejs       benchmark specific impls
#        ./bench/run.sh                default: all implementations

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TIMESTAMP="$(date +%Y%m%d_%H%M%S)"
RESULTS_DIR="$REPO_ROOT/bench/results"
RESULTS_FILE="$RESULTS_DIR/${TIMESTAMP}.md"
REQUESTS=100
WARMUP=20
SEED_ROWS=1000
CONCURRENT=0
HEALTH_TIMEOUT=30
BENCH_TMP="$(mktemp -d)"

# ── Dependencies ─────────────────────────────────────────────────────────────

check_deps() {
    local missing=()
    for cmd in curl jq awk sqlite3; do
        command -v "$cmd" &>/dev/null || missing+=("$cmd")
    done
    if [[ ${#missing[@]} -gt 0 ]]; then
        echo "ERROR: Missing required dependencies: ${missing[*]}" >&2
        exit 1
    fi
}

# ── Logging ──────────────────────────────────────────────────────────────────

log() { printf '[bench] %s\n' "$*" >&2; }

# ── Implementation registry ─────────────────────────────────────────────────
# Format: name|port|directory|build_cmd|start_cmd|binary_path

get_port() {
    local impl="$1" default="$2" port
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
PORT_ASM=$(get_port asm 3018)
PORT_HASKELL=$(get_port haskell 3019)
PORT_FORTH=$(get_port forth 3020)

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
    "asm|${PORT_ASM}|asm|cd asm && make clean && make|cd asm && ./airgradientz|asm/airgradientz"
    "haskell|${PORT_HASKELL}|haskell|cd haskell && export PATH=\$HOME/.ghcup/bin:\$PATH && cabal build 2>/dev/null|cd haskell && export PATH=\$HOME/.ghcup/bin:\$PATH && cabal run airgradientz 2>/dev/null|n/a"
    "forth|${PORT_FORTH}|forth|cd forth && bash build.sh|cd forth && gforth src/main.fs|n/a"
)

declare -A DISPLAY_NAMES=(
    [c]="C" [nodejs]="Node.js" [rust]="Rust" [zig]="Zig" [d]="D"
    [elixir]="Elixir" [nim]="Nim" [go]="Go" [bash]="Bash"
    [asm]="x86_64 ASM" [haskell]="Haskell" [forth]="Forth"
)

IMPL_ORDER=(c nodejs rust zig d elixir nim go bash asm haskell forth)

# ── Process management ───────────────────────────────────────────────────────

ACTIVE_PID=""
ACTIVE_PORT=""

cleanup() {
    if [[ -n "$ACTIVE_PID" ]]; then
        kill_impl "$ACTIVE_PID" "$ACTIVE_PORT"
    fi
    rm -rf "$BENCH_TMP"
}
trap cleanup EXIT INT TERM

kill_impl() {
    local pid="$1" port="${2:-}"
    # Kill the entire process group (negative PID) to catch child processes
    # that outlive the subshell (e.g., ncat, beam, cabal-run wrappers).
    kill -0 "$pid" 2>/dev/null || { wait "$pid" 2>/dev/null || true; return; }
    kill -- "-$pid" 2>/dev/null || kill "$pid" 2>/dev/null || true
    local waited=0
    while kill -0 "$pid" 2>/dev/null && [[ $waited -lt 6 ]]; do
        sleep 0.5
        waited=$((waited + 1))
    done
    if kill -0 "$pid" 2>/dev/null; then
        kill -9 -- "-$pid" 2>/dev/null || kill -9 "$pid" 2>/dev/null || true
    fi
    wait "$pid" 2>/dev/null || true
    # Final safety: if the port is still held, force-kill whatever owns it
    if [[ -n "$port" ]]; then
        fuser -k "${port}/tcp" 2>/dev/null || true
    fi
}

# ── Utility functions ────────────────────────────────────────────────────────

get_field() { echo "$1" | cut -d'|' -f"$2"; }

bytes_to_mb() {
    local b="$1"
    [[ -z "$b" || "$b" == "-" || "$b" == "null" ]] && { echo "-"; return; }
    awk "BEGIN { printf \"%.2f\", $b / 1048576 }"
}

get_binary_size() {
    [[ "$1" == "n/a" ]] && { echo "-"; return; }
    local f="$REPO_ROOT/$1"
    if [[ -f "$f" ]]; then
        bytes_to_mb "$(stat --printf='%s' "$f" 2>/dev/null || echo "")"
    else
        echo "-"
    fi
}

get_stats() { curl -sf "http://localhost:$1/api/stats" 2>/dev/null || echo "{}"; }

# ── Build & startup ─────────────────────────────────────────────────────────

build_impl() {
    local name="$1" build_cmd="$2" t0
    log "Building $name..."
    t0="$(date +%s%N)"
    if ! (cd "$REPO_ROOT" && eval "$build_cmd") >/dev/null 2>&1; then
        log "WARNING: Build failed for $name, skipping."
        return 1
    fi
    local elapsed_ms=$(( ($(date +%s%N) - t0) / 1000000 ))
    log "Built $name (${elapsed_ms}ms)."
    echo "$elapsed_ms"
}

start_impl() {
    # Start in a new session (setsid) so the server and all its children
    # share a process group ID equal to the leader PID.  This lets kill_impl
    # send signals to -$pid and reliably reap the entire tree.
    setsid bash -c "cd \"$REPO_ROOT\" && eval \"\$1\"" -- "$1" >/dev/null 2>&1 &
    echo $!
}

measure_startup() {
    local port="$1" t0 deadline
    t0="$(date +%s%N)"
    deadline=$(( t0 + HEALTH_TIMEOUT * 1000000000 ))
    while [[ "$(date +%s%N)" -lt "$deadline" ]]; do
        if curl -sf -o /dev/null "http://localhost:$port/api/health" 2>/dev/null; then
            echo "$(( ($(date +%s%N) - t0) / 1000000 ))"
            return 0
        fi
        sleep 0.05
    done
    echo "-"
    return 1
}

# ── Database seeding ─────────────────────────────────────────────────────────

seed_db() {
    local db_path="$1"
    log "Seeding DB with $SEED_ROWS rows..."
    local now_ms
    now_ms="$(date +%s)000"

    {
        echo ".bail on"
        cat "$REPO_ROOT/schema.sql"
        echo "PRAGMA journal_mode=WAL;"
        echo "PRAGMA busy_timeout=5000;"
        echo "BEGIN;"
        awk -v now="$now_ms" -v rows="$SEED_ROWS" -v q="'" '
        BEGIN {
            interval = int(86400000 / rows)
            cols = "timestamp,device_id,device_type,device_ip,pm01,pm02,pm10,pm02_compensated,rco2,atmp,atmp_compensated,rhum,rhum_compensated,tvoc_index,nox_index,wifi,raw_json"
            for (i = 0; i < rows; i++) {
                ts = int(now - (rows - i) * interval)
                if (i % 2 == 0) {
                    printf "INSERT INTO readings (%s) VALUES (%d,%s84fce602549c%s,%sindoor%s,%s10.0.0.100%s,23.83,41.67,54.5,31.18,489,20.78,20.78,32.19,32.19,423.0,1,-51,%s{}%s);\n", cols, ts, q,q, q,q, q,q, q,q
                } else {
                    printf "INSERT INTO readings (%s) VALUES (%d,%secda3b1d09d8%s,%soutdoor%s,%s10.0.0.101%s,23.17,35.33,39.17,23.72,440,9.8,6.27,35.0,51.41,231.08,1,-42,%s{}%s);\n", cols, ts, q,q, q,q, q,q, q,q
                }
            }
        }'
        echo "COMMIT;"
    } | sqlite3 "$db_path" >/dev/null
}

clean_db() {
    rm -f "$1/airgradientz.db" "$1/airgradientz.db-wal" "$1/airgradientz.db-shm"
}

# ── Load testing ─────────────────────────────────────────────────────────────

warmup() {
    local port="$1"
    log "Warming up ($WARMUP requests per endpoint)..."
    local endpoints=(/api/health /api/stats /api/devices /api/readings/latest
        "/api/readings?from=0&to=9999999999999"
        "/api/readings/count?from=0&to=9999999999999"
        "/api/readings?from=0&to=9999999999999&downsample=1h")
    for ep in "${endpoints[@]}"; do
        for (( i=0; i<WARMUP; i++ )); do
            curl -sf -o /dev/null "http://localhost:$port$ep" 2>/dev/null || true
        done
    done
}

# Run sequential load test. Returns "avg / p95" in milliseconds.
run_load() {
    local port="$1" endpoint="$2" label="$3"
    local url="http://localhost:$port$endpoint"
    local tmpfile="$BENCH_TMP/${label}.txt"
    : > "$tmpfile"

    for (( i=0; i<REQUESTS; i++ )); do
        curl -sf -o /dev/null -w '%{time_total}\n' "$url" >> "$tmpfile" 2>/dev/null || true
    done

    sort -n "$tmpfile" | awk '
    { t[NR] = $1; sum += $1; n++ }
    END {
        if (n == 0) { print "-"; exit }
        avg = (sum / n) * 1000
        p95i = int(n * 0.95); if (p95i < 1) p95i = 1
        p95 = t[p95i] * 1000
        printf "%.1f / %.1f", avg, p95
    }'
}

# Run concurrent load test. Returns "avg / p95 / max" in milliseconds.
run_concurrent_load() {
    local port="$1" endpoint="$2" n="$3"
    local url="http://localhost:$port$endpoint"
    local tmpdir="$BENCH_TMP/conc"
    mkdir -p "$tmpdir"

    for (( i=0; i<n; i++ )); do
        curl -sf -o /dev/null -w '%{time_total}\n' "$url" > "$tmpdir/$i" 2>/dev/null &
    done
    wait

    cat "$tmpdir"/* 2>/dev/null | sort -n | awk '
    { t[NR] = $1; sum += $1; n++ }
    END {
        if (n == 0) { print "-"; exit }
        avg = (sum / n) * 1000
        p95i = int(n * 0.95); if (p95i < 1) p95i = 1
        p95 = t[p95i] * 1000
        max = t[n] * 1000
        printf "%.1f / %.1f / %.1f", avg, p95, max
    }'
    rm -rf "$tmpdir"
}

# ── Argument parsing ─────────────────────────────────────────────────────────

resolve_impls() {
    local requested=("$@")
    if [[ ${#requested[@]} -eq 0 ]]; then
        printf '%s\n' "${IMPL_REGISTRY[@]}"
        return
    fi
    for req in "${requested[@]}"; do
        local found=false
        for entry in "${IMPL_REGISTRY[@]}"; do
            if [[ "$(get_field "$entry" 1)" == "$req" ]]; then
                echo "$entry"
                found=true
                break
            fi
        done
        [[ "$found" == "false" ]] && log "WARNING: Unknown implementation '$req', skipping."
    done
}

POSITIONAL=()
while [[ $# -gt 0 ]]; do
    case "$1" in
        --concurrent)  CONCURRENT="$2"; shift 2 ;;
        --requests)    REQUESTS="$2"; shift 2 ;;
        --seed-rows)   SEED_ROWS="$2"; shift 2 ;;
        *)             POSITIONAL+=("$1"); shift ;;
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

# ── Metric storage ───────────────────────────────────────────────────────────

declare -A R_BUILD R_STARTUP R_BINARY
declare -A R_RSS_IDLE R_RSS_LOADED
declare -A R_READINGS R_LATEST R_COUNT R_DOWNSAMPLE R_DEVICES R_STATS
declare -A R_TOTAL_REQ R_CONC

for impl_name in "${IMPL_ORDER[@]}"; do
    R_BUILD[$impl_name]="-"
    R_STARTUP[$impl_name]="-"
    R_BINARY[$impl_name]="-"
    R_RSS_IDLE[$impl_name]="-"
    R_RSS_LOADED[$impl_name]="-"
    R_READINGS[$impl_name]="-"
    R_LATEST[$impl_name]="-"
    R_COUNT[$impl_name]="-"
    R_DOWNSAMPLE[$impl_name]="-"
    R_DEVICES[$impl_name]="-"
    R_STATS[$impl_name]="-"
    R_TOTAL_REQ[$impl_name]="-"
    R_CONC[$impl_name]="-"
done

# ── Main ─────────────────────────────────────────────────────────────────────

check_deps

log "Benchmarking ${#IMPLS[@]} implementation(s)"
log "  Requests per endpoint: $REQUESTS (+ $WARMUP warmup)"
log "  Seed rows: $SEED_ROWS"
[[ $CONCURRENT -gt 0 ]] && log "  Concurrent connections: $CONCURRENT"
log ""

for entry in "${IMPLS[@]}"; do
    name="$(get_field "$entry" 1)"
    port="$(get_field "$entry" 2)"
    dir="$(get_field "$entry" 3)"
    build_cmd="$(get_field "$entry" 4)"
    start_cmd="$(get_field "$entry" 5)"
    binary_path="$(get_field "$entry" 6)"

    log "════════════════════════════════════════════"
    log "  $name (port $port)"
    log "════════════════════════════════════════════"

    if [[ ! -d "$REPO_ROOT/$dir" ]]; then
        log "WARNING: Directory $dir does not exist, skipping."
        continue
    fi

    # Clean slate
    clean_db "$REPO_ROOT/$dir"

    # Build
    build_time="$(build_impl "$name" "$build_cmd")" || continue
    R_BUILD[$name]="$build_time"

    # Seed database before starting
    seed_db "$REPO_ROOT/$dir/airgradientz.db"

    # Start and measure startup time
    pid="$(start_impl "$start_cmd")"
    ACTIVE_PID="$pid"
    ACTIVE_PORT="$port"

    startup_ms="$(measure_startup "$port")" || true
    if [[ "$startup_ms" == "-" ]]; then
        log "WARNING: $name failed to start within ${HEALTH_TIMEOUT}s, skipping."
        kill_impl "$pid" "$port"
        ACTIVE_PID=""
        ACTIVE_PORT=""
        clean_db "$REPO_ROOT/$dir"
        continue
    fi
    R_STARTUP[$name]="$startup_ms"

    # Warmup
    warmup "$port"

    # Baseline RSS (after warmup, before load)
    stats="$(get_stats "$port")"
    R_RSS_IDLE[$name]="$(bytes_to_mb "$(echo "$stats" | jq -r '.memory_rss_bytes // empty')")"

    # Load tests (avg / p95)
    log "Load testing ($REQUESTS requests per endpoint)..."

    log "  /api/readings ..."
    R_READINGS[$name]="$(run_load "$port" '/api/readings?from=0&to=9999999999999' "${name}_readings")"

    log "  /api/readings/latest ..."
    R_LATEST[$name]="$(run_load "$port" '/api/readings/latest' "${name}_latest")"

    log "  /api/readings/count ..."
    R_COUNT[$name]="$(run_load "$port" '/api/readings/count?from=0&to=9999999999999' "${name}_count")"

    log "  /api/readings?downsample=1h ..."
    R_DOWNSAMPLE[$name]="$(run_load "$port" '/api/readings?from=0&to=9999999999999&downsample=1h' "${name}_downsample")"

    log "  /api/devices ..."
    R_DEVICES[$name]="$(run_load "$port" '/api/devices' "${name}_devices")"

    log "  /api/stats ..."
    R_STATS[$name]="$(run_load "$port" '/api/stats' "${name}_stats")"

    # Concurrent load
    if [[ $CONCURRENT -gt 0 ]]; then
        log "  Concurrent ($CONCURRENT connections to /api/readings/latest)..."
        R_CONC[$name]="$(run_concurrent_load "$port" '/api/readings/latest' "$CONCURRENT")"
    fi

    # Post-load stats
    stats="$(get_stats "$port")"
    R_RSS_LOADED[$name]="$(bytes_to_mb "$(echo "$stats" | jq -r '.memory_rss_bytes // empty')")"
    R_TOTAL_REQ[$name]="$(echo "$stats" | jq -r '.requests_served // "-"')"

    # Binary size
    R_BINARY[$name]="$(get_binary_size "$binary_path")"

    # Stop and clean up
    log "Stopping $name..."
    kill_impl "$pid" "$port"
    ACTIVE_PID=""
    ACTIVE_PORT=""
    clean_db "$REPO_ROOT/$dir"

    log "$name done."
    log ""
done

# ── Report generation ────────────────────────────────────────────────────────

# Determine which impls ran (in canonical order)
requested_names=()
for entry in "${IMPLS[@]}"; do
    requested_names+=("$(get_field "$entry" 1)")
done

cols=()
for impl_name in "${IMPL_ORDER[@]}"; do
    for req in "${requested_names[@]}"; do
        if [[ "$impl_name" == "$req" ]]; then
            cols+=("$impl_name")
            break
        fi
    done
done

generate_table() {
    local header="| Metric |"
    local separator="|--------|"
    for col in "${cols[@]}"; do
        header+=" ${DISPLAY_NAMES[$col]} |"
        separator+="---:|"
    done
    echo "$header"
    echo "$separator"

    print_row() {
        local label="$1"; shift
        local -n arr=$1
        local row="| $label |"
        for col in "${cols[@]}"; do
            row+=" ${arr[$col]} |"
        done
        echo "$row"
    }

    print_row "Build (ms)" R_BUILD
    print_row "Startup (ms)" R_STARTUP
    print_row "Binary (MB)" R_BINARY
    print_row "RSS idle (MB)" R_RSS_IDLE
    print_row "RSS loaded (MB)" R_RSS_LOADED
    echo "|---|$(printf -- '---|%.0s' "${cols[@]}")"
    print_row "/readings (avg/p95 ms)" R_READINGS
    print_row "/latest (avg/p95 ms)" R_LATEST
    print_row "/count (avg/p95 ms)" R_COUNT
    print_row "/downsample (avg/p95 ms)" R_DOWNSAMPLE
    print_row "/devices (avg/p95 ms)" R_DEVICES
    print_row "/stats (avg/p95 ms)" R_STATS
    echo "|---|$(printf -- '---|%.0s' "${cols[@]}")"
    print_row "Requests served" R_TOTAL_REQ

    if [[ $CONCURRENT -gt 0 ]]; then
        print_row "Concurrent (avg/p95/max ms) [N=$CONCURRENT]" R_CONC
    fi
}

footer_text() {
    echo ""
    echo "*$REQUESTS requests/endpoint (+ $WARMUP warmup), $SEED_ROWS seeded rows, avg/p95 in ms.*"
    [[ $CONCURRENT -gt 0 ]] && echo "*Concurrent: $CONCURRENT simultaneous connections, avg/p95/max in ms.*"
    echo ""
    echo "*Generated $(date -Iseconds)*"
}

# Write report
mkdir -p "$RESULTS_DIR"

{
    echo "# Benchmark Results — $TIMESTAMP"
    echo ""
    generate_table
    footer_text
} > "$RESULTS_FILE"

log "════════════════════════════════════════════"
log "Results written to $RESULTS_FILE"
log "════════════════════════════════════════════"

# Also print to stdout
echo ""
echo "# Benchmark Results — $TIMESTAMP"
echo ""
generate_table
footer_text
