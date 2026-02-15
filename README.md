# AirGradientz

Local dashboard for AirGradient air quality monitors. Seven independent implementations (C, Node.js, Rust, Zig, D, Elixir, Nim) share one web UI and one JSON config.

## Architecture

- `c/` — C23 implementation (port 3011), canonical web UI in `c/public/`
- `nodejs/` — Node.js/Express (port 3010), requires fnm + Node 24
- `rust/` — Rust 2024 edition (port 3009)
- `zig/` — Zig 0.15 (port 3012), references `../c/sqlite3.c`
- `d/` — D/LDC (port 3013)
- `elixir/` — Elixir/OTP (port 3013), requires Elixir 1.17+ / Erlang 26+
- `nim/` — Nim 2.2 (port 3015), references `../c/sqlite3.c`
- `airgradientz.json` — shared config (devices, poll interval, timeouts)
- `schema.sql` — shared DB schema (single source of truth)
- `c/public/` — canonical web UI (HTML/CSS/JS); all other impls symlink to it
- `bench/run.sh` — benchmark harness comparing all implementations

## Build & Run

Every implementation has a `build.sh` and `start.sh`. Root scripts orchestrate them:

```bash
# Root orchestration
./build.sh                   # build all implementations
./build.sh c rust nim        # build specific ones
./start.sh c                 # start one implementation (exec's into it)
./test.sh                    # run all tests (all 7 implementations)
./test.sh c                  # run specific tests

# Per-implementation (from repo root or impl directory)
c/build.sh && c/start.sh     # C (port 3011)
nodejs/build.sh              # Node.js deps; nodejs/start.sh runs (port 3010)
rust/build.sh && rust/start.sh  # Rust (port 3009)
zig/build.sh && zig/start.sh    # Zig (port 3012)
d/build.sh && d/start.sh        # D (port 3013)
elixir/build.sh && elixir/start.sh  # Elixir (port 3013)
nim/build.sh && nim/start.sh    # Nim (port 3015)

# Debug / direct commands
cd c && make debug           # ASan + UBSan
cd rust && cargo test        # Rust tests

# Benchmark (all impls, or specify: bench/run.sh nodejs rust)
./bench/run.sh
```

## Configuration

Priority: env vars > `airgradientz.json` > hardcoded defaults.

| Source | Keys |
|--------|------|
| Env vars | `PORT`, `DB_PATH`, `CONFIG_PATH` |
| JSON file | `devices[].ip/label`, `pollIntervalMs`, `fetchTimeoutMs`, `maxApiRows` |

Config file search order: `CONFIG_PATH` env, `./airgradientz.json`, `../airgradientz.json`.

## Code Conventions

- C: C23 standard, `-Werror -Wpedantic`, all warnings enabled
- C: epoll event loop, pool allocator with string arenas for query results
- Rust: 2024 edition, mio-based event loop, RAII memory management
- D: LDC compiler, epoll event loop
- Elixir: OTP supervision tree, hand-rolled :gen_tcp HTTP server, exqlite + jason deps
- Nim: 2.2, epoll event loop, ORC GC, SQLite via C FFI to vendored sqlite3.c
- Zig: 0.15 APIs, epoll event loop, per-connection arena allocator
- SQLite: WAL mode, bundled amalgamation (vendored in `c/sqlite3.c`)
- Web UI: vanilla HTML/CSS/JS, no build step
- All compiled impls: single-threaded non-blocking I/O (no thread-per-connection)

## API Endpoints (all impls)

All implementations expose identical endpoints:
- `GET /api/readings?from=&to=&device=&limit=` — historical readings
- `GET /api/readings/latest` — latest reading per device
- `GET /api/devices` — device summaries
- `GET /api/health` — per-device poll health
- `GET /api/config` — active configuration
- `GET /api/stats` — runtime introspection (memory, uptime, counters)

## Testing

All implementations have tests. Run `./test.sh` for all, or `./test.sh <impl>` for one.

| Impl | Framework | Command | Count |
|------|-----------|---------|-------|
| C | Custom test runner | `make -C c test` | 65 |
| Node.js | `node:test` | `cd nodejs && npm test` | 66 |
| Rust | `cargo test` | `cd rust && cargo test` | 35 |
| Zig | `zig build test` | `cd zig && zig build test` | 35 |
| D | Built-in `unittest` | `source d/activate-toolchain.sh && cd d && dub test` | 3 modules |
| Elixir | ExUnit | `cd elixir && mix test` | 13 |
| Nim | `unittest` module | `cd nim && nim c -r --path:src tests/test_db.nim` | 16 |

## Gotchas

- `c/sqlite3.c` and `c/sqlite3.h` are vendored (marked in `.gitattributes`) — don't edit
- Zig and Nim impls share C's SQLite via `../c/sqlite3.c` — C impl must be present
- Node.js requires fnm for version management (v24 pinned)
- `references/` is gitignored — local-only reference materials
- `.env` in `nodejs/` is gitignored — contains PORT/DB_PATH overrides
- Each impl creates its own `airgradientz.db` in its directory
- D and Elixir share port 3013 — don't run both simultaneously
- Schema changes go in `schema.sql`, not in individual implementations
