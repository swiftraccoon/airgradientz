# AirGradientz

[![Lint](https://github.com/swiftraccoon/airgradientz/actions/workflows/lint.yml/badge.svg)](https://github.com/swiftraccoon/airgradientz/actions/workflows/lint.yml)
[![Test](https://github.com/swiftraccoon/airgradientz/actions/workflows/test.yml/badge.svg)](https://github.com/swiftraccoon/airgradientz/actions/workflows/test.yml)

Local dashboard for AirGradient air quality monitors. Multiple independent implementations share one web UI and one JSON config.

## Architecture

- `c/` — C23 implementation (port 3011), canonical web UI in `c/public/`
- `nodejs/` — Node.js (port 3010), requires fnm + Node 24
- `rust/` — Rust 2024 edition (port 3009)
- `zig/` — Zig 0.15 (port 3012), references `../c/sqlite3.c`
- `d/` — D/LDC (port 3014)
- `elixir/` — Elixir/OTP (port 3013), requires Elixir 1.17+ / Erlang 26+
- `nim/` — Nim 2.2 (port 3015), references `../c/sqlite3.c`
- `go/` — Go 1.25 (port 3016), `github.com/mattn/go-sqlite3` (CGo)
- `bash/` — Pure Bash (port 3017), ncat + sqlite3 CLI + jq + curl
- `asm/` — x86_64 NASM assembly (port 3018), links libc + vendored sqlite3.o
- `haskell/` — Haskell/GHC 9.10 (port 3019), direct-sqlite
- `airgradientz.json` — shared config (devices, poll interval, timeouts)
- `schema.sql` — shared DB schema (single source of truth)
- `c/public/` — canonical web UI (HTML/CSS/JS); all other impls symlink to it
- `queries.sql` — named SQL queries (column lists, INSERT/SELECT patterns)
- `test-fixtures.json` — canonical AirGradient device payloads for test suites
- `test-spec.json` — shared edge case definitions (response shapes, query behaviors, downsample buckets)
- `conformance/` — cross-implementation conformance test suite (compares all impls against C reference)
- `bench/run.sh` — benchmark harness comparing all implementations
- `docs/api-spec.md` — API response contract (JSON shapes for all endpoints)
- `docs/polling-spec.md` — device polling contract (field mapping, health state machine)

## Build & Run

Every implementation has a `build.sh` and `start.sh`. Root scripts orchestrate them:

```bash
# Root orchestration
./build.sh                   # build all implementations
./build.sh c rust nim        # build specific ones
./start.sh c                 # start one implementation (exec's into it)
./test.sh                    # run all tests
./test.sh c                  # run specific tests
./lint.sh                    # lint all implementations
./lint.sh rust go            # lint specific ones

# Per-implementation (from repo root or impl directory)
c/build.sh && c/start.sh     # C (port 3011)
nodejs/build.sh              # Node.js deps; nodejs/start.sh runs (port 3010)
rust/build.sh && rust/start.sh  # Rust (port 3009)
zig/build.sh && zig/start.sh    # Zig (port 3012)
d/build.sh && d/start.sh        # D (port 3014)
elixir/build.sh && elixir/start.sh  # Elixir (port 3013)
nim/build.sh && nim/start.sh    # Nim (port 3015)
go/build.sh && go/start.sh      # Go (port 3016)
bash/build.sh && bash/start.sh  # Bash (port 3017)
asm/build.sh && asm/start.sh    # ASM (port 3018)
haskell/build.sh && haskell/start.sh  # Haskell (port 3019)

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
| JSON file | `defaults.*`, `ports.<impl>`, `devices[].ip/label`, `pollIntervalMs`, `fetchTimeoutMs`, `maxApiRows`, `downsampleBuckets` |

Config file search order: `CONFIG_PATH` env, `./airgradientz.json`, `../airgradientz.json`.

## Code Conventions

- C: C23 standard, `-Werror -Wpedantic`, all warnings enabled
- C: epoll event loop, pool allocator with string arenas for query results
- Rust: 2024 edition, mio-based event loop, RAII memory management
- D: LDC compiler, epoll event loop
- Elixir: OTP supervision tree, hand-rolled :gen_tcp HTTP server, exqlite for SQLite
- Nim: 2.2, epoll event loop, ORC GC, SQLite via C FFI to vendored sqlite3.c
- Zig: 0.15 APIs, epoll event loop, per-connection arena allocator
- Go: 1.25, net/http (goroutine-per-connection), `github.com/mattn/go-sqlite3` (CGo)
- Bash: ncat fork-per-connection, sqlite3 CLI, jq for JSON, shellcheck -o all clean
- ASM: x86_64 NASM, epoll event loop, System V AMD64 ABI, links libc + sqlite3.o from `../c/sqlite3.c`
- Haskell: GHC2021, `-Wall -Wextra -Werror`, direct-sqlite, custom HTTP server via network, MVar-synchronized DB
- SQLite: WAL mode, bundled amalgamation (vendored in `c/sqlite3.c`)
- Web UI: vanilla HTML/CSS/JS, no build step
- All compiled impls except Go, Bash, and Haskell: single-threaded non-blocking I/O (no thread-per-connection, ASM uses poller thread for background polling)

## API Endpoints (all impls)

All implementations expose identical endpoints:
- `GET /api/readings?from=&to=&device=&limit=&downsample=` — historical readings (downsample: `5m|10m|15m|30m|1h|1d|1w`)
- `GET /api/readings/latest` — latest reading per device
- `GET /api/readings/count?from=&to=&device=` — count matching readings
- `GET /api/devices` — device summaries
- `GET /api/health` — per-device poll health
- `GET /api/config` — active configuration (includes `downsampleBuckets`)
- `GET /api/stats` — runtime introspection (memory, uptime, counters)

## Testing

All implementations have tests. Run `./test.sh` for all, or `./test.sh <impl>` for one.

| Impl | Framework | Command | Count |
|------|-----------|---------|-------|
| C | Custom test runner | `make -C c test` | 84 |
| Node.js | `node:test` | `cd nodejs && npm test` | 147 |
| Rust | `cargo test` | `cd rust && cargo test` | 63 |
| Zig | `zig build test` | `cd zig && zig build test` | 50 |
| D | Built-in `unittest` | `source d/activate-toolchain.sh && cd d && dub test` | 5 modules |
| Elixir | ExUnit | `cd elixir && mix test` | 156 |
| Nim | `unittest` module | `cd nim && nim c -r --path:src tests/test_db.nim` | 43 |
| Go | `go test` | `cd go && go test ./...` | 131 |
| Bash | Custom test runner | `cd bash && bash tests/run_tests.sh` | 164 |
| ASM | Custom test runner | `make -C asm test` | 148 |
| Haskell | tasty + tasty-hunit | `cd haskell && cabal test` | 178 |

## Linting

Run `./lint.sh` for all, or `./lint.sh <impl>` for specific implementations. CI runs on push/PR via GitHub Actions (`.github/workflows/lint.yml`).

| Impl | Tool | Config |
|------|------|--------|
| C | gcc `-Werror -Wpedantic` + cppcheck | `c/Makefile` |
| Node.js | ESLint 9 + eslint-plugin-security | `nodejs/eslint.config.mjs` |
| Rust | clippy (pedantic + nursery) + cargo-audit | `#![deny]` in `rust/src/main.rs` |
| Go | go vet + golangci-lint (~50 linters) | `go/.golangci.yml` |
| Elixir | credo (strict) + dialyxir + mix_audit | `elixir/.credo.exs` |
| Bash | shellcheck `-x -o all` | — |
| Zig | compiler warnings | — |
| Nim | compiler hints/warnings | — |
| D | LDC compiler | — |
| ASM | NASM syntax check | — |
| Haskell | GHC `-Wall -Wextra -Werror` | `haskell/cabal.project` |

## Gotchas

- `c/sqlite3.c` and `c/sqlite3.h` are vendored (marked in `.gitattributes`) — don't edit
- Zig, Nim, and ASM impls share C's SQLite via `../c/sqlite3.c` — C impl must be present
- Node.js requires fnm for version management (v24 pinned)
- `references/` is gitignored — local-only reference materials
- `.env` in `nodejs/` is gitignored — contains PORT/DB_PATH overrides
- Each impl creates its own `airgradientz.db` in its directory
- D uses port 3014, Elixir uses port 3013 — verify `airgradientz.json` ports map if overriding
- Bash requires ncat (nmap), sqlite3, jq, curl at runtime
- ASM requires nasm assembler and gcc linker at build time
- Haskell requires GHCup (GHC 9.10, cabal 3.14) — `~/.ghcup/bin` must be on PATH
- Schema changes go in `schema.sql`, not in individual implementations
- Query column lists come from `queries.sql` — keep in sync with `schema.sql`
- Test suites should load `test-fixtures.json` for device payloads instead of hardcoding
