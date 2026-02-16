#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

ALL_IMPLS=(c nodejs rust zig d elixir nim go bash asm)
STRICT=true

usage() {
    echo "Usage: $0 [--no-strict] [impl...]"
    echo "  No args: lint all implementations (strict by default, matches CI)"
    echo "  --no-strict: skip missing tools instead of failing"
    echo "  Impls: ${ALL_IMPLS[*]}"
}

# Parse flags
args=()
for arg in "$@"; do
    case "$arg" in
        --no-strict) STRICT=false ;;
        --strict) STRICT=true ;;
        --help|-h) usage; exit 0 ;;
        *) args+=("$arg") ;;
    esac
done

if [[ ${#args[@]} -eq 0 ]]; then
    impls=("${ALL_IMPLS[@]}")
else
    impls=("${args[@]}")
fi

failed=()
succeeded=()
skipped=()

has_tool() {
    command -v "$1" &>/dev/null
}

run_lint() {
    local name="$1"
    shift
    if "$@"; then
        return 0
    else
        echo "  LINT FAIL: $name" >&2
        return 1
    fi
}

# In strict mode, a missing tool is a failure, not a skip.
skip_tool() {
    local tool="$1"
    local install_hint="$2"
    if $STRICT; then
        echo "  FAIL (--strict): $tool not found ($install_hint)" >&2
        return 1
    else
        echo "  SKIP: $tool not found ($install_hint)"
        return 0
    fi
}

for impl in "${impls[@]}"; do
    if [[ ! -d "$impl" ]]; then
        echo "error: unknown implementation '$impl'" >&2
        failed+=("$impl")
        continue
    fi

    echo "==> Linting ${impl}..."
    impl_ok=true

    case "$impl" in
        c)
            # GCC with strict warnings (compile check)
            if has_tool gcc; then
                if ! run_lint "gcc-warnings" make -C c clean all; then
                    impl_ok=false
                fi
            else
                if ! skip_tool "gcc" "install: sudo dnf install gcc"; then impl_ok=false; fi
            fi
            # cppcheck (static analysis)
            if has_tool cppcheck; then
                if ! run_lint "cppcheck" cppcheck --enable=all --suppress=missingIncludeSystem \
                    --suppress=unusedFunction --suppress=unmatchedSuppression \
                    --suppress=checkersReport --suppress=normalCheckLevelMaxBranches \
                    --error-exitcode=1 \
                    --std=c11 -DSQLITE_THREADSAFE=2 --inline-suppr \
                    -I c/ c/src/; then
                    impl_ok=false
                fi
            else
                if ! skip_tool "cppcheck" "install: sudo dnf install cppcheck"; then impl_ok=false; fi
            fi
            ;;
        nodejs)
            if has_tool node && [[ -f nodejs/node_modules/.bin/eslint ]]; then
                if ! run_lint "eslint" bash -c 'cd nodejs && npx eslint .'; then
                    impl_ok=false
                fi
            else
                if ! skip_tool "eslint" "run: cd nodejs && npm install"; then impl_ok=false; fi
            fi
            # npm audit (dependency vulnerabilities)
            if has_tool npm; then
                if ! run_lint "npm-audit" bash -c 'cd nodejs && npm audit --omit=dev 2>/dev/null || true'; then
                    impl_ok=false
                fi
            fi
            ;;
        rust)
            if has_tool cargo; then
                # clippy (already strict via #![deny] in source)
                if ! run_lint "clippy" bash -c 'cd rust && cargo clippy -- -D warnings'; then
                    impl_ok=false
                fi
                # cargo audit (dependency vulnerabilities)
                if has_tool cargo-audit; then
                    if ! run_lint "cargo-audit" bash -c 'cd rust && cargo audit'; then
                        impl_ok=false
                    fi
                else
                    if ! skip_tool "cargo-audit" "install: cargo install cargo-audit"; then impl_ok=false; fi
                fi
            else
                if ! skip_tool "cargo" "install: rustup"; then impl_ok=false; fi
            fi
            ;;
        zig)
            # Zig's compiler IS the linter — strict errors baked into build
            if has_tool zig; then
                if ! run_lint "zig-build" bash -c 'cd zig && zig build 2>&1'; then
                    impl_ok=false
                fi
            else
                if ! skip_tool "zig" "install: https://ziglang.org/download/"; then impl_ok=false; fi
            fi
            ;;
        d)
            # D compiler warnings as errors
            if [[ -f d/activate-toolchain.sh ]]; then
                # dub build checks compiler warnings
                if ! run_lint "dub-build" bash -c 'source d/activate-toolchain.sh && cd d && dub build --force 2>&1'; then
                    impl_ok=false
                fi
                # dscanner (optional — not in CI)
                if has_tool dscanner || ls ~/dlang/*/bin/dscanner &>/dev/null 2>&1; then
                    if ! run_lint "dscanner" bash -c 'source d/activate-toolchain.sh && cd d && dscanner --styleCheck source/'; then
                        impl_ok=false
                    fi
                else
                    echo "  SKIP: dscanner not found (optional, not in CI)"
                fi
            else
                if ! skip_tool "D toolchain" "run: d/setup.sh"; then impl_ok=false; fi
            fi
            ;;
        elixir)
            if has_tool mix; then
                # credo (strict mode)
                if [[ -d elixir/deps/credo ]]; then
                    if ! run_lint "credo" bash -c 'cd elixir && mix credo --strict'; then
                        impl_ok=false
                    fi
                else
                    if ! skip_tool "credo" "run: cd elixir && mix deps.get"; then impl_ok=false; fi
                fi
                # dialyxir (type checking)
                if [[ -d elixir/deps/dialyxir ]]; then
                    if ! run_lint "dialyxir" bash -c 'cd elixir && mix dialyzer'; then
                        impl_ok=false
                    fi
                else
                    if ! skip_tool "dialyxir" "run: cd elixir && mix deps.get"; then impl_ok=false; fi
                fi
                # mix_audit (dependency vulnerabilities)
                if [[ -d elixir/deps/mix_audit ]]; then
                    if ! run_lint "mix-audit" bash -c 'cd elixir && mix deps.audit'; then
                        impl_ok=false
                    fi
                fi
            else
                if ! skip_tool "mix" "install: https://elixir-lang.org/install.html"; then impl_ok=false; fi
            fi
            ;;
        nim)
            # Nim compiler with strict hints/warnings
            if has_tool nim || [[ -x "$HOME/.nimble/bin/nim" ]]; then
                nimcmd="${HOME}/.nimble/bin/nim"
                if ! run_lint "nim-check" bash -c "cd nim && \"${nimcmd}\" check --path:src --hints:off --warnings:on src/airgradientz.nim 2>&1"; then
                    impl_ok=false
                fi
            else
                if ! skip_tool "nim" "install: curl -sSf https://nim-lang.org/choosenim/init.sh | bash"; then impl_ok=false; fi
            fi
            ;;
        go)
            if has_tool go; then
                # go vet (built-in)
                if ! run_lint "go-vet" bash -c 'cd go && go vet ./...'; then
                    impl_ok=false
                fi
                # golangci-lint (comprehensive)
                golangci=""
                if has_tool golangci-lint; then
                    golangci="golangci-lint"
                elif [[ -x "${GOPATH:-$HOME/go}/bin/golangci-lint" ]]; then
                    golangci="${GOPATH:-$HOME/go}/bin/golangci-lint"
                fi
                if [[ -n "$golangci" ]]; then
                    if ! run_lint "golangci-lint" bash -c "cd go && \"$golangci\" run"; then
                        impl_ok=false
                    fi
                else
                    if ! skip_tool "golangci-lint" "install: go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest"; then impl_ok=false; fi
                fi
            else
                if ! skip_tool "go" "install: https://go.dev/dl/"; then impl_ok=false; fi
            fi
            ;;
        bash)
            if has_tool shellcheck; then
                if ! run_lint "shellcheck" bash -c \
                    'cd bash && shellcheck -x -o all server.sh handler.sh lib/*.sh build.sh start.sh tests/run_tests.sh'; then
                    impl_ok=false
                fi
            else
                if ! skip_tool "shellcheck" "install: sudo dnf install ShellCheck"; then impl_ok=false; fi
            fi
            ;;
        asm)
            # NASM syntax check (assemble without linking)
            if has_tool nasm; then
                asm_ok=true
                for f in asm/src/*.asm; do
                    if ! nasm -f elf64 -o /dev/null "$f" 2>&1; then
                        asm_ok=false
                    fi
                done
                if ! $asm_ok; then
                    impl_ok=false
                fi
            else
                if ! skip_tool "nasm" "install: sudo dnf install nasm"; then impl_ok=false; fi
            fi
            ;;
        *)
            echo "  No linter configured for $impl"
            skipped+=("$impl")
            continue
            ;;
    esac

    if $impl_ok; then
        succeeded+=("$impl")
    else
        failed+=("$impl")
    fi
    echo ""
done

# Summary
echo "==> Lint summary"
if [[ ${#succeeded[@]} -gt 0 ]]; then
    echo "  PASSED: ${succeeded[*]}"
fi
if [[ ${#skipped[@]} -gt 0 ]]; then
    echo "  SKIPPED: ${skipped[*]}"
fi
if [[ ${#failed[@]} -gt 0 ]]; then
    echo "  FAILED: ${failed[*]}"
    exit 1
fi
