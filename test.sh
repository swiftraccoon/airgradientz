#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

ALL_IMPLS=(c nodejs rust zig d elixir nim go bash asm haskell forth)

# Determine which impls to test
if [[ $# -eq 0 ]]; then
    impls=("${ALL_IMPLS[@]}")
else
    impls=("$@")
fi

failed=()
succeeded=()
skipped=()

for impl in "${impls[@]}"; do
    if [[ ! -d "$impl" ]]; then
        echo "error: unknown implementation '$impl'" >&2
        failed+=("$impl")
        continue
    fi

    case "$impl" in
        c)
            echo "==> Testing $impl..."
            if make -C c test; then
                succeeded+=("$impl")
            else
                failed+=("$impl")
            fi
            ;;
        nodejs)
            echo "==> Testing $impl..."
            if (cd nodejs && npm test); then
                succeeded+=("$impl")
            else
                failed+=("$impl")
            fi
            ;;
        rust)
            echo "==> Testing $impl..."
            if (cd rust && cargo test); then
                succeeded+=("$impl")
            else
                failed+=("$impl")
            fi
            ;;
        zig)
            echo "==> Testing $impl..."
            if (cd zig && zig build test); then
                succeeded+=("$impl")
            else
                failed+=("$impl")
            fi
            ;;
        d)
            echo "==> Testing $impl..."
            if (source d/activate-toolchain.sh && cd d && dub test); then
                succeeded+=("$impl")
            else
                failed+=("$impl")
            fi
            ;;
        elixir)
            echo "==> Testing $impl..."
            if (cd elixir && mix test); then
                succeeded+=("$impl")
            else
                failed+=("$impl")
            fi
            ;;
        nim)
            echo "==> Testing $impl..."
            if (cd nim && export PATH="$HOME/.nimble/bin:$PATH" && nim c -r --path:src --threads:on --mm:orc --passC:"-DSQLITE_THREADSAFE=2" --passC:"-DSQLITE_OMIT_LOAD_EXTENSION" tests/test_db.nim); then
                succeeded+=("$impl")
            else
                failed+=("$impl")
            fi
            ;;
        go)
            echo "==> Testing $impl..."
            if (cd go && go test -count=1 ./...); then
                succeeded+=("$impl")
            else
                failed+=("$impl")
            fi
            ;;
        bash)
            echo "==> Testing $impl..."
            if (cd bash && shellcheck -x -o all server.sh handler.sh lib/*.sh build.sh start.sh tests/run_tests.sh && bash tests/run_tests.sh); then
                succeeded+=("$impl")
            else
                failed+=("$impl")
            fi
            ;;
        asm)
            echo "==> Testing $impl..."
            if make -C asm test; then
                succeeded+=("$impl")
            else
                failed+=("$impl")
            fi
            ;;
        haskell)
            echo "==> Testing $impl..."
            if (export PATH="$HOME/.ghcup/bin:$PATH" && cd haskell && cabal test --project-file=cabal.project); then
                succeeded+=("$impl")
            else
                failed+=("$impl")
            fi
            ;;
        forth)
            echo "==> Testing $impl..."
            if (cd forth && bash tests/run_tests.sh); then
                succeeded+=("$impl")
            else
                failed+=("$impl")
            fi
            ;;
        *)
            echo "==> Skipping $impl (no tests)"
            skipped+=("$impl")
            ;;
    esac
    echo ""
done

# Summary
echo "==> Test summary"
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
