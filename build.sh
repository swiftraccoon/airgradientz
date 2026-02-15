#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

ALL_IMPLS=(c nodejs rust zig d elixir nim go)

usage() {
    echo "Usage: $0 [impl...]"
    echo "  No args: build all implementations"
    echo "  Impls: ${ALL_IMPLS[*]}"
}

# Determine which impls to build
if [[ $# -eq 0 ]]; then
    impls=("${ALL_IMPLS[@]}")
else
    impls=("$@")
fi

failed=()
succeeded=()

for impl in "${impls[@]}"; do
    if [[ ! -d "$impl" ]]; then
        echo "error: unknown implementation '$impl'" >&2
        echo "  Available: ${ALL_IMPLS[*]}" >&2
        failed+=("$impl")
        continue
    fi

    if [[ ! -f "$impl/build.sh" ]]; then
        echo "error: $impl/build.sh not found" >&2
        failed+=("$impl")
        continue
    fi

    echo "==> Building $impl..."
    if "$impl/build.sh"; then
        succeeded+=("$impl")
    else
        echo "error: $impl build failed" >&2
        failed+=("$impl")
    fi
    echo ""
done

# Summary
echo "==> Build summary"
if [[ ${#succeeded[@]} -gt 0 ]]; then
    echo "  OK: ${succeeded[*]}"
fi
if [[ ${#failed[@]} -gt 0 ]]; then
    echo "  FAILED: ${failed[*]}"
    exit 1
fi
