#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

ALL_IMPLS=(c nodejs rust zig d elixir nim go bash asm)

if [[ $# -ne 1 ]]; then
    echo "Usage: $0 <impl>"
    echo "  Impls: ${ALL_IMPLS[*]}"
    exit 1
fi

impl="$1"

if [[ ! -d "$impl" ]]; then
    echo "error: unknown implementation '$impl'" >&2
    echo "  Available: ${ALL_IMPLS[*]}" >&2
    exit 1
fi

if [[ ! -f "$impl/start.sh" ]]; then
    echo "error: $impl/start.sh not found" >&2
    exit 1
fi

exec "$impl/start.sh"
