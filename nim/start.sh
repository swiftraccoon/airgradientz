#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

# Build if binary is missing
if [[ ! -f airgradientz ]]; then
    echo "Binary not found, building..."
    ./build.sh
fi

exec ./airgradientz
