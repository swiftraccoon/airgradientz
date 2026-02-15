#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

# Build if binary is missing
if [[ ! -f target/release/airgradientz ]]; then
    echo "Binary not found, building..."
    ./build.sh
fi

exec ./target/release/airgradientz
