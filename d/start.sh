#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

# shellcheck source=activate-toolchain.sh
source ./activate-toolchain.sh

# Build if binary is missing
if [[ ! -f airgradientz ]]; then
    echo "Binary not found, building..."
    dub build -b release
fi

exec ./airgradientz
