#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

# shellcheck source=activate-toolchain.sh
source ./activate-toolchain.sh

dub build -b release
