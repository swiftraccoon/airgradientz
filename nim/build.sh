#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

# Ensure nimble binaries are on PATH
export PATH="$HOME/.nimble/bin:$PATH"

nim c -d:release --threads:on --mm:orc --path:src -o:airgradientz src/airgradientz.nim
