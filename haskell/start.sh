#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

export PATH="$HOME/.ghcup/bin:$PATH"

exec cabal run --project-file=cabal.project airgradientz -- "$@"
