#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

export PATH="$HOME/.ghcup/bin:$PATH"

echo "[haskell] Building with cabal..."
cabal build --project-file=cabal.project 2>&1
echo "[haskell] Build complete"
