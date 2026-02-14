#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

# Locate fnm — check PATH first, then common install locations
if ! command -v fnm &>/dev/null; then
  for dir in "$HOME/.local/share/fnm" "$HOME/.fnm"; do
    if [[ -x "$dir/fnm" ]]; then
      export PATH="$dir:$PATH"
      break
    fi
  done
fi

if ! command -v fnm &>/dev/null; then
  echo "error: fnm not found. Install it: https://github.com/Schniz/fnm#installation" >&2
  exit 1
fi

# Activate fnm and use the version pinned in .node-version
eval "$(fnm env)"
fnm use --install-if-missing

exec npm start
