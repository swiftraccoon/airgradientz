#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"

# Create public symlink if missing
if [[ ! -e public ]]; then
    ln -s ../c/public public
fi

# Verify dependencies
for cmd in ncat sqlite3 jq curl; do
    if ! command -v "${cmd}" >/dev/null 2>&1; then
        echo "error: ${cmd} is required but not found" >&2
        exit 1
    fi
done

# Run shellcheck on all scripts
echo "Running shellcheck..."
shellcheck -o all server.sh handler.sh lib/*.sh build.sh start.sh tests/run_tests.sh
echo "shellcheck: OK"
