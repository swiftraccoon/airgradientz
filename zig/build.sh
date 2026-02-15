#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

# Zig impl links against ../c/sqlite3.c — ensure it exists
if [[ ! -f ../c/sqlite3.c ]]; then
    echo "SQLite source not found, running C build to download it..."
    ../c/build.sh
fi

zig build -Doptimize=ReleaseFast
