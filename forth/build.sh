#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"

# Create public symlink if missing (remove broken symlinks first)
if [[ -L public ]] && [[ ! -e public ]]; then
    rm -f public
fi
if [[ ! -e public ]]; then
    ln -s ../c/public public
fi

# Verify dependencies
for cmd in gforth gcc curl sqlite3 jq; do
    if ! command -v "${cmd}" >/dev/null 2>&1; then
        echo "error: ${cmd} is required but not found" >&2
        exit 1
    fi
done

# Verify sqlite3 dev headers available
if ! pkg-config --exists sqlite3 2>/dev/null; then
    echo "error: sqlite3 development headers required (sqlite-devel)" >&2
    exit 1
fi

# Verify libcurl dev headers available
if ! pkg-config --exists libcurl 2>/dev/null; then
    echo "error: libcurl development headers required (libcurl-devel)" >&2
    exit 1
fi

# Clean any stale gforth FFI caches for our library
rm -rf ~/.gforth/libcc-named/airgrad*

echo "Build OK (gforth is interpreted, no compilation needed)"
