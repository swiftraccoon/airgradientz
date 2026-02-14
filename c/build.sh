#!/bin/sh
set -e

cd "$(dirname "$0")"

SQLITE_URL="https://sqlite.org/2026/sqlite-amalgamation-3510200.zip"
SQLITE_DIR="sqlite-amalgamation-3510200"

if [ ! -f sqlite3.c ] || [ ! -f sqlite3.h ]; then
    echo "Downloading SQLite amalgamation..."
    curl -sL "$SQLITE_URL" -o sqlite.zip
    unzip -o sqlite.zip
    mv "$SQLITE_DIR/sqlite3.c" "$SQLITE_DIR/sqlite3.h" .
    rm -rf "$SQLITE_DIR" sqlite.zip
fi

make "$@"
