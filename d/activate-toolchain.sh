#!/usr/bin/env bash
# Shared D toolchain activation — sourced by build.sh and start.sh

# shellcheck disable=SC1090
if ! source ~/dlang/ldc-*/activate 2>/dev/null; then
    echo "error: D toolchain not found. Run ./setup.sh first." >&2
    exit 1
fi
