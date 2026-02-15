#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

exec mix run --no-halt
