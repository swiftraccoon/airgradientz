#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

# shellcheck source=setup-fnm.sh
source ./setup-fnm.sh

npm install
