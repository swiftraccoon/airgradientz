#!/bin/bash
set -euo pipefail

# Install D toolchain (LDC) via official installer
# Requires: curl, xz
# Installs to ~/dlang/

if command -v ldc2 &>/dev/null && command -v dub &>/dev/null; then
    echo "D toolchain already installed"
    ldc2 --version | head -1
    exit 0
fi

echo "Installing D toolchain (LDC)..."
curl -fsS https://dlang.org/install.sh | bash -s ldc

echo ""
echo "Activate with:  source ~/dlang/ldc-*/activate"
echo "Then build:     cd d && dub build"
