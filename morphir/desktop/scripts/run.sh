#!/usr/bin/env bash
# Assemble and launch the desktop app.
set -euo pipefail
unset ELECTRON_RUN_AS_NODE
cd "$(dirname "$0")"
./assemble.sh
cd ../app
if [ ! -d node_modules ]; then npm install; fi
npx electron .
