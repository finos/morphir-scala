#!/usr/bin/env bash
# Smoke: boot Electron hidden, renderer completes three RPC calls, fires smokeDone, main quits 0.
# A hang means failure: the timeout kills the run and the script exits non-zero.
set -euo pipefail
unset ELECTRON_RUN_AS_NODE
cd "$(dirname "$0")"
./assemble.sh
cd ../app
if [ ! -d node_modules ]; then npm install; fi

if command -v timeout >/dev/null; then
  TIMEOUT="timeout 90"
elif command -v gtimeout >/dev/null; then
  TIMEOUT="gtimeout 90"
else
  TIMEOUT=""
fi

MORPHIR_SMOKE=1 $TIMEOUT npx electron . && echo "SMOKE OK"
