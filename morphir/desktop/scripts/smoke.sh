#!/usr/bin/env bash
# Smoke: boot Electron hidden and drive the mounted renderer through Chromium DOM events.
# A hang means failure: the timeout kills the run and the script exits non-zero.
set -euo pipefail
unset ELECTRON_RUN_AS_NODE
cd "$(dirname "$0")"
./assemble.sh
cd ../app

ELECTRON=./node_modules/.bin/electron
if [ ! -x "$ELECTRON" ]; then
  echo "SMOKE SETUP REQUIRED: run 'cd morphir/desktop/app && npm install' first" >&2
  exit 1
fi

SCREENSHOT=/tmp/morphir-desktop-smoke.png
RESULT=/tmp/morphir-desktop-smoke.json
LOG=/tmp/morphir-desktop-smoke.log
RENDERER_LOG=/tmp/morphir-desktop-smoke-renderer-console.log
SENTINEL=ghp_MORPHIR_TASK6_SENTINEL_TOKEN_1234567890
USER_DATA=$(mktemp -d /tmp/morphir-desktop-smoke-user-data-XXXXXX)
valid_user_data() {
  case "$USER_DATA" in
    /tmp/morphir-desktop-smoke-user-data-??????)
      [ -d "$USER_DATA" ] && [ ! -L "$USER_DATA" ] && [ -O "$USER_DATA" ]
      ;;
    *)
      return 1
      ;;
  esac
}
cleanup_user_data() {
  if [ ! -e "$USER_DATA" ]; then
    return
  fi
  if valid_user_data; then
    find "$USER_DATA" -depth -delete
  else
    echo "SMOKE CLEANUP REFUSED: isolated userData path failed validation" >&2
  fi
}
trap cleanup_user_data EXIT
if ! valid_user_data; then
  echo "SMOKE FAILED: isolated userData path failed validation" >&2
  exit 1
fi
rm -f "$SCREENSHOT" "$RESULT" "$LOG" "$RENDERER_LOG"

if command -v timeout >/dev/null; then
  TIMEOUT=(timeout 90)
elif command -v gtimeout >/dev/null; then
  TIMEOUT=(gtimeout 90)
else
  TIMEOUT=()
fi

set +e
"${TIMEOUT[@]}" "$ELECTRON" run-capture.cjs "$USER_DATA" "$@" >"$LOG" 2>&1
status=$?
set -e

scan_failed=0
for artifact in "$SCREENSHOT" "$RESULT" "$LOG" "$RENDERER_LOG"; do
  if [ ! -f "$artifact" ]; then
    continue
  fi
  if rg -a -F -q "$SENTINEL" "$artifact"; then
    echo "SMOKE FAILED: sentinel found in capture artifact"
    scan_failed=1
  else
    scan_status=$?
    if [ "$scan_status" -gt 1 ]; then
      echo "SMOKE FAILED: capture artifact scan could not complete"
      scan_failed=1
    fi
  fi
done
if ! valid_user_data; then
  echo "SMOKE FAILED: isolated userData was missing or failed validation after Electron exit"
  scan_failed=1
elif rg -a -F -q "$SENTINEL" "$USER_DATA"; then
  echo "SMOKE FAILED: sentinel found in isolated userData"
  scan_failed=1
else
  scan_status=$?
  if [ "$scan_status" -gt 1 ]; then
    echo "SMOKE FAILED: isolated userData scan could not complete"
    scan_failed=1
  fi
fi
if [ "$scan_failed" -ne 0 ]; then
  exit 1
fi

if [ "$status" -ne 0 ]; then
  echo "SMOKE FAILED: Electron exited with status $status"
  sed "s/$SENTINEL/<redacted>/g" "$LOG" | sed -n '1,240p'
  exit "$status"
fi

test -s "$SCREENSHOT"
test -s "$RESULT"
test -f "$RENDERER_LOG"
node - "$RESULT" <<'NODE'
const fs = require('fs')
const result = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'))
const expected = [
  'clearedAfterFailure',
  'clearedAfterSessionSuccess',
  'clearedAfterSuccess',
  'disconnectedThroughButton',
  'mountedRenderer',
  'rememberFalseReadLive',
  'rememberReadLive',
  'rememberTrueReadLive',
  'removedStoredCredentialThroughButton',
  'rendererConsoleSentinelFree',
  'retainedOnFailure',
  'retainedOnSessionSuccess',
  'retainedOnSuccess',
  'safeConnectedStatus',
  'safeRejectedError',
  'safeSessionStatus',
  'submittedThroughForm',
  'transientDomSentinelFree'
]
const actual = Object.keys(result).sort()
if (JSON.stringify(actual) !== JSON.stringify(expected) || expected.some((key) => result[key] !== true)) {
  process.stderr.write('SMOKE FAILED: capture result contract did not pass\n')
  process.exit(1)
}
NODE

echo "SMOKE OK"
