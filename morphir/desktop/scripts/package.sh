#!/usr/bin/env bash
# Link release bundles and package the Electron app for one platform token.
#
# Usage: scripts/package.sh <platform-token> <version>
# Leaves raw electron-builder output in morphir/desktop/app/release/.
set -euo pipefail

TOKEN="${1:?platform token required (mac-aarch64|mac-amd64|linux-amd64|linux-aarch64|win-amd64)}"
VERSION="${2:?version required}"

cd "$(dirname "$0")/../../.."

# Release builds use fullLinkJS; scripts/assemble.sh uses fastLinkJS for the dev loop.
./mill morphir.desktop.boot.js.fullLinkJS morphir.desktop.renderer.js.fullLinkJS

APP="morphir/desktop/app"
mkdir -p "$APP/dist"
cp out/morphir/desktop/boot/js/fullLinkJS.dest/main.js "$APP/dist/main.js"
cp out/morphir/desktop/renderer/js/fullLinkJS.dest/main.js "$APP/dist/renderer.js"

case "$TOKEN" in
  mac-aarch64)   BUILDER_ARGS=(--mac --arm64) ;;
  mac-amd64)     BUILDER_ARGS=(--mac --x64) ;;
  linux-amd64)   BUILDER_ARGS=(--linux --x64) ;;
  linux-aarch64) BUILDER_ARGS=(--linux --arm64) ;;
  win-amd64)     BUILDER_ARGS=(--win --x64) ;;
  *) echo "unknown platform token: $TOKEN" >&2; exit 1 ;;
esac

cd "$APP"
npm ci
rm -rf release

# Notarize only when the Apple credentials are present. electron-builder picks up CSC_LINK and
# CSC_KEY_PASSWORD on its own; absent them it produces an unsigned build rather than failing.
if [ -n "${APPLE_ID:-}" ] && [ -n "${APPLE_TEAM_ID:-}" ]; then
  BUILDER_ARGS+=(-c.mac.notarize=true)
fi

npx electron-builder "${BUILDER_ARGS[@]}" \
  --publish never \
  -c.extraMetadata.version="$VERSION"

echo "packaged $TOKEN into $APP/release"
