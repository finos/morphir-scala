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
#
# The `+` is required. Two task selectors written side by side are not two tasks: Mill reads the
# second as an argument to the first, links only the boot bundle, and exits 0. That failure is
# invisible on a machine where the renderer was linked at some earlier point, and shows up on a
# fresh checkout as a missing directory much later in the script.
./mill morphir.desktop.boot.js.fullLinkJS + morphir.desktop.renderer.js.fullLinkJS

APP="morphir/desktop/app"
mkdir -p "$APP/dist"

# Copy the linked bundle out of a Mill task's dest directory, without assuming its filename.
# The name comes from the Scala.js module ID and the link mode, and hardcoding `main.js` here
# failed in CI while holding locally, with nothing in the error to say what was there instead.
# Listing the directory on failure is the point: the next person gets the answer, not a guess.
copy_linked_bundle() {
  local dest="$1" target="$2"
  if [ ! -d "$dest" ]; then
    echo "no link output directory at $dest" >&2
    exit 1
  fi
  local src
  src="$(find "$dest" -maxdepth 1 -type f -name '*.js' ! -name '*.js.map' | head -n 1)"
  if [ -z "$src" ]; then
    echo "no linked .js file in $dest — contents:" >&2
    ls -la "$dest" >&2
    exit 1
  fi
  echo "linked bundle: $src -> $target"
  cp "$src" "$target"
}

copy_linked_bundle out/morphir/desktop/boot/js/fullLinkJS.dest "$APP/dist/main.js"
copy_linked_bundle out/morphir/desktop/renderer/js/fullLinkJS.dest "$APP/dist/renderer.js"

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
