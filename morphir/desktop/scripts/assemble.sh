#!/usr/bin/env bash
# Link both Scala.js bundles and assemble them into app/dist/.
set -euo pipefail
cd "$(dirname "$0")/../../.."

# The `+` is required — two selectors side by side make Mill read the second as an argument to the
# first, so only the boot bundle links and the run still exits 0. See scripts/package.sh.
./mill morphir.desktop.boot.js.fastLinkJS + morphir.desktop.renderer.js.fastLinkJS

APP="morphir/desktop/app"
mkdir -p "$APP/dist"
cp out/morphir/desktop/boot/js/fastLinkJS.dest/main.js "$APP/dist/main.js"
cp out/morphir/desktop/boot/js/fastLinkJS.dest/main.js.map "$APP/dist/main.js.map" 2>/dev/null || true
cp out/morphir/desktop/renderer/js/fastLinkJS.dest/main.js "$APP/dist/renderer.js"
cp out/morphir/desktop/renderer/js/fastLinkJS.dest/main.js.map "$APP/dist/renderer.js.map" 2>/dev/null || true

echo "assembled $APP/dist"
