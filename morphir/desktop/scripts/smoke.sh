#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../.."
exec ./mill --ticker false morphir.desktop.smokeRun "$@"
