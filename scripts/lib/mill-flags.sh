#!/usr/bin/env sh
# Shared: decide whether ./mill should run with --no-server.
#
# Historically this assumed "running under Claude Code" == "JVM sockets are
# sandboxed", which isn't true — whether a given session is actually
# restricted depends on that session's sandbox config and varies. This asks
# squire's live probe instead of guessing from CLAUDE_CODE_* env vars.
#
# Usage:
#   . scripts/lib/mill-flags.sh
#   ./mill $(mill_flags) some.target
#
# Overrides (skip the probe entirely):
#   MILL_NO_SERVER=1   always --no-server
#   MILL_USE_SERVER=1  always daemon mode
#
# CI always gets --no-server: daemon reuse doesn't pay off on ephemeral
# runners, and it avoids the (rare) parallel task-resolution issues noted in
# the lint task.

mill_flags() {
  if [ "${MILL_NO_SERVER}" = "1" ]; then
    echo "--no-server"
    return
  fi
  if [ "${MILL_USE_SERVER}" = "1" ]; then
    echo ""
    return
  fi
  if [ -n "${CI}" ] || [ -n "${GITHUB_ACTIONS}" ]; then
    echo "--no-server"
    return
  fi

  probe_script=".claude/skills/squire/scripts/ai-env-info.py"
  if command -v python3 >/dev/null 2>&1 && [ -f "${probe_script}" ] \
      && python3 "${probe_script}" --check jvm-network >/dev/null 2>&1; then
    echo ""
  else
    # No probe available, or it reported (or failed to rule out) a blocked
    # JVM socket — fail safe to --no-server.
    echo "--no-server"
  fi
}
