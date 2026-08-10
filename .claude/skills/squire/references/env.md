# Squire AI Env Info — Sandbox/Network Detection

`squire ai env info` reports whether the current session is actually restricted
(network sockets, effective JVM-temp writes) as structured JSON, so other skills and
build scripts can make a data-driven decision instead of guessing from
`CLAUDE_CODE_*` env vars — those only tell you "running under Claude Code", not
"sandboxed". Whether a session is restricted depends on the `sandbox` config in
Claude settings (managed/user/project/project-local) and varies per session.

---

## Usage

```bash
# Full structured report
${CLAUDE_PLUGIN_ROOT}/squire ai env info

# Single check, exit-code only (0 = ok, 1 = blocked) — for shell scripting
${CLAUDE_PLUGIN_ROOT}/squire ai env info --check jvm-network
${CLAUDE_PLUGIN_ROOT}/squire ai env info --check var-folders

# Bound the live probe (default 8s)
${CLAUDE_PLUGIN_ROOT}/squire ai env info --timeout 15
```

From project shell scripts (Mise tasks, `morphir-local`), the launcher is called by
its stable in-repo path rather than `${CLAUDE_PLUGIN_ROOT}` (that variable is only
populated when Claude itself issues the command; plain shell scripts run
independently of Claude, e.g. from a terminal or CI):

```bash
.claude/skills/squire/squire ai env info --check jvm-network
```

`scripts/lib/mill-flags.sh` wraps this for mill's daemon-vs-`--no-server` decision
— see below.

## Output shape

```json
{
  "generated_at": "2026-07-25T06:02:52-0500",
  "claude_code": { "detected": true, "entrypoint": "cli", "session_id": "...", "child_session": true },
  "ci": false,
  "checks": {
    "jvm_network":    { "ok": true, "detail": "...", "duration_s": 0.3 },
    "var_folders_writable": { "ok": false, "detail": "PermissionError: ..." }
  },
  "sandboxed": false,
  "claude_settings": {
    "sources": { "managed": {...}, "user": {...}, "project": {...}, "project_local": {...} },
    "sandbox_enabled": { "managed": null, "user": null, "project": null, "project_local": null },
    "network_allowed_domains": [],
    "network_denied_domains": []
  },
  "recommendation": { "mill_daemon": "ok" }
}
```

- **`checks.*.ok`** — `true`/`false`/`null` (`null` = check skipped, e.g. `java`
  not on `PATH`). This is the authoritative, live-probed signal.
- **`sandboxed`** — convenience top-level bool, mirrors `checks.jvm_network.ok ==
  false` (the check most consumers — mill wrappers — care about). Consumers with
  a different concern (cellar's JVM-temp writes, say) should read the
  specific check rather than this bool.
- **`claude_settings`** — best-effort static context from `sandbox.enabled` /
  `sandbox.network.*` across the settings files Claude Code merges. Informational
  only: a missing `sandbox` key does not prove unsandboxed, since the default can
  apply without appearing in any file. Trust `checks`, not this section, when the
  two disagree.

## Why a live probe and not just settings

Squire probes `jvm_network` with the same JVM socket mechanism Mill uses, so the
result answers the actual daemon-connectivity question instead of inferring it
from an unrelated runtime. The probe is fresh, bounded by `--timeout`, and
closes its sockets before returning; no daemon is left running and no result is
cached between calls.

## `scripts/lib/mill-flags.sh`

A tiny POSIX-`sh` helper, sourced by `morphir-local` and the `fmt`/`lint` mise
tasks, that turns the `jvm-network` check into the flag mill actually wants:

```sh
. scripts/lib/mill-flags.sh
MILL_FLAGS="$(mill_flags)"
./mill ${MILL_FLAGS} some.target
```

Precedence: `MILL_NO_SERVER=1` / `MILL_USE_SERVER=1` (explicit override) → CI
(`CI` or `GITHUB_ACTIONS` set → always `--no-server`) → live probe → fail safe to
`--no-server` if the probe itself is unavailable or errors.

Tasks that already pass `-i`/`--interactive` (an alias for `--no-daemon` as of
Mill 1.1.0 — see `./mill --help-advanced`) don't need this: `test/jvm`,
`test/js`, `test/runtime-jvm`, and `publish/sonatype` already force no-daemon
mode by that flag and were never at risk from this issue.
