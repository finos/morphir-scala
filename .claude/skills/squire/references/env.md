# Squire AI Env Info — Sandbox/Network Detection

`squire ai env info` reports whether the current session is actually restricted
(network sockets, `/var/folders` writes) as structured JSON, so other skills and
build scripts can make a data-driven decision instead of guessing from
`CLAUDE_CODE_*` env vars — those only tell you "running under Claude Code", not
"sandboxed". Whether a session is restricted depends on the `sandbox` config in
Claude settings (managed/user/project/project-local) and varies per session.

---

## Usage

```bash
# Full structured report
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/ai-env-info.py

# Single check, exit-code only (0 = ok, 1 = blocked) — for shell scripting
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/ai-env-info.py --check jvm-network
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/ai-env-info.py --check python-network
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/ai-env-info.py --check var-folders

# Bound the live probes (default 8s)
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/ai-env-info.py --timeout 15
```

From project shell scripts (mise tasks, `morphir-local`), the script is called by
its stable in-repo path rather than `${CLAUDE_PLUGIN_ROOT}` (that variable is only
populated when Claude itself issues the command; plain shell scripts run
independently of Claude, e.g. from a terminal or CI):

```bash
python3 .claude/skills/squire/scripts/ai-env-info.py --check jvm-network
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
    "python_network": { "ok": true, "detail": "...", "duration_s": 0.0 },
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
  a different concern (cellar's `/var/folders` writes, say) should read the
  specific check rather than this bool.
- **`claude_settings`** — best-effort static context from `sandbox.enabled` /
  `sandbox.network.*` across the settings files Claude Code merges. Informational
  only: a missing `sandbox` key does not prove unsandboxed, since the default can
  apply without appearing in any file. Trust `checks`, not this section, when the
  two disagree.

## Why a live probe and not just settings

`check-mill-daemon.py` already documents the core gotcha this project has hit
repeatedly: **Python socket success does not guarantee JVM `java.net.Socket`
success** — they take different OS-level paths, and a sandbox profile can block
one while allowing the other. So `ai-env-info.py` runs both:

- `python_network` — stdlib `socket` loopback bind+accept+connect.
- `jvm_network` — the same probe, but as a self-contained single-file Java
  program (`java Probe.java`, JEP 330 — no `javac`/build step needed), run under
  `timeout` so a sandbox-induced hang (not just a fast `EPERM`) still resolves in
  bounded time instead of hanging the caller forever.

Both are fresh, self-contained, and clean up after themselves — no daemon is
left running, unlike probing via a real `./mill` invocation. Cheap enough
(~0.3–2s normally) to run on every call; no caching.

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
