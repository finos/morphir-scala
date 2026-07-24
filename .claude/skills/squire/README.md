# Squire

A Claude Code project skill for the morphir-scala repository that diagnoses and unblocks common development environment issues.

## Overview

Squire is invoked as a slash command within Claude Code. It provides targeted guidance when Claude hits build, network, or sandbox failures — rather than blindly retrying or guessing at fixes.

```
/squire doctor   — Run a full environment diagnostic
```

## How It Works

### Skill loading

Claude Code automatically discovers skills in `.claude/skills/`. When a session starts in this project, squire is loaded from `SKILL.md` and listed as an available skill. The `description` field in the frontmatter tells Claude when to offer it proactively.

### Progressive disclosure

The skill is structured in layers to keep each file focused:

```
.claude/skills/squire/
├── SKILL.md              # Entry point — command list and when to invoke
├── references/
│   └── doctor.md         # Full diagnostic procedure and issue catalogue
└── scripts/
    ├── check-mill-daemon.py      # Probes mill daemon TCP connectivity
    ├── check-var-folders.py      # Probes /var/folders write access
    └── check-project-config.py  # Checks project config correctness
```

`SKILL.md` is concise — Claude reads it on every invocation. `references/doctor.md` is only loaded when running `/squire doctor`, keeping context usage low. Scripts are called via `${CLAUDE_PLUGIN_ROOT}` which resolves to the skill's root directory at runtime.

### `/squire doctor`

When invoked, Claude reads `references/doctor.md` then runs the three diagnostic scripts in sequence:

1. **`check-mill-daemon.py`** — Determines whether the mill daemon is reachable. Reads the daemon port from `out/mill-daemon/socketPort` (if present) or parses `out/mill-daemon/server.log` for `listening on port N`. Probes with a Python socket. Reports `PORT_OPEN`, `SANDBOX`, `REFUSED`, or `NO_DAEMON`. Includes a caveat that Python socket success does not guarantee JVM `java.net.Socket` success — they use different OS paths and sandbox restrictions may treat them differently.

2. **`check-var-folders.py`** — Attempts a real write probe at `/var/folders/.squire-probe`. This is ground truth: if the write succeeds, cellar can write its temp `.tasty` files there. Reports `OK` or `BLOCKED` with remediation steps.

3. **`check-project-config.py`** — Checks three project-level invariants:
   - The `ELM_TOOLING_INSTALL` guard in `.config/mise/tasks/setup` (prevents elm binary downloads in restricted networks)
   - The `Task { }` wrapper on `mainClass` in `morphir/package.mill` (prevents mill assembly introspection warning)
   - `/var/folders` write access via real probe (same as script 2, for a single-script summary pass)

   Reports `OK` per check or `ISSUE` with the specific fix.

Claude reports each result as ✅ or ⚠️ and applies fixes from `references/doctor.md`.

## Known Issues Covered

| Issue | Detected by |
|-------|-------------|
| Mill daemon TCP blocked by sandbox | `check-mill-daemon.py` |
| Mill assembly `mainClass` introspection warning | `check-project-config.py` |
| `cellar` temp file write blocked | `check-var-folders.py`, `check-project-config.py` |
| `mise run setup` elm-tooling 504 failures (CI) | `check-project-config.py` |
| Scalafmt lint failures | `references/doctor.md` (guidance only) |

## Important Caveats

**Sandbox restrictions are configuration-dependent.** The blockers documented here are specific to sandbox configurations in `~/.claude/settings.json` or managed settings. Not every Claude Code instance will experience these. The scripts detect the actual runtime state rather than assuming.

**Python sockets ≠ JVM sockets.** The mill daemon probe uses Python's `socket` module. A successful Python probe does not guarantee that Java's `java.net.Socket` (used by the mill client) will also succeed — the sandbox may restrict JVM NIO sockets at the OS level while allowing Python sockets. The script clearly flags this.

**Sandbox config requires restart.** Changes to `~/.claude/settings.json` sandbox settings only take effect after restarting Claude Code.

## Adding New Issues

1. Document the symptom, cause, and fix in `references/doctor.md` under a new numbered section
2. If the issue is detectable programmatically, add a script to `scripts/` and call it from the Diagnostic Workflow section of `doctor.md` using `python3 ${CLAUDE_PLUGIN_ROOT}/scripts/<script>.py`
3. Update the Known Issues table in this README
4. Bump the `version` in `SKILL.md` frontmatter
