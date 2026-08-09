# Squire

A Claude Code project skill for morphir-scala development environment diagnostics, reference repositories, safe post-squash branch refreshes, task tracking, and Morphir specification/schema workflows.

## Overview

Squire is invoked as a slash command within Claude Code. It routes each maintained command to focused guidance and project-local automation.

```text
/squire ai env info          — Report sandbox/network status as structured JSON
/squire doctor               — Run a full environment diagnostic
/squire mill morphir         — Develop and diagnose the Mill Morphir plugins
/squire reference repo ...   — Manage repositories under .refs/
/squire branch refresh       — Refresh develop from main after a squash merge
/squire tracking ...         — Resolve and maintain optional beads tracking
/squire spec sync|export     — Round-trip the Morphir IR specification
/squire schemas              — Build and check generated IR JSON schemas
```

The branch refresh workflow is deliberately two-step:

```bash
python3 .claude/skills/squire/scripts/branch-refresh.py --dry-run
python3 .claude/skills/squire/scripts/branch-refresh.py

# Parameterized target
python3 .claude/skills/squire/scripts/branch-refresh.py --dry-run --target <branch>
python3 .claude/skills/squire/scripts/branch-refresh.py --target <branch>
```

See [references/branch.md](references/branch.md) for the complete safety proof and failure recovery.

## How It Works

### Skill loading

Claude Code automatically discovers skills in `.claude/skills/`. When a session starts in this project, squire is loaded from `SKILL.md` and listed as an available skill. The `description` field in the frontmatter tells Claude when to offer it proactively.

### Progressive disclosure

The skill is structured in layers to keep each file focused:

```text
.claude/skills/squire/
├── SKILL.md              # Entry point — command list and when to invoke
├── references/
│   ├── branch.md         # Post-squash branch refresh lifecycle and safety proof
│   ├── cellar.md         # JVM dependency API inspection
│   ├── doctor.md         # Full diagnostic procedure and issue catalogue
│   ├── env.md            # ai env info — sandbox/network detection reference
│   ├── mill-morphir.md   # Fast and published-plugin dogfood workflows
│   ├── repo.md           # Reference repository management
│   ├── spec-sync.md      # Morphir IR import/export workflow
│   └── tracking.md       # Optional beads tracking configuration
├── scripts/
│   ├── ai-env-info.py            # Structured sandbox/network detection (JSON)
│   ├── branch-refresh.py         # Proves and refreshes a post-squash target
│   ├── cellar-query.py           # Runs project-configured JVM API queries
│   ├── check-mill-daemon.py      # Probes mill daemon TCP connectivity
│   ├── check-var-folders.py      # Probes effective JVM temp write access
│   ├── check-project-config.py   # Checks project config correctness
│   ├── temp_directory.py         # Resolves and probes the effective JVM temp path
│   ├── repo-*.py                 # Manages entries under .refs/
│   ├── schemas-to-json.ts        # Builds/checks mirrored JSON schemas
│   ├── spec-*.py                 # Imports/exports the Morphir IR spec
│   └── tracking-*.py             # Resolves and repairs tracking guidance
└── tests/
    ├── test_branch_refresh.py    # Branch refresh safety and CLI tests
    ├── test_ci_policy.py        # Hosted CI and publishing policy tests
    └── test_mise_task_policy.py # Local CI task metadata tests
```

`scripts/lib/mill-flags.sh` (repo root, not under `.claude/`) is a shell consumer
of `ai-env-info.py` — see [references/env.md](references/env.md).

`SKILL.md` is concise — Claude reads it on every invocation. The matching reference is loaded completely only when its command is used, keeping context usage low. Scripts are normally called from the repository root; references that support plugin installation may use `${CLAUDE_PLUGIN_ROOT}` for the skill root.

### Maintained command areas

| Area | Command or entry point | Full reference |
|------|-------------------------|----------------|
| Environment | `/squire ai env info`, `/squire doctor` | `references/env.md`, `references/doctor.md` |
| Mill Morphir plugins | `/squire mill morphir` | `references/mill-morphir.md` |
| Reference repos | `/squire reference repo add\|list\|status\|remove` | `references/repo.md` |
| Branch lifecycle | `/squire branch refresh` | `references/branch.md` |
| Task tracking | `/squire tracking status\|sync\|doctor` | `references/tracking.md` |
| Morphir spec | `/squire spec sync`, `/squire spec export` | `references/spec-sync.md` |
| Schemas | `/squire schemas` | `SKILL.md` |
| JVM API inspection | `cellar-query.py` | `references/cellar.md` |

### `/squire doctor`

When invoked, Claude reads `references/doctor.md` then runs the three diagnostic scripts in sequence:

1. **`check-mill-daemon.py`** — Determines whether the mill daemon is reachable. Reads the daemon port from `out/mill-daemon/socketPort` (if present) or parses `out/mill-daemon/server.log` for `listening on port N`. Probes with a Python socket. Reports `PORT_OPEN`, `SANDBOX`, `REFUSED`, or `NO_DAEMON`. Includes a caveat that Python socket success does not guarantee JVM `java.net.Socket` success — they use different OS paths and sandbox restrictions may treat them differently.

2. **`check-var-folders.py`** — Queries Java for `java.io.tmpdir`, then writes a bounded probe there. Reports `OK`, `BLOCKED`, or `UNAVAILABLE` without assuming Python and Java use the same path.

3. **`check-project-config.py`** — Checks project-level invariants:
   - Mise setup skips workspace postinstall hooks and leaves Morphir Elm provisioning to Mill
   - The `mainClass` entry in `morphir/package.mill.yaml`
   - All Mill Morphir plugin modules are present
   - Published-plugin tests resolve from their task-local repository
   - Machine acquisition cache state is usable or intentionally disabled
   - Metabuild output is newer than its inputs
   - effective JVM temp write access via real probe (same as script 2, for a single-script summary pass)

   Reports `OK` per check or `ISSUE` with the specific fix.

Claude reports each result as ✅ or ⚠️ and applies fixes from `references/doctor.md`.

## Known Issues Covered

| Issue | Detected by |
|-------|-------------|
| Mill daemon TCP blocked by sandbox | `check-mill-daemon.py` |
| Mill assembly `mainClass` introspection warning | `check-project-config.py` |
| Effective JVM temp file write blocked | `check-var-folders.py`, `check-project-config.py` |
| Mise setup bypassing Mill-owned Morphir Elm tooling | `check-project-config.py` |
| Missing Mill Morphir plugin modules | `check-project-config.py` |
| Broken task-local plugin repository wiring | `check-project-config.py` |
| Corrupt or disabled machine acquisition cache | `check-project-config.py` |
| Stale Mill metabuild compilation | `check-project-config.py` |
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

## Local Output & Scratch Work

Per the project's [AGENTS.md](../../../../AGENTS.md) conventions:

- Any diagnostic output files, logs, or scratch artifacts produced while running squire should go under `.dev/out/squire/` (gitignored)
- Planning or design work related to squire improvements belongs in `.dev/.sdlc/squire/`
- The `.dev/` folder is safe for temporary files — nothing there is committed
