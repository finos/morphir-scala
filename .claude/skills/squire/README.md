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
.claude/skills/squire/squire branch refresh --dry-run
.claude/skills/squire/squire branch refresh

# Parameterized target
.claude/skills/squire/squire branch refresh --dry-run --target <branch>
.claude/skills/squire/squire branch refresh --target <branch>
```

See [references/branch.md](references/branch.md) for the complete safety proof and failure recovery.

## How It Works

### Skill loading

Claude Code automatically discovers skills in `.claude/skills/`. When a session starts in this project, squire is loaded from `SKILL.md` and listed as an available skill. The `description` field in the frontmatter tells Claude when to offer it proactively.

### Progressive disclosure

The skill is structured in layers to keep each file focused:

```text
.claude/skills/squire/
├── SKILL.md                  # Entry point — command list and when to invoke
├── squire / squire.bat       # Stable POSIX and Windows launchers
├── squire.scala              # Unified command tree and routing
├── Squire*.scala             # Typed implementations by command area
├── SquireTests.scala         # Kyo command, policy, and migration tests
├── test-resources/           # Suite registry and schema fixtures
├── references/
│   ├── branch.md         # Post-squash branch refresh lifecycle and safety proof
│   ├── cellar.md         # JVM dependency API inspection
│   ├── doctor.md         # Full diagnostic procedure and issue catalogue
│   ├── env.md            # ai env info — sandbox/network detection reference
│   ├── mill-morphir.md   # Fast and published-plugin dogfood workflows
│   ├── repo.md           # Reference repository management
│   ├── spec-sync.md      # Morphir IR import/export workflow
│   └── tracking.md       # Optional beads tracking configuration
```

`scripts/lib/mill-flags.sh` (repo root, not under `.claude/`) is a shell consumer
of `squire ai env info` — see [references/env.md](references/env.md).

`SKILL.md` is concise — Claude reads it on every invocation. The matching reference is loaded completely only when its command is used, keeping context usage low. The launcher is normally called as `.claude/skills/squire/squire` from the repository root; references that support plugin installation may use `${CLAUDE_PLUGIN_ROOT}/squire`.

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
| JVM API inspection | `/squire cellar get\|search\|deps` | `references/cellar.md` |

### `/squire doctor`

When invoked, Claude reads `references/doctor.md` then runs `${CLAUDE_PLUGIN_ROOT}/squire doctor`. The typed diagnostic covers these areas:

1. **Mill daemon connectivity** — Reads the daemon port from `out/mill-daemon/socketPort` (if present) or parses `out/mill-daemon/server.log` for `listening on port N`. It probes with the same JVM socket mechanism Mill uses and reports `PORT_OPEN`, `SANDBOX`, `REFUSED`, or `NO_DAEMON`.

2. **Effective JVM temp access** — Reads the running JVM's `java.io.tmpdir`, then writes and removes a bounded probe there. Reports `OK`, `BLOCKED`, or `UNAVAILABLE` without assuming another runtime uses the same path.

3. **Project configuration** — Checks project-level invariants:
   - Mise setup skips workspace postinstall hooks and leaves Morphir Elm provisioning to Mill
   - The `mainClass` entry in `morphir/package.mill.yaml`
   - All Mill Morphir plugin modules are present
   - Published-plugin tests resolve from their task-local repository
   - Machine acquisition cache state is usable or intentionally disabled
   - Metabuild output is newer than its inputs
   - effective JVM temp write access via the same typed Scala probe

4. **Typed report** — Returns one finding per area, preserving actionable blocker codes while keeping all diagnostic logic in the Scala command.

Claude reports each result as ✅ or ⚠️ and applies fixes from `references/doctor.md`.

## Known Issues Covered

| Issue | Detected by |
|-------|-------------|
| Mill daemon TCP blocked by sandbox | `squire doctor` |
| Mill assembly `mainClass` introspection warning | `squire doctor` |
| Effective JVM temp file write blocked | `squire doctor` |
| Mise setup bypassing Mill-owned Morphir Elm tooling | `squire doctor` |
| Missing Mill Morphir plugin modules | `squire doctor` |
| Broken task-local plugin repository wiring | `squire doctor` |
| Invalid machine acquisition cache configuration | `squire doctor` |
| Scalafmt lint failures | `references/doctor.md` (guidance only) |

## Important Caveats

**Sandbox restrictions are configuration-dependent.** The blockers documented here are specific to sandbox configurations in `~/.claude/settings.json` or managed settings. Not every Claude Code instance will experience these. The scripts detect the actual runtime state rather than assuming.

**The network probe is JVM-native.** It uses `java.net.Socket`, the same runtime boundary Mill crosses, so a successful result directly answers whether the daemon connection path is available.

**Sandbox config requires restart.** Changes to `~/.claude/settings.json` sandbox settings only take effect after restarting Claude Code.

## Adding New Issues

1. Document the symptom, cause, and fix in `references/doctor.md` under a new numbered section
2. If the issue is detectable programmatically, add the typed check to the matching `Squire*.scala` command area and cover it in `SquireTests.scala`
3. Route it through the unified command tree when it needs a new user-facing operation
4. Update the Known Issues table in this README
5. Bump the `version` in `SKILL.md` frontmatter

## Local Output & Scratch Work

Per the project's [AGENTS.md](../../../../AGENTS.md) conventions:

- Any diagnostic output files, logs, or scratch artifacts produced while running squire should go under `.dev/out/squire/` (gitignored)
- Planning or design work related to squire improvements belongs in `.dev/.sdlc/squire/`
- The `.dev/` folder is safe for temporary files — nothing there is committed
