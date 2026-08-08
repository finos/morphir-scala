# Squire Tracking — Task-Tracking Setup, Config and Opt-Out

`squire tracking` manages this project's task-tracking configuration: resolving whether
[beads](https://github.com/steveyegge/beads) (`bd`) is in use for the current checkout, letting a
contributor opt out, and keeping the agent instruction files pointing at one canonical guidance
document instead of accumulating tool-generated blocks.

The contributor-facing guidance itself lives in
[docs/task-tracking.md](../../../../docs/task-tracking.md). This file covers the commands.

---

## Why this exists

Two problems, both structural rather than cosmetic:

1. **Beads is optional, but agent instructions read as mandatory.** `bd init` writes guidance into
   `AGENTS.md` and `CLAUDE.md` saying to use `bd` for *all* task tracking. Nothing in the build,
   the test suites, or CI reads `.beads/`, so a contributor who doesn't want beads should be able to
   say so and have agents respect it. There was no mechanism for that.
2. **Tool-generated blocks don't stay put.** `bd init` and `bd setup <editor>` each append their own
   marker-delimited block, and re-append on every run — `bd init` alone wrote two near-identical
   `## Beads Issue Tracker` sections to `AGENTS.md`. Hand-editing inside bd's markers is not durable,
   because bd owns that region.

The answer to both: the repo owns a short pointer block under its own markers, `docs/task-tracking.md`
holds the real guidance, and a per-contributor `tracking.mode` setting decides whether beads applies
at all.

## Usage

```bash
# Resolved mode and full state as JSON
${CLAUDE_PLUGIN_ROOT}/squire tracking status

# Just the mode, for scripting
${CLAUDE_PLUGIN_ROOT}/squire tracking status --quiet

# Exit-code check (0 = matches, 1 = doesn't)
${CLAUDE_PLUGIN_ROOT}/squire tracking status --check beads
${CLAUDE_PLUGIN_ROOT}/squire tracking status --check off

# Re-apply the repo-owned pointer to AGENTS.md / CLAUDE.md
${CLAUDE_PLUGIN_ROOT}/squire tracking sync           # apply
${CLAUDE_PLUGIN_ROOT}/squire tracking sync --check   # report drift, exit 1 if any
${CLAUDE_PLUGIN_ROOT}/squire tracking sync --diff    # preview, write nothing
```

From a plain shell (CI, a terminal, a mise task) use the stable in-repo path instead —
`${CLAUDE_PLUGIN_ROOT}` is only populated when Claude itself issues the command:

```bash
.claude/skills/squire/squire tracking status --quiet
```

## `/squire tracking status`

Resolves three inputs into one answer:

| Input | Source |
|---|---|
| `configured_mode` | `tracking.mode` in `.config/squire/settings.local.yaml` (gitignored), default `auto` |
| `bd.installed` | `bd` on `PATH` |
| `beads_dir_present` | `.beads/` exists in the checkout |

Resolution order — the opt-out wins over everything, then availability:

```
configured_mode == off                     -> effective_mode = off
bd not installed                           -> effective_mode = unavailable
.beads/ absent                             -> effective_mode = unavailable
otherwise                                  -> effective_mode = beads
```

`effective_mode` is what agents and scripts should branch on. `configured_mode` is only what the
contributor wrote.

A `warning` field appears when the configuration is self-contradictory — `mode: beads` with `bd`
missing, or an unrecognised mode value (which falls back to `auto` rather than failing).

**YAML's `off` is a boolean.** `mode: off` parses as `False` and `mode: on` as `True`, since those
are YAML 1.1 boolean spellings. Both are what a human would naturally write, so the script accepts
them: `False` maps to `off`, `True` to `beads`. Quoting (`mode: "off"`) also works.

`guidance_drift` reports whether `AGENTS.md`/`CLAUDE.md` carry a bd-managed block instead of the
repo pointer — the signal that `/squire tracking sync` should run.

**When to invoke:** at the start of any session where you're about to track work, before creating a
`bd` issue, and any time you'd otherwise assume beads is in play.

### Worktrees

`.beads/` existing is not the same as `bd` resolving to a workspace, and in a git worktree the two
come apart.

`bd` walks up from the working directory for `.beads/config.yaml` and, finding none, falls back to
the **main clone's** `.beads/` (`worktreeFallbackConfigPath`, beads `internal/config/config.go`).
That default is right — one issue database per repository, not one per worktree. The catch is that
the fallback targets a *tracked* file, so it resolves against whichever branch the main clone
happens to have checked out. If that branch predates the repo adopting beads, `bd` finds no config,
silently defaults the database name to `beads` instead of the configured one, and reports an empty
workspace — while `.beads/` sits in the worktree, fully populated, on this branch.

The symptom is a checkout that looks healthy and fails anyway:

```
$ bd create --title "…"
Error: database not initialized: issue_prefix config is missing
```

`tracking status` reports this as `effective_mode: unavailable` with the offending path and the
remedy, rather than `beads`, because reporting `beads` sends an agent off to run commands that
cannot work. The `workspace` block carries the detail:

```json
"workspace": {
  "is_worktree": true,
  "local_store": false,
  "fallback_config": "/path/to/main-clone/.beads/config.yaml",
  "status": "unresolvable",
  "remedy": "run `bd bootstrap` here to clone the workspace from the remote ref"
}
```

**`bd bootstrap` is the fix**, not `bd init`. Issue data lives on the git remote under
`refs/dolt/data` (see `sync.remote` in `.beads/config.yaml`), so `bootstrap` clones the existing
workspace — correct prefix, existing issues, nothing invented. `bd init --prefix morphir` would
create a *new empty* database sharing the same prefix, free to mint IDs that collide with the real
ones.

`status` values for the `workspace` block:

| Value | Meaning |
| ----- | ------- |
| `local` | This working copy has a beads store of its own; no fallback involved. |
| `shared` | Not a worktree, or the main clone's config is present and usable. |
| `unresolvable` | A worktree with no store of its own, and the fallback config is missing. |
| `no-repo` | Not inside a git repository. |

## `/squire tracking sync`

Removes any `BEADS INTEGRATION` or `BEADS CODEX SETUP` block from `AGENTS.md` and `CLAUDE.md`, then
installs or refreshes the repo-owned block between `<!-- BEGIN MORPHIR TRACKING -->` and
`<!-- END MORPHIR TRACKING -->`.

Idempotent — a second run reports `OK` and writes nothing. Only the pointer region changes; the
script deliberately does not normalise whitespace elsewhere in the file, so the diff stays a single
hunk per file and doesn't churn unrelated content.

**When to invoke:** after anyone runs `bd init` or `bd setup <editor>` in this repo, or when
`tracking status` reports non-empty `guidance_drift`. `--check` is suitable for a pre-commit hook
or CI step if we ever want the drift enforced rather than just detected.

## `/squire tracking doctor`

`tracking doctor` combines the same typed status report and guidance check used by the other tracking commands.
Treat "doctor" as: run status, read
`warning` and `guidance_drift`, and act on them.

Known issues and their fixes:

| Symptom | Cause | Fix |
|---|---|---|
| `effective_mode: unavailable`, `bd.installed: false` | `bd` not on `PATH` | Install beads, or set `tracking.mode: off` to opt out deliberately |
| `warning` about `mode: beads` but unavailable | Config asks for beads, environment can't provide it | Install `bd`, or change the mode |
| `guidance_drift` non-empty | `bd init`/`bd setup` re-added its own block | `squire tracking sync` |
| `bd setup claude --check` says "no beads section found" while a block is present | bd's check looks for a different marker than `bd init` writes | Harmless. Do not "fix" it by running `bd setup claude` — that appends a second block. Run `tracking sync` instead |
| Issue changes invisible to teammates | `bd dolt push` not run | Run it; `git push` does not carry issue data |
| `bd ready`/`bd list` empty on a fresh clone | Issue data lives on the Dolt remote, not in the checkout | `bd dolt pull` |
| beads git hooks never fire | `.beads/hooks/` is committed, but `core.hooksPath` lives in per-clone `.git/config` | `bd hooks install --beads`. Re-run after moving or re-cloning — it writes an absolute path |
| `bd dolt pull` fails on authentication | The clone has no GitHub SSH key and the remote is SSH | The committed remote is HTTPS for this reason. If a local override set SSH, revert it |

## Opting out

Per-contributor, in the gitignored `.config/squire/settings.local.yaml`:

```yaml
tracking:
  mode: off
```

Create the file from the tracked template if it doesn't exist yet:

```bash
cp .config/squire/settings.local.yaml.template .config/squire/settings.local.yaml
```

With `off`, agents must not run `bd` write commands in that checkout and should fall back to
session-scoped task tracking, reporting follow-ups in their summary instead of filing them. The
committed `.beads/` directory stays where it is — opting out is about behaviour, not about removing
files from a shared repo.

This is intentionally not enforceable. It is a declaration of intent that agents and scripts read.

## What this does not manage

- **`bd`'s own configuration.** `bd config set …` and `.beads/config.yaml` remain bd's. Notably
  `metrics.disabled` is a per-user setting in `~/.config/bd/config.yaml`, so the repo cannot disable
  bd's telemetry on a contributor's behalf; `docs/task-tracking.md` documents the opt-out.
- **Issue data.** Squire never reads or writes issues. That's `bd` and the Dolt remote
  (`refs/dolt/data`).
- **`bd`'s habit of committing to your branch.** Several `bd` commands write tracked files under
  `.beads/` and commit them by themselves — `bd dolt remote add/remove` produces `bd: clear
  sync.remote` and `bd: update sync.remote` with no prompt. Squire does not intercept this. Check
  `git log` after running a `bd` config command, and drop the commits if they were a local
  preference rather than a repo change.
- **Installing beads.** Deliberately not automated, and deliberately not in `mise run setup` —
  installing a task tracker for every contributor is exactly what the opt-out exists to avoid.
  `effective_mode: unavailable` is a supported state, not a broken one.
- **Git hooks and per-tool agent hooks.** Both are opt-in (`bd hooks install --beads`,
  `bd setup <editor>`), and both are the contributor's environment rather than the repo's. The repo
  previously committed `.codex/hooks.json`, which ran `bd` unguarded on every prompt and so failed
  for anyone without it installed; that is why per-tool hook configuration is no longer tracked here.
