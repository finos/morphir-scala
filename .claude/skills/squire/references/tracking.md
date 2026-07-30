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
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/tracking-status.py

# Just the mode, for scripting
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/tracking-status.py --quiet

# Exit-code check (0 = matches, 1 = doesn't)
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/tracking-status.py --check beads
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/tracking-status.py --check off

# Re-apply the repo-owned pointer to AGENTS.md / CLAUDE.md
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/tracking-guidance.py           # apply
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/tracking-guidance.py --check   # report drift, exit 1 if any
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/tracking-guidance.py --diff    # preview, write nothing
```

From a plain shell (CI, a terminal, a mise task) use the stable in-repo path instead —
`${CLAUDE_PLUGIN_ROOT}` is only populated when Claude itself issues the command:

```bash
python3 .claude/skills/squire/scripts/tracking-status.py --quiet
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

## `/squire tracking sync`

Removes any `BEADS INTEGRATION` or `BEADS CODEX SETUP` block from `AGENTS.md` and `CLAUDE.md`, then
installs or refreshes the repo-owned block between `<!-- BEGIN MORPHIR TRACKING -->` and
`<!-- END MORPHIR TRACKING -->`.

Idempotent — a second run reports `OK` and writes nothing. Only the pointer region changes; the
script deliberately does not normalise whitespace elsewhere in the file, so the diff stays a single
hunk per file and doesn't churn unrelated content.

**When to invoke:** after anyone runs `bd init` or `bd setup <editor>` in this repo, or when
`tracking-status.py` reports non-empty `guidance_drift`. `--check` is suitable for a pre-commit hook
or CI step if we ever want the drift enforced rather than just detected.

## `/squire tracking doctor`

There is no separate doctor script — `tracking-status.py` already reports everything diagnosable, and
`tracking-guidance.py --check` covers the one repairable drift. Treat "doctor" as: run status, read
`warning` and `guidance_drift`, and act on them.

Known issues and their fixes:

| Symptom | Cause | Fix |
|---|---|---|
| `effective_mode: unavailable`, `bd.installed: false` | `bd` not on `PATH` | Install beads, or set `tracking.mode: off` to opt out deliberately |
| `warning` about `mode: beads` but unavailable | Config asks for beads, environment can't provide it | Install `bd`, or change the mode |
| `guidance_drift` non-empty | `bd init`/`bd setup` re-added its own block | `python3 …/tracking-guidance.py` |
| `bd setup claude --check` says "no beads section found" while a block is present | bd's check looks for a different marker than `bd init` writes | Harmless. Do not "fix" it by running `bd setup claude` — that appends a second block. Run `tracking sync` instead |
| Issue changes invisible to teammates | `bd dolt push` not run | Run it; `git push` does not carry issue data |

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
- **Installing beads.** Deliberately not automated — installing a tool is the contributor's call.
