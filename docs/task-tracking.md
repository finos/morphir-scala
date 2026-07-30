# Task tracking in morphir-scala

This project tracks multi-session work in [beads](https://github.com/steveyegge/beads) (`bd`), a
graph-based issue tracker that lives in the repo. **Using it is optional for contributors** — see
[Opting out](#opting-out) — but the tracked data is shared, so if you do use it, a few conventions
matter.

Agents: the machine-readable state is `/squire tracking status`. Read
[Guidance for AI agents](#guidance-for-ai-agents) before deciding whether to use `bd` in a session.

## Why it's here

Design and planning artifacts live under the gitignored `.dev/.sdlc/` directory per
[AGENTS.md](../AGENTS.md), which means a plan and its task breakdown are invisible from a fresh
checkout and evaporate when a session ends. Beads keeps the *task graph* durable and queryable
(`bd ready` answers "what can I work on now?", with blockers respected) while design rationale stays
in markdown, where prose belongs.

The rule of thumb for what belongs where:

| Where | What |
|---|---|
| beads | work items, dependencies, status, follow-ups discovered in passing |
| `.dev/.sdlc/<slug>/` | design specs, decision rationale, plans-as-prose |
| `docs/adr/` | decisions durable enough to outlive the work that prompted them |
| module `CONTRIBUTING.md` | follow-ups worth finding from a checkout without `bd` installed |

That last row is deliberate redundancy. [morphir/model/CONTRIBUTING.md](../morphir/model/CONTRIBUTING.md)
duplicates follow-ups that also exist as beads, so someone who has neither `bd` nor the original
session can still find them.

## What is and isn't in git

**Not committed:** the issue database itself. Issues live in an embedded Dolt database under
`.beads/embeddeddolt/` (a few MB), which `.beads/.gitignore` excludes. Issue data syncs over a
git-compatible protocol into `refs/dolt/data` on the git remote — a ref outside `refs/heads/*`, so
issue history never appears in a code branch's diff and never conflicts with a merge.

This is the answer to "shouldn't this be on its own branch?": **it already is off your branches**,
just via a non-branch ref rather than a branch. `bd init` in this version has no option to put the
git-tracked side on a separate branch, and it doesn't need one.

**Committed:** configuration and documentation only.

| Path | What |
|---|---|
| `.beads/config.yaml`, `.beads/metadata.json` | prefix (`morphir`), sync remote, defaults |
| `.beads/README.md`, `.beads/hooks/` | upstream docs and git hooks |
| `.beads/interactions.jsonl` | append-only audit log — see below |
| `.agents/skills/beads/` | agent-facing description of the bd workflow |
| `AGENTS.md`, `CLAUDE.md` | a short pointer to this file |

### `.beads/interactions.jsonl`

Committing it **is** the intended behaviour. `bd audit --help` states the file "is intended to be
versioned in git" for two purposes: auditing ("why did the agent do that?") and dataset generation
for fine-tuning. Each line is one event; entries are append-only and never rewritten.

Two consequences to know about:

- **It dirties the working tree during ordinary use.** Any `bd` status change appends a line. Commit
  it alongside whatever work produced it rather than reverting it — reverting discards the audit
  trail, which is the whole point of the file.
- **Merge conflicts are expected but trivial.** Two branches appending different lines conflict
  textually while being semantically compatible. `.gitattributes` sets `merge=union` for this file so
  git takes both sides automatically. Line order carries no meaning; each entry has its own `id` and
  `created_at`.

If the fine-tuning-dataset purpose is not something you want your activity contributing to, that is
a reason to opt out (below) rather than to hand-edit the file.

### Telemetry

`bd` reports usage metrics to a third-party endpoint by default (`metrics.endpoint` in `bd config
show`, pointing at `gastownhall-eventsapi.com`). This is a per-user setting in
`~/.config/bd/config.yaml`, not a repo setting, so the repo cannot turn it off on your behalf. To
disable it for yourself:

```bash
bd config set metrics.disabled true
```

## Everyday use

```bash
bd ready                    # unblocked work, highest priority first
bd show <id>                # full context for one issue
bd update <id> --claim      # claim and start atomically
bd close <id> --reason "…"  # complete, with a reason that survives
bd dolt push                # publish issue changes to the shared remote
```

Prefix is `morphir`, so IDs look like `morphir-c1x`. A few conventions specific to this repo:

- **Close with `--reason`, don't delete.** The reason is the durable record of how something was
  resolved; deleting an issue destroys it.
- **Push when you push code.** `git push` and `bd dolt push` are separate operations. Issue changes
  are invisible to everyone else until the second one runs.
- **File what you discover.** If you find a real problem outside the scope of what you're doing,
  `bd create … --deps discovered-from:<current-id>` keeps the provenance.
- **Don't hand-edit `.beads/`.** Use the CLI; the Dolt database is the source of truth and the JSONL
  files are exports or logs.

## Opting out

Beads is not a prerequisite for contributing. You never need `bd` installed to build, test, review,
or submit changes to this repository — nothing in the build, the test suites, or CI reads `.beads/`.

To opt out explicitly, so that agents working in your checkout don't try to use it:

```bash
# once, if you haven't already
cp .config/squire/settings.local.yaml.template .config/squire/settings.local.yaml
```

Then set:

```yaml
tracking:
  mode: off
```

`.config/squire/settings.local.yaml` is gitignored, so this is a per-contributor choice that never
lands in a commit or affects anyone else.

Check it took effect:

```bash
python3 .claude/skills/squire/scripts/tracking-status.py
```

`effective_mode` should read `off`. With tracking off, agents must fall back to their own
session-scoped task tracking and must not run `bd` write commands in your checkout.

The three modes:

| `tracking.mode` | Meaning |
|---|---|
| `auto` (default) | Use beads if `bd` is installed and `.beads/` exists; otherwise proceed without it |
| `beads` | Require beads; treat a missing `bd` as a setup problem to fix rather than to work around |
| `off` | Opt out. Don't read or write beads; use session-scoped tracking instead |

Nothing here can be enforced by the repo, and that's deliberate — it's a declaration of intent that
agents and scripts read, not a lock.

## Guidance for AI agents

**Resolve the mode before tracking anything.** Run `/squire tracking status` (or the script directly)
and act on `effective_mode`:

- **`beads`** — use `bd` for work that outlives the session: multi-step tasks, anything with
  dependencies, follow-ups discovered in passing. Claim before starting, add notes as you go (they
  are what survives context compaction), close with a reason.
- **`off`** — do not run `bd` write commands. Use your own session-scoped task list. Report
  follow-ups in your summary instead of filing them.
- **`unavailable`** — `bd` is not installed but tracking isn't off either. Say so once and continue
  with session-scoped tracking; don't install anything unprompted.

**Ephemeral vs durable.** A session-scoped todo list is the right tool for "the four steps I'm about
to take". Beads is the right tool for "this needs doing and I won't be here when it happens". The
test is whether the context is still needed in two weeks.

**The upstream bd guidance is more absolutist than this repo's.** `.agents/skills/beads/SKILL.md` and
`bd prime` say to use `bd` for *all* task tracking and to prefer `bd remember` over memory files.
Treat those as bd's house style, not this project's rules: this file governs, and a contributor's
`tracking.mode` governs over both. Explicit instructions from the person you're working with outrank
all of it.

**Don't commit or push on the strength of this file.** Neither `git push` nor `bd dolt push` is
implied by any instruction here. Both publish, and both need the contributor's say-so.

## Setup and maintenance

Managed through the project's squire skill:

```
/squire tracking status     # resolved mode, bd version, drift
/squire tracking doctor     # diagnose and report fixes
/squire tracking sync       # re-apply the repo-owned pointer to AGENTS.md / CLAUDE.md
```

See [.claude/skills/squire/references/tracking.md](../.claude/skills/squire/references/tracking.md)
for the full reference, including why `sync` exists (`bd init` and `bd setup <editor>` re-add their
own long guidance blocks to `AGENTS.md`/`CLAUDE.md`; `sync` replaces them with the pointer that
refers back here).
