# Morphir Claude Configuration

## Common Instructions

See @AGENTS.md for common guidelines. [AGENTS.md](./AGENTS.md) is the primary source of truth for common guidelines; however, we can place Claude specific instructions here if needed.

## Claude Specific Instructions

Use `./morphir-local` to build and run the CLI locally — it handles sandbox detection automatically.

<!-- BEGIN MORPHIR TRACKING -->
## Task tracking

This project tracks multi-session work in [beads](https://github.com/steveyegge/beads) (`bd`).
**Using it is optional** — contributors can opt out, and nothing in the build, tests or CI reads
`.beads/`.

**Before tracking anything, resolve the mode** — do not assume beads is in play:

```bash
.claude/skills/squire/squire tracking status --quiet
```

- `beads` — use `bd` for work that outlives the session (anything with dependencies, or that
  needs to survive context compaction). Claim before starting; close with a reason.
- `off` — the contributor opted out. Do not run `bd` write commands; use session-scoped
  tracking and report follow-ups in your summary instead.
- `unavailable` — `bd` is not installed. Say so once, continue with session-scoped tracking,
  and don't install anything unprompted.

Session-scoped todo lists remain the right tool for the handful of steps you're about to take.
The test for beads is whether the context is still needed in two weeks.

**Full guidance, conventions and opt-out instructions: [docs/task-tracking.md](docs/task-tracking.md).**
That file governs over the more absolutist upstream bd guidance in
`.agents/skills/beads/SKILL.md` and `bd prime`, and a contributor's own instructions govern over
both. Neither `git push` nor `bd dolt push` is ever implied — both publish, and both need the
contributor's say-so.
<!-- END MORPHIR TRACKING -->
