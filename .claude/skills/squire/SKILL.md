---
name: squire
description: "Development environment diagnostics and unblocking for the morphir-scala project. Use when hitting build tool failures, sandbox restrictions, mill daemon errors, or SSL/network issues. Also manages reference repositories under .refs/ — invoke when asked to add, list, check, or remove a reference repo, clone a repo for reference, or work with a local copy of an upstream project. And round-trips the Morphir IR specification between finos/morphir and the knowledge base — invoke when asked to sync, import or export the spec, schemas or upstream docs, refresh the mirrored upstream bundle, or prepare spec changes to send back upstream. Also owns task-tracking configuration — invoke before tracking work to resolve whether beads (bd) is in use for this checkout, and when asked to opt out of beads, change tracking settings, or repair beads guidance that a bd command re-added to AGENTS.md/CLAUDE.md."
allowed-tools: Bash(cat *), Bash(ls *), Bash(find *), Bash(python3 *), Bash(git *), Bash(gh *), Read, Edit, Write
metadata:
  version: 0.4.0
---

# Squire — morphir-scala Dev Environment Assistant

Squire diagnoses and unblocks development environment issues, manages reference repositories, and owns task-tracking configuration for the morphir-scala project.

## Commands

### `/squire ai env info`

Reports whether the current session is actually sandboxed (JVM/Python network sockets, `/var/folders` writes) as structured JSON — live-probed, not guessed from `CLAUDE_CODE_*` env vars. Other skills and build scripts (mill task wrappers, etc.) can consume this instead of assuming "running under Claude Code" implies restricted.

Read the full reference before running:
→ [references/env.md](references/env.md)

**When to invoke:** Before deciding whether to use a daemon/server process, a JVM network call, or a `/var/folders`-writing tool — or any time you'd otherwise guess sandbox status from environment variables alone.

### `/squire doctor`

Runs a full environment diagnostic and reports actionable fixes for known blockers.

Read the full diagnostic procedure and issue catalogue before running:
→ [references/doctor.md](references/doctor.md)

**When to invoke:** Any time a Bash tool call fails with a build, network, or sandbox error — especially before retrying the failed command.

### `/squire reference repo`

Manages reference repositories under `.refs/`. Clone external repos, symlink existing local repos, or create git worktrees for ref-based snapshots. A manifest at `.refs/manifest.json` tracks all entries.

Read the full reference before running:
→ [references/repo.md](references/repo.md)

**When to invoke:** When asked to add a reference repo, clone an upstream project, list or check existing references, or when context about an external codebase is needed locally.

Sub-commands: `squire reference repo add`, `squire reference repo list`, `squire reference repo status`, `squire reference repo remove`

### `/squire tracking`

Owns task-tracking configuration: resolves whether [beads](https://github.com/steveyegge/beads) (`bd`) applies to this checkout, supports opting out per contributor, and keeps `AGENTS.md`/`CLAUDE.md` pointing at one canonical guidance document rather than accumulating tool-generated blocks.

Beads is **optional** here — nothing in the build, the test suites, or CI reads `.beads/`. Never assume it is in use; resolve the mode first.

Read the full reference before running:
→ [references/tracking.md](references/tracking.md)

**When to invoke:** Before creating or updating a task/issue, at the start of any session where work will be tracked, when asked to opt out of (or back into) beads, and when a `bd init`/`bd setup` run has re-added its own guidance block to the agent instruction files.

Sub-commands: `squire tracking status`, `squire tracking sync`, `squire tracking doctor`

Contributor-facing guidance, conventions and opt-out steps live in [docs/task-tracking.md](../../../docs/task-tracking.md).

### `/squire spec sync` and `/squire spec export`

Round-trips the Morphir IR specification, design documents and JSON schemas between `finos/morphir` and the knowledge base bundle that mirrors them. `spec sync` imports upstream's changes; `spec export` projects local edits back into a reference checkout, runs upstream's own validators, and stops short of committing.

The `kb` skill owns the mechanism (`kb sync status|pull|push|diff`); squire owns the Morphir specifics — the repo, the ref, the sparse subtrees, the validators, and the order things run in.

Read the full reference before running:
→ [references/spec-sync.md](references/spec-sync.md)

**When to invoke:** When asked to sync, import or export the Morphir spec or schemas, to refresh the mirrored upstream bundle, or to prepare spec changes for a pull request against `finos/morphir`. Also run `spec sync` before starting spec work, so the bundle reflects today's upstream.

Sub-commands: `squire spec sync`, `squire spec export`

### `/squire schemas`

Generates the Morphir IR JSON schemas from the YAML the knowledge base mirrors, and checks that the two are in step.

```bash
mise run schemas:build     # YAML → JSON, into .dev/out/squire/schemas/
mise run schemas:check     # metaschema conformance, then validate every mirrored v4 document
bun .claude/skills/squire/scripts/schemas-to-json.ts --from <dir> --check
```

The knowledge base mirrors the YAML only. Upstream generates the `.json` siblings with
`website/scripts/yaml-to-json-schemas.js`, which runs during its Netlify build and nowhere else — so nothing verifies
the committed JSON still matches the YAML, and the generator cannot run in a checkout without `npm install`. This
reproduces that generator exactly, using bun's native YAML parsing, with no dependencies; `--check` is the
verification upstream lacks, and `spec export` runs it.

Two things it deliberately does not do. It does not reformat: byte-identical output is what stops the next Netlify
deploy rewriting whatever we send, and none of upstream's committed JSON is canonical by `jsonschema fmt` anyway. And
it does not fail on documents that were already failing — at the pinned commit every complete v4 document upstream
publishes is rejected by upstream's own v4 schema. That is recorded as a finding in the `morphir-ir-v4-draft` bundle,
not treated as a gate.

**When to invoke:** When a mirrored schema has been edited, before an export, or when asked whether the IR documents
and the IR schemas actually agree.
