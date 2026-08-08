# Squire Spec Sync — Round-tripping the Morphir spec

The IR specification, its design notes and its JSON schemas live upstream in [finos/morphir](https://github.com/finos/morphir). They are also mirrored into the knowledge base under `kb/bundles/morphir/morphir-upstream/`, so they can be read, searched, linked and edited alongside everything else here.

Squire keeps those two copies honest. `spec sync` brings upstream's changes in; `spec export` sends this side's changes back out.

```text
finos/morphir ──── spec sync ────▶ kb/bundles/morphir/morphir-upstream/
      ▲                                          │
      └──────────── spec export ─────────────────┘
```

---

## Division of labour

**`kb` owns the mechanism.** The `kb sync` commands are domain-neutral: they read a bundle's `sync.yaml`, mirror the paths it names, record what was imported in `sync.lock.yaml`, and inject the knowledge base's own frontmatter keys inside a fenced `# kb:begin` … `# kb:end` region. Export removes exactly that region, so the bytes that go back upstream are the bytes that came from it. None of that knows or cares that the upstream is Morphir.

**Squire owns the Morphir specifics.** Which repository (`finos/morphir`), which ref, which subtrees the reference checkout needs, which of upstream's validators have to pass before a change is fit to send, and the order the steps run in. Squire drives `kb`; it never re-implements it.

Keep it that way. A rule that only makes sense for Morphir belongs here. A capability any mirrored bundle would want belongs in `kb`.

---

## Prerequisite: the reference checkout

Both halves need a checkout of upstream at `.refs/finos/morphir`. Morphir is a large repository and the mirror maps four subtrees of it, so clone it sparsely:

```bash
${CLAUDE_PLUGIN_ROOT}/squire reference repo add https://github.com/finos/morphir \
  --sparse docs website tests/bdd wit
```

- `docs/` — the specification prose, the design documents behind it, and the ADRs
- `website/` — `static/schemas/` (the schemas that actually define the IR), `static/ir/examples/`, and the `scripts/yaml-to-json-schemas.js` generator the export gauntlet runs
- `tests/bdd/` — the fixtures and feature files that are the executable ground truth
- `wit/` — the IR expressed as WebAssembly Interface Types

See [repo.md](repo.md) for what `--sparse` does. `spec sync` prints this exact command and stops if the checkout is missing.

**Keep the sparse set at least as wide as the manifest.** These paths are the top-level roots of every glob in the bundle's `sync.yaml`; add a mapping there and this set has to grow with it. A file that is mapped but not checked out is indistinguishable from a file upstream deleted, so it reports as `deleted-upstream` — and `--prune` acts on that. `spec sync` warns when the checkout is narrower than it expects, and refuses outright to run `--prune` in that state. Widen an existing checkout with:

```bash
git -C .refs/finos/morphir sparse-checkout set docs website tests/bdd wit
```

---

## `squire spec sync` — import

```bash
# Ordinary import from main
${CLAUDE_PLUGIN_ROOT}/squire spec sync

# See what would happen, touching nothing
${CLAUDE_PLUGIN_ROOT}/squire spec sync --dry-run

# Import from a tag or branch
${CLAUDE_PLUGIN_ROOT}/squire spec sync --ref v4-draft

# Take upstream's side of files edited on both sides, and drop files upstream deleted
${CLAUDE_PLUGIN_ROOT}/squire spec sync --theirs --prune

# Use the checkout exactly as it stands
${CLAUDE_PLUGIN_ROOT}/squire spec sync --no-fetch
```

Five steps, each reported, any of which aborts the run non-zero:

1. **checkout** — `.refs/finos/morphir` is present. If not, the `repo add` command to run is printed.
2. **fetch** — `git fetch --depth 1 origin <ref>` then a detached checkout of `FETCH_HEAD`. Refuses to move a checkout with uncommitted changes: those are usually a half-finished export, and overwriting them would lose real work. Skipped by `--no-fetch` and by `--dry-run`.
3. **status** — `kb sync status --json`, summarised by state (see below).
4. **pull** — `kb sync pull`, with `--dry-run`, `--theirs` and `--prune` passed through. This writes the mirrored concepts and assets and rewrites `sync.lock.yaml` and the bundle index.
5. **check** — `kb check --no-provenance`, whose findings are printed. Provenance is off deliberately: it verifies concept `sources:` against `.refs/`, which mirrored documents do not carry.

**When to run it:** before starting spec work, so you are editing today's upstream rather than last month's; and again before exporting, so a diverged file is caught here rather than in a pull request.

---

## `squire spec export` — export

```bash
# Project local edits into the reference checkout on a review branch
${CLAUDE_PLUGIN_ROOT}/squire spec export

# See what would be written and which validators would run
${CLAUDE_PLUGIN_ROOT}/squire spec export --dry-run

# Export into some other morphir checkout — a fork you already have a remote on
${CLAUDE_PLUGIN_ROOT}/squire spec export --to ~/code/github/me/morphir

# Also send files that moved upstream since the last import (reconcile them first)
${CLAUDE_PLUGIN_ROOT}/squire spec export --include-diverged
```

Four steps:

1. **push** — `kb sync push --to <checkout>`. Only `local-only` files are exported by default; `diverged` ones need `--include-diverged` and a human's judgement first. A file whose `# kb:begin` fence is damaged is refused, and the step fails.
2. **branch** — `git switch -c morphir-kb/spec-sync` in the checkout (override with `--branch`, suppress with `--no-branch`). Re-running switches to the existing branch, so successive exports accumulate on one.
3. **validators** — run from inside the checkout. Each is skipped with a warning when its tool is not on PATH or when
   what it judges is not in the checkout, which a sparse clone makes normal:
   - `jsonschema fmt --check website/static/schemas/`
   - `jsonschema lint website/static/schemas/*.yaml`
   - `jsonschema metaschema website/static/schemas/*.yaml`
   - `squire schemas compare --from website/static/schemas` — the YAML↔JSON sync check upstream does not have
4. **status** — `git status --short` in the checkout, and stop.

**A validator blames only what the export touched.** Upstream's GitHub Actions runs none of these, so schemas arrive
already failing — at the pinned commit `morphir-ir-v4-document-tree-files.yaml` does not satisfy its own metaschema,
and none of the committed JSON is canonical by `jsonschema fmt`. A failure in a subtree this export did not write is
reported in full and does not fail the run. Blocking a prose change on a schema defect somebody else introduced would
only teach people to route around the gauntlet.

Two notes on the schema tooling. `jsonschema fmt` refuses YAML input in 16.3.0, and every schema upstream keeps is
YAML, so that first validator currently always skips — which also means upstream's own `fmt:schema` task and the
`.husky/pre-push` hook gating on it cannot be doing what they appear to. The last validator reproduces
`node scripts/yaml-to-json-schemas.js` in Scala using Kyo YAML. The upstream generator needs `js-yaml`, a
devDependency no sparse reference checkout will have installed; Squire's in-process comparison is verified
byte-for-byte against upstream's committed output and runs without a Node or Bun runtime.

**It does not commit and it does not push.** The contribution is the human's, made under their FINOS CLA, and only they can judge whether what landed in the checkout is what they meant to send.

### No AI attribution on the commit

When you do commit in the checkout, the message must carry no `Generated with …`, no `Co-authored-by:` for any agent, no tool credit of any kind. Upstream's `.husky/commit-msg` hook strips such trailers, and FINOS EasyCLA requires every author named on a commit to have a signed CLA — which an agent cannot have. The same rule governs pull request descriptions and review comments. See this repository's [AGENTS.md](../../../../AGENTS.md).

---

## What the states mean

`kb sync status` derives a state per mirrored file by comparing three things: the local copy reduced to its upstream form, the hash recorded in `sync.lock.yaml` at the last import, and the file in the reference checkout. Nothing is stored; it is recomputed every run.

| State | Meaning | What to do |
|-------|---------|------------|
| `clean` | All three agree. | Nothing. |
| `local-only` | Edited here since the import; upstream has not moved. | This is what `spec export` sends. |
| `upstream-only` | Upstream moved; no local edits. | `spec sync` takes it, losing nothing. |
| `diverged` | Both sides changed since the import. | Reconcile by hand. `kb sync diff <path>` shows both. `--theirs` takes upstream's; `--include-diverged` sends yours. |
| `missing-local` | In the lockfile, absent from the mirror. | `spec sync` restores it. If upstream dropped it, `--prune`. |
| `deleted-upstream` | Mirrored here, gone upstream, unmodified. | `spec sync --prune` removes it here too, if that is what you want. |
| `deleted-upstream-edited` | Gone upstream, but edited here. | Held back by everything, `--prune` included. Restore it upstream and export, or revert the edit. |
| `untracked` | Matches a mapping in `sync.yaml` but was never imported. | `spec sync` imports it. |
| `unreadable` | The `# kb:begin` … `# kb:end` region is damaged, so the file cannot be reduced to its upstream form. | Repair the fence by hand, or re-import with `--theirs`. An error, not a warning: an export would send the wrong bytes. |

Only `unreadable` and `missing-local` are errors in `kb check`. The rest are drift — a prompt to decide something, not a broken build.

---

## Editing rules

- **Edit in the knowledge base, never in `.refs/`.** The reference checkout is a mirror that `spec sync` overwrites; anything typed there is lost on the next fetch. The one exception is the export branch, which is the checkout's whole purpose — and even there, the source of truth is the bundle.
- **Do not hand-edit `sync.lock.yaml`.** It is generated, and it is the only record of what the import baseline was.
- **Do not hand-edit below the `<!-- kb:sources -->` marker** in the bundle index. `kb sync pull` regenerates it.
- **Frontmatter inside the `# kb:begin` … `# kb:end` fence is ours; everything outside it is upstream's.** Adding a key inside the fence is fine. Moving upstream's keys into it is not — they would vanish on export.
