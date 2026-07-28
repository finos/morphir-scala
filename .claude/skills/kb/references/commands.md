# `kb` Command Reference

All commands take `--kb <path>` (auto-detected by walking up from the skill directory) and `--json`.

`--json` writes structured output on stdout; Mill's progress goes to stderr. Pipe freely:

```bash
.claude/skills/kb/kb check --json | jq '.findings[] | select(.severity=="error")'
```

---

## `list`

Bundles, their OKF version, concept count and title.

| Flag | Meaning |
| ---- | ------- |
| `--bundle <name>` | List that bundle's concepts instead. Accepts `morphir-ir-v3` or `morphir/morphir-ir-v3` |

```bash
.claude/skills/kb/kb list --bundle morphir-ir-v3
```

JSON gives `{root, bundles[{label, name, group, okfVersion, title, description, concepts, subIndexes, hasLog}]}`, or
with `--bundle`, `{bundle, concepts[...]}`.

---

## `show`

One document: frontmatter, outbound links, heading outline.

| Flag | Meaning |
| ---- | ------- |
| `--path <p>` | Required. Bundle-relative (`/naming.md`) or any path suffix (`morphir-ir-v3/naming.md`) |
| `--bundle <name>` | Disambiguates a bundle-relative path when several bundles share it |
| `--body` | Include the document body |

```bash
.claude/skills/kb/kb show --path /naming.md --bundle morphir-ir-v3
```

---

## `search`

| Flag | Meaning |
| ---- | ------- |
| `--query <text>` | Matches titles, descriptions, types, tags and paths, case-insensitively |
| `--body` | Also search prose, reporting matching line numbers |
| `--type <t>` | Filter by frontmatter `type` |
| `--tag <t>` | Filter by tag; repeatable, all must match |
| `--status <s>` | Filter by `status` |
| `--bundle <b>` | Restrict to one bundle |
| `--index` | Search through the SQLite index (FTS5) instead of scanning — bodies included, ranked by relevance |
| `--limit <n>` | Row cap when using `--index` (default 20) |
| `--db <path>` | Index location (default `<repo>/.dev/kb/index.db`) |

Filters combine, and any of them works without `--query` — `--status draft` alone lists every draft concept.

```bash
.claude/skills/kb/kb search --tag v4 --status draft
```

```bash
.claude/skills/kb/kb search --query "format version" --body
```

---

## `check`

Runs every check and exits non-zero when there are errors.

| Flag | Meaning |
| ---- | ------- |
| `--verbose` | Include info-level findings |
| `--strict` | Exit non-zero on warnings too |
| `--allow-dangling` | Dangling links become warnings — OKF's stance that they mark not-yet-written knowledge |
| `--refs <path>` | Reference checkout root (default `<repo>/.refs`) |
| `--no-provenance` | Skip the `.refs/` comparison entirely |
| `--out <path>` | Write the report to a file instead of stdout. Put these under `.dev/` |

```bash
.claude/skills/kb/kb check --verbose
```

```bash
.claude/skills/kb/kb check --json --out .dev/kb/out/check.json
```

→ [checks.md](checks.md) for what each finding means.

---

## `index`

Builds the SQLite index over the knowledge base.

| Flag | Meaning |
| ---- | ------- |
| `--status` | Report when the index was built and which files changed since, instead of rebuilding |
| `--db <path>` | Database location (default `<repo>/.dev/kb/index.db`) |

```bash
.claude/skills/kb/kb index
```

```bash
.claude/skills/kb/kb index --status
```

→ [index-db.md](index-db.md) for the schema and worked queries.

---

## `refresh`

Brings derived state back in line with the markdown. There are two kinds of it, and `kb refresh` on its own does
both: rewrites index bullets that have drifted from their concept's `description`, then rebuilds the SQLite index if
anything changed.

```bash
.claude/skills/kb/kb refresh
```

```bash
.claude/skills/kb/kb refresh --dry-run
```

### Narrowing it

Either a subcommand or a flag. They are the same operation; the subcommands just read better.

| Form | Does |
| ---- | ---- |
| `kb refresh` | Both halves |
| `kb refresh markdown` (alias `md`) | Index bullets only — same as `kb refresh --no-db` |
| `kb refresh db` (alias `index`) | SQLite index only — same as `kb refresh --no-markdown` |

### Flags

| Flag | `refresh` | `refresh markdown` | `refresh db` |
| ---- | :-------: | :----------------: | :----------: |
| `--dry-run` — report, write nothing | ✓ | ✓ | ✓ |
| `--force` — rebuild even when up to date | ✓ | | ✓ |
| `--add-missing` — append entries for unindexed concepts | ✓ | ✓ | |
| `--section <s>` — section for appended entries (default `Orientation`) | ✓ | ✓ | |
| `--db <path>` — database location | ✓ | | ✓ |
| `--no-markdown` / `--no-db` — narrow the scope | ✓ | | |

```bash
.claude/skills/kb/kb refresh markdown --add-missing --section "Design rationale"
```

```bash
.claude/skills/kb/kb refresh db --force
```

Description drift is fixed automatically because the repair is purely mechanical — the bullet is *supposed* to mirror
the description, so there is only one right answer. Only the trailing text is rewritten; the link is preserved
verbatim, so a hand-written link title survives.

Appending a **missing** entry means choosing which section it belongs under, which is a judgement call, so it is
opt-in via `--add-missing`. Without that flag, unindexed concepts are reported and left alone.

When the markdown changes, the knowledge base is reloaded before the database is rebuilt, so the index always
reflects what ended up on disk.

---

## `query`

Read-only SQL over the index.

| Flag | Meaning |
| ---- | ------- |
| `--sql <sql>` | Required. `SELECT`, `WITH`, `PRAGMA` or `EXPLAIN`; anything else is refused |
| `--db <path>` | Database location |

```bash
.claude/skills/kb/kb query --sql "SELECT type, count(*) FROM v_concept GROUP BY type ORDER BY 2 DESC"
```

---

## `new-bundle`

| Flag | Meaning |
| ---- | ------- |
| `--name <slug>` | Required. Slugified if it is not already kebab-case |
| `--title <t>` | Required |
| `--description <d>` | Required. One sentence — it becomes the bundle's `description` |
| `--group <g>` | Grouping directory under `bundles/`, e.g. `morphir` |
| `--okf-version <v>` | Defaults to `0.2` |
| `--date <YYYY-MM-DD>` | Override today's date in the log entry |

```bash
.claude/skills/kb/kb new-bundle --group morphir --name morphir-ir-v5 \
  --title "Morphir IR v5" --description "The v5 IR specification."
```

Creates `index.md` and `log.md`. It does **not** update `kb/README.md` or the group's `README.md` — it prints a
reminder instead, because that wording is a judgement call.

---

## `add-concept`

Creates the concept, inserts an index bullet, and appends a log entry.

| Flag | Meaning |
| ---- | ------- |
| `--bundle <b>` | Required |
| `--path <p>` | Required. Within the bundle: `naming.md`, or `design/naming.md` for a subdirectory |
| `--type <t>` | Required. The one universally required OKF field |
| `--title <t>` | Required |
| `--description <d>` | Required. Also becomes the index bullet text |
| `--tag <t>` | Repeatable |
| `--status <s>` | `draft`, `stable` or `deprecated` |
| `--source <s>` | Repeatable. `URL`, `id=URL`, or `id=URL=Title` |
| `--section <s>` | Index heading to file under. Defaults to `Orientation`; the section is created if absent |
| `--generated-by <a>` | Actor for `generated.by`, e.g. `process:kb-seed` |
| `--date <YYYY-MM-DD>` | Override today's date |

```bash
.claude/skills/kb/kb add-concept --bundle morphir/morphir-ir-v3 --path naming.md \
  --type "Specification Section" --title Naming \
  --description "Name, Path, QName and FQName." \
  --tag morphir --tag ir --status stable --section "Identity and structure" \
  --source "ir-spec=https://github.com/finos/morphir/blob/<sha>/docs/spec/ir/morphir-ir-specification.md=Morphir IR Specification"
```

A concept whose path is in a subdirectory is filed in that subdirectory's `index.md` when one exists, otherwise in
the bundle root index.

The body is a stub with a `TODO` comment. Write it yourself — → [authoring.md](authoring.md).

---

## `intent …`

Intent management. The *process* — states, kinds, obligations, when to reach for what — is documented in the
[`intent` skill](../../intent/SKILL.md); this is the flag reference.

| Command | Flags |
| ------- | ----- |
| `intent init` | `--name` (default `intent`), `--system <purl>`, `--capability-bundle <label>`, `--stale-after-days` |
| `intent new` | `--title`, `--description`, `--kind` (all required), `--breaking`, `--issue`, `--tag` |
| `intent list` | `--state`, `--kind`, `--breaking`, `--open`, `--user-visible` |
| `intent show <id>` | — |
| `intent check` | `--strict`, `--date` |
| `intent refine <id>` | — |
| `intent start <id>` | — |
| `intent move <id>` | `--state <State>` |
| `intent release <id>` | `--capability bundle:/path.md`, `--artifact <purl>` (repeatable) |
| `intent cancel <id>` | `--reason` |
| `intent supersede <id>` | `--by <id>` |

Ids are positional — `kb intent start 0007`, not `--id 0007`. All commands take `--json` and `--date`.

```bash
.claude/skills/kb/kb intent new --title "WASM linking" --description "Link Scala.js output as a WASM module." --kind feature
```

```bash
.claude/skills/kb/kb intent release 0007 --capability morphir/morphir-scala:/wasm-linking.md
```

The transition verbs refuse up front when the target state's obligation is unmet, rather than letting `check` catch
it later. Run `kb refresh` afterwards to regenerate the intent index.
