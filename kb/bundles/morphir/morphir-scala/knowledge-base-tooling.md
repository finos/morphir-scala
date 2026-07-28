---
type: Capability
title: Knowledge Base Tooling
description: "The kb skill manages the OKF knowledge base and the intent recorded in it, from the command line."
tags: [tooling, kb]
status: stable
---

# Knowledge Base Tooling

morphir-scala carries its own knowledge base under `kb/`, and a command-line tool that maintains it. The tool lives in
`.claude/skills/kb/` and is written in Scala, run through Mill's single-file scripting — no build file, no install
step.

```bash
.claude/skills/kb/kb list
```

## What it does

| Area | Commands |
| ---- | -------- |
| Navigation | `list`, `show`, `search` |
| Correctness | `check` — OKF conformance and provenance drift against `.refs/` |
| Indexing | `index`, `query --sql`, `search --index` |
| Authoring | `new-bundle`, `add-concept` |
| Reconciliation | `refresh`, narrowed by `refresh markdown` or `refresh db` |
| Intent | `intent new`, `intent list`, `intent show`, the transition verbs, `intent check` |

Every command accepts `--json`, and Mill's progress output goes to stderr, so JSON on stdout pipes cleanly into `jq`.

## How it is built

Scala scripts with `//|` YAML headers declaring their own dependencies. kyo is the standard library — `kyo.Path` for
paths and file access, `kyo.Command` for subprocesses, kyo-case-app for the CLI. Markdown is parsed with
commonmark-java and frontmatter with SnakeYAML, so links come from a real parser rather than a regex, and nested
frontmatter such as `sources` survives intact.

## What it maintains

- **Bundles** under `kb/bundles/`, following the [Open Knowledge Format](https://github.com/GoogleCloudPlatform/knowledge-catalog/blob/main/okf/SPEC.md).
- **A SQLite index** at `.dev/kb/index.db` — derived state, gitignored, rebuilt from the markdown. It carries
  full-text search, the link graph in both directions, headings, provenance, and a generic frontmatter table that
  lets other tooling query its own facets without changing the schema.
- **Intent** — work this project means to do, is doing, or has done, with a lifecycle whose obligations are enforced
  rather than merely documented.

## What keeps it honest

`kb check` re-reads the markdown and is always current; the index is a snapshot and can be stale. `kb refresh`
reconciles both kinds of derived state — rewriting index bullets that have drifted from their concept's
`description`, and rebuilding the database when files have changed.

The generated intent index is the clearest case: nobody hand-maintains it, so it cannot rot.

## Related

Intent lives in the `intent` bundle. Its lifecycle, and the reasoning behind keeping intent and capability as
separate documents, are recorded in `kb/CONTEXT.md` and in ADRs 0001 through 0003.
