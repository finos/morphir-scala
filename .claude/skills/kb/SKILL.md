---
name: kb
description: "Manages the Morphir knowledge base under kb/ — OKF bundles and concept documents. Use when adding content to a bundle, creating a new bundle, checking the knowledge base for conformance or provenance drift, or navigating, searching and listing its bundles and concepts."
allowed-tools: Bash(.claude/skills/kb/kb *), Bash(cat *), Bash(ls *), Bash(find *), Bash(git *), Read, Edit, Write
metadata:
  version: 0.1.0
---

# kb — Morphir Knowledge Base Assistant

Manages `kb/`, the Open Knowledge Format knowledge base. Bundles live under `kb/bundles/`, optionally grouped a level
deeper. [kb/AGENTS.md](../../../kb/AGENTS.md) is the source of truth for the conventions; this skill automates the
mechanical parts of following them.

## The `kb` command

Everything runs through the launcher in this directory. It wraps Mill's single-file Scala scripting — no build file,
no install step. The first run resolves dependencies and compiles; later runs are incremental.

```bash
.claude/skills/kb/kb list
```

```bash
.claude/skills/kb/kb check --verbose
```

**Every command accepts `--json`.** Progress output goes to stderr, so `--json` on stdout is clean and pipeable —
prefer it when you need to consume the result rather than read it.

Full flag reference: → [references/commands.md](references/commands.md)

| Command | Does |
| ------- | ---- |
| `list` | Bundles and their concept counts; `--bundle X` lists that bundle's concepts |
| `show --path /x.md` | One document: frontmatter, outbound links, heading outline |
| `search --query X` | Search titles, descriptions, tags and paths; `--body` to include prose |
| `check` | Conformance and provenance findings; non-zero exit on errors |
| `new-bundle` | Scaffolds a bundle with `index.md` and `log.md` |
| `add-concept` | Scaffolds a concept and wires it into the index and log |

## When to use what

**Adding content to an existing bundle.** Run `add-concept` to create the file and wire it up, then write the body
yourself. The scaffold deliberately leaves a `TODO` comment rather than plausible-looking prose.

→ [references/authoring.md](references/authoring.md) before writing the body.

**Creating a bundle.** Run `new-bundle`, then add it to the Bundles table in `kb/README.md` and to the group's
`README.md` if it is in a group. The command reminds you; it does not edit those files, because their wording is a
judgement call.

**Checking the knowledge base.** Run `check`. It reports structural problems (missing `type`, broken links,
unindexed concepts, frontmatter that does not parse) and provenance drift (commit-pinned sources whose reference
checkout has moved on). Nothing here touches the network.

→ [references/checks.md](references/checks.md) for the catalogue and how to fix each finding.

**Finding divergence in the *content*.** `check` finds mechanical inconsistency. Contradictions between what two
concepts assert — the thing that actually matters in a knowledge base — cannot be detected by a script.

→ [references/divergence.md](references/divergence.md) for that procedure.

## Rules that the tooling assumes

- A **bundle root** is a directory whose `index.md` carries `okf_version`. That is how bundles are discovered.
- Only `index.md` and `log.md` are reserved. Every other `.md` file inside a bundle is a concept and needs `type:`.
- A **grouping directory** gets a `README.md` and never an `index.md`. `README.md` inside a bundle is an error.
- Sub-directory `index.md` files carry **no frontmatter**.
- Index bullets mirror the target concept's `description`. Changing one means changing the other.

## Working on the skill itself

The scripts are Scala, run through Mill's single-file mode. Each file declares its own dependencies in a `//|` YAML
header; `kb.scala` is the entry point and names the others in `moduleDeps`.

| File | Holds |
| ---- | ----- |
| `kb.scala` | kyo-case-app command definitions and CLI plumbing |
| `KbModel.scala` | Pure domain model — bundles, docs, frontmatter, path arithmetic |
| `KbStore.scala` | Loading and parsing: frontmatter via SnakeYAML, bodies via commonmark-java |
| `KbCheck.scala` | The check catalogue |
| `KbScaffold.scala` | Bundle and concept creation, index and log editing |
| `KbRender.scala` | Text and JSON rendering |

kyo is the standard library here: `kyo.Path` for paths and file access, `kyo.Command` for subprocesses, kyo-case-app
for the CLI. Build state lands in `out/`, which is gitignored.

Compile without running:

```bash
.claude/skills/kb/mill compile kb.scala
```

One caveat worth knowing if you extend the scripts: inside Mill's script sandbox, `os.Path` values built from an
environment variable and those returned by `os.list` can render identically yet compare unequal. `kyo.Path` is a
value over its segments and does not have that problem — which is why the scripts use it throughout.
