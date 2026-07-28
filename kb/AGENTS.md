# AI Agent Guidelines for the Morphir Knowledge Base (`kb/`)

See the root [AGENTS.md](../AGENTS.md) for project-wide guidelines. This file covers everything specific to `kb/` and
is the primary source of truth for authoring and consuming knowledge bundles here.

## What lives here

`kb/` is the knowledge base root. Bundles land under `kb/bundles/`, one directory per bundle. Nothing under `kb/` is
compiled, referenced by the Mill build, or shipped as part of a published artifact — it is documentation-as-data,
read by humans and agents.

Bundles conform to the **Open Knowledge Format (OKF)**, currently **v0.2**:

- Spec: <https://github.com/GoogleCloudPlatform/knowledge-catalog/blob/main/okf/SPEC.md>
- OKF overview: <https://github.com/GoogleCloudPlatform/knowledge-catalog/tree/main/okf>

When this file and the spec disagree, the spec wins — and the disagreement is a bug in this file worth fixing.

## Bundle structure

```
kb/bundles/<bundle-slug>/
  index.md                 # Bundle root index; the only file that may carry `okf_version`
  log.md                   # Optional update history
  <concept>.md             # Concept document at bundle root
  <subdirectory>/
    index.md               # Directory index (no frontmatter)
    <concept>.md
  references/              # Optional: mirrored external material, run instructions, code
```

- `<bundle-slug>` is lower-case kebab-case, matching the slug convention used elsewhere in this repo.
- Every `.md` file that is not `index.md` or `log.md` is a **concept document**.
- `index.md` and `log.md` are reserved filenames. Do not use them for concepts.
- Subdirectories nest freely; depth is a modelling choice, not a spec constraint.
- `references/` is a naming convention, not a requirement. Use it when mirroring external material so that `sources`,
  executors, and attesters have a stable place to point at.

## Concept documents

Every concept document starts with a YAML frontmatter block.

### Required

- `type` — short string naming the kind of concept (`Playbook`, `Metric`, `Module`, `Attested Computation`, …).
  This is the only universally required field; consumers route, filter, and present on it. Type values are not
  centrally registered, so pick self-explanatory names and reuse the ones already present in the bundle rather than
  inventing near-synonyms.

### Recommended

- `title` — human-readable display name. Consumers may derive one from the filename if omitted; supply it anyway.
- `description` — one sentence. Index generators and search snippets pull from this, so write it to stand alone.
- `resource` — URI uniquely identifying the underlying asset. Omit for abstract concepts.
- `tags` — YAML list of short strings for cross-cutting categorization.

### Optional families

- **Provenance** — `sources`, a list of the materials the concept derives from. Each entry requires `resource` (a
  concrete artifact URL or scope descriptor) and may carry `id` (stable key for per-claim attribution), `title`,
  `author`, `usage_count`, `last_modified`.
- **Trust** — `generated` (`by` actor, `at` ISO 8601 datetime) and `verified` (a single mapping or a list of
  verification events, each with `by` and `at`).
- **Lifecycle** — `status` (`draft`, `stable` (default), or `deprecated`) and `stale_after` (`YYYY-MM-DD`).
- **Computation** — for `type: Attested Computation` only: `runtime` (required), `parameters` (typed named holes of
  `{ name, type, required }`), `computation`, `executor` (`resource`, `receipt`), `attester`.

Example:

```markdown
---
type: Playbook
title: Publishing a Morphir IR from Elm sources
description: End-to-end steps for turning an Elm model into a published Morphir IR artifact.
tags: [elm, ir, publishing]
status: stable
generated:
  by: human:damianreeves
  at: 2026-07-28T00:00:00Z
---

Prose body starts here.
```

### Actors

Actor strings follow a fixed convention: `<producer>/<version>` for agents, `human:<id>` for people, and
`process:<id>` for automated processes. Use it in `generated.by` and `verified.by`.

## Cross-linking

- Link concepts with plain markdown links. Prefer **bundle-relative** paths beginning with `/` — they survive file
  moves better than relative paths, which are also permitted. Absolute URLs are fine for external material.
- The meaning of a link — dependency, inheritance, join, "see also" — comes from the surrounding prose, not from the
  link itself. Say what the relationship is.
- Broken links are legitimate: they mark not-yet-written knowledge. Do not delete a link merely because its target
  does not exist yet, and do not treat one as a failure when consuming a bundle.

## Index files

`index.md` supports progressive disclosure — it lets a reader or agent see what a directory holds before opening
anything in it.

- Frontmatter appears **only** in the bundle-root `index.md`, and carries `okf_version: "0.2"`. Every other
  `index.md` has no frontmatter at all.
- The body is one or more headed sections of bulleted entries:

```markdown
## Playbooks

* [Publishing a Morphir IR from Elm sources](/publishing-ir.md) - End-to-end steps for turning an Elm model into a published Morphir IR artifact.
```

- Entry descriptions should match the target concept's `description` frontmatter. When you change a `description`,
  update the index entries that mirror it.

## Log files

`log.md` is optional and may appear at any level of the hierarchy.

- Date headings use ISO 8601 `YYYY-MM-DD`, newest first.
- Entries are prose bullets, conventionally prefixed `**Update**`, `**Creation**`, or `**Deprecation**` — suggested
  conventions, not requirements.

```markdown
## 2026-07-28
* **Creation**: Added the [IR publishing playbook](/publishing-ir.md).
```

## Adding a new bundle

1. Create `kb/bundles/<bundle-slug>/` with a kebab-case slug.
2. Write `index.md` with `okf_version: "0.2"` frontmatter and a body listing the bundle's concepts.
3. Add concept documents. Give every one a `type`; give nearly every one a `title` and `description`.
4. Add `log.md` if the bundle's history is worth tracking.
5. Add the bundle to the **Bundles** table in [README.md](./README.md), using the same description as its `index.md`.
6. Re-read the [spec](https://github.com/GoogleCloudPlatform/knowledge-catalog/blob/main/okf/SPEC.md) if you are doing
   anything the sections above do not cover — this file is a working summary, not a replacement.

## Consuming a bundle

Agents should discover concepts by reading `index.md` first, route on `type`, follow cross-links to build up domain
understanding, and check `status` and `stale_after` before treating content as current. Content marked `draft` or past
its `stale_after` date is a lead, not a fact. For `Attested Computation` concepts, bind parameters to the sanctioned
computation and submit it to the declared executor rather than reimplementing the logic — the separation between
agent-authored values and machine-sanctioned logic is the point.

## House rules

- Bundle content is knowledge, not build input. Do not wire `kb/` into `build.mill` or any `package.mill.yaml`.
- Do not put secrets, credentials, or customer data in a bundle. These files are public.
- Scratch work, spikes, and planning artifacts belong in `.dev/` (gitignored), not in `kb/`. A bundle holds knowledge
  that has settled.
- The CLA and no-tool-attribution rules in the root [AGENTS.md](../AGENTS.md) apply to changes here exactly as they do
  to code.
