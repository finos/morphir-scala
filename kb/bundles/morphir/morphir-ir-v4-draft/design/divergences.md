---
type: Concept
title: Specification and Design Divergences
description: Where the v4 design documents and the v4 spec draft disagree, and what those disagreements leave open.
tags: [morphir, ir, v4, draft, divergences, open-questions]
status: draft
stale_after: 2026-12-31
sources:
  - id: design-ir
    resource: https://github.com/finos/morphir/tree/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/design/draft/ir
    title: docs/design/draft/ir (design documents)
  - id: spec-draft
    resource: https://github.com/finos/morphir/tree/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft
    title: docs/spec/draft (specification draft)
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Specification and Design Divergences

The v4 [specification draft](/index.md) and the [design documents](/design/index.md) were written at different times
and disagree in concrete, checkable ways. None of these is resolved upstream. **Verify against the v4 schemas before
implementing either side.**

## Encoding disagreements

| Subject | Spec draft | Design |
| ------- | ---------- | ------ |
| `formatVersion` | Integer `4` | Semver string `"4.0.0"` |
| Parameterized type shorthand | `{"Reference": ["morphir/sdk:list#list", "a"]}` | Bare array `["morphir/sdk:list#list", "a"]` |
| Tuple type | `{"Tuple": {"elements": [...]}}` | Canonical is `{"Tuple": [...]}`; the object form is "expanded" |
| Record type fields | Fields directly under `Record` | Some examples nest them under a `fields` key |
| Value shorthand | Values **always** use object wrappers | Bare `42` and `true` are permitted when attributes are empty |
| `AccessControlled` | Flattened `{"access": "Public", ...}` | Canonical is the tag form `{"Public": {...}}`; flattened form also shown |

The `formatVersion` disagreement is the one most likely to break a real implementation, because it changes the JSON
*type* of a field every file carries.

## Document Tree layout disagreements

| Subject | Spec draft | Design |
| ------- | ---------- | ------ |
| Definition file location | `modules.md`: files "reside directly in the module directory"; `distribution.md` shows `types/` and `values/` subdirectories | Consistently `types/` and `values/` subdirectories |
| `module.json` path key | `"module": "main/domain"` | `"path": "my-org/domain"` |
| `module.json` contents | Manifest style has metadata only; inline style embeds full definitions | A manifest listing type and value **names** |
| Node file header | Not shown | `formatVersion` and `name` precede `def` / `spec` |
| Distribution root | `format.json`, `morphir.toml`, `pkg/`, `deps/` | Also `session.jsonl` (transaction journal) and `deco/` (decorations) |

Note that the spec draft is **internally** inconsistent on the first row — its modules and distribution documents
disagree with each other, and the design sides with subdirectories.

## Model disagreements

- **Module documentation.** The spec draft shows a `doc` field on the module object. The design's `ModuleDefinition`
  has only `types` and `values`, with the stated decision "module-level docs in separate metadata" — meaning
  [decorations](/design/decorations.md), which support a `moduleDoc` marker and a `ModuleTarget`.
- **Application dependencies.** The spec draft describes Application as statically linked without saying what its
  `dependencies` hold. The design is explicit: `Dict(PackagePath, PackageDefinition)` — full definitions, unlike
  Library and Specs, which hold specifications.
- **Package version.** The design attaches a full SemVer 2.0.0 `SemanticVersion` to every distribution through
  `PackageInfo`. The spec draft mentions a version string in passing.
- **Annotations on specifications.** The design's `ModuleSpecification` and every specification variant carry an
  `annotations` list. The spec draft has no annotations at all.

## Present in the design, absent from the specification draft

Five features exist only in the design documents:

- [Annotations](/design/annotations.md) — semantic labels on specifications
- [Layered decorations](/design/decorations.md) — the `deco/` tree
- [Document type](/design/document-type.md) — schema-less JSON-like values, adding a `DocumentLiteral`
- [`$meta`](/design/file-metadata.md) — file-level provenance
- [`$ref`](/design/node-references.md) — file-local deduplication

The `Document` type is the consequential one: it adds a variant to `Literal`, so it changes the value model rather
than sitting beside it.

## Terminology

The design says **VFS mode**; the spec draft says **Document Tree mode**. Same concept. The design's schema tree uses
`schemas/v4/vfs/` where the spec draft writes `schemas/v4/tree/`.

## A separate inconsistency inside the spec draft

Not a design divergence, but worth recording: the spec draft's value system names the literal `IntegerLiteral` in its
literal table, then writes `{"Literal": {"IntLiteral": 42}}` in every JSON example. The design consistently uses
`IntegerLiteral`, and explicitly documents it as the rename of v3's `WholeNumberLiteral`. `IntLiteral` appears to be
an error in the spec draft's examples.

## How to treat all of this

The design documents are ahead of the spec draft and carry more detail, but the design's own status table marks
nothing in IR v4 as Approved or Complete — see [v4 Architecture](/design/architecture.md). Neither document is
authoritative over the other. Where an answer matters, the v4 JSON schemas are the tiebreak; where the schemas are
silent, the question is genuinely open.
