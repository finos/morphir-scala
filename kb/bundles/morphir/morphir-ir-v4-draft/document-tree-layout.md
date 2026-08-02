---
type: Specification Section
title: Document Tree Layout
description: The .morphir-dist directory structure that lets ordinary shell tools read Morphir IR.
tags: [morphir, ir, v4, draft, document-tree, filesystem, tooling]
status: draft
stale_after: 2026-12-31
sources:
  - id: distribution
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/distribution.md
    title: Distribution (IR v4 draft) — Document Tree Layout
  - id: whats-new
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/whats-new.md
    title: What's New in Version 4 — Document Tree Layout
  - id: document-tree-files-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/ir/schemas/v4/document-tree-files.md
    title: Document Tree File Formats (published v4 spec tree)
  - id: schema-document-tree
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/website/static/schemas/morphir-ir-v4-document-tree-files.yaml
    title: morphir-ir-v4-document-tree-files.yaml (the document tree file schemas)
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Document Tree Layout

The **Document Tree** is v4's alternative to a single JSON blob: a directory where each definition lives in its own
file and the layout mirrors the logical IR structure.

The motivation is tooling. One file per definition means a compiler can rewrite only what changed (incremental
builds), and a developer can `grep` and `find` their way around the IR without a specialized viewer.

## Layout

Two layouts are documented upstream, and they differ on both points that matter: what the root metadata file is
called, and whether definition files nest under `types/` and `values/`.

The spec draft's `docs/spec/draft/distribution.md`:

```text
.morphir-dist/
├── format.json            # Layout metadata and spec version
├── morphir.toml           # Project-level configuration
├── pkg/                   # Local project IR
│   └── my-org/
│       └── my-project/
│           ├── module.json       # Module manifest
│           ├── types/
│           │   └── user.type.json
│           └── values/
│               └── login.value.json
├── deps/                  # Dependency IR
│   └── morphir/
│       └── sdk/
│           └── 1.2.0/
│               └── ...
```

The published spec tree's `docs/spec/ir/schemas/v4/document-tree-files.md`, which is what the JSON schema follows:

```text
.morphir-dist/
├── manifest.json          # Distribution metadata
└── pkg/
    └── my-org/
        └── my-project/
            └── domain/            # Module directory
                ├── module.json    # Module manifest
                ├── user.type.json
                └── login.value.json
```

| Path | Holds |
| ---- | ----- |
| `format.json` *or* `manifest.json` | Distribution metadata — `format.json` "layout metadata and spec version" in the spec draft, `manifest.json` with `formatVersion`, `distribution` and `package` in the schema. Not one file under two names |
| `morphir.toml` | Project-level configuration (spec draft only) |
| `pkg/{package-path}/` | The local project's IR — a [Library distribution](/distribution.md) |
| `deps/{package-path}/{version}/` | Dependency IR — a Specs distribution (spec draft only) |

Directory names come from the canonical kebab-case form of IR paths; see [Packages](/packages.md) and
[Naming](/naming.md). Modules nest by nesting directories: a `domain/orders/` subdirectory with its own `module.json`
is the `domain/orders` submodule.

## File kinds

- **`manifest.json`** — distribution identity: format version, distribution kind, package name, and optionally
  version, creation timestamp, layout and entry points. The spec draft calls this file `format.json`.
- **`module.json`** — a module, either as a manifest or with definitions inlined. See [Modules](/modules.md).
- **`*.type.json`** — a single type, with a root key of `def` or `spec`.
- **`*.value.json`** — a single value, with a root key of `def` or `spec`.

Type and value files also carry a header: `formatVersion` and a `name` that must match the filename without its
suffix. The `def`/`spec` exclusion is stated in prose only and is not enforced by the schema.

## Addressing

Files in the tree are addressable as `morphir://pkg/...`, `morphir://deps/...`, or `morphir://session/...` URIs, and a
`Locator` can name an entity either by URI or by FQName. See [URI and Locator](/uri-and-locator.md).

## Validation

There is no per-file-kind root schema. All four kinds are `definitions` inside one file,
`website/static/schemas/morphir-ir-v4-document-tree-files.yaml`, mirrored in the sibling `morphir/morphir-upstream`
bundle: `DistributionManifestFile`, `ModuleManifestFile`, `TypeDefinitionFile`, `ValueDefinitionFile`. A validator
picks one by `$ref`, choosing on the filename; the schema itself has no root and cannot make that choice. Bodies
below `def` and `spec` are `additionalProperties: true` stubs and are not checked. See
[Schema Architecture](/schema-architecture.md).

## Note on the example layout

Three upstream documents put definition files **directly in the module directory** and one nests them under `types/`
and `values/`:

| Document | Says |
| -------- | ---- |
| `docs/spec/draft/modules.md` | Flat — "reside directly in the module directory" |
| `docs/spec/draft/names.md` | Flat — the URI example is `morphir://pkg/my-org/project/main/domain/user.type.json` |
| `docs/spec/ir/schemas/v4/document-tree-files.md` | Flat, in all four of its directory diagrams |
| `docs/spec/draft/distribution.md` | Nested under `types/` and `values/` |

The [design documents](/design/index.md) side with `distribution.md`, and also add `session.jsonl` (a transaction
journal) and `deco/` (layered decorations) at the distribution root, which nothing else mentions. The JSON schema
describes file *contents* and never directory layout, so it does not settle this — but it does name the root
metadata file `manifest.json`, against `distribution.md`'s `format.json`. See
[Specification and Design Divergences](/design/divergences.md) before relying on either arrangement.
