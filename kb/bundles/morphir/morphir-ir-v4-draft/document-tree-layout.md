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

```
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

| Path | Holds |
| ---- | ----- |
| `format.json` | Layout metadata and spec version |
| `morphir.toml` | Project-level configuration |
| `pkg/{package-path}/` | The local project's IR — a [Library distribution](/distribution.md) |
| `deps/{package-path}/{version}/` | Dependency IR — a Specs distribution |

Directory names come from the canonical kebab-case form of IR paths; see [Packages](/packages.md) and
[Naming](/naming.md).

## File kinds

- **`module.json`** — a module, either as a manifest or with definitions inlined. See [Modules](/modules.md).
- **`*.type.json`** — a single type, with a root key of `def` or `spec`.
- **`*.value.json`** — a single value, with a root key of `def` or `spec`.

## Addressing

Files in the tree are addressable as `morphir://pkg/...`, `morphir://deps/...`, or `morphir://session/...` URIs, and a
`Locator` can name an entity either by URI or by FQName. See [URI and Locator](/uri-and-locator.md).

## Validation

Each file kind has its own root schema — `tree/format.yaml`, `tree/module.yaml`, `tree/type-node.yaml`,
`tree/value-node.yaml` — over shared common definitions. See [Schema Architecture](/schema-architecture.md).

## Note on the example layout

The upstream distribution spec shows definition files under `types/` and `values/` subdirectories, while the modules
spec states that in manifest style definition files "reside directly in the module directory" and shows them
unnested. The two examples disagree; the drafts have not been reconciled. Verify against `tree/module.yaml` before
relying on either arrangement.
