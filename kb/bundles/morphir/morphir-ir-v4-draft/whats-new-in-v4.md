---
type: Specification Section
title: What's New in v4
description: The four headline changes in Morphir IR v4 against format version 3.
tags: [morphir, ir, v4, draft, changes, versioning]
status: draft
stale_after: 2026-12-31
sources:
  - id: whats-new
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/whats-new.md
    title: What's New in Version 4
  - id: draft-index
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/index.md
    title: Draft Specifications (IR v4) — Key Changes in v4
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# What's New in v4

Four architectural changes, plus a handful of consequences.

## 1. Document Tree layout

v4 introduces a **Document Tree** distribution mode (`.morphir-dist/`) alongside the Classic single-blob mode:

- **Granular files** — one file per type or value definition.
- **Incremental builds** — a compiler can touch specific files instead of rewriting a monolith.
- **Shell friendly** — `grep` and `find` work on the IR.

See [Document Tree Layout](/document-tree-layout.md) and [Distribution](/distribution.md).

## 2. Removal of generic parameters

The generic type attribute parameter `a`, present in v1 through v3, is gone.

- Replaced by explicit `TypeAttributes` and `ValueAttributes` structures.
- Attributes now have a standard schema including source location, constraints (types), and inferred types (values).

This is the largest structural break from v3. See [Attributes](/attributes.md).

## 3. Canonical string naming

Names, Paths, and FQNames serialize to **canonical strings** instead of nested arrays.

- **Readability** — `"morphir/(sdk):list#map"` instead of nested arrays.
- **Keys** — names can be used directly as JSON object keys.
- **Legacy support** — array decoding is still accepted for backward compatibility.

See [Naming](/naming.md).

## 4. Canonical module definitions (`module.json`)

First-class support for `module.json` manifests, letting modules be defined flexibly within the Document Tree — either
as a manifest pointing at per-definition files, or with definitions inlined. See [Modules](/modules.md).

## Consequences

- **Incomplete definitions** — new `IncompleteTypeDefinition` and `IncompleteBody` support best-effort compilation.
  See [Incompleteness](/incompleteness.md).
- **Native and external values** — first-class support for platform builtins and FFI. See
  [Native and External Values](/native-and-external-values.md).

## Migration

The upstream draft records the migration guide as *"Coming Soon"*. See [Migration from v3](/migration-from-v3.md) for
what can be said today.
