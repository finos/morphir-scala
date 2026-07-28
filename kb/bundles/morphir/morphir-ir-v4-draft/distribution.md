---
type: Specification Section
title: Distribution
description: The three v4 distribution kinds — Library, Specs, and Application — across two physical modes.
tags: [morphir, ir, v4, draft, distribution, deployment]
status: draft
stale_after: 2026-12-31
sources:
  - id: distribution
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/distribution.md
    title: Distribution (IR v4 draft)
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Distribution

A **Distribution** is a complete, versioned unit of a Morphir project or dependency. v4 makes two independent choices
here: *which kind* of distribution, and *which physical mode* it is stored in.

## Two physical modes

### Classic mode

A single monolithic JSON blob, e.g. `morphir-ir.json`. Suits compatibility with existing tooling and simple projects.
The entire package definition — modules, types, values — nests inside one object. This is v3's only mode.

### Document Tree mode

A hierarchical file layout (`.morphir-dist/`) where each definition or specification is its own file, mirroring the
logical IR path structure. Suits large projects, shell-tool integration, and incremental updates. See
[Document Tree Layout](/document-tree-layout.md).

## Three distribution kinds

Both modes support all three. v3 had only `Library`.

### Library

Full implementation logic — `TypeDefinition`, `ValueDefinition`. Used for the project being compiled; corresponds to
the `pkg/` directory in Document Tree mode.

- **Required**: `packageName`
- **Optional**: `dependencies` (default empty), `def` (default empty)

```json
{ "Library": { "packageName": "my-org/my-lib", "dependencies": {}, "def": {} } }
```

```json
{ "Library": { "packageName": "my-org/my-lib" } }
```

### Specs

Public interface only — `TypeSpecification`, `ValueSpecification`. Used for dependencies, to speed up compilation;
corresponds to the `deps/` directory in Document Tree mode.

- **Required**: `packageName`
- **Optional**: `dependencies` (default empty), `spec` (default empty)

```json
{ "Specs": { "packageName": "morphir/sdk" } }
```

This is v3's "dependencies are specifications" rule promoted into a first-class distribution kind, so a
specifications-only artifact can be published and consumed on its own.

### Application

A self-contained distribution with all dependencies statically linked, plus **named entry points** invocable by
tooling or a runtime. Used for deployment and execution.

- **Required**: `packageName`, `entryPoints`
- **Optional**: `dependencies` (default empty), `def` (default empty)

```json
{
  "Application": {
    "packageName": "my-org/my-app",
    "entryPoints": { "main": { "target": "my-org/my-app:main#run", "kind": "main" } }
  }
}
```

An entry point names a `target` [FQName](/naming.md) and a `kind`.

## Compact forms

Every kind supports omitting empty optional fields, so the compact form carries only what is present. Consumers must
treat a missing `dependencies`, `def`, or `spec` as empty rather than as an error.
