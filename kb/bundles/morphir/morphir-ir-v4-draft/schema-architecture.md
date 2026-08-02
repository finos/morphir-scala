---
type: Specification Section
title: Schema Architecture
description: The two JSON Schema files that actually define v4, and the modular hierarchy upstream documents but never built.
tags: [morphir, ir, v4, draft, json-schema, validation]
status: draft
stale_after: 2026-12-31
sources:
  - id: schema-classic
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/website/static/schemas/morphir-ir-v4.yaml
    title: morphir-ir-v4.yaml (the v4 distribution schema)
  - id: schema-document-tree
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/website/static/schemas/morphir-ir-v4-document-tree-files.yaml
    title: morphir-ir-v4-document-tree-files.yaml (the document tree file schemas)
  - id: schemas-doc
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/schemas.md
    title: Schema Architecture (IR v4 draft) — describes a hierarchy that does not exist
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Schema Architecture

v4 is defined by **two JSON Schema files**, both draft-07 written in YAML, both living flat in
`website/static/schemas/` beside their v1, v2 and v3 siblings:

| File | Size | Shape |
| ---- | ---- | ----- |
| `morphir-ir-v4.yaml` | ~2000 lines | A root schema: `type: object`, `required: [formatVersion, distribution]`, plus 90 `definitions` |
| `morphir-ir-v4-document-tree-files.yaml` | ~400 lines | `definitions` only — no root type, no root `required` |

Both are mirrored in the sibling `morphir/morphir-upstream` bundle under
`sources/website/static/schemas/`. The `.json` files published next to them upstream are *generated* from the YAML by
`website/scripts/yaml-to-json-schemas.js`, so the YAML is the only copy worth reading or editing.

## The hierarchy that does not exist

Upstream's [`docs/spec/draft/schemas.md`](https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/schemas.md)
describes a nine-file tree — `schemas/v4/common/{naming,types,values,access}.yaml`,
`schemas/v4/classic/distribution.yaml`, `schemas/v4/tree/{format,module,type-node,value-node}.yaml` — and
[the design's own README](https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/design/draft/ir/README.md)
describes the same tree with `vfs/` in place of `tree/`.

**No such directory exists in `finos/morphir`, at this commit or on `main`.** Not one of those nine files was ever
written. Both documents are aspirational and neither says so, so a reader meeting either one will otherwise go
looking for schemas that are not there — and will not find `format.json` described anywhere in a schema, because the
document tree schema calls that file `manifest.json` (see [Divergences](/design/divergences.md)).

Read those two documents as a statement of intent about *modularity*, not as a map. The intent is real: two
distribution modes validated from one vocabulary. The realization is two files.

## What the split actually is

The split is by **distribution mode**, which is the one axis the aspirational hierarchy got right.

`morphir-ir-v4.yaml` is the root schema for the Classic mode — the single monolithic blob the spec draft calls
`morphir-ir.json`. Validate a whole distribution against it directly. It carries the entire vocabulary: `Name`, `Path`, `FQName`, `Annotation`, every `Type` and
`Value` node, every specification and definition variant, `Literal`, and the three distributions
(`LibraryDistribution`, `SpecsDistribution`, `ApplicationDistribution`) under a `distribution` key.

`morphir-ir-v4-document-tree-files.yaml` describes the four file kinds of the
[Document Tree](/document-tree-layout.md), one `definitions` entry apiece:

| Definition | Validates |
| ---------- | --------- |
| `DistributionManifestFile` | `.morphir-dist/manifest.json` — `formatVersion`, `distribution`, `package`, and optional `version`, `created`, `layout`, `entryPoints` |
| `ModuleManifestFile` | `module.json`, in both manifest and inline styles |
| `TypeDefinitionFile` | An individual `*.type.json` |
| `ValueDefinitionFile` | An individual `*.value.json` |

Because the file has no root, none of these is reachable by validating a document against the schema as a whole. A
tool must select the right definition by `$ref` — `…#/definitions/ModuleManifestFile` — from the filename. Which
definition applies to which file is a convention the schema states in prose and cannot enforce.

## The shared vocabulary is copied, not referenced

The document tree schema opens its `definitions` with the comment *"Import core types from main schema (these would
be `$ref` in practice)"* and then restates them locally: `Name`, `ModuleName`, `PackageName`, `AccessControlled`,
`TypeDefinition`, `TypeSpecification`, `ValueDefinition`, `ValueSpecification`. There is no cross-file `$ref`
anywhere in either schema.

Four of those local copies are stubs — `type: object`, `additionalProperties: true`, a one-line description naming
the variants they are supposed to admit. So the body of a `*.type.json` or `*.value.json` is **not validated at all**
in document tree mode. Everything below `def` or `spec` passes. The same content inside a Classic blob is checked
against the full `TypeDefinition` / `ValueSpecification` definitions in `morphir-ir-v4.yaml`.

That asymmetry is the practical consequence of never building the `common/` layer. The two schemas share a
vocabulary by having been written twice, so they can and do drift — see the `formatVersion` and `Record` rows in
[Specification and Design Divergences](/design/divergences.md).

## Polymorphism via `def` and `spec` is prose, not schema

Type and value node files distinguish an implementation from an interface by root key:

```json
{ "def": { } }
```

```json
{ "spec": { } }
```

`TypeDefinitionFile` and `ValueDefinitionFile` are each an `allOf` over `VfsNodeHeader` — which requires
`formatVersion` and `name` — and an object declaring `doc`, `def` and `spec` as **optional sibling properties**, with
`required: []`. The description says *"Exactly one of 'def' or 'spec' must be present"*, but no `oneOf`, `anyOf` or
`not` expresses it. A file carrying both keys validates; so does a file carrying neither.

The mutual exclusion is therefore a rule implementations must enforce themselves. Do not expect a validator to catch
a node that got it wrong.
