---
type: Specification Section
title: Schema Architecture
description: How v4 splits its JSON schemas into separate root schemas over shared $ref definitions.
tags: [morphir, ir, v4, draft, json-schema, validation]
status: draft
stale_after: 2026-12-31
sources:
  - id: schemas
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/schemas.md
    title: Schema Architecture (IR v4 draft)
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Schema Architecture

v4 replaces v3's single monolithic schema with **separate root schemas sharing common `$ref` definitions** — the
modular arrangement needed to validate two distribution modes with one set of building blocks.

## Hierarchy

```text
schemas/v4/
├── common/                 # Shared $ref definitions
│   ├── naming.yaml         # Path, Name, FQName, Locator
│   ├── types.yaml          # Type expressions & definitions
│   ├── values.yaml         # Value expressions & definitions
│   └── access.yaml         # AccessControlled wrapper
├── classic/                # Single-blob mode
│   └── distribution.yaml   # Root: Distribution
└── tree/                   # Document Tree mode
    ├── format.yaml         # .morphir-dist/format.json
    ├── module.yaml         # module.json schema
    ├── type-node.yaml      # *.type.json schema
    └── value-node.yaml     # *.value.json schema
```

## Common schemas

The `common/*.yaml` files define the reusable building blocks. They are **not** root schemas — do not validate a
document against them directly; they exist to be referenced.

## Distribution-specific roots

### Classic

`classic/distribution.yaml` validates a monolithic `morphir-ir.json`, referencing the common definitions to build the
full nested structure.

### Document Tree

| Schema | Validates |
| ------ | --------- |
| `tree/format.yaml` | `format.json` at the distribution root |
| `tree/module.yaml` | `module.json`, in both manifest and inline styles |
| `tree/type-node.yaml` | An individual `*.type.json` |
| `tree/value-node.yaml` | An individual `*.value.json` |

See [Document Tree Layout](/document-tree-layout.md).

## Polymorphism via mutually exclusive keys

Type and value node schemas distinguish implementations from interfaces by root key:

```json
{ "def": { } }
```

```json
{ "spec": { } }
```

`def` validates against `TypeDefinition` or `ValueDefinition`; `spec` against `TypeSpecification` or
`ValueSpecification`. Because the keys are mutually exclusive, a validator can strictly check a node's content
according to its role rather than accepting a union of both.
