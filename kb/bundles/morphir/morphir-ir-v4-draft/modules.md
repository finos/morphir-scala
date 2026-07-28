---
type: Specification Section
title: Modules
description: module.json in manifest and inline styles, granular definition files, and the Documentation type.
tags: [morphir, ir, v4, draft, modules, document-tree, documentation]
status: draft
stale_after: 2026-12-31
sources:
  - id: modules
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/modules.md
    title: Modules (IR v4 draft)
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Modules

A **Module** contains related types and values. Conceptually it is a name (a `Path` such as `main/domain`), a
collection of type definitions or specifications, a collection of value definitions or specifications, and optional
documentation. What is new in v4 is that its *physical* representation depends on the distribution mode.

## Documentation

The **Documentation** type supports multi-line docs with cross-platform line-ending handling.

- **Structure**: an opaque type containing a list of lines.
- **Input formats**: a single string (split on `\n`) or an array of strings.
- **Normalization**: trailing `\r` characters are trimmed.
- **JSON**: single-line as `"doc": "Brief description"`, multi-line as `"doc": ["Line 1", "Line 2"]`.

## Documented wrapper

`Documented(doc: Option(Documentation), value: a)` attaches optional documentation to any definition or
specification.

- The wrapper is **flattened** in JSON — `doc` sits alongside the value's own fields rather than nesting it.
- If documentation is `None`, the `doc` field is omitted entirely.

```json
{
  "doc": "A user in the system",
  "TypeAliasDefinition": { }
}
```

## Classic mode

A module is a JSON object nesting its types and values. `types` and `values` may each be **omitted when empty**, so a
module with only values is `{"values": {...}}` and an empty module is `{}`.

```json
{
  "types": { },
  "values": { },
  "doc": "Module documentation"
}
```

## Document Tree mode

A module is a `module.json` file, in one of two styles — or a mix of them.

### Manifest style (granular)

`module.json` holds metadata; definitions live in their own files.

```
pkg/my-org/my-project/orders/
├── module.json
├── order.type.json
├── line-item.type.json
├── create-order.value.json
├── calculate-total.value.json
└── shipping/
    ├── module.json
    ├── address.type.json
    └── calculate-cost.value.json
```

```json
{
  "formatVersion": 4,
  "module": "main/domain",
  "doc": "Domain model for main application"
}
```

### Inline style (hybrid)

`module.json` holds the definitions directly, reducing file count for smaller modules.

```json
{
  "formatVersion": 4,
  "module": "main/domain",
  "types": { "user": { "def": { } } },
  "values": { "login": { "def": { } } }
}
```

## Granular definition files

In manifest style the rule is one file per definition:

- **Naming** — canonical kebab-case name plus suffix, `.type.json` or `.value.json`.
- **Location** — directly in the module directory.
- **Polymorphism** — the root key says which it is: `def` for an implementation, `spec` for an interface.

```json
{
  "doc": "Represents a user in the system",
  "spec": { "OpaqueTypeSpecification": {} }
}
```

The mutually exclusive `def` / `spec` keys are what let a validator pick the right schema for the file. See
[Schema Architecture](/schema-architecture.md).

## Specifications vs definitions

`ModuleSpecification` is the public interface — public types as specifications, value signatures only.
`ModuleDefinition` is the full implementation, public and private. Access control is applied at the definition level;
specifications are always derived from the public subset.
