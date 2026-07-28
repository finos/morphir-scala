---
type: Design Note
title: Annotations
description: Semantic labels attached to IR specifications, in the manner of Java or Scala annotations.
tags: [morphir, ir, v4, draft, annotations, metadata]
status: draft
stale_after: 2026-12-31
sources:
  - id: design-annotations
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/design/draft/ir/annotations.md
    title: Annotations (design)
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Annotations

**Not present in the v4 spec draft.** Annotations attach structured metadata to IR **specification** types —
higher-level semantic labeling of signatures, in the manner of Java or Scala annotations.

## Where annotations sit among v4's four metadata mechanisms

| Mechanism | Carries | Attached to |
| --------- | ------- | ----------- |
| [Attributes](/attributes.md) | Implementation metadata — source location, inferred type | Every type and value node |
| **Annotations** | Semantic labels on signatures | Specifications only |
| [Decorations](/design/decorations.md) | Layered external metadata | IR nodes, stored outside the IR |
| [`$meta`](/design/file-metadata.md) | Provenance and tooling data | VFS files |

Annotations being **specification-only** is the defining constraint. They describe the contract, so they travel with
it to consumers — unlike attributes, which describe the implementation.

## Structure

```
Annotation(name: FQName, arguments: List(AnnotationArgument))

AnnotationArgument =
  | PositionalArgument(value: Value)
  | NamedArgument(name: Name, value: Value)
```

Arguments are Morphir **values**, not free-form JSON — so an annotation argument can be a literal, a list, or a
reference to a constructor.

## Serialization

### Shorthand

| Case | Format |
| ---- | ------ |
| Marker, 0 arguments | `"package:module#name"` |
| Single value, 1 argument | `"package:module#name:value"` |

```
"morphir/sdk:annotations#stable"
"my-org/sdk:annotations#deprecated:Use new-function instead"
"my-org/sdk:annotations#version:1.0.0"
```

### Canonical

```json
{
  "name": "my-org/sdk:annotations#deprecated",
  "arguments": [
    { "Literal": { "StringLiteral": "Use new-function instead" } }
  ]
}
```

Named and mixed arguments are supported:

```json
{
  "name": "my-org/sdk:annotations#task",
  "arguments": [
    { "Literal": { "StringLiteral": "Refactor this" } },
    { "name": "priority", "value": { "Literal": { "IntegerLiteral": 1 } } }
  ]
}
```

An annotation list may mix both encodings — a marker as a bare string next to a full object.

## Where they attach

`ModuleSpecification`, `TypeSpecification` (each variant), and `ValueSpecification` each gain an `annotations` array:

```json
{
  "ValueSpecification": {
    "annotations": ["my-org/sdk:annotations#pure"],
    "inputs": { },
    "output": { }
  }
}
```

## Intended uses

- **Stability and lifecycle** — `@deprecated("Use newMethod instead", "2.0.0")`, `@stable`.
- **Physical schema mapping** — `@jsonName("user_id")` as a code-generation or database-mapping hint.
- **Tooling hints and domain labels** — `@security(level = High, roles = ["admin", "auditor"])`, where the level is a
  `Reference` to a constructor and the roles are a `List` of literals.

The third case is the one that justifies arguments being IR values rather than JSON: a security level referencing
`my-org/security:types#security-level.high` is checkable against the model, where a bare string would not be.

## Open question

Annotations and [decorations](/design/decorations.md) overlap heavily — a `deprecated` annotation and a `deprecated`
decoration carry near-identical information. The design documents do not say when to reach for which, beyond
annotations living inside the IR and decorations living beside it.
