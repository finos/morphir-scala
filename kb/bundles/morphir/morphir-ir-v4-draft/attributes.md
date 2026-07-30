---
type: Specification Section
title: Attributes
description: TypeAttributes and ValueAttributes — the explicit structures replacing v3's generic attribute parameter.
tags: [morphir, ir, v4, draft, attributes, metadata]
status: draft
stale_after: 2026-12-31
sources:
  - id: attributes
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/attributes.md
    title: Attributes (IR v4 draft)
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Attributes

In v4, attributes are **explicit structures** attached to Type and Value nodes rather than generic parameters. This
is the single largest structural break from v3, where `Type a` and `Value ta va` were parameterized over an arbitrary
attribute type — most commonly the unit type `()`.

The consequence: attributes now have a *standard schema*, so a tool can rely on finding source location or inferred
type without knowing which compiler produced the IR.

## TypeAttributes

Attached to every `Type` node.

```json
{
  "source": { "startLine": 1, "startColumn": 1, "endLine": 1, "endColumn": 10 },
  "constraints": { },
  "extensions": { }
}
```

| Field | Type | Meaning |
| ----- | ---- | ------- |
| `source` | optional `SourceLocation` | Where the type came from in the source text |
| `constraints` | optional `TypeConstraints` | Type constraints |
| `extensions` | dictionary | `FQName` keys to arbitrary extension values |

## ValueAttributes

Attached to every `Value` node.

```json
{
  "source": { },
  "inferredType": { },
  "extensions": { }
}
```

| Field | Type | Meaning |
| ----- | ---- | ------- |
| `source` | optional `SourceLocation` | Where the value came from in the source text |
| `inferredType` | optional `Type` | The inferred type of the value expression |
| `extensions` | dictionary | `FQName` keys to arbitrary extension values |

## The extensions escape hatch

`extensions` is keyed by `FQName`, which means a tool wanting to attach its own metadata namespaces it under a name it
owns rather than colliding in a flat keyspace. This is what preserves v3's open-ended extensibility now that the
generic parameter is gone.

## Related

[Type Expressions](/type-expressions.md) and [Value Expressions](/value-expressions.md) carry these structures on
every node. The [Morphir attribution evolution case study](https://github.com/finos/morphir-scala/blob/main/kb/bundles/programming-language-tooling/morphir-attribution-evolution.md)
traces the change from v3 generic payloads and keeps this draft schema distinct from the separate decorations design.
The draft [typed attribution guide for morphir-scala](https://github.com/finos/morphir-scala/blob/main/kb/bundles/programming-language-tooling/typed-attribution-guidance-for-morphir-scala.md)
ranks implementation strategies for prototyping; it does not change this specification's draft status or schema.
