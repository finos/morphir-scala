---
type: Design Note
title: Document Type
description: A schema-less JSON-like value for untyped or dynamically-typed data inside a statically typed IR.
tags: [morphir, ir, v4, draft, document, json, interop]
status: draft
stale_after: 2026-12-31
sources:
  - id: design-document
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/design/draft/ir/document.md
    title: Document Type (design)
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Document Type

**Not present in the v4 spec draft.** A first-class, schema-less JSON-like structure for representing untyped or
dynamically-typed data.

## The key design choice

> **Document is a `Literal` variant, not a `Type` variant.**

The type is just `morphir/sdk:document#document` — an ordinary SDK reference. No new `Type` constructor is added.
The design compares the alternatives directly:

| Approach | Pros | Cons |
| -------- | ---- | ---- |
| **Document (chosen)** | Clean, JSON-native, simple | No compile-time structure checking |
| Type variant | Type-level operations | Complicates the `Type` sum type |
| Extensible records | Some structure | Still needs known fields |
| Any / Dynamic | Maximum flexibility | No structure at all |

Keeping it out of `Type` means every existing consumer of type expressions — every backend, every analyzer — needs no
change to handle it. The cost is that nothing about a document's shape is checkable at compile time.

## Structure

```
DocumentValue =
  | DocNull
  | DocBool(Bool)
  | DocInt(Int)
  | DocFloat(Float)
  | DocString(String)
  | DocArray(List(DocumentValue))
  | DocObject(Dict(String, DocumentValue))
```

The `Literal` type gains one variant: `DocumentLiteral(value: DocumentValue)`. See
[Value Expressions](/value-expressions.md).

## Serialization

Canonical form wraps every node:

```json
{ "DocObject": {
    "name": { "DocString": "Alice" },
    "age": { "DocInt": 30 }
  }
}
```

Shorthand uses plain JSON where context is unambiguous:

```json
{ "DocumentLiteral": { "name": "Alice", "age": 30, "tags": ["admin"], "metadata": null } }
```

Decoding is by JSON type: `null` → `DocNull`, integer → `DocInt`, floating number → `DocFloat`, and so on.

## Operations are specs-only

The `Morphir.SDK.Document` module is a **specification** — construction (`null`, `bool`, `int`, `string`, `array`,
`object`), extraction (`asBool`, `asInt`, `asString`, `asArray`, `asObject`), navigation (`get`, `getPath`),
predicates (`isNull`, `isString`, …), and `encode` / `decoder`. Backends implement them natively.

Extraction returns `Maybe`, which is where the type safety actually lives — a document cannot be used without an
explicit failure path.

Backends map the type onto whatever they already have: `unknown` or a union in TypeScript, a sealed trait or
`io.circe.Json` in Scala, `any` or an interface in Go, sealed interfaces with records in Java 17+.

## Use cases

External API responses with unknown or varying schemas, configuration that needs no compile-time typing,
pass-through forwarding of opaque data, context-dependent metadata, and interop with JSON-based systems.

## Open considerations

Not in the core design, but named as possible extensions: `DocBinary` for base64 blobs, `DocTimestamp` for ISO 8601
dates, `DocDecimal` for arbitrary precision, `DocReference` for links to IR nodes, runtime schema validation supplied
through decorations, and a `merge` operation with deep-merge semantics for objects.
