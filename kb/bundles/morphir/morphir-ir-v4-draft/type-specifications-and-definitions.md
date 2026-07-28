---
type: Specification Section
title: Type Specifications and Definitions
description: Alias, opaque, custom, and derived type forms in v4, plus the new incomplete type definition.
tags: [morphir, ir, v4, draft, types, specifications, definitions]
status: draft
stale_after: 2026-12-31
sources:
  - id: types
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/types.md
    title: Type System (IR v4 draft) — Type Specifications, Type Definitions
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Type Specifications and Definitions

A **Type Specification** is the public contract a module exposes; a **Type Definition** is the implementation the
module owns. Specifications are always public; definitions carry an `AccessControlled` wrapper.

Why the split exists: when module A depends on module B, A sees only B's specifications. That enables separate
compilation, API stability under internal change, and information hiding.

**Deriving specifications** from definitions:

- `TypeAliasDefinition` → `TypeAliasSpecification`
- `CustomTypeDefinition` → `CustomTypeSpecification` (public constructors only)
- `IncompleteTypeDefinition` → `OpaqueTypeSpecification` (hides internal brokenness)

## Specifications

### TypeAliasSpecification

`TypeAliasSpecification typeParams type`.

```elm
type alias Person = { name : String, age : Int, email : Maybe String }
```

```json
{
  "TypeAliasSpecification": {
    "typeParams": [],
    "type": {
      "Record": {
        "name": "morphir/sdk:string#string",
        "age": "morphir/sdk:basics#int",
        "email": ["morphir/sdk:maybe#maybe", "morphir/sdk:string#string"]
      }
    }
  }
}
```

With parameters, `type alias Pair a b = ( a, b )` becomes
`{"typeParams": ["a", "b"], "type": {"Tuple": {"elements": ["a", "b"]}}}`.

### OpaqueTypeSpecification

`OpaqueTypeSpecification typeParams` — structure hidden.

```json
{ "OpaqueTypeSpecification": { "typeParams": ["k", "v"] } }
```

### CustomTypeSpecification

`CustomTypeSpecification typeParams constructors`, where `constructors` maps constructor names to argument lists of
`(Name, Type)` pairs. Constructor and argument names are kebab-case.

```elm
type Result error value = Ok value | Err error
```

```json
{
  "CustomTypeSpecification": {
    "typeParams": ["error", "value"],
    "constructors": {
      "ok": [["value", "value"]],
      "err": [["error", "error"]]
    }
  }
}
```

Recursive types reference themselves by FQName —
`"cons": [["head", "a"], ["tail", ["morphir/sdk:list#list", "a"]]]`.

### DerivedTypeSpecification

`DerivedTypeSpecification typeParams details` — a platform-specific representation with a known serialization.
`details` carries `baseType`, `fromBaseType`, and `toBaseType`.

```json
{
  "DerivedTypeSpecification": {
    "typeParams": [],
    "baseType": "morphir/sdk:string#string",
    "fromBaseType": "morphir/sdk:local-date#from-i-s-o",
    "toBaseType": "morphir/sdk:local-date#to-i-s-o"
  }
}
```

The base type may be structured (a `Record` for a `Money` type) and the derived type may be parameterized (a
`NonEmpty a` serializing as `["morphir/sdk:list#list", "a"]`).

## Definitions

Definitions contain everything needed to generate code, type check within the module, and derive the public
specification. `AccessControlled` marks each `Public` or `Private`.

### TypeAliasDefinition

`TypeAliasDefinition typeParams type`.

### CustomTypeDefinition

`CustomTypeDefinition typeParams constructors`, where constructors are `AccessControlled`. Private constructors
derive an `OpaqueTypeSpecification`.

### IncompleteTypeDefinition (v4)

`IncompleteTypeDefinition typeParams incompleteness partialBody` — a definition that is incomplete or broken,
enabling best-effort compilation and incremental development.

- `incompleteness` — why it is incomplete (`Incompleteness`)
- `partialBody` — an optional partial type body

It derives as an `OpaqueTypeSpecification`, so dependents never see the brokenness. See
[Incompleteness](/incompleteness.md).
