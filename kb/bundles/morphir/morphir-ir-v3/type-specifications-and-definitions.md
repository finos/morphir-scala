---
type: Specification Section
title: Type Specifications and Definitions
description: Alias, opaque, custom, and derived types, and how a type definition collapses into a specification.
tags: [morphir, ir, v3, types, specifications, definitions]
status: stable
sources:
  - id: ir-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/ir/morphir-ir-specification.md
    title: Morphir IR Specification — Type Specifications, Type Definitions
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Type Specifications and Definitions

A **Type Specification** is the interface of a type; a **Type Definition** is its complete implementation. The general
pattern is described in [Specifications vs Definitions](/specification-vs-definition.md); this concept covers the
type-level forms.

## Type Specifications

### TypeAliasSpecification

`TypeAliasSpecification (List Name) Type` — type parameters plus the aliased [type expression](/type-expressions.md).

`type alias UserId = String`. Gives a meaningful name to a type expression; nothing is hidden.

### OpaqueTypeSpecification

`OpaqueTypeSpecification (List Name)` — type parameters only.

The structure is hidden from consumers. Consequences worth knowing: opaque values **cannot be automatically
serialized**, and they can only be created or manipulated through functions the owning module provides.

### CustomTypeSpecification

`CustomTypeSpecification (List Name) Constructors` — type parameters plus a dictionary of constructor names to their
arguments, where each argument list is `List (Name, Type)`.

`type Result e a = Ok a | Err e`. This is Morphir's tagged union / sum type.

### DerivedTypeSpecification

`DerivedTypeSpecification (List Name) Details` — a type with a platform-specific representation but a known
serialization. `Details` carries:

- **baseType** — the type used for serialization.
- **fromBaseType** — FQName of the function converting *from* the base type.
- **toBaseType** — FQName of the function converting *to* the base type.

A `LocalDate` that serializes to and from `String` is the canonical example. This is the escape hatch that lets a
platform use its own representation without giving up serializability — the thing `OpaqueTypeSpecification` cannot do.

## Type Definitions

### TypeAliasDefinition

`TypeAliasDefinition (List Name) Type` — identical in shape to the specification, because an alias has no hidden
implementation.

### CustomTypeDefinition

`CustomTypeDefinition (List Name) (AccessControlled Constructors)` — type parameters plus constructors wrapped in
[`AccessControlled`](/attributes-and-wrappers.md).

The wrapper is what makes opaque types possible from a custom type:

| Constructor access | Derived specification |
| ------------------ | --------------------- |
| Public | `CustomTypeSpecification` — consumers may pattern match |
| Private | `OpaqueTypeSpecification` — the type is visible, its shape is not |

## v4 divergence

v4 adds a fifth definition form, `IncompleteTypeDefinition`, carrying an `Incompleteness` reason and an optional
partial body, which derives as an `OpaqueTypeSpecification`. It supports best-effort compilation of work in progress.
See the v4 draft bundle's incompleteness concept.
