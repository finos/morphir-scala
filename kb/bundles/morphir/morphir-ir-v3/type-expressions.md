---
type: Specification Section
title: Type Expressions
description: The seven type expression nodes — Variable, Reference, Tuple, Record, ExtensibleRecord, Function, and Unit.
tags: [morphir, ir, v3, types, type-system]
status: stable
sources:
  - id: ir-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/ir/morphir-ir-specification.md
    title: Morphir IR Specification — Type System
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Type Expressions

A **Type** is a recursive tree of type expressions. Every node carries a type attribute `a` — see
[Attributes and Wrappers](/attributes-and-wrappers.md). The system is in the ML family, closest to Elm.

## The seven nodes

### Variable

`Variable a Name` — a type variable, the `a` in `List a`. Enables polymorphism.

### Reference

`Reference a FQName (List Type)` — a reference to another type or type alias, with type arguments for generic types.

- `String` → `Reference a (["morphir"], ["s","d","k"], ["string"]) []`
- `List Int` → `Reference a (["morphir"], ["s","d","k"], ["list"]) [intType]`

References are always [fully qualified](/naming.md), so no scope resolution is needed to interpret them.

### Tuple

`Tuple a (List Type)` — a product type with positional access; element types in order.

- `(Int, String)` → `Tuple a [intType, stringType]`

A zero-element tuple is equivalent to Unit; a single-element tuple is equivalent to the element type itself.

### Record

`Record a (List Field)` — a product type with named field access. Each field is `{ name: Name, tpe: Type }`.

- `{ firstName: String, age: Int }`

Field order is preserved but is not semantically significant. All fields are required — there are no optional fields.

### ExtensibleRecord

`ExtensibleRecord a Name (List Field)` — a record type open to extension. The `Name` is the type variable standing for
the extension.

- `{ a | firstName: String, age: Int }` — "type `a` with at least these fields".

### Function

`Function a Type Type` — argument type and return type.

- `Int -> String` → `Function a intType stringType`
- `Int -> Int -> Bool` → `Function a intType (Function a intType boolType)`

Multi-argument functions are curried: nested `Function` nodes, one per argument.

### Unit

`Unit a` — the type with exactly one value; the placeholder where a type is required but the value is unused.
Corresponds to `void` in some languages.

## Where types appear

Type expressions are the payload of [type specifications and definitions](/type-specifications-and-definitions.md),
the signature material in [value specifications and definitions](/value-specifications-and-definitions.md), and — via
the value attribute — often the inferred-type annotation on [value expressions](/value-expressions.md).

## v4 divergence

The node set is unchanged in v4. What changes is that the generic attribute parameter `a` is replaced by an explicit
`TypeAttributes` structure, records become a field *dictionary* rather than a field list, and JSON gains compact forms
(a bare string for a Variable or an argument-less Reference). See the v4 draft bundle.
