---
type: Specification Section
title: Value Specifications and Definitions
description: Function signatures and their implementations — the value-level half of the specification/definition split.
tags: [morphir, ir, v3, values, specifications, definitions]
status: stable
sources:
  - id: ir-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/ir/morphir-ir-specification.md
    title: Morphir IR Specification — Value Specifications, Value Definitions
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Value Specifications and Definitions

## Value Specification

A **Value Specification** is the type signature of a value or function — no implementation.

- **inputs** — a list of `(Name, Type)` pairs, one per parameter.
- **output** — the return type.

`add : Int -> Int -> Int` becomes:

```
{ inputs = [("a", Int), ("b", Int)]
, output = Int
}
```

Note that specifications *flatten* currying: a two-argument function has two entries in `inputs`, not a nested
`Function` type. A zero-argument value (a constant) has an empty `inputs` list.

## Value Definition

A **Value Definition** is the complete implementation.

- **inputTypes** — a list of `(Name, va, Type)` triples: parameter name, value attribute, parameter type.
- **outputType** — the return type.
- **body** — the [value expression](/value-expressions.md) implementing the logic.

The extra `va` per parameter is the [value attribute](/attributes-and-wrappers.md), which is where a compiler records
things like the parameter's source location.

## Lambda extraction

Parameters are extracted from nested lambdas where possible: rather than a body that is a chain of `Lambda` nodes, a
definition lists its parameters in `inputTypes` and its body is the innermost expression. Tools generating IR should
do this extraction — see [Implementing Morphir Tools](/implementing-tools.md).

## Deriving the specification

Dropping `body`, and dropping the `va` from each input triple, turns a definition into a specification. That is the
value-level case of [Specifications vs Definitions](/specification-vs-definition.md).

## v4 divergence

v4 introduces a `ValueDefinitionBody` with four variants — `ExpressionBody` (the v3 behavior), `NativeBody`,
`ExternalBody`, and `IncompleteBody` — all of which derive the *same* `ValueSpecification`. Consumers cannot tell from
a specification how a value is implemented. See the v4 draft bundle.
