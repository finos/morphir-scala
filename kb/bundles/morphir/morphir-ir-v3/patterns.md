---
type: Specification Section
title: Patterns
description: The eight pattern forms used in lambdas, destructuring, and pattern matching.
tags: [morphir, ir, v3, values, patterns]
status: stable
sources:
  - id: ir-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/ir/morphir-ir-specification.md
    title: Morphir IR Specification — Patterns
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Patterns

**Patterns** destructure and filter values. They appear in exactly three places: `Lambda`, `Destructure`, and
`PatternMatch` — see [Value Expressions](/value-expressions.md). Every pattern carries an attribute, written `a`
below.

| Pattern | Structure | Source syntax | Purpose |
| ------- | --------- | ------------- | ------- |
| **WildcardPattern** | `WildcardPattern a` | `_` | Matches anything, binds nothing |
| **AsPattern** | `AsPattern a Pattern Name` | `pattern as name` | Binds a name to what the nested pattern matched |
| **TuplePattern** | `TuplePattern a (List Pattern)` | `(x, y)` | Destructures a tuple element-wise |
| **ConstructorPattern** | `ConstructorPattern a FQName (List Pattern)` | `Just x` | Matches a constructor and its arguments |
| **EmptyListPattern** | `EmptyListPattern a` | `[]` | Matches the empty list |
| **HeadTailPattern** | `HeadTailPattern a Pattern Pattern` | `x :: xs` | Splits a non-empty list into head and tail |
| **LiteralPattern** | `LiteralPattern a Literal` | `42`, `"hello"`, `True` | Matches an exact value |
| **UnitPattern** | `UnitPattern a` | | Matches the unit value |

## The plain variable binding

There is no dedicated "variable pattern". A simple binding is expressed as an `AsPattern` wrapping a wildcard:

```
x  →  AsPattern a (WildcardPattern a) ["x"]
```

This is worth internalizing, because it is what every lambda parameter looks like in the IR.

## Matching semantics

Patterns in a `PatternMatch` are tested **in order, first match wins**. Variables bound by a pattern are in scope only
in that pattern's associated branch. See [IR Semantics](/semantics.md).

## v4 divergence

The pattern set is unchanged in v4; only the attribute representation changes. See the v4 draft bundle.
