---
type: Specification Section
title: Value Expressions
description: The expression nodes that encode all computation in the IR, from Literal through UpdateRecord.
tags: [morphir, ir, v3, values, expressions]
status: stable
sources:
  - id: ir-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/ir/morphir-ir-specification.md
    title: Morphir IR Specification — Value System
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Value Expressions

A **Value** is a recursive tree representing computation. Values encode both data and logic — there is no separate
statement form, because everything is an expression. Every node carries a value attribute `va`; see
[Attributes and Wrappers](/attributes-and-wrappers.md).

## Data nodes

### Literal

`Literal va Literal` — a constant. Supported literal kinds:

| Literal | Holds |
| ------- | ----- |
| `BoolLiteral` | `True`, `False` |
| `CharLiteral` | a single character |
| `StringLiteral` | text |
| `WholeNumberLiteral` | integers |
| `FloatLiteral` | floating-point numbers |
| `DecimalLiteral` | arbitrary-precision decimals |

### Constructor

`Constructor va FQName` — a reference to a custom type constructor. If the constructor takes arguments, it appears
wrapped in `Apply` nodes.

### Tuple

`Tuple va (List Value)` — positional grouping: `(42, "hello", True)`.

### List

`List va (List Value)` — a homogeneous sequence: `[1, 2, 3, 4]`.

### Record

`Record va (Dict Name Value)` — named fields: `{ firstName = "John", age = 30 }`.

### Unit

`Unit va` — the single value of the Unit type.

## Reference nodes

### Variable

`Variable va Name` — a variable in scope: a function parameter or a let binding.

### Reference

`Reference va FQName` — a defined value or function elsewhere, e.g. `Morphir.SDK.List.map`. Always
[fully qualified](/naming.md).

## Record access

### Field

`Field va Value Name` — field access on a record. `user.firstName` becomes
`Field va (Variable va ["user"]) ["first", "name"]`.

### FieldFunction

`FieldFunction va Name` — a *function* that extracts a field. Elm's `.firstName`, equivalent to `\r -> r.firstName`.

### UpdateRecord

`UpdateRecord va Value (Dict Name Value)` — `{ user | age = 31 }`. Copy-on-update: the original record is unchanged.

## Functions and application

### Apply

`Apply va Value Value` — function applied to one argument. Multi-argument calls curry into nested `Apply` nodes:

```
add 1 2  →  Apply va (Apply va (Reference va add) (Literal va 1)) (Literal va 2)
```

### Lambda

`Lambda va Pattern Value` — an anonymous function; the argument is a [Pattern](/patterns.md), not a bare name.
`\x -> x + 1` becomes `Lambda va (AsPattern va (WildcardPattern va) ["x"]) body`.

## Binding and control flow

### LetDefinition

`LetDefinition va Name Definition Value` — binds one name to a definition, in scope in the trailing expression.
`let x = 5 in x + x`.

### LetRecursion

`LetRecursion va (Dict Name Definition) Value` — several bindings that may reference each other, enabling mutual
recursion.

### Destructure

`Destructure va Pattern Value Value` — matches a pattern against an expression and brings the extracted variables into
scope. `let (x, y) = point in ...`.

### IfThenElse

`IfThenElse va Value Value Value` — condition, then-branch, else-branch.

### PatternMatch

`PatternMatch va Value (List (Pattern, Value))` — matches an expression against a list of pattern/branch pairs.
Patterns are tested in order and the first match wins; see [IR Semantics](/semantics.md).

## v4 divergence

v4 keeps all of the above and adds three nodes — `Hole` (a broken or unfinished reference), `Native` (a platform
builtin), and `External` (an FFI call) — alongside a structured `ValueAttributes`. See the v4 draft bundle.
