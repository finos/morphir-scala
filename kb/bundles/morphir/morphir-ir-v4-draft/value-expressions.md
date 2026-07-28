---
type: Specification Section
title: Value Expressions
description: v4 literals, value expression nodes and their JSON forms, patterns, and the new Hole node.
tags: [morphir, ir, v4, draft, values, expressions, patterns, json]
status: draft
stale_after: 2026-12-31
sources:
  - id: values
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/values.md
    title: Value System (IR v4 draft)
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Value Expressions

Values encode both data and logic. Every node carries a [`ValueAttributes`](/attributes.md) structure — `source`,
`inferredType`, `extensions` — rather than v3's generic parameter.

## Literals

| Literal | Holds |
| ------- | ----- |
| `BoolLiteral` | `true`, `false` |
| `CharLiteral` | a single character |
| `StringLiteral` | text |
| `IntegerLiteral` | arbitrary-precision integer, including negatives |
| `FloatLiteral` | floating-point number |
| `DecimalLiteral` | arbitrary-precision decimal, stored as a string for precision |

Note the rename: v3's `WholeNumberLiteral` is `IntegerLiteral` here.

## Expression nodes

Value expressions **always** use object wrappers, with compact inner values where possible.

| Node | Structure | JSON |
| ---- | --------- | ---- |
| Literal | `Literal attributes literal` | `{"Literal": {"IntLiteral": 42}}` |
| Constructor | `Constructor attributes fqName` | `{"Constructor": "morphir/sdk:maybe#just"}` |
| Tuple | `Tuple attributes elements` | `{"Tuple": {"elements": [...]}}` |
| List | `List attributes elements` | `{"List": {"items": [...]}}` |
| Record | `Record attributes fields` | `{"Record": {"name": {"Variable": "x"}}}` |
| Variable | `Variable attributes name` | `{"Variable": "x"}` |
| Reference | `Reference attributes fqName` | `{"Reference": "morphir/sdk:basics#add"}` |
| Field | `Field attributes recordExpression fieldName` | `{"Field": {"target": {...}, "name": "field-name"}}` |
| FieldFunction | `FieldFunction attributes fieldName` | `{"FieldFunction": "field-name"}` |
| Apply | `Apply attributes function argument` | `{"Apply": {"function": {...}, "argument": {...}}}` |
| Lambda | `Lambda attributes argumentPattern body` | `{"Lambda": {"pattern": {...}, "body": {...}}}` |
| LetDefinition | `LetDefinition attributes name definition body` | `{"LetDefinition": {"name": "x", "definition": {...}, "in": {...}}}` |
| LetRecursion | `LetRecursion attributes bindings body` | `{"LetRecursion": {"definitions": {...}, "in": {...}}}` |
| Destructure | `Destructure attributes pattern valueToDestructure body` | `{"Destructure": {"pattern": {...}, "value": {...}, "in": {...}}}` |
| IfThenElse | `IfThenElse attributes condition thenBranch elseBranch` | `{"IfThenElse": {"condition": {...}, "then": {...}, "else": {...}}}` |
| PatternMatch | `PatternMatch attributes valueToMatch cases` | `{"PatternMatch": {"value": {...}, "cases": [...]}}` |
| UpdateRecord | `UpdateRecord attributes recordToUpdate fieldsToUpdate` | `{"UpdateRecord": {"target": {...}, "fields": {...}}}` |
| Unit | `Unit attributes` | `{"Unit": {}}` |

Record fields sit directly under `Record` with kebab-case names, and **field order does not affect equality** — two
records with the same fields in different orders are equal. The same holds for the field map in `UpdateRecord`.

## Why the wrappers differ from types

[Type expressions](/type-expressions.md) may serialize a Variable or an argument-less Reference as a bare string.
Value expressions never do. The asymmetry lets a parser unambiguously identify an expression's kind in any position.

## New in v4: Hole

`Hole attributes reason expectedType` — an incomplete or broken reference that lets compilation proceed.

- `reason` — a `HoleReason`
- `expectedType` — optional expected `Type`

Use cases: a reference to a deleted or renamed function, a placeholder during incremental development, or
representing a compilation error without failing the whole build. See [Incompleteness](/incompleteness.md).

## New in v4: Native and External

`Native attributes fqName nativeInfo` and `External attributes externalName targetPlatform` — see
[Native and External Values](/native-and-external-values.md).

## Patterns

Unchanged from v3, now carrying `ValueAttributes`:

`WildcardPattern`, `AsPattern attributes pattern name`, `TuplePattern attributes patterns`,
`ConstructorPattern attributes fqName patterns`, `EmptyListPattern`, `HeadTailPattern attributes headPattern
tailPattern`, `LiteralPattern attributes literal`, `UnitPattern`.
