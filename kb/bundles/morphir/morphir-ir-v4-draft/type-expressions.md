---
type: Specification Section
title: Type Expressions
description: The v4 type expression nodes and their compact and expanded JSON serializations.
tags: [morphir, ir, v4, draft, types, type-system, json]
status: draft
stale_after: 2026-12-31
sources:
  - id: types
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/types.md
    title: Type System (IR v4 draft)
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Type Expressions

A **Type** is a recursive tree of type expressions. The node set is unchanged from v3; every node now carries a
[`TypeAttributes`](/attributes.md) structure instead of a generic parameter.

## The nodes

### Variable

`Variable attributes name` — a type variable, e.g. the `a` in `List a`.

- **JSON (compact)**: `"a"` — a bare name string.
- **JSON (expanded)**: `{"Variable": {"name": "a"}}`

### Reference

`Reference attributes fqName args` — a reference to another type or alias, with type arguments.

- `String` → `morphir/sdk:string#string`
- `List Int` → `morphir/sdk:list#list` with argument `morphir/sdk:basics#int`
- **JSON (compact, no args)**: `"morphir/sdk:string#string"`
- **JSON (compact, with args)**: `{"Reference": ["morphir/sdk:list#list", "a"]}`
- **JSON (expanded)**: `{"Reference": {"fqname": "morphir/sdk:list#list", "args": [...]}}`
- **Legacy**: the v3 array-of-arrays form is still decodable.

### Tuple

`Tuple attributes elements` — element types in order.

```json
{"Tuple": {"elements": ["morphir/sdk:int#int", "morphir/sdk:string#string"]}}
```

### Record

`Record attributes fields` — a **dictionary** of field names to types, not v3's ordered field list. Fields sit
directly under `Record` with no wrapper, and field names are kebab-case.

```json
{"Record": {"field-name": "morphir/sdk:string#string", "age": "morphir/sdk:int#int"}}
```

### ExtensibleRecord

`ExtensibleRecord attributes variable fields` — a record open to extension via a type variable.

```json
{"ExtensibleRecord": {"variable": "a", "fields": {"name": "morphir/sdk:string#string"}}}
```

### Function

`Function attributes argumentType returnType`.

```json
{"Function": {"argumentType": "morphir/sdk:int#int", "returnType": "morphir/sdk:string#string"}}
```

### Unit

`Unit attributes` → `{"Unit": {}}`

## Serialization: compact vs expanded

### Compact (default)

| Type expression | JSON form | Example |
| --------------- | --------- | ------- |
| Variable | Bare name string | `"a"` |
| Reference (no args) | Bare FQName string | `"morphir/sdk:int#int"` |
| Reference (with args) | Array: fqname then args | `{"Reference": ["morphir/sdk:list#list", "a"]}` |
| Record | Object with field map | `{"Record": {"name": "morphir/sdk:string#string"}}` |
| Tuple | Object with elements | `{"Tuple": {"elements": [...]}}` |
| Function | Object with argument and return | `{"Function": {...}}` |
| Unit | Empty object | `{"Unit": {}}` |

**Disambiguation**: a Variable and an argument-less Reference are both bare strings. They are told apart by
punctuation — a Reference is an FQName containing `:` and `#`; a Variable is a simple name without them. See
[Naming](/naming.md).

### Expanded

Identical to compact for Record, Tuple, Function, and Unit; differs only for Variable and Reference, which gain
object wrappers with explicit keys. Produce it with `morphir ir migrate --expanded`.

## Contrast with value expressions

[Value expressions](/value-expressions.md) *always* use object wrappers — `{"Variable": "x"}`, never bare `"x"`. That
asymmetry is deliberate: it lets a parser identify an expression's kind in any context without tracking whether it is
in type or value position.
