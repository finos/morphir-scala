---
type: Design Note
title: Value Encoding Decisions
description: The IntegerLiteral rename, value shorthand, and which value forms must stay explicitly wrapped.
tags: [morphir, ir, v4, draft, values, json, rationale]
status: draft
stale_after: 2026-12-31
sources:
  - id: design-values
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/design/draft/ir/values.md
    title: Types & Values (design) — Values
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Value Encoding Decisions

Design detail behind [Value Expressions](/value-expressions.md).

## The IntegerLiteral rename

| v1 / v2 / v3 | v4 | Notes |
| ------------ | -- | ----- |
| `WholeNumberLiteral` | `IntegerLiteral` | **Breaking change**: renamed for correctness — whole numbers are non-negative, integers include negatives |

The old name was simply wrong for what it held. Decoders should accept **both** tags; v4 encoders should output
`IntegerLiteral`.

## Value shorthand

Like types, values support shorthand when attributes are empty — but with a smaller surface, because value position
is more ambiguous than type position.

| Kind | Shorthand | Canonical |
| ---- | --------- | --------- |
| Bool | `true` | `{"Literal": {"BoolLiteral": true}}` |
| Number | `42` | `{"Literal": {"IntegerLiteral": 42}}` |
| Reference | `"pkg:mod#val"` | `{"Reference": "pkg:mod#val"}` |
| Variable | `"name"` | `{"Variable": "name"}` |
| List | `[v1, v2]` | `{"List": [v1, v2]}` |

### Disambiguation, and what cannot be shortened

A string is checked against the **FQName** pattern first; failing that, against the **Name** pattern for a
`Variable`.

**String literals and tuples must always use explicit wrappers.** The reason is direct: a bare string would be
indistinguishable from a variable name or an FQName, and a bare array is already claimed by `List`. This is the
concrete cost of value shorthand, and it is why the spec draft's simpler rule — value expressions *always* use object
wrappers — is a defensible simplification rather than a contradiction.

## Permissive input for tuples

Tuples follow the same permissive-input policy seen in [type encoding](/design/type-encoding-decisions.md):

| Format | Example | Notes |
| ------ | ------- | ----- |
| **Canonical** | `{ "Tuple": [value1, value2] }` | Wrapper with a bare array |
| Expanded | `{ "Tuple": { "elements": [value1] } }` | Wrapper with an object |

The design's canonical form is the bare array; the spec draft shows only the `elements` object form. Both decode.
