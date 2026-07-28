---
type: Design Note
title: Type Encoding Decisions
description: Permissive input and canonical output, type shorthand forms, and backwards-compatible decoding of v1 through v3.
tags: [morphir, ir, v4, draft, types, json, rationale, compatibility]
status: draft
stale_after: 2026-12-31
sources:
  - id: design-types
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/design/draft/ir/types.md
    title: Types & Values (design)
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Type Encoding Decisions

Design detail behind [Type Expressions](/type-expressions.md), plus material the spec draft does not carry.

## Permissive input, canonical output

The governing policy for v4 decoders: accept many forms, emit exactly one. It shows up first in access control.

### Access

| Format | Example | Notes |
| ------ | ------- | ----- |
| Canonical | `"Public"`, `"Private"` | Preferred output |
| Lowercase | `"public"`, `"private"` | Accepted |
| Abbreviation | `"pub"` | Accepted, means Public |

### AccessControlled

| Format | Example | Notes |
| ------ | ------- | ----- |
| **Canonical** | `{ "Public": {...} }` | Access is the key, value is the value |
| Lowercase key | `{ "public": {...} }` | Accepted |
| Abbreviation key | `{ "pub": {...} }` | Accepted |
| Legacy | `{ "access": "Public", "value": {...} }` | v3 compatibility |

Note that the canonical v4 form makes access a *tag*, where v3 made it a field. The v3 shape is still decodable.

## Type shorthand

Shorthand applies when attributes are empty or null.

| Form | Interpretation | Disambiguation |
| ---- | -------------- | -------------- |
| `"morphir/sdk:basics#int"` | `Reference`, no args | Contains `:` and `#` → FQName |
| `"a"` | `Variable` | No `:` or `#` → variable name |
| `["morphir/sdk:list#list", ...]` | `Reference` with args | Array → parameterized type |

The disambiguation is mechanical:

```
if string contains ":" and "#":   → FQName reference
else if plain string:             → variable name
else if array:                    → parameterized type; head is the FQName, tail are the args
else if object:                   → canonical wrapper form
```

The **array form for parameterized types** is the part the spec draft states differently — it writes
`{"Reference": ["morphir/sdk:list#list", "a"]}`, keeping the wrapper. The design's bare-array form composes more
cleanly when nested:

```json
["morphir/sdk:list#list", ["morphir/sdk:maybe#maybe", "morphir/sdk:basics#int"]]
```

```json
["morphir/sdk:result#result", "morphir/sdk:string#string", "a"]
```

See [Specification and Design Divergences](/design/divergences.md).

### Encoding and decoding rules

- **Encoding** — use shorthand when attributes are empty or null; fall back to canonical for types that carry
  attributes.
- **Decoding** — accept both, dispatching on JSON shape as above.

The attribute condition is the crux: shorthand is not a separate dialect, it is what the canonical form collapses to
when there is nothing to record. An IR with source locations on every node cannot use it.

## Backwards-compatible decoding

A v4 decoder accepts three generations of encoding:

| Format | Example | Source |
| ------ | ------- | ------ |
| Wrapper object | `{ "Variable": { "name": "a" } }` | v4 canonical |
| Tagged array, capitalized | `["Variable", {}, ["a"]]` | v2 / v3 |
| Tagged array, lowercase | `["variable", {}, ["a"]]` | v1 |

The strategy is to try the v4 wrapper object first and fall back to tagged-array decoding. This is more than the spec
draft's "legacy decoding" note claims — it covers the *node* encoding, not just identifiers. See
[Migration from v3](/migration-from-v3.md).
