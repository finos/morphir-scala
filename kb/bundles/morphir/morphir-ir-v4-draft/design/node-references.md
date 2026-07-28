---
type: Design Note
title: Node References ($ref)
description: File-local structural deduplication using JSON Schema style $defs and $ref.
tags: [morphir, ir, v4, draft, json, deduplication, vfs]
status: draft
stale_after: 2026-12-31
sources:
  - id: design-refs
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/design/draft/ir/refs.md
    title: Node References ($ref) (design)
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Node References (`$ref`)

**Not present in the v4 spec draft.** Structural deduplication *within* a VFS JSON file, using the `$defs` and `$ref`
patterns borrowed from JSON Schema.

## `$ref` is not FQName

This is the distinction to keep straight, and the design states it as a principle:

| Mechanism | Purpose | Scope | Example |
| --------- | ------- | ----- | ------- |
| **FQName** | Semantic type or value reference | Cross-package | `"morphir/sdk:string#string"` |
| **`$ref`** | Structural deduplication | File-local | `{ "$ref": "user" }` |

FQName references a *thing* in the IR graph. `$ref` avoids repeating the same *JSON structure* in one file. They
compose — a `$defs` entry typically contains an FQName reference.

## Design principles

- **File-local only** — no cross-file resolution.
- **JSON Schema style** — familiar `$defs` and `$ref`.
- **Shorthand support** — a simple name resolves to `$defs` without pointer syntax.
- **Orthogonal to FQName**.
- **Optional** — deduplication is an optimization; files may be written fully expanded.

## Resolution

| Reference | Resolves to |
| --------- | ----------- |
| `{ "$ref": "user" }` | `$defs.user` |
| `{ "$ref": "#/$defs/user" }` | `$defs.user` |
| `{ "$ref": "#/def/body" }` | The value at `def.body` |

A string beginning `#/` is a JSON Pointer; anything else is a shorthand `$defs` lookup.

## Example

```json
{
  "formatVersion": "4.0.0",
  "name": "audit-record",
  "$defs": {
    "user": { "Reference": { "fqname": "my-org/domain:types#user" } },
    "date-time": { "Reference": { "fqname": "my-org/sdk:date-time#date-time" } },
    "maybe-user": { "Reference": { "fqname": "morphir/sdk:maybe#maybe", "args": [{ "$ref": "user" }] } }
  },
  "def": {
    "TypeAliasDefinition": {
      "body": {
        "Record": {
          "fields": {
            "created-by": { "$ref": "user" },
            "updated-by": { "$ref": "user" },
            "deleted-by": { "$ref": "maybe-user" }
          }
        }
      }
    }
  }
}
```

References may nest — a `$defs` entry may reference another — and resolution order does not matter, since expansion is
lazy or recursive.

## Processing

- **Reading** — expand every `$ref` by replacing it with a copy of its target, recursively, detecting cycles.
- **Writing** — optionally extract repeated subtrees into `$defs` by structural equality. Entirely optional.
- **Validation** — unresolved ref and circular ref are **errors**; unused `$defs` entry and shadowing are warnings.

## When to use it

**Do** use `$ref` for types appearing three or more times in a file, complex nested structures such as
`Maybe (List User)`, and common SDK type combinations.

**Don't** use it for simple types appearing once or twice, for cross-file deduplication (that is FQName's job), or to
express semantic relationships.

Naming convention runs `"string"`, `"user"` for simple types; `"list-of-string"`, `"maybe-user"` for parameterized
ones; `"dict-string-int"` for complex ones.

## Reserved keys

`$ref`, `$defs`, and `$meta` are reserved and must not appear as IR content. See
[File Metadata](/design/file-metadata.md).

## Interactions

`$ref` composes with [type shorthand](/design/type-encoding-decisions.md) — a `$defs` entry may itself be a bare
FQName string or a shorthand array. It is independent of `$meta`. It does **not** interact with decorations, which
live in `deco/` and target nodes by FQName.

## Future considerations

Cross-file references (`{ "$ref": "./common.type.json#/$defs/user" }`), which would require file resolution,
dependency ordering, and cross-file cycle detection; reference metadata via `$comment`; and YAML-style inline anchors
— all deliberately excluded from the initial design to keep it simple.
