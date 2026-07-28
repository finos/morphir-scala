---
type: Specification Section
title: Naming
description: Canonical string serialization for names, paths, qualified names, and fully-qualified names in v4.
tags: [morphir, ir, v4, draft, naming, identifiers]
status: draft
stale_after: 2026-12-31
sources:
  - id: names
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/names.md
    title: Naming (IR v4 draft)
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Naming

v4 keeps Morphir's convention-independent naming model and adds a **canonical string serialization**, so names,
paths, and fully-qualified names become readable strings usable as JSON object keys.

## Name

- **Canonical serialization**: a kebab-case string, e.g. `"user-account"`.
- **Abbreviations and acronyms**: sequences of single letters are enclosed in parentheses — `"value-in-(usd)"`.
- **Normalization**: when parsing from other formats, detected abbreviations and acronyms are wrapped in parentheses.
- **Legacy decoding**: the v3 array form (`["value", "in", "u", "s", "d"]`) is still accepted.

### Abbreviation handling

| Structured (words) | Canonical | camelCase | PascalCase |
| ------------------ | --------- | --------- | ---------- |
| `["value", "in", "u", "s", "d"]` | `value-in-(usd)` | `valueInUSD` | `ValueInUSD` |
| `["morphir", "s", "d", "k"]` | `morphir-(sdk)` | `morphirSDK` | `MorphirSDK` |
| `["get", "h", "t", "m", "l"]` | `get-(html)` | `getHTML` | `GetHTML` |

The parentheses are load-bearing, not decorative: `["sdk"]` (one word) renders as `Sdk` in PascalCase, while
`["s", "d", "k"]` (three single letters) renders as `SDK`. The canonical string has to preserve that distinction.

## TypeVariable

A **TypeVariable** wraps a `Name` to mark it as a type parameter, keeping type variable names and value names from
being mixed up in type-safe implementations. It serializes exactly like a Name (`"a"`, `"comparable"`).

This wrapper is new in v4; v3 used a bare `Name` in `Type.Variable`.

## Path

- **Canonical serialization**: Names joined by forward slashes — `"main/domain"`, `"morphir/(sdk)"`.
- **Legacy decoding**: array of name arrays, e.g. `[["morphir"], ["s", "d", "k"]]`.

## PackageName

A `Path` identifying a package: `"morphir/(sdk)"`, `"my-org/my-project"`.

## ModuleName

A `PackageName` plus a module `Path`, serialized as package path followed by module segments —
`"morphir/(sdk)/list"`, `"my-org/finance/pricing/models"`.

## Qualified Name (QName)

Module path and local name, joined by `#`:

```
main/orders#create-order
```

## Fully-Qualified Name (FQName)

Package path, module path, and local name, joined by `:` and `#`:

```
morphir/(sdk):list#map
```

The array form `[packagePath, modulePath, localName]` is still decodable.

## Why the separators matter

The `:` and `#` are what let v4's compact JSON tell a type `Variable` from an argument-less `Reference` — both
serialize as bare strings, and a string containing `:` and `#` is an FQName. See
[Type Expressions](/type-expressions.md).

## Related

Physical addressing in the Document Tree layers on top of these identities — see
[URI and Locator](/uri-and-locator.md).
