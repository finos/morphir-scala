---
type: Design Note
title: Naming Decisions
description: Why v4 names are opaque newtypes stored as canonical strings, and why acronyms are parenthesized.
tags: [morphir, ir, v4, draft, naming, rationale]
status: draft
stale_after: 2026-12-31
sources:
  - id: design-naming
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/design/draft/ir/naming.md
    title: Naming Conventions (design)
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Naming Decisions

The rationale behind [Naming](/naming.md).

| Decision | Choice | Rationale |
| -------- | ------ | --------- |
| Type safety | Newtype wrappers (opaque) | Prevents mixing `Name` / `Path` / `PackagePath` at compile time |
| Internal storage | Canonical string | Optimized for serialization, keys, and URLs |
| Abbreviation format | Parentheses `(usd)` | URL-safe, readable, unambiguous |
| Input parsing | Permissive | Accept multiple formats, always output canonical |

## What each one buys

**Opaque newtypes.** v3's `Name`, `Path`, and package path are all lists of strings, so nothing stops a caller from
passing a module path where a package path belongs. Wrapping each in its own opaque type makes that a compile error.
The same reasoning produced `TypeVariable` as a wrapper over `Name`.

**Canonical string as the internal representation.** v3 stored names as word lists and serialized them that way. v4
inverts it: the canonical string *is* the storage, and word structure is derived. This is what makes names usable
directly as JSON object keys and as URL path segments — see [URI and Locator](/uri-and-locator.md).

**Parentheses for acronyms.** The constraint is that `["sdk"]` and `["s","d","k"]` must round-trip differently, since
they render as `Sdk` and `SDK`. Parentheses are URL-safe, so `morphir/(sdk)` survives being embedded in a
`morphir://` URI without escaping — which a separator like `.` or `~` would not have guaranteed as cleanly.

**Permissive input, canonical output.** This policy recurs throughout v4 — see
[Type Encoding Decisions](/design/type-encoding-decisions.md). Decoders accept the legacy array forms and any
reasonable variant; encoders emit exactly one form. It is what lets v4 readers consume v3-shaped identifiers without
the format itself having two blessed encodings.
