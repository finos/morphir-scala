---
type: Playbook
title: Migration from v3
description: What is known today about moving format version 3 IR to version 4, and what is not yet written.
tags: [morphir, ir, v4, draft, migration, versioning]
status: draft
stale_after: 2026-12-31
sources:
  - id: whats-new
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/whats-new.md
    title: What's New in Version 4 — Migration Guide
  - id: names
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/names.md
    title: Naming (IR v4 draft) — Legacy Decoding
  - id: types
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/types.md
    title: Type System (IR v4 draft) — JSON Serialization Summary
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Migration from v3

> **The upstream migration guide does not exist yet.** `whats-new.md` records it as *"Coming Soon"*. Everything below
> is assembled from migration-relevant statements scattered across the draft, not from a migration document. Treat it
> as an inventory of what will have to be dealt with, not as instructions.

## What is stated in the draft

### Legacy decoding is supported

v4 decoders still accept the v3 array forms:

- **Names** — `["value", "in", "u", "s", "d"]` alongside `"value-in-(usd)"`.
- **Paths** — `[["morphir"], ["s", "d", "k"]]` alongside `"morphir/(sdk)"`.
- **FQNames** — `[packagePath, modulePath, localName]` alongside `"morphir/(sdk):list#map"`.
- **Type references** — the legacy package/module/local array form.

So a v4 reader can consume v3-shaped identifiers. That is one-directional: it does not make v3 readers able to consume
v4 output.

### There is a migration command

`morphir ir migrate --expanded` is named in the type system draft as producing expanded-format output. The draft
describes the flag's effect on serialization; it does not document the command's full behavior.

## What a migration has to account for

Working from the [change list](/whats-new-in-v4.md), these are the transformations a v3 → v4 migration must perform or
decide about:

| Change | Migration consequence |
| ------ | --------------------- |
| Generic attribute parameter removed | Every `Type a` / `Value ta va` node needs a `TypeAttributes` / `ValueAttributes` structure. v3 IR carrying `()` has no source locations or inferred types to supply — these become empty. See [Attributes](/attributes.md). |
| Canonical string naming | Identifiers can be re-encoded mechanically; the acronym-parenthesization rule must be applied when converting from word arrays. See [Naming](/naming.md). |
| Record types become dictionaries | v3's ordered field list becomes a field map. Field order was already not semantically significant, so this is lossless. |
| `WholeNumberLiteral` → `IntegerLiteral` | A literal tag rename. |
| Distribution kinds | A v3 `Library` maps to a v4 `Library`. `Specs` and `Application` have no v3 source. |
| Document Tree mode | Optional. A migrated distribution may stay in Classic mode. |
| `TypeVariable` wrapper | v3 type variables are bare `Name`s and need wrapping. |

## What is genuinely open

- Whether migration is lossless in both directions, and what a v4 → v3 downgrade would drop.
- How `formatVersion` is negotiated by tools that must read both.
- Whether existing v3 producers (notably the Elm implementation, which pins `currentFormatVersion = 3`) will emit v4,
  or whether a separate converter is intended.

Revisit this concept when the upstream migration guide lands.
