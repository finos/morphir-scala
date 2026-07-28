---
type: Concept
title: Incompleteness
description: Holes, drafts, and hole reasons — how v4 represents broken or unfinished code without failing the build.
tags: [morphir, ir, v4, draft, incompleteness, holes, compilation]
status: draft
stale_after: 2026-12-31
sources:
  - id: types
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/types.md
    title: Type System (IR v4 draft) — Incompleteness, HoleReason
  - id: values
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/values.md
    title: Value System (IR v4 draft) — Hole
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Incompleteness

v4 can represent code that does not work yet. Rather than failing a build when a reference is dangling or a definition
is half-written, a compiler can emit an incomplete node and carry on — **best-effort compilation**.

This shows up in four places:

- `IncompleteTypeDefinition` — see [Type Specifications and Definitions](/type-specifications-and-definitions.md)
- `IncompleteBody` — see [Value Specifications and Definitions](/value-specifications-and-definitions.md)
- The `Hole` value expression — see [Value Expressions](/value-expressions.md)
- The `Incompleteness` and `HoleReason` structures described here

## Incompleteness

Describes *why* a type or value definition is incomplete. Two forms:

### Hole

`Hole reason` — a reference to something deleted, renamed, or otherwise broken. Carries a `HoleReason`.

### Draft

`Draft notes` — author-marked work in progress, with optional notes.

The distinction matters: a `Hole` is damage (something the toolchain noticed), a `Draft` is intent (something the
author declared).

## HoleReason

Specific reasons a `Hole` exists:

### UnresolvedReference

`UnresolvedReference target` — a reference to a type or value that cannot be resolved. `target` is the `FQName` that
failed to resolve.

### DeletedDuringRefactor

`DeletedDuringRefactor txId` — a reference deleted by a refactoring operation, identified by the transaction ID.

This one implies a transactional editing model over the IR, which is also why v4's addressing includes a
`morphir://session/...` scheme. See [URI and Locator](/uri-and-locator.md).

### TypeMismatch

`TypeMismatch expected found` — a type that does not match expectations, with string descriptions of both sides.

## What consumers see

Incompleteness does not leak across module boundaries. An `IncompleteTypeDefinition` derives an
`OpaqueTypeSpecification`, and an `IncompleteBody` derives the same ordinary `ValueSpecification` as any other body.
A dependent module sees a well-formed interface either way; only the owning module's definition records that
something is unfinished.
