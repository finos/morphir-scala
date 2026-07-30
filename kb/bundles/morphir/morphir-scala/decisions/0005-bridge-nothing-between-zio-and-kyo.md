---
type: Decision Record
title: Bridge nothing between ZIO and Kyo
description: "New code is written against Kyo, existing ZIO code is left untouched, and no ZIO-to-Kyo adapter is permitted at the boundary."
state: Accepted
decided: 2026-07-29
tags: [kyo, zio, architecture, strangler-fig, json]
status: stable
---

# 0005 — Bridge nothing between ZIO and Kyo

This project is migrating off ZIO onto Kyo by the strangler-fig pattern: new code is written against Kyo, existing
ZIO-based code is left running untouched, and the boundary moves only when a piece is replaced outright. **No
ZIO ⇄ Kyo adapter is permitted.** A one-way lowering between IR versions is a sanctioned seam; a bridge that lets
both sides live indefinitely is not.

Concretely, for the first substantial instance: JSON for `morphir.model` and for every module built from here on goes
through kyo-schema's `Json`, derived from `Schema` — not zio-json. `morphir/model` is ZIO-free by construction, while
core `morphir`, the v1–v3 codecs and the classic runtime keep their ZIO dependencies indefinitely.

## Why

A migration between two effect systems fails in a predictable way: an adapter is written to make the boundary
painless, the adapter makes it cheap to leave things where they are, and the boundary stops moving. What is left is
permanently two runtimes, two `Chunk` types and two conventions, plus the conversion layer — strictly worse than
either system alone. Refusing the adapter is what forces each decision to be *replace this* or *leave this alone*,
both of which terminate.

This is not hypothetical here. An earlier draft of the slice-1 plan had a task inserting `zio.Chunk` ⇄ `kyo.Chunk`
conversions into `MorphirJsonEncodingSupportV4`/`DecodingSupportV4` so those codecs would keep compiling after the
code model switched to `kyo.Chunk`. Under this rule that is exactly backwards: those 808 lines were replaced by
derived codecs, so the correct move was to delete them and their two JVM loaders — which is what happened.

The v1/v2/v3 codecs stay on zio-json permanently, and that is not an exception to the rule. They encode Elm's
tagged-array wire format, which is not derivable, and they serve the classic runtime. They are outside the fig, not
bridged across it.

## Consequences

An adapter appearing in review is a signal to re-examine where the boundary was drawn, not a thing to review on its
merits — either the new code should own the whole piece, or the old code should have been left alone.

The cost is duplication during the migration: two `Chunk` types, two JSON stacks and two test frameworks coexist, and
a module has to pick a side rather than straddling. The `morphir/model/lowering` module exists precisely because of
this — it is separate from `morphir/model` so that `morphir/model` can depend on Kyo and naming alone while still
being reachable from the v3 IR.

Revisit this if Kyo ever ships a first-class, maintained ZIO interop layer whose semantics are well enough defined
that a boundary crossing is not a source of surprise. Note that a `kyo-zio` artifact already exists; the objection
here is not that conversion is technically impossible but that its availability is what stalls migrations.
