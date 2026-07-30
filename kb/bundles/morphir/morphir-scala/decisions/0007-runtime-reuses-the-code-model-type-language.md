---
type: Decision Record
title: The runtime reuses the code model's type language and targets v4
description: "Runtime types are codemodel.Type rather than a second type language, and the new runtime targets the v4 code model with a one-way lowering from v3."
state: Accepted
decided: 2026-07-29
tags: [runtime, ir, codemodel, types, lowering]
status: stable
---

# 0007 — The runtime reuses the code model's type language and targets v4

Runtime types are `org.finos.morphir.codemodel.Type`. The runtime does not define a type language of its own.

The runtime targets the v4 code model rather than the v3 IR, and a one-way `V3Lowering` translates real
`morphir-ir.json` into it so the new runtime works against actual compiler output from day one.

## Why

**On reusing the type language.** Morphir already has a nominal type language — that is most of what the IR is.
Inventing a second one is the mistake `Concept` made in the classic runtime: it produced two vocabularies for the
same domain, a conversion between them, and a class of bug where the two disagree about what a value is. Since
[0006](/decisions/0006-runtime-values-are-val-on-kyo-schema.md) makes the value representation purely structural,
type identity has to come from somewhere, and the IR's own nominal types are the answer that already exists.

**On targeting v4.** v4's enums have no type parameters, so kyo-schema derives instances for them. v3 is
`Type[+A]` and `Value[+TA, +VA]` over Elm's tagged-array wire format, and the 4567 lines of hand-written codecs in
this repository are the direct consequence — that shape is not derivable. Building the new runtime against v3 would
mean either inheriting those codecs or writing more of them.

**On the lowering being one-way and its own module.** A bidirectional bridge between IR versions is the adapter that
[0005](/decisions/0005-bridge-nothing-between-zio-and-kyo.md) rules out — it would let both versions live
indefinitely. A lowering only goes v3 → v4, so it terminates. It lives in `morphir/model/lowering` rather than in
`morphir/model` precisely because it needs v3 types from core, and `morphir/model` must not: see
[0008](/decisions/0008-model-and-naming-are-dependency-constrained-modules.md).

## Consequences

The runtime cannot represent a type the code model cannot represent. That is the intended constraint, but it means
any runtime-only type concept — a partially-evaluated type, a type variable under unification — has to be expressed
within the code model's vocabulary or the code model has to grow.

The lowering is currently exercised against exactly one real fixture, a `Library`-only distribution. v3's `Specs` and
`Application` kinds, and `Bundle`, are covered only by hand-constructed unit cases. The round-trip spec over real IR
asserts `decode(encode(lowered)) == lowered`, which cannot detect lowering information loss — both sides derive from
the same value — so correspondence is verified by unit cases, not by that spec.

`V3Lowering` recurses without a stack-safety mechanism. The 25-module fixture passes comfortably; deeply nested
`Apply` chains from generated IR could overflow. Acceptable while the lowering is a one-off migration path, relevant
if it becomes part of a production pipeline.
