---
type: Playbook
title: Verifying v3 Claims
description: How to use the morphir-elm repository to check specification claims against working code.
tags: [morphir-elm, verification, methodology, v3]
status: stable
sources:
  - id: source-tree
    resource: https://github.com/finos/morphir-elm/tree/1956c36d3715851a2f215775a45395690746d801
    title: finos/morphir-elm at 1956c36d
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Verifying v3 Claims

The `morphir-ir-v3` bundle records what the specification *says*. This repository shows what an implementation *does*.
When the two could disagree, this is where to look.

## Ground rules

- **`finos/morphir-elm` is authoritative for v3 behavior.** It implements format version 3 and pins
  `currentFormatVersion = 3`. See [Format Version](/format-version.md).
- **`finos/morphir`'s code is not.** Only its `docs/` tree is a source for spec knowledge; the code outside `docs/`
  in that repository is experimental and must not be used to verify anything.
- **v4 cannot be verified here at all.** No part of this implementation targets the v4 draft. A v4 claim checked
  against this code is checked against the wrong thing.

## Where to look for what

| Question | Look at |
| -------- | ------- |
| What shape is a distribution really? | `src/Morphir/IR/Distribution.elm` — see [Distribution and Component](/distribution-and-component.md) |
| How is a construct encoded in JSON? | `src/Morphir/IR/<Concept>/Codec.elm` — see [JSON Codecs](/codecs.md) |
| What does format version 1 look like? | The parallel `CodecV1.elm` modules |
| Which node forms actually exist? | The type definitions in `src/Morphir/IR/Type.elm` and `Value.elm` |
| What must a backend support? | `src/Morphir/SDK/` — see [Morphir SDK](/morphir-sdk.md) |
| How are names resolved from source? | `src/Morphir/Elm/IncrementalResolve.elm` — see [Elm Frontend](/elm-frontend.md) |
| How does a consumer resolve an FQName? | The lookup functions in `Distribution.elm` |
| What does real IR look like? | `morphir-ir.json` at the repository root, and the `tests-integration/` tree |

## Method

1. **Find the type first, then the codec.** The Elm type says what the construct is; the codec says how it appears in
   `morphir-ir.json`. A specification claim about structure and a claim about JSON encoding are checked in different
   files.
2. **Prefer the type definition over documentation comments.** Doc comments in this repository are generally good but
   they are not the implementation.
3. **Record divergences as divergences.** Where the implementation and the specification differ, the honest move is to
   note both in the relevant concept — not to quietly rewrite the spec bundle to match the code, or the reverse.
4. **Pin what you cite.** Cite a commit-pinned URL in the concept's `sources`, as every concept in this bundle does.
   `main` moves.

## Known correspondence notes

- `Distribution` has exactly one constructor, `Library`, matching the specification. The `Component` record alongside
  it has no specification counterpart.
- There is no `CodecV2`; only v1 and the current version have dedicated codec modules.
- The v4 draft's `Application` distribution resembles `Component`, but no source states that one supersedes the
  other. Do not assert that it does.
