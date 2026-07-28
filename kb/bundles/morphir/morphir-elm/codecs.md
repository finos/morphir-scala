---
type: Implementation
title: JSON Codecs
description: Per-concept encoders and decoders, and the parallel V1 codec set that still reads format version 1.
tags: [morphir-elm, ir, json, codecs, versioning]
status: stable
sources:
  - id: codec-tree
    resource: https://github.com/finos/morphir-elm/tree/1956c36d3715851a2f215775a45395690746d801/src/Morphir/IR
    title: src/Morphir/IR — Codec and CodecV1 modules
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# JSON Codecs

Serialization is not centralized. Each IR concept carries its own codec module alongside it, so `Morphir.IR.Type` has
`Morphir.IR.Type.Codec`, `Morphir.IR.Value` has `Morphir.IR.Value.Codec`, and so on.

## The two codec generations

Most concepts have **two** codec modules:

| Module | Reads and writes |
| ------ | ---------------- |
| `<Concept>.Codec` | The current format — version 3 |
| `<Concept>.CodecV1` | Format version 1 |

Concepts with both: `AccessControlled`, `Distribution`, `Documented`, `FQName`, `Literal`, `Module`, `Name`,
`Package`, `Path`, `QName`, `Type`, `Value`.

Concepts with only a current codec: `Decoration`, `FormatVersion`, `KindOfName`, `Repo`, `Source`.

## What the pairing tells you

The v1/v2/v3 differences are almost entirely constructor-tag capitalization, and the codec split is where that lives.
Keeping a complete parallel `CodecV1` set — rather than branching inside one codec — means version 1 files stay
readable without complicating the current path.

There is no `CodecV2` in the tree. Format version 2 was an intermediate step whose distinguishing feature was that
*some* tags were capitalized and some were not; only v1 and the current version have dedicated codecs here.

## Related modules

- `Morphir.Codec` and `Morphir.JsonExtra` at the top level provide shared codec helpers.
- `Morphir.IR.Type.DataCodec` generates codecs for *data described by* the IR, which is a different job from encoding
  the IR itself.
- `Morphir.SDK.Json.Decode` / `Morphir.SDK.Json.Encode` are SDK-level JSON support available to modeled business
  logic, not IR serialization. See [Morphir SDK](/morphir-sdk.md).

## Format version wrapper

The version number itself is carried by `Morphir.IR.FormatVersion` and its codec, wrapping the distribution rather
than living inside it. See [Format Version](/format-version.md).
