---
type: Implementation
title: Format Version
description: The manually managed IR format version constant in morphir-elm, pinned at 3.
resource: https://github.com/finos/morphir-elm/blob/1956c36d3715851a2f215775a45395690746d801/src/Morphir/IR/FormatVersion.elm
tags: [morphir-elm, ir, versioning, v3]
status: stable
sources:
  - id: format-version
    resource: https://github.com/finos/morphir-elm/blob/1956c36d3715851a2f215775a45395690746d801/src/Morphir/IR/FormatVersion.elm
    title: Morphir.IR.FormatVersion
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Format Version

`Morphir.IR.FormatVersion` wraps a distribution with the version of the IR format it is encoded in.

```elm
type alias VersionedDistribution =
    { formatVersion : Int
    , distribution : Distribution
    }
```

```elm
currentFormatVersion : Int
currentFormatVersion =
    3
```

Two things follow, and both matter when reasoning about compatibility.

## It is manually managed

The source comments it as "a manually managed version number to be able to handle breaking changes in the IR format
more explicitly". It is not derived from the package version, the npm version, or anything else — someone bumps it
deliberately when the encoding breaks.

## It is the concrete evidence that this is a v3 implementation

At commit `1956c36d`, `currentFormatVersion = 3`. Every `morphir-ir.json` this toolchain writes carries
`formatVersion: 3`, which is what makes this repository the reference for the v3 specification rather than for the
v4 draft.

## Relationship to the published schemas

The v1, v2, and v3 JSON schemas published at `morphir.finos.org/schemas/` correspond to the values this constant has
held over time. See the `morphir-ir-v3` bundle's JSON encoding concept for the tag-level differences between them,
and [JSON Codecs](/codecs.md) for how this repository still decodes version 1.
