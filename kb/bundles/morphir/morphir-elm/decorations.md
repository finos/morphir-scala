---
type: Implementation
title: Decorations
description: Sidecar metadata attached to IR nodes without modifying the IR itself.
tags: [morphir-elm, decorations, metadata, node-id]
status: stable
sources:
  - id: decoration
    resource: https://github.com/finos/morphir-elm/blob/1956c36d3715851a2f215775a45395690746d801/src/Morphir/IR/Decoration.elm
    title: Morphir.IR.Decoration
  - id: morphir-json-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/morphir-json/morphir-json-specification.md
    title: Morphir JSON Project Configuration Specification — decorations
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Decorations

A **decoration** attaches metadata to IR nodes from outside the IR. The metadata lives in a **sidecar file**, not in
`morphir-ir.json`, so annotating a model does not require changing it or recompiling.

This is v3's answer to a problem v4 addresses differently, with the `extensions` dictionary on
`TypeAttributes` / `ValueAttributes`. Decorations stay outside; v4 extensions live on the node.

## Data model

```elm
type alias DecorationID = String

type alias DecorationData = SDKDict.Dict NodeID RawValue

type alias DecorationConfigAndData =
    { displayName : String
    , entryPoint : FQName
    , iR : Distribution
    , data : DecorationData
    }

type alias AllDecorationConfigAndData = Dict DecorationID DecorationConfigAndData
```

Three things are worth drawing out:

1. **Values are IR values.** `DecorationData` maps a `NodeID` to a `RawValue` — the decoration payload is a Morphir
   value expression, not free-form JSON.
2. **Decorations are typed by a Morphir model.** `entryPoint` is the `FQName` of the type describing the decoration,
   and `iR` is the distribution that type lives in. A decoration schema is itself a Morphir package.
3. **Targets are node IDs.** `NodeID` (from `Morphir.IR.NodeId`) is what addresses an individual node inside a
   distribution.

## Querying

`Morphir.IR.Decoration` provides lookups over the decoration set:

- `getDecoratedNodeIds` — every node carrying a given decoration
- `getNodeIdsDecoratedWithValue` — every node whose decoration equals a given value
- `filterDecorations` — the general predicate form both are built on

## Configuration

Decorations are declared in `morphir.json` under `decorations`, keyed by decoration id, each with `displayName`, `ir`
(path to the decoration schema IR file), and `entryPoint` (`Package:Module:Type`). See
[Project Configuration](/project-configuration.md).

The repository also ships `morphir-decoration-extension/`, a VS Code extension for editing decoration data.
