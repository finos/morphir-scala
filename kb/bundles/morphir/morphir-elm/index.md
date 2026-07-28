---
okf_version: "0.2"
title: "morphir-elm"
description: "The Elm implementation of Morphir — the reference producer and consumer of IR format version 3."
---

# morphir-elm

Knowledge bundle for [finos/morphir-elm](https://github.com/finos/morphir-elm), the Elm implementation of Morphir.
It is the working implementation of **IR format version 3**, and therefore the place to verify claims about how v3
behaves in practice.

Seeded from commit `1956c36d` (2026-05-28): npm package `morphir-elm` 2.100.0, Elm package `finos/morphir-elm`
22.0.2.

For the specification this implements, see the sibling `morphir-ir-v3` bundle.

## Orientation

* [morphir-elm Overview](/overview.md) - What the project ships, and how its pieces relate.
* [Verifying v3 Claims](/verifying-v3-claims.md) - Playbook for using this repository to check specification claims against working code.

## The IR API

* [IR Module Map](/ir-api.md) - How `src/Morphir/IR/` maps onto the specification's concepts.
* [Format Version](/format-version.md) - The manually managed format version constant, pinned at 3.
* [Distribution and Component](/distribution-and-component.md) - The single Library constructor, and the separate Component record.
* [JSON Codecs](/codecs.md) - Per-concept encoders and decoders, and the V1 codec pair that preserves format version 1.
* [Decorations](/decorations.md) - Sidecar metadata attached to IR nodes.

## Compilation pipeline

* [Elm Frontend](/elm-frontend.md) - Turning Elm source into IR, including the incremental path.
* [Morphir SDK](/morphir-sdk.md) - The base types and functions every backend must support.
* [Backends](/backends.md) - The code generation targets shipped in this repository.

## Tooling

* [Command-Line Interface](/cli.md) - The morphir-elm and morphir commands, and the MCP server.
* [Project Configuration](/project-configuration.md) - How this implementation consumes morphir.json.
