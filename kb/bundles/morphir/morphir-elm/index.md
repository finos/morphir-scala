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

* [morphir-elm Overview](/overview.md) - What the morphir-elm project ships and how its pieces relate to the Morphir IR specification.
* [Verifying v3 Claims](/verifying-v3-claims.md) - How to use the morphir-elm repository to check specification claims against working code.

## The IR API

* [IR Module Map](/ir-api.md) - How the Elm modules under src/Morphir/IR map onto the concepts in the IR specification.
* [Format Version](/format-version.md) - The manually managed IR format version constant in morphir-elm, pinned at 3.
* [Distribution and Component](/distribution-and-component.md) - The single Library constructor that matches the spec, and the Component record that has no spec counterpart.
* [JSON Codecs](/codecs.md) - Per-concept encoders and decoders, and the parallel V1 codec set that still reads format version 1.
* [Decorations](/decorations.md) - Sidecar metadata attached to IR nodes without modifying the IR itself.

## Compilation pipeline

* [Elm Frontend](/elm-frontend.md) - The components that turn Elm source into Morphir IR, including the incremental compilation path.
* [Type Inference](/type-inference.md) - The constraint-based type inference engine that annotates IR values with their inferred types.
* [Morphir SDK](/morphir-sdk.md) - The base set of types and functions every Morphir backend is expected to support.
* [Backends](/backends.md) - The code generation targets shipped in morphir-elm, and the shared shape they follow.

## Executing and checking models

* [Value Interpreter](/value-interpreter.md) - How morphir-elm evaluates IR value expressions, and the native function escape hatch.
* [Testing and Coverage](/testing-and-coverage.md) - How test cases are expressed against a Morphir model and how branch coverage is measured.

## Tooling

* [Command-Line Interface](/cli.md) - The morphir-elm and morphir commands, their generation subcommands, and the MCP server.
* [Project Configuration](/project-configuration.md) - How morphir-elm consumes morphir.json, and the reference forms its dependency resolution accepts.
