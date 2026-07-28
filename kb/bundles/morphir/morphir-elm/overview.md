---
type: Implementation
title: morphir-elm Overview
description: What the morphir-elm project ships and how its pieces relate to the Morphir IR specification.
resource: https://github.com/finos/morphir-elm
tags: [morphir-elm, elm, implementation, v3]
status: stable
sources:
  - id: readme
    resource: https://github.com/finos/morphir-elm/blob/1956c36d3715851a2f215775a45395690746d801/README.md
    title: morphir-elm README
  - id: package-json
    resource: https://github.com/finos/morphir-elm/blob/1956c36d3715851a2f215775a45395690746d801/package.json
    title: morphir-elm package.json
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# morphir-elm Overview

`finos/morphir-elm` is the Elm implementation of Morphir. It is both a **toolchain** (an npm package with CLI
commands that compile Elm to IR and generate code from it) and a **library** (an Elm package providing the SDK and a
type-safe IR API).

## Two published artifacts

| Artifact | Registry | Version at seed time | Provides |
| -------- | -------- | -------------------- | -------- |
| `morphir-elm` | npm | 2.100.0 | The CLI commands — see [Command-Line Interface](/cli.md) |
| `finos/morphir-elm` | Elm packages | 22.0.2 | The [Morphir SDK](/morphir-sdk.md) and the [IR API](/ir-api.md) |

The version numbers move independently and neither is the IR format version — that is a separate constant, pinned at
3. See [Format Version](/format-version.md).

## Pipeline

```
Elm source ──frontend──▶ Morphir IR (morphir-ir.json) ──backend──▶ target code
```

- The [Elm frontend](/elm-frontend.md) parses and resolves Elm sources into IR.
- The IR is serialized through [JSON codecs](/codecs.md) as a `Distribution`.
- A [backend](/backends.md) reads the distribution and emits Scala, Spark, JSON Schema, TypeSpec, TypeScript, or
  another target.

Configuration for the whole pipeline comes from [`morphir.json`](/project-configuration.md).

## Relationship to the specification

This repository is the reference implementation of **IR format version 3**. Where the specification says what the IR
*is*, this repository shows what a producer and consumer of it actually do. That makes it the right place to verify
v3 claims — see [Verifying v3 Claims](/verifying-v3-claims.md).

Its correspondence to the specification is close but not mechanical. The clearest example is `Distribution`, which
matches the spec's single `Library` constructor but sits alongside a `Component` record with no specification
counterpart. See [Distribution and Component](/distribution-and-component.md).

## Source layout

```
src/Morphir/
├── IR/          # The IR data model and codecs
├── SDK/         # The Morphir SDK
├── Elm/         # The Elm frontend and an Elm backend
├── Scala/  Spark/  JsonSchema/  TypeSpec/  TypeScript/  Snowpark/  SpringBoot/  Relational/
├── Correctness/  TestCoverage/  Stats/   # Analysis over the IR
├── Visual/  Web/  Graph/                 # Visualization and browsing
└── Compiler/  Generator/  Type/  Value/  Dependency/  File/
cli/            # The morphir-elm command (make, gen, develop, test, treeview)
cli2/           # The newer morphir command, in TypeScript
```
