---
type: Implementation
title: Backends
description: The code generation targets shipped in morphir-elm, and the shared shape they follow.
tags: [morphir-elm, backends, codegen, scala, spark, json-schema]
status: stable
sources:
  - id: source-tree
    resource: https://github.com/finos/morphir-elm/tree/1956c36d3715851a2f215775a45395690746d801/src/Morphir
    title: src/Morphir
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Backends

A **backend** consumes a [Distribution](/distribution-and-component.md) and emits something else. This repository
ships several.

| Backend | Target |
| ------- | ------ |
| `Morphir.Scala` | Scala source |
| `Morphir.Spark` | Apache Spark, via a relational treatment of the IR |
| `Morphir.Snowpark` | Snowflake Snowpark |
| `Morphir.Relational` | A relational intermediate form, shared by SQL-shaped targets |
| `Morphir.JsonSchema` | JSON Schema documents |
| `Morphir.TypeSpec` | TypeSpec definitions |
| `Morphir.TypeScript` | TypeScript source |
| `Morphir.SpringBoot` | Spring Boot scaffolding |
| `Morphir.Elm.Backend` | Elm source — Elm is a target as well as a source language |

## The common shape

Most backends follow the same three-module structure:

```
Morphir/<Target>/
├── AST.elm            # An abstract syntax tree for the target language
├── Backend.elm        # IR → target AST
└── PrettyPrinter.elm  # Target AST → text
```

The separation matters: mapping is a semantic problem (how does a Morphir `PatternMatch` become a Scala `match`?) and
printing is a formatting one, and keeping them apart is what makes either testable on its own.

`Morphir.Scala` also carries `WellKnownTypes.elm` and a `Feature/` directory, and `Morphir.Scala.Spark` sits at the
intersection of the Scala and Spark backends.

## Non-codegen consumers

Not everything that reads a distribution generates code:

| Module | Does |
| ------ | ---- |
| `Morphir.Correctness` | Test definitions and correctness checking over the IR |
| `Morphir.TestCoverage` | Coverage analysis |
| `Morphir.Stats` | Statistics about a distribution |
| `Morphir.Visual`, `Morphir.Web` | Visualization and browsing, behind `morphir-elm develop` |
| `Morphir.Graph` | Graph-shaped views of the IR |
| `Morphir.Dependency` | Dependency analysis |

## What a backend must support

The [Morphir SDK](/morphir-sdk.md) defines the minimum surface a backend has to handle. A backend's real work is
mapping every SDK type and function, plus every [value expression node](/ir-api.md), onto its target — including the
awkward ones like `ExtensibleRecord` and curried `Apply` chains.
