---
type: Implementation
title: Elm Frontend
description: The components that turn Elm source into Morphir IR, including the incremental compilation path.
tags: [morphir-elm, frontend, compiler, elm]
status: stable
sources:
  - id: elm-tree
    resource: https://github.com/finos/morphir-elm/tree/1956c36d3715851a2f215775a45395690746d801/src/Morphir/Elm
    title: src/Morphir/Elm
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Elm Frontend

The frontend turns Elm source files into a Morphir [Distribution](/distribution-and-component.md). It is what
`morphir-elm make` runs — see [Command-Line Interface](/cli.md).

## Components

| Module | Role |
| ------ | ---- |
| `Morphir.Elm.Frontend` | The original, whole-project frontend |
| `Morphir.Elm.IncrementalFrontend` | The incremental frontend, compiling only what changed |
| `Morphir.Elm.IncrementalResolve` | Name resolution for the incremental path |
| `Morphir.Elm.ParsedModule` | A parsed Elm module awaiting conversion |
| `Morphir.Elm.ModuleName` | Elm module names, and their mapping to IR paths |
| `Morphir.Elm.WellKnownOperators` | Elm operators the frontend recognizes and maps to SDK references |

`src/Morphir/Elm/Backend/` also exists — Elm is a code generation *target* as well as a source language.

## Two frontends

The presence of both a whole-project and an incremental frontend is the notable structural fact here. Incremental
compilation is what `Morphir.IR.Repo` exists to support: a mutable in-progress repository of IR that the incremental
frontend adds to and resolves against, rather than rebuilding a distribution from scratch each time.

## Name resolution

Resolution is where the frontend does the specification's real work: Elm's scoped, import-aware names must become
[fully-qualified names](/ir-api.md). Every `Reference` in the emitted IR carries a package path, module path, and
local name, because the IR has no notion of imports or scope to resolve against later.

`WellKnownOperators` is part of this — Elm's `+`, `::`, `|>` and friends have no IR node of their own, so they resolve
to SDK function references like any other call.

## Relationship to the SDK

The frontend maps `elm/core` types and functions to their [Morphir SDK](/morphir-sdk.md) counterparts automatically,
so modelers need not import the SDK to use ordinary Elm. Only SDK features beyond `elm/core` require an explicit
import.
