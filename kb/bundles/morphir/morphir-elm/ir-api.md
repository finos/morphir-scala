---
type: Implementation
title: IR Module Map
description: How the Elm modules under src/Morphir/IR map onto the concepts in the IR specification.
tags: [morphir-elm, ir, api, v3]
status: stable
sources:
  - id: source-tree
    resource: https://github.com/finos/morphir-elm/tree/1956c36d3715851a2f215775a45395690746d801/src/Morphir/IR
    title: src/Morphir/IR
  - id: readme
    resource: https://github.com/finos/morphir-elm/blob/1956c36d3715851a2f215775a45395690746d801/README.md
    title: morphir-elm README — Morphir IR
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# IR Module Map

`Morphir.IR` is a type-safe Elm API over the intermediate representation. Its module structure deliberately follows
the structure of the IR itself, so the mapping to the specification is close to one-to-one.

## Core structure

| Elm module | Specification concept |
| ---------- | --------------------- |
| `Morphir.IR.Distribution` | Distribution — the output of `morphir-elm make` |
| `Morphir.IR.Package` | Package — a set of modules versioned together |
| `Morphir.IR.Module` | Module — a container grouping types and values |
| `Morphir.IR.Type` | Type expressions, specifications, and definitions |
| `Morphir.IR.Value` | Value expressions, specifications, and definitions |
| `Morphir.IR.Literal` | The literal kinds |

## Naming

| Elm module | Specification concept |
| ---------- | --------------------- |
| `Morphir.IR.Name` | Name — convention-agnostic identifier |
| `Morphir.IR.Path` | Path — a list of names |
| `Morphir.IR.QName` | Qualified name — module path plus local name |
| `Morphir.IR.FQName` | Fully-qualified name — package path plus qualified name |

## Wrappers and metadata

| Elm module | Specification concept |
| ---------- | --------------------- |
| `Morphir.IR.AccessControlled` | Public/private visibility constraints |
| `Morphir.IR.Documented` | Documentation attached to an element |
| `Morphir.IR.Source` | Source location information |
| `Morphir.IR.Decoration` | Sidecar metadata — see [Decorations](/decorations.md) |

## Modules without a specification counterpart

| Elm module | What it is |
| ---------- | ---------- |
| `Morphir.IR.FormatVersion` | The versioned-distribution wrapper — see [Format Version](/format-version.md) |
| `Morphir.IR.Repo` | An in-memory repository of IR under construction, used by the incremental frontend |
| `Morphir.IR.KindOfName` | Distinguishes what kind of thing a name refers to |
| `Morphir.IR.NodeId` | Addressing for individual IR nodes |
| `Morphir.IR.SDK` | The SDK's own package specification, expressed as IR |
| `Morphir.IR.NameConstructorArguments` | Naming support for constructor arguments |

`Morphir.IR.SDK` is worth noticing: the SDK is not special-cased in the compiler, it is described *as IR* like any
other package. See [Morphir SDK](/morphir-sdk.md).

## Exposed surface

`morphir.json` in this repository exposes a deliberately narrow public API — `IR.Name`, `IR.Path`, `IR.QName`,
`IR.FQName`, `IR.AccessControlled`, `IR.Type`, `IR.Value`, `IR.Module`, `IR.Package`, `IR.Distribution`,
`IR.FormatVersion`, and `IR.Source`. That list is the intended integration surface for tools built on this library.

## Serialization

Each concept has a companion codec module — see [JSON Codecs](/codecs.md).
