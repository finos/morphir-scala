---
type: Implementation
title: Morphir SDK
description: The base set of types and functions every Morphir backend is expected to support.
tags: [morphir-elm, sdk, modeling, backends]
status: stable
sources:
  - id: readme
    resource: https://github.com/finos/morphir-elm/blob/1956c36d3715851a2f215775a45395690746d801/README.md
    title: morphir-elm README — Morphir SDK
  - id: sdk-tree
    resource: https://github.com/finos/morphir-elm/tree/1956c36d3715851a2f215775a45395690746d801/src/Morphir/SDK
    title: src/Morphir/SDK
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Morphir SDK

`Morphir.SDK` provides the basic building blocks for modeling a domain and its business logic. It has a second, less
obvious job that is arguably the more important one:

> It also serves as a specification for backend developers that describes the minimum set of functionality each
> backend implementation should support.

So the SDK is the contract between modelers and [backends](/backends.md). A backend that cannot map an SDK function
cannot claim to support Morphir.

## Relationship to elm/core

The SDK is based on `elm/core` 1.0.5 and provides most of it, **excluding** the modules that fall outside business
knowledge modeling: `Debug`, `Platform`, `Process`, and `Task`. The exclusions follow directly from the IR's purity
and side-effect-free semantics.

Everything else in `elm/core` can be used **without importing the SDK** — the [Elm frontend](/elm-frontend.md) maps
those types and functions to their Morphir SDK equivalents automatically. An explicit import is needed only for SDK
features that go beyond `elm/core`.

## Modules

**elm/core counterparts**: `Basics`-level numerics via `Int`, `Float`, `Bool`, `Char`-adjacent `String`, plus
`List`, `Dict`, `Maybe`, `Result` (as `ResultList`), `Function`, `Tuple`-adjacent support.

**Beyond elm/core**:

| Module | Provides |
| ------ | -------- |
| `Decimal` | Arbitrary-precision decimals, matching the IR's `DecimalLiteral` |
| `Number` | Numeric abstraction |
| `LocalDate`, `LocalTime`, `Instant` | Date and time types |
| `UUID` | Universally unique identifiers |
| `Aggregate` | Aggregation over collections |
| `Rule`, `Validate`, `Constraints` | Business rule and validation modeling |
| `Key` | Key extraction, used by relational backends |
| `Comparable`, `Equality` | Ordering and equality abstractions |
| `StatefulApp` | Stateful application modeling |
| `Json.Decode`, `Json.Encode` | JSON support for modeled logic |

## Described as IR

The SDK is not a compiler special case. `Morphir.IR.SDK` expresses the SDK's own package specification *as Morphir
IR*, which is why an SDK reference in a distribution looks like any other FQName —
`morphir/(sdk):list#map` is resolved through the same machinery as a reference into a user's own package.

Several SDK types are `DerivedTypeSpecification`s: `LocalDate` and `Decimal` both have a base type of `String` with
`fromBaseType` / `toBaseType` conversion functions. See the `morphir-ir-v3` bundle's type specifications concept.
