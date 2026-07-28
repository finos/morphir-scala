---
type: Specification Section
title: Packages
description: Package specifications, package definitions, and package names — the unit of versioning and distribution.
tags: [morphir, ir, v3, packages]
status: stable
sources:
  - id: ir-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/ir/morphir-ir-specification.md
    title: Morphir IR Specification — Package
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Packages

A **Package** is a collection of modules versioned and distributed together — the same notion of "package" used by
NPM, NuGet, Maven, or pip.

## Package Specification

The public interface of a package.

- **Structure**: `modules` — a dictionary mapping module names (Paths) to [Module Specifications](/modules.md).

It contains only publicly exposed modules; within those, only public types and only public values; and no
implementation details whatsoever. This is the form in which a package appears as someone else's dependency.

## Package Definition

The complete implementation of a package.

- **Structure**: `modules` — a dictionary mapping module names (Paths) to `AccessControlled` [Module
  Definitions](/modules.md).

It contains all modules (public and private), all types, and all values with their implementations. Each module is
wrapped in [`AccessControlled`](/attributes-and-wrappers.md) to record its visibility.

## Package Name

A **Package Name** is the globally unique identifier for a package.

- **Structure**: a [Path](/naming.md) — a list of Names.
- **Examples**: `[["morphir"], ["s", "d", "k"]]`, `[["my"], ["company"], ["models"]]`.

## Relationship to Distribution

A [Distribution](/distribution.md) holds one package definition (the package being compiled) and a dictionary of
package specifications (its dependencies). Deriving the former's specification is how a package becomes usable as
someone else's dependency — see [Specifications vs Definitions](/specification-vs-definition.md).

## v4 divergence

v4 adds an explicit **version** to package identity and maps package paths onto directory structures in Document Tree
mode (`pkg/{package-path}/` for local packages, `deps/{package-path}/{version}/` for dependencies). It also makes the
`modules` field optional, so an empty package serializes as `{}`. See the v4 draft bundle.
