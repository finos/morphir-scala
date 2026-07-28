---
type: Specification Section
title: Distribution
description: The self-contained output of Morphir compilation — a Library plus the specifications of its dependencies.
tags: [morphir, ir, v3, distribution, compilation]
status: stable
sources:
  - id: ir-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/ir/morphir-ir-specification.md
    title: Morphir IR Specification — Distribution
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Distribution

A **Distribution** is a complete, self-contained package of Morphir code together with all its dependency
information. It is the output of the Morphir compilation process — what `morphir-elm make` writes to
`morphir-ir.json`.

## The Library distribution

v3 defines exactly one distribution type: **Library**. A Library contains:

- **Package name** — the globally unique identifier for the package, analogous to an NPM package name or a Maven
  `GroupId:ArtifactId`. See [Packages](/packages.md).
- **Dependencies** — a dictionary mapping package names to their **specifications**. Dependencies carry type
  signatures only, never implementations.
- **Package definition** — the complete implementation of this package, including private modules, with both
  signatures and implementations.

That asymmetry — definition for me, specifications for my dependencies — is the [specification/definition
pattern](/specification-vs-definition.md) applied at the distribution boundary.

## What a distribution is for

- It is the unit produced by compilation.
- It is complete enough to be executed, analyzed, or transformed without consulting anything else.
- It is the entry point for reference resolution: given an FQName, a consumer looks it up through the distribution.

## v4 divergence

v4 replaces the single Library type with three — **Library**, **Specs**, and **Application** — and adds a second
physical distribution mode, the Document Tree (`.morphir-dist/`), alongside the classic single-blob JSON. See the v4
draft bundle's distribution and document-tree concepts.

## Implementation note

The Elm implementation defines `Distribution` with the single `Library` constructor, matching this specification, and
separately defines a `Component` record for tree-shaken, runnable bundles with inputs, states, and outputs. `Component`
is not a `Distribution` constructor. See the sibling `morphir-elm` bundle.
