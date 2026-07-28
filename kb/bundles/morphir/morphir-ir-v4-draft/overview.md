---
type: Specification Section
title: Morphir IR v4 Overview
description: What Morphir IR v4 is trying to achieve, and how the draft specification is organized.
tags: [morphir, ir, v4, draft, overview]
status: draft
stale_after: 2026-12-31
sources:
  - id: draft-index
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/index.md
    title: Draft Specifications (IR v4) — Overview
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Morphir IR v4 Overview

Morphir IR **format version 4** is a draft revision aimed at a polyglot, tool-friendly ecosystem. Its changes cluster
around three goals: make the IR legible to ordinary shell tools, make attributes a real schema rather than a generic
hole, and let compilers emit useful output for code that is not finished.

See [What's New in v4](/whats-new-in-v4.md) for the change list against v3.

## Specifications vs Definitions

The pattern inherited unchanged from v3 remains the organizing idea of the format:

| Concept | Specification | Definition |
| ------- | ------------- | ---------- |
| **Purpose** | Public interface / contract | Full implementation |
| **Contains** | Signatures, public structure | Implementation details, bodies |
| **Visibility** | Always public | Can be public or private |
| **Used by** | Consumers / dependents | Owner module only |

It applies at four levels — `TypeSpecification`/`TypeDefinition`, `ValueSpecification`/`ValueDefinition`,
`ModuleSpecification`/`ModuleDefinition`, `PackageSpecification`/`PackageDefinition`.

**Key principle**: a specification can always be derived from a definition by extracting only the public interface.
v4 extends this — an incomplete definition exposes as an *opaque* specification, hiding internal brokenness from
consumers.

## Type specification and definition pairings

| Specification | Definition | Notes |
| ------------- | ---------- | ----- |
| `TypeAliasSpecification` | `TypeAliasDefinition` | Alias visible to consumers |
| `OpaqueTypeSpecification` | — | No structure exposed |
| `CustomTypeSpecification` | `CustomTypeDefinition` | Sum type with constructors |
| `DerivedTypeSpecification` | — | Opaque with conversion functions |
| — | `IncompleteTypeDefinition` | **v4**: exposes as `OpaqueTypeSpecification` |

See [Type Specifications and Definitions](/type-specifications-and-definitions.md).

## Value specification and body pairings

| Specification | Definition Body | Notes |
| ------------- | --------------- | ----- |
| `ValueSpecification` | `ExpressionBody` | Normal IR implementation |
| `ValueSpecification` | `NativeBody` | **v4**: platform builtin |
| `ValueSpecification` | `ExternalBody` | **v4**: FFI call |
| `ValueSpecification` | `IncompleteBody` | **v4**: work in progress |

A `ValueSpecification` carries only the signature — input types and output type. All four body kinds derive the same
specification, so a consumer cannot tell from the interface how a value is implemented. See
[Value Specifications and Definitions](/value-specifications-and-definitions.md).

## Rationale material not yet consumed

The upstream draft points at `docs/design/draft/README.md` and the `docs/design/draft/ir/` tree for the reasoning
behind these decisions. That material was not read during this seeding pass; a later pass should mine it for the
"why".
