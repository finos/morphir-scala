---
type: Specification Section
title: Morphir IR Overview
description: What the Morphir IR is, what it is for, and the design principles behind it.
tags: [morphir, ir, v3, overview]
status: stable
sources:
  - id: ir-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/ir/morphir-ir-specification.md
    title: Morphir IR Specification
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Morphir IR Overview

The Morphir **Intermediate Representation (IR)** is a language-independent, platform-agnostic representation of
business logic and domain models. It captures the semantics of functional programs as data, so that the same logic can
be transformed, analyzed, and executed across different platforms and languages.

## What the specification defines

- **Building blocks** — the core concepts and data structures that make up the IR.
- **Relationships** — how the components reference each other.
- **Semantics** — what each construct means and how it behaves. See [IR Semantics](/semantics.md).

It exists to guide implementers building tools that generate, consume, or transform IR; to give LLMs context for
working with Morphir; and to serve as the authoritative reference for the IR structure.

## Design principles

| Principle | What it means |
| --------- | ------------- |
| **Functional** | All logic is expressed as pure functions without side effects. |
| **Type-safe** | Complete type information is preserved throughout the IR. |
| **Hierarchical** | Code is organized as Package → Module → Type/Value. |
| **Naming-agnostic** | Names are stored canonically, independent of any naming convention. See [Naming](/naming.md). |
| **Explicit** | All references are fully-qualified, eliminating ambiguity. |

## The hierarchy

```
Distribution
  └─ Package (with dependencies)
      └─ Module
          ├─ Types
          │   └─ Type Definition/Specification
          └─ Values
              └─ Value Definition/Specification
```

Each level is documented separately: [Distribution](/distribution.md), [Packages](/packages.md),
[Modules](/modules.md), the type system ([Type Expressions](/type-expressions.md),
[Type Specifications and Definitions](/type-specifications-and-definitions.md)), and the value system
([Value Expressions](/value-expressions.md), [Value Specifications and Definitions](/value-specifications-and-definitions.md)).

Cutting across every level is the split between public interface and full implementation — see
[Specifications vs Definitions](/specification-vs-definition.md).

## What the IR enables

Portability across target platforms, analysis of logic for correctness and properties, transformation and
optimization, rich tooling built on a standard format, and interoperability between languages that share logic via the
IR.

## Version scope

This bundle documents **format version 3**, the current version. Format versions 1 and 2 differ in JSON tag
capitalization and module structure only — see [JSON Encoding and Format Versions](/json-encoding.md). Version 4 is in
draft and is documented in the sibling `morphir-ir-v4-draft` bundle.
