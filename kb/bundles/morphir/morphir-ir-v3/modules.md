---
type: Specification Section
title: Modules
description: Module specifications, module definitions, and the two forms of module naming.
tags: [morphir, ir, v3, modules]
status: stable
sources:
  - id: ir-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/ir/morphir-ir-specification.md
    title: Morphir IR Specification — Module
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Modules

A **Module** groups related types and values, playing the role that packages play in Java or namespaces play
elsewhere. Modules live inside [packages](/packages.md).

## Module Specification

The public interface of a module.

- **types** — dictionary of type names to `Documented` [Type Specifications](/type-specifications-and-definitions.md).
- **values** — dictionary of value names to `Documented` [Value Specifications](/value-specifications-and-definitions.md).
- **doc** — optional module-level documentation string.

It includes only publicly exposed types and values, carries signatures without implementations, and preserves
documentation from source.

## Module Definition

The complete implementation of a module.

- **types** — dictionary of type names to `AccessControlled`, `Documented` Type Definitions.
- **values** — dictionary of value names to `AccessControlled`, `Documented` Value Definitions.
- **doc** — optional module-level documentation string.

Every type and value carries an [`AccessControlled`](/attributes-and-wrappers.md) wrapper recording its visibility;
that wrapper is what the specification derivation reads. Both public and private items are present, with complete
implementations.

## Module Name

- **Structure**: a [Path](/naming.md).
- **Examples**: `[["morphir"], ["i", "r"], ["type"]]`, `[["my"], ["module"]]`.

A module name is package-relative. To name a module globally you need the package too:

## Qualified Module Name

- **Structure**: a tuple of *(package path, module path)*.
- **Purpose**: unambiguous module references across packages.

## v4 divergence

v4 keeps the conceptual model but adds a physical one: in Document Tree mode a module is a `module.json` file, which
may either act as a manifest pointing at per-definition `*.type.json` / `*.value.json` files, or inline the
definitions directly. v4 also introduces a structured `Documentation` type supporting multi-line docs with
cross-platform line-ending normalization. See the v4 draft bundle's modules concept.
