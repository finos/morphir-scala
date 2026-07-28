---
type: Specification Section
title: Naming
description: Name, Path, QName, and FQName — the convention-independent identifier system used throughout the IR.
tags: [morphir, ir, v3, naming, identifiers]
status: stable
sources:
  - id: ir-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/ir/morphir-ir-specification.md
    title: Morphir IR Specification — Naming
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Naming

Morphir stores identifiers in a canonical form that is independent of any specific naming convention. The same IR can
therefore be rendered as camelCase, TitleCase, or snake_case depending on the target platform, without the IR itself
committing to any of them.

## Name

A **Name** is a human-readable identifier made up of one or more words.

- **Structure**: a list of lowercase word strings.
- **Purpose**: the atomic unit for all identifiers.

`["value", "in", "u", "s", "d"]` renders as `valueInUSD` (camelCase), `ValueInUSD` (TitleCase), or `value_in_USD`
(snake_case). Note that the acronym is stored as three single-letter words — that is what lets a renderer decide to
uppercase it.

## Path

A **Path** is a hierarchical location in the IR structure.

- **Structure**: a list of Names.
- **Purpose**: identifies packages and modules.

`[["morphir"], ["s", "d", "k"], ["string"]]` is the path to the String module.

## Qualified Name (QName)

A **QName** identifies a type or value within a package.

- **Structure**: a tuple of *(module path, local name)*.
- **Purpose**: identifies items relative to a package.

## Fully-Qualified Name (FQName)

An **FQName** is the globally unique identifier for any type or value.

- **Structure**: a tuple of *(package path, module path, local name)*.
- **Purpose**: unambiguous references across package boundaries.

FQNames are what make the IR's "explicit" design principle real: every reference inside a value or type expression
carries its full path, so no scope resolution is needed to interpret it. See
[Type Expressions](/type-expressions.md) and [Value Expressions](/value-expressions.md) for the nodes that carry them.

## Reference resolution

References resolve at three levels of granularity:

1. **Within expressions** — references use FQName.
2. **Within modules** — items use local Names, looked up in module context.
3. **Within packages** — modules use Paths, looked up in package context.

This eliminates ambiguity and enables dependency tracking, cross-package linking, and independent processing of
modules.

## v4 divergence

IR v4 keeps the same conceptual model but adds a canonical *string* serialization (`"morphir/(sdk):list#map"`) and a
distinct `TypeVariable` wrapper. See the v4 draft bundle's naming concept for the details; array decoding remains
supported there for backward compatibility.
