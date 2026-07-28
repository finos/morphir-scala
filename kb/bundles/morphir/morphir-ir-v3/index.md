---
okf_version: "0.2"
title: "Morphir IR v3 (Current)"
description: "The Morphir Intermediate Representation at format version 3 — the current, active IR format."
---

# Morphir IR v3

Knowledge bundle for the Morphir Intermediate Representation at **format version 3**, the version currently produced
and consumed by shipping Morphir tooling. Seeded from the `docs/` tree of [finos/morphir](https://github.com/finos/morphir)
at commit `4d5e5c06`.

For the in-progress successor, see the sibling `morphir-ir-v4-draft` bundle. For a working implementation of this
version, see the sibling `morphir-elm` bundle.

## Orientation

* [Morphir IR Overview](/overview.md) - What the Morphir IR is, what it is for, and the design principles behind it.
* [Specifications vs Definitions](/specification-vs-definition.md) - The pattern that separates public interface from full implementation at every level of the IR.
* [IR Semantics](/semantics.md) - Evaluation, typing, and access-control semantics that give IR structures their meaning.

## Identity and structure

* [Naming](/naming.md) - Name, Path, QName, and FQName — the convention-independent identifier system.
* [Attributes and Wrappers](/attributes-and-wrappers.md) - Type and value attributes, AccessControlled, and Documented.
* [Distribution](/distribution.md) - The self-contained output of compilation: a Library and its dependency specifications.
* [Packages](/packages.md) - Package specifications, package definitions, and package names.
* [Modules](/modules.md) - Module specifications, module definitions, and module naming.

## Type system

* [Type Expressions](/type-expressions.md) - The seven type expression nodes: Variable, Reference, Tuple, Record, ExtensibleRecord, Function, Unit.
* [Type Specifications and Definitions](/type-specifications-and-definitions.md) - Alias, opaque, custom, and derived types, and how definitions collapse to specifications.

## Value system

* [Value Expressions](/value-expressions.md) - The expression nodes that encode all computation, from Literal through UpdateRecord.
* [Patterns](/patterns.md) - The eight pattern forms used in lambdas, destructuring, and pattern matching.
* [Value Specifications and Definitions](/value-specifications-and-definitions.md) - Function signatures and their implementations.

## Serialization and configuration

* [JSON Encoding and Format Versions](/json-encoding.md) - The versioned JSON schemas, the v1 → v2 → v3 tag changes, and how to validate an IR file.
* [morphir.json Project Configuration](/project-configuration.md) - The project configuration file consumed by morphir-elm tooling.

## Practice

* [Implementing Morphir Tools](/implementing-tools.md) - Playbook for generating, consuming, and transforming IR.
