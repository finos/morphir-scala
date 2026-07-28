---
okf_version: "0.2"
title: "Morphir IR v4 (Draft)"
description: "The draft specification for Morphir IR format version 4 — not yet active and subject to change."
---

# Morphir IR v4 (Draft)

Knowledge bundle for the **draft** specification of Morphir IR format version 4. Seeded from `docs/spec/draft/` in
[finos/morphir](https://github.com/finos/morphir) at commit `4d5e5c06`.

> **This is a draft.** Nothing here is settled, and shipping tooling still produces and consumes format version 3.
> Treat every concept in this bundle as a lead about where the format is going, not as a contract. For the active
> format see the sibling `morphir-ir-v3` bundle.

## Orientation

* [Morphir IR v4 Overview](/overview.md) - What v4 is trying to achieve and how it is organized.
* [What's New in v4](/whats-new-in-v4.md) - The four headline changes against format version 3.
* [Migration from v3](/migration-from-v3.md) - What is known about moving v3 IR to v4, and what is not yet written.

## Identity and addressing

* [Naming](/naming.md) - Canonical string serialization for names, paths, and fully-qualified names.
* [URI and Locator](/uri-and-locator.md) - Protocol-level addressing for the Document Tree virtual filesystem.

## Node metadata

* [Attributes](/attributes.md) - TypeAttributes and ValueAttributes, replacing v3's generic attribute parameter.

## Type system

* [Type Expressions](/type-expressions.md) - The type nodes and their compact and expanded JSON forms.
* [Type Specifications and Definitions](/type-specifications-and-definitions.md) - Alias, opaque, custom, derived, and the new incomplete type definition.

## Value system

* [Value Expressions](/value-expressions.md) - Literals, expression nodes, patterns, and the new Hole node.
* [Value Specifications and Definitions](/value-specifications-and-definitions.md) - The four ValueDefinitionBody variants and the single specification they all derive.
* [Native and External Values](/native-and-external-values.md) - First-class platform builtins and FFI calls.
* [Incompleteness](/incompleteness.md) - Holes, drafts, and best-effort compilation of broken code.

## Structure and distribution

* [Modules](/modules.md) - module.json, manifest versus inline style, and the Documentation type.
* [Packages](/packages.md) - Package identity with versions, and namespace-to-directory mapping.
* [Distribution](/distribution.md) - Library, Specs, and Application distributions across two physical modes.
* [Document Tree Layout](/document-tree-layout.md) - The .morphir-dist directory structure.

## Serialization

* [Schema Architecture](/schema-architecture.md) - Separate root schemas over shared $ref definitions.
