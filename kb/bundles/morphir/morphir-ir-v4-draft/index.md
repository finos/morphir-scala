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

* [Morphir IR v4 Overview](/overview.md) - What Morphir IR v4 is trying to achieve, and how the draft specification is organized.
* [What's New in v4](/whats-new-in-v4.md) - The four headline changes in Morphir IR v4 against format version 3.
* [Migration from v3](/migration-from-v3.md) - What is known today about moving format version 3 IR to version 4, and what is not yet written.

## Identity and addressing

* [Naming](/naming.md) - Canonical string serialization for names, paths, qualified names, and fully-qualified names in v4.
* [URI and Locator](/uri-and-locator.md) - Protocol-level addressing for the Document Tree virtual filesystem, and the hybrid Locator identifier.

## Node metadata

* [Attributes](/attributes.md) - TypeAttributes and ValueAttributes — the explicit structures replacing v3's generic attribute parameter.

## Type system

* [Type Expressions](/type-expressions.md) - The v4 type expression nodes and their compact and expanded JSON serializations.
* [Type Specifications and Definitions](/type-specifications-and-definitions.md) - Alias, opaque, custom, and derived type forms in v4, plus the new incomplete type definition.

## Value system

* [Value Expressions](/value-expressions.md) - v4 literals, value expression nodes and their JSON forms, patterns, and the new Hole node.
* [Value Specifications and Definitions](/value-specifications-and-definitions.md) - The four ValueDefinitionBody variants and the single ValueSpecification they all derive.
* [Native and External Values](/native-and-external-values.md) - First-class representation of platform builtins and foreign function calls in v4.
* [Incompleteness](/incompleteness.md) - Holes, drafts, and hole reasons — how v4 represents broken or unfinished code without failing the build.

## Structure and distribution

* [Modules](/modules.md) - module.json in manifest and inline styles, granular definition files, and the Documentation type.
* [Packages](/packages.md) - Package identity with versions in v4, and how IR paths map onto directories.
* [Distribution](/distribution.md) - The three v4 distribution kinds — Library, Specs, and Application — across two physical modes.
* [Document Tree Layout](/document-tree-layout.md) - The .morphir-dist directory structure that lets ordinary shell tools read Morphir IR.

## Serialization

* [Schema Architecture](/schema-architecture.md) - How v4 splits its JSON schemas into separate root schemas over shared $ref definitions.

## Design rationale

The [`design/`](/design/index.md) subdirectory covers `docs/design/draft/ir/` — the reasoning behind the format, plus
five features the spec draft has not absorbed.

* [v4 Architecture](/design/architecture.md) - The hub-and-spoke daemon model behind v4, its design principles, and how the design documents track status.
* [Specification and Design Divergences](/design/divergences.md) - Where the v4 design documents and the v4 spec draft disagree, and what those disagreements leave open.
* [Naming Decisions](/design/naming-decisions.md) - Why v4 names are opaque newtypes stored as canonical strings, and why acronyms are parenthesized.
* [Type Encoding Decisions](/design/type-encoding-decisions.md) - Permissive input and canonical output, type shorthand forms, and backwards-compatible decoding of v1 through v3.
* [Value Encoding Decisions](/design/value-encoding-decisions.md) - The IntegerLiteral rename, value shorthand, and which value forms must stay explicitly wrapped.
* [Module and Package Decisions](/design/module-and-package-decisions.md) - Dictionary storage, wrapper flattening, and the algorithm that derives a specification from a definition.
* [Distribution Design](/design/distribution-design.md) - The full distribution records, entry point kinds, semantic versioning, and VFS manifests.
* [Annotations](/design/annotations.md) - Semantic labels attached to IR specifications, in the manner of Java or Scala annotations.
* [Layered Decorations](/design/decorations.md) - The deco/ tree, layer precedence, deep-merge semantics, and schema-validated decoration values.
* [Document Type](/design/document-type.md) - A schema-less JSON-like value for untyped or dynamically-typed data inside a statically typed IR.
* [File Metadata ($meta)](/design/file-metadata.md) - Provenance, tooling, and extension metadata carried at the top level of VFS files.
* [Node References ($ref)](/design/node-references.md) - File-local structural deduplication using JSON Schema style $defs and $ref.
