# Design Rationale

The concepts in this directory come from `docs/design/draft/ir/` in `finos/morphir` — the design documents behind the
v4 specification. Where the [specification concepts](/index.md) at the bundle root say *what* the format is, these say
*why*, and they carry material the spec draft has not yet absorbed.

Design documents use Gleam syntax as their canonical reference notation and carry their own status markers (Draft,
Review, Approved, POC, Partial, Complete). They move faster and less carefully than the spec draft — treat a conflict
between the two as unresolved, not as the design winning.

## Orientation

* [v4 Architecture](/design/architecture.md) - The hub-and-spoke daemon model behind v4, its design principles, and how the design documents track status.
* [Specification and Design Divergences](/design/divergences.md) - Where the v4 design documents and the v4 spec draft disagree, and what those disagreements leave open.

## Encoding decisions

* [Naming Decisions](/design/naming-decisions.md) - Why v4 names are opaque newtypes stored as canonical strings, and why acronyms are parenthesized.
* [Type Encoding Decisions](/design/type-encoding-decisions.md) - Permissive input and canonical output, type shorthand forms, and backwards-compatible decoding of v1 through v3.
* [Value Encoding Decisions](/design/value-encoding-decisions.md) - The IntegerLiteral rename, value shorthand, and which value forms must stay explicitly wrapped.
* [Module and Package Decisions](/design/module-and-package-decisions.md) - Dictionary storage, wrapper flattening, and the algorithm that derives a specification from a definition.
* [Distribution Design](/design/distribution-design.md) - The full distribution records, entry point kinds, semantic versioning, and VFS manifests.

## Features not yet in the spec draft

* [Annotations](/design/annotations.md) - Semantic labels attached to IR specifications, in the manner of Java or Scala annotations.
* [Layered Decorations](/design/decorations.md) - The deco/ tree, layer precedence, deep-merge semantics, and schema-validated decoration values.
* [Document Type](/design/document-type.md) - A schema-less JSON-like value for untyped or dynamically-typed data inside a statically typed IR.
* [File Metadata ($meta)](/design/file-metadata.md) - Provenance, tooling, and extension metadata carried at the top level of VFS files.
* [Node References ($ref)](/design/node-references.md) - File-local structural deduplication using JSON Schema style $defs and $ref.
