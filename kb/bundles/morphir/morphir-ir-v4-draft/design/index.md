# Design Rationale

The concepts in this directory come from `docs/design/draft/ir/` in `finos/morphir` — the design documents behind the
v4 specification. Where the [specification concepts](/index.md) at the bundle root say *what* the format is, these say
*why*, and they carry material the spec draft has not yet absorbed.

Design documents use Gleam syntax as their canonical reference notation and carry their own status markers (Draft,
Review, Approved, POC, Partial, Complete). They move faster and less carefully than the spec draft — treat a conflict
between the two as unresolved, not as the design winning.

## Orientation

* [v4 Architecture](/design/architecture.md) - The hub-and-spoke daemon model, design principles, and how the design documents track their own status.
* [Specification and Design Divergences](/design/divergences.md) - Where the design documents and the spec draft disagree, and which questions those disagreements leave open.

## Encoding decisions

* [Naming Decisions](/design/naming-decisions.md) - Why names are opaque newtypes stored as canonical strings, and why acronyms are parenthesized.
* [Type Encoding Decisions](/design/type-encoding-decisions.md) - Permissive input and canonical output, shorthand forms, and backwards-compatible decoding of v1 through v3.
* [Value Encoding Decisions](/design/value-encoding-decisions.md) - The IntegerLiteral rename, value shorthand, and what must stay explicitly wrapped.
* [Module and Package Decisions](/design/module-and-package-decisions.md) - Dictionary storage, wrapper flattening, and the specification-derivation algorithm.
* [Distribution Design](/design/distribution-design.md) - The full distribution records, entry point kinds, and semantic versioning.

## Features not yet in the spec draft

* [Annotations](/design/annotations.md) - Semantic labels on specifications, in the manner of Java or Scala annotations.
* [Layered Decorations](/design/decorations.md) - The deco/ tree, layer precedence, and deep-merged metadata.
* [Document Type](/design/document-type.md) - A schema-less JSON-like value for untyped data.
* [File Metadata ($meta)](/design/file-metadata.md) - Provenance and tooling metadata at the file level.
* [Node References ($ref)](/design/node-references.md) - File-local structural deduplication.
