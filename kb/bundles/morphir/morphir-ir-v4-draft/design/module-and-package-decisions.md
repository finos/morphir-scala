---
type: Design Note
title: Module and Package Decisions
description: Dictionary storage, wrapper flattening, and the algorithm that derives a specification from a definition.
tags: [morphir, ir, v4, draft, modules, packages, rationale]
status: draft
stale_after: 2026-12-31
sources:
  - id: design-modules
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/design/draft/ir/modules.md
    title: Module Structure (design)
  - id: design-packages
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/design/draft/ir/packages.md
    title: Package Structure (design)
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Module and Package Decisions

Design detail behind [Modules](/modules.md) and [Packages](/packages.md).

## Module decisions

| Decision | Choice | Rationale |
| -------- | ------ | --------- |
| Storage structure | `Dict(Name, ...)` | O(1) lookup by name, canonical key ordering |
| Documentation | Opaque `Documentation` type | Multi-line support, cross-platform line endings |
| Doc wrapper | Generic `Documented(a)` | Reusable across specifications and definitions |
| Access control | On definitions only | Specifications are public by definition |

## Package decisions

| Decision | Choice | Rationale |
| -------- | ------ | --------- |
| Module access | `AccessControlled` on definitions | Modules can be package-private |
| No module docs | Docs at type and value level | Module-level docs live in separate metadata |
| Path as key | `Dict(ModulePath, ...)` | Hierarchical organization preserved |

"No module docs" is worth pausing on: `PackageDefinition` holds `Dict(ModulePath, AccessControlled(ModuleDefinition))`
with no documentation slot, and the design's `ModuleDefinition` carries only `types` and `values`. Module-level
documentation is pushed to [Layered Decorations](/design/decorations.md), whose documentation decoration supports a
`moduleDoc` marker. The spec draft, by contrast, still shows a `doc` field on the module object. See
[Specification and Design Divergences](/design/divergences.md).

## Annotations on module specifications

The design's `ModuleSpecification` carries an `annotations: List(Annotation)` field that the spec draft does not
mention. See [Annotations](/design/annotations.md).

## JSON wrapper flattening

| Type | JSON |
| ---- | ---- |
| `Documentation` | String, or array of strings |
| `Documented(a)` | `{ "doc": "...", ...a }` — inlined, omitted when `None` |
| `AccessControlled(a)` | `{ "access": "Public", ...a }` — inlined |
| `AccessControlled(Documented(a))` | `{ "access": "Public", "doc": "...", ...a }` |

Both wrappers vanish into the object they wrap rather than nesting it. Note that this legacy `access`-as-field form
coexists with the design's newer canonical `{ "Public": {...} }` tag form described in
[Type Encoding Decisions](/design/type-encoding-decisions.md) — the design documents show both, and do not say which
supersedes the other.

## Documentation encoding

- **Encoding** — no newlines in content produces a string; content with newlines produces an array, preserving
  readability; empty or `None` omits the field entirely. Always emitted with Unix `\n` endings.
- **Decoding, permissively** — a string is split on `\n` with trailing `\r` trimmed per line; an array is normalized
  per line; a missing field becomes `None`.

## Deriving a specification

The design gives the derivation as executable pseudocode. Filter definitions to `Public`, then map each definition to
its specification:

| Definition | Specification |
| ---------- | ------------- |
| `CustomTypeDefinition(params, constructors)` | `CustomTypeSpecification(params, constructors.value)` |
| `TypeAliasDefinition(params, body)` | `TypeAliasSpecification(params, body)` |
| `IncompleteTypeDefinition(params, _, _)` | `OpaqueTypeSpecification(params)` |

For values, the derivation reads `inputs` and `output` off whichever `ValueDefinitionBody` is present and discards the
rest — which is the mechanism behind the claim in
[Value Specifications and Definitions](/value-specifications-and-definitions.md) that consumers cannot tell how a
value is implemented.

Documentation is carried through derivation unchanged: `Documented(doc, def)` becomes `Documented(doc, spec)`.
