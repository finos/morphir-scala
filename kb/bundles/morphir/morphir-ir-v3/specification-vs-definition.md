---
type: Concept
title: Specifications vs Definitions
description: The pattern that separates public interface from full implementation at every level of the IR.
tags: [morphir, ir, v3, access-control, modularity]
status: stable
sources:
  - id: ir-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/ir/morphir-ir-specification.md
    title: Morphir IR Specification — Relationships Between Concepts
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Specifications vs Definitions

The single most load-bearing pattern in the Morphir IR: every structural level exists in two forms, one exposing only
the public interface and one carrying the complete implementation.

| | Specification | Definition |
| --- | --- | --- |
| **Purpose** | Public interface / contract | Full implementation |
| **Contains** | Signatures, public structure | Implementation details, bodies |
| **Visibility** | Always public | Can be public or private |
| **Used by** | Consumers and dependents | The owning package only |

The pattern applies at four levels:

- **Types** — `TypeSpecification` vs `TypeDefinition` ([details](/type-specifications-and-definitions.md))
- **Values** — `ValueSpecification` vs `ValueDefinition` ([details](/value-specifications-and-definitions.md))
- **Modules** — `ModuleSpecification` vs `ModuleDefinition` ([details](/modules.md))
- **Packages** — `PackageSpecification` vs `PackageDefinition` ([details](/packages.md))

## Why it matters

A [Distribution](/distribution.md) carries the definition of the package being compiled but only the *specifications*
of its dependencies. That asymmetry is what makes separate compilation possible: a consumer needs a dependency's type
signatures, never its bodies. It also gives information hiding real teeth — a private type simply does not appear in
the specification a dependent sees.

## Deriving one from the other

A specification can always be derived from a definition by extracting only the public interface:

```
Definition → Specification
  Package Definition → Package Specification
  Module Definition  → Module Specification
  Type Definition    → Type Specification
  Value Definition   → Value Specification
```

Two derivation modes exist:

- **`definitionToSpecification`** — public items only.
- **`definitionToSpecificationWithPrivate`** — all items included.

The second is useful for tooling that needs the full picture (documentation generators, analyzers) without wanting the
implementation bodies.

## Interaction with access control

Derivation is governed by the [`AccessControlled` wrapper](/attributes-and-wrappers.md). One case is worth calling out
because it changes the *kind* of the resulting specification rather than merely omitting it: a
`CustomTypeDefinition` whose constructors are `Private` derives an `OpaqueTypeSpecification`, not a
`CustomTypeSpecification`. The type remains visible; its shape does not.

## v4 divergence

v4 preserves this pattern and extends it: an `IncompleteTypeDefinition` also exposes as an
`OpaqueTypeSpecification`, hiding in-progress brokenness from consumers. See the v4 draft bundle.
