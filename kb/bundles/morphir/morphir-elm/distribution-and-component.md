---
type: Implementation
title: Distribution and Component
description: The single Library constructor that matches the spec, and the Component record that has no spec counterpart.
tags: [morphir-elm, ir, distribution, v3]
status: stable
sources:
  - id: distribution
    resource: https://github.com/finos/morphir-elm/blob/1956c36d3715851a2f215775a45395690746d801/src/Morphir/IR/Distribution.elm
    title: Morphir.IR.Distribution
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Distribution and Component

## Distribution

```elm
type Distribution
    = Library PackageName (Dict PackageName (Package.Specification ())) (Package.Definition () (Type ()))
```

Exactly one constructor, matching the v3 specification. The three arguments are the package name, the dependency
specifications, and this package's definition.

Note the attribute types: `Package.Specification ()` and `Package.Definition () (Type ())`. The unit type appears
wherever the generic attribute parameter is unused, and `Type ()` is the value attribute — a fully type-annotated
distribution. This is the concrete shape of what the v4 draft replaces with explicit attribute structures.

`morphir-elm make` produces a JSON representation of this type.

## What the module offers

Beyond the type itself, the module is largely a **resolution API** over a distribution:

- **Lookups** — `lookupModuleSpecification`, `lookupTypeSpecification`, `lookupValueSpecification`,
  `lookupValueDefinition`, `lookupPackageSpecification`, `lookupPackageName`, `lookupTypeConstructor`,
  `lookupBaseTypeName`, `typeSpecifications`
- **Resolution** — `resolveAliases`, `resolveType`, `resolveRecordConstructors`
- **Updates** — `insertDependency`

This is the practical answer to "how does a consumer resolve an FQName": through the distribution, exactly as the
specification's guidance for tool implementers describes.

`lookupBaseTypeName` and `resolveAliases` are the pair to reach for when handling derived and alias types — they
follow the indirection the specification describes for `DerivedTypeSpecification` and `TypeAliasSpecification`.

## Component

```elm
type alias Component =
    { name : Path
    , libraries : Dict PackageName (Package.Definition () (Type ()))
    , inputs : Dict Name (Type ())
    , states : Dict Name (Type ())
    , outputs : Dict Name (Value () (Type ()))
    }
```

A **Component** is a complete, encapsulated, tree-shaken unit. The source describes it as a superset of a Library,
similar to an application: it carries all library dependencies *with their implementations*, plus named inputs,
states, and outputs.

Two things to be clear about:

1. **It is not a `Distribution` constructor.** It is a separate record type in the same module. The v3 specification
   says the only distribution type is `Library`, and this implementation agrees.
2. **It is the closest thing here to v4's Application distribution.** Both describe a self-contained, statically
   linked unit with named entry points. Whether the v4 `Application` is intended to subsume `Component` is not
   stated in either the specification or this source — do not assume it.
