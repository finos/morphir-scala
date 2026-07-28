---
type: Implementation
title: Type Inference
description: The constraint-based type inference engine that annotates IR values with their inferred types.
tags: [morphir-elm, type-inference, constraints, unification, v3]
status: stable
sources:
  - id: infer
    resource: https://github.com/finos/morphir-elm/blob/1956c36d3715851a2f215775a45395690746d801/src/Morphir/Type/Infer.elm
    title: Morphir.Type.Infer
  - id: constraint
    resource: https://github.com/finos/morphir-elm/blob/1956c36d3715851a2f215775a45395690746d801/src/Morphir/Type/Constraint.elm
    title: Morphir.Type.Constraint
  - id: class
    resource: https://github.com/finos/morphir-elm/blob/1956c36d3715851a2f215775a45395690746d801/src/Morphir/Type/Class.elm
    title: Morphir.Type.Class
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Type Inference

`Morphir.Type.*` implements constraint-based type inference over the IR. It is what turns a `Value () ()` into a
fully annotated value, and it explains what the specification's type attribute is *for*.

## What it produces

```elm
type alias TypedValue va =
    Value () ( va, Type () )
```

The value attribute becomes a pair of the original attribute and the inferred `Type ()`. That is exactly the shape a
[Distribution](/distribution-and-component.md) carries — `Package.Definition () (Type ())` — so inference is the step
that produces a distribution's annotations.

Entry points work top-down: `inferPackageDefinition`, then `inferModuleDefinition`, then per-value inference. Each
takes a `Distribution` for reference lookups, and errors are accumulated across all modules rather than failing on
the first.

## The machinery

| Module | Role |
| ------ | ---- |
| `Morphir.Type.MetaType` | The inference-time type representation — `MetaVar`, `MetaRef`, `MetaTuple`, records |
| `Morphir.Type.MetaTypeMapping` | Converts between concrete `Type` and `MetaType`, and looks up values and constructors |
| `Morphir.Type.Constraint` | A single constraint |
| `Morphir.Type.ConstraintSet` | Collections of constraints |
| `Morphir.Type.Solve` | Unification, producing a `SolutionMap` |
| `Morphir.Type.Class` | Type classes |
| `Morphir.Type.Count` | Fresh variable generation |
| `Morphir.Type.Cardinality` | Cardinality analysis |

This is textbook Hindley-Milner shape: map the IR into a metatype language with unification variables, generate
constraints, solve them, map back.

`MetaType` carries a `Set Variable` of the free variables in each node, computed at construction — an optimization
that makes occurs-checks and substitution cheap. `MetaRef` additionally carries an optional resolved target, which is
how type aliases are followed during unification.

## Constraints

```elm
type Constraint
    = Equality (Set Variable) MetaType MetaType
    | Class (Set Variable) MetaType Class
```

Only two forms: two types must be equal, or a type must belong to a class.

## Type classes

```elm
type Class
    = Number
    | Appendable
```

Exactly two, and they are Elm's. `Number` covers `Int` and `Float`; `Appendable` covers `String` and `List`. Morphir
does not have user-definable type classes — these exist because Elm's `+` and `++` are overloaded, and inference has
to represent that.

This is worth knowing before assuming the IR supports ad-hoc polymorphism: it does not. A `Type.Variable` is
constrained at inference time and erased afterwards.

## Errors

```elm
type TypeError
    = TypeErrors (List TypeError)
    | ClassConstraintViolation MetaType Class
    | RecursiveConstraint MetaType MetaType
    | LookupError LookupError
    | UnknownError String
    | UnifyError UnificationError
```

`TypeErrors` nesting a list is how per-module accumulation is expressed. `RecursiveConstraint` is the occurs check —
a type that would have to contain itself.

`ValueTypeError` pairs a `Name` with a `TypeError`, so a failure is attributed to the value it came from.

## Solutions

`Morphir.Type.Solve.SolutionMap` maps unification variables to metatypes, with the usual operations — `emptySolution`,
`singleSolution`, `fromList`, `get`, `diff`. `singleSolution` takes an `Aliases` argument and wraps the result, so
alias information survives substitution rather than being flattened away — which is what lets the final annotated
type report `UserId` rather than `String`.

## Relationship to the v4 draft

v4 makes `inferredType` an explicit optional field on `ValueAttributes`, rather than something a producer encodes by
choosing `Type ()` as its attribute type. The information is the same; v4 gives it a name in the schema. See the
`morphir-ir-v4-draft` bundle's attributes concept.
