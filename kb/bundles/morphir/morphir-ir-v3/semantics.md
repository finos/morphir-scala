---
type: Specification Section
title: IR Semantics
description: Evaluation, typing, and access-control semantics that give IR structures their meaning.
tags: [morphir, ir, v3, semantics, evaluation]
status: stable
sources:
  - id: ir-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/ir/morphir-ir-specification.md
    title: Morphir IR Specification — Semantics
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# IR Semantics

Structure alone does not determine behavior. These are the rules an implementation must honor for two tools to agree
on what a piece of IR means.

## Type system semantics

- **Type safety** — all values have types; type checking ensures correctness.
- **Polymorphism** — type variables enable generic programming.
- **Structural typing** — records and tuples are compared structurally.
- **Nominal typing** — custom types are compared by name.
- **Immutability** — all values are immutable; an update produces a new value.

The mixed structural/nominal rule is the one most likely to bite: two records with the same fields are the same type,
but two custom types with identical constructors are not.

## Value evaluation semantics

- **Pure functions** — no side effects, anywhere.
- **Eager evaluation** — arguments are evaluated before function application.
- **Pattern matching** — patterns are tested in order; the first match wins. See [Patterns](/patterns.md).
- **Scope rules**:
  - Lambda parameters are in scope in the lambda body.
  - Let bindings are in scope in the let expression body.
  - Pattern variables are in scope in the associated branch.

Eager evaluation is a real commitment, not a detail: a backend that compiles Morphir to a lazy target must preserve
strict argument evaluation order to stay faithful.

## Access control semantics

- **Public** — appears in package specifications; accessible to consumers.
- **Private** — visible only inside the package definition; never exposed.
- **Custom type constructors** — public constructors permit pattern matching by consumers; private constructors make
  the type opaque.

See [Attributes and Wrappers](/attributes-and-wrappers.md) for the mechanism and
[Specifications vs Definitions](/specification-vs-definition.md) for how it drives derivation.
