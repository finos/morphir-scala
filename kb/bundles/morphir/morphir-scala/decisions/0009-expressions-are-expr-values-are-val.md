---
type: Decision Record
title: Expressions are Expr, values are Val — diverging from Morphir's Elm-inherited vocabulary
description: "The code model's expression type is renamed from Value to Expr, so that the word value is free for what an expression evaluates to."
state: Accepted
decided: 2026-07-29
tags: [codemodel, naming, ir, spec, divergence]
status: stable
---

# 0009 — Expressions are `Expr`, values are `Val`

The code model's expression type is `org.finos.morphir.codemodel.Expr`. Upstream Morphir, the v3 IR and the v4 draft
specification all call this type `Value`. The renamed package is `org.finos.morphir.codemodel` rather than
`org.finos.morphir.ir.v4`.

`ValueDefinition`, `ValueSpecification` and `ValueAttributes` keep their spec names — they genuinely concern value
*bindings* — and now hold an `Expr` body.

## Why

Morphir inherits from Elm the convention that "value" means *expression*. That convention is the direct cause of the
runtime's naming problem: with `Value` taken by the expression type, there is no good name left for what an
expression evaluates to, and every evaluator signature ends up with two different things called "value". The
alternative was to name the runtime value something evasive — `MValue`, `RTValue`, `Datum` — which is how the classic
runtime ended up with `RTValue` and the confusion that follows it.

`codemodel.Expr` versus `datamodel.Val` is unambiguous in a way no pair of names built around `Value` can be. It was
folded into a package rename that was already touching every one of those files, so the marginal cost was near zero;
doing it later, once there were more consumers, would not be.

Dropping `v4` from the package name is the same argument at module scale: the name a reader imports should not carry
a format version number that will be wrong eventually.

## Consequences

**This is a deliberate divergence from the specification, and the spec has to catch up.** The v4 draft bundle under
`kb/bundles/morphir/morphir-ir-v4-draft/` still calls the type `Value`, and its front matter cites upstream
`finos/morphir` `docs/spec/draft/values.md` as its source. Two things are outstanding: updating
`value-expressions.md` and `value-specifications-and-definitions.md`, which describe the type by name; and recording
the divergence in that bundle's `design/divergences.md`, since a reader diffing against upstream needs to know the
rename is intentional rather than an error.

Until that lands, the code and the spec disagree in a way that looks like a mistake. The debt is recorded in a
comment block at the top of `morphir/model/src/org/finos/morphir/codemodel/Expr.scala`, which is the source of truth
for what remains.

Anyone porting code between this project and upstream Morphir has to translate the name. That cost is real and was
accepted, on the grounds that the runtime is the larger body of code and it reads worse under the inherited
convention.
