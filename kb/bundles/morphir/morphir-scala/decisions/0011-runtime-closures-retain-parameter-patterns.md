---
type: Decision Record
title: Runtime closures retain parameter patterns
description: "Val.Closure stores each remaining parameter as a code-model Pattern, preserving destructuring lambdas in the serializable runtime value."
state: Accepted
decided: 2026-08-06
tags: [runtime, datamodel, closures, patterns, serialization]
status: stable
---

# 0011 — Runtime closures retain parameter patterns

`Val.Closure.params` is `Chunk[Pattern]`, not `Chunk[Name]`. Each unapplied lambda parameter therefore retains the
same code-model pattern that `Expr.Lambda` binds, including tuple, constructor, list, alias, literal, unit, and
wildcard patterns.

This changes the derived `Val` wire format before its first release. There is no decoder for the earlier development
shape.

## Why

`Expr.Lambda` accepts any `Pattern`, but a closure containing only names cannot represent a lambda such as
`\(x, y) -> x + y` or `\(Just x) -> x`. Losing that pattern when the evaluator creates a closure would make the
runtime value an incomplete encoding of the suspended computation. That conflicts with the reason closures are data:
snapshot, restore, replay, and cross-platform schema round-trips must preserve everything evaluation still needs.

`Pattern` already belongs to the code model and derives `Schema`, so retaining it adds no opaque runtime state and no
new dependency. A schema round-trip using a tuple and alias pattern proves the derived JSON format preserves a
destructuring parameter on both JVM and JavaScript.

## Alternatives rejected

The evaluator could have replaced every pattern with a fresh synthetic name and inserted a `PatternMatch` into the
body. That would keep the smaller closure shape, but it would make closure creation perform an implicit IR rewrite,
require deterministic collision-free name generation, and preserve synthetic rather than original suspended code.
It also moves a value-model mismatch into every evaluator implementation instead of fixing it once at the boundary.

Restricting runtime lambdas to name patterns was rejected because it would accept less of the code model and would
make valid Elm destructuring lambdas fail only after compilation.

## Consequences

The evaluator must bind an argument by matching the corresponding `Pattern`, reporting pattern-match failure through
its normal typed outcome. Partial application retains the unmatched pattern sequence unchanged.

The derived serialized representation of `Val.Closure` changes. This is intentionally breaking while the runtime is
pre-release; adding compatibility machinery for an unpublished development format would create policy without a
consumer.

If a future normalized runtime IR deliberately desugars all binders before closure creation, that design may choose a
name-only closure again. It must supersede this record and demonstrate that the normalization is explicit,
deterministic, and lossless for snapshot and replay rather than silently changing this record.
