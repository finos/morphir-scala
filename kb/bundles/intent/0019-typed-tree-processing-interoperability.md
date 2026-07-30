---
type: Intent
title: Typed tree-processing interoperability
description: Let pipeline stages inspect typed language and IR trees through structural capabilities and explicit unist projections.
state: Backlog
kind: feature
breaking: false
created: 2026-07-29
state_since: 2026-07-29
issue: 932
tags: [buildkit, langkit, trees, unist]
---

# 0019 — Typed tree-processing interoperability

Let pipeline stages inspect typed language and IR trees through structural capabilities and explicit unist projections.

## Problem

Frontend, transformation, testing, and analysis tooling need to inspect multiple tree representations without each
consumer learning every concrete node hierarchy. Making buildkit own one dynamic universal AST would discard useful
Scala type information and force Elm CST, Elm AST, Morphir IR, and future language models into a lowest-common-
denominator representation.

Morphir already has a smaller interoperability foundation in `QueryableTree` and `UnistProjection`, but the pipeline
design must state how generic tree processing uses it and where the boundary ends.

## Approach

Keep every CST, AST, Morphir IR, and backend model strongly typed as it passes between stages. Generic inspection
plugins consume separate structural capabilities such as `QueryableTree`; an explicit `UnistProjection` may expose a
tree to unist-compatible tooling without changing the pipeline's internal representation.

The contract preserves stable child ordering, named-field relationships, leaf text, and optional half-open source
positions. Generated nodes may omit positions. Representation-specific metadata remains in domain types or separate
typed capabilities rather than a shared untyped node data map.

Parsing, inspection, transformation, and generation are independently invokable. An inspection preserves its typed
representation while reporting diagnostics or run metadata; a transformation explicitly declares a different typed
output when it changes representation. Cross-platform contract tests cover traversal stability, source-position
projection, positionless generated nodes, phase composition, and equivalence between phase-by-phase and preset
execution.

A universal buildkit node hierarchy, promotion of the tooling-layer `VFile`, and a generic document envelope are
outside this intent. The last may be reconsidered only when a concrete pipeline consumer cannot be represented by
typed stage values and run-scoped context.

The evidence and terminology behind this boundary live in
[structural tree interoperability](https://github.com/finos/morphir-scala/blob/main/kb/bundles/programming-language-tooling/structural-tree-interoperability.md);
the intent records future work rather than duplicating that general reference.

Parent: [0007 Multi-frontend Morphir transformation pipeline](/0007-multi-frontend-morphir-transformation-pipeline.md).

Depends on [0018 Typed pipeline plugins and immutable presets](/0018-typed-pipeline-plugins-and-immutable-presets.md).
