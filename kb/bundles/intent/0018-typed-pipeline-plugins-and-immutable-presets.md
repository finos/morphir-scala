---
type: Intent
title: Typed pipeline plugins and immutable presets
description: Provide public typed plugins and immutable presets that produce validated, reusable pipeline execution plans.
state: Backlog
kind: feature
breaking: false
created: 2026-07-29
state_since: 2026-07-29
issue: 932
tags: [buildkit, pipeline, plugins, presets]
---

# 0018 — Typed pipeline plugins and immutable presets

Provide public typed plugins and immutable presets that produce validated, reusable pipeline execution plans.

## Problem

A stage or task graph can describe executable work, but does not by itself give library users a safe way to package,
share, extend, and validate reusable pipeline configuration. A mutable processor or runtime-discovered plugin model
would make composition order, option conflicts, cross-platform support, and repeated execution difficult to reason
about.

## Approach

Add a public typed composition layer to
[buildkit core](/0008-buildkit-core-task-graph.md):

- A typed pipeline plugin contributes graph structure from immutable options.
- An immutable pipeline definition combines plugins and directly declared stages.
- An immutable preset creates a reusable definition that consumers may derive and extend without mutating its parent.
- Sealing validates the complete definition and returns a shareable, repeatable execution plan.

Sealing reports deterministic typed errors for duplicate node identities, missing dependencies, invalid joins,
incompatible plugin placement, and unresolved replacement conflicts. Repeating or replacing plugin configuration is
explicit; there is no implicit untyped option merge. Execution state remains per run, so using the same plan
concurrently cannot leak inputs, metadata, diagnostics, progress, or results between executions.

The API is statically assembled and cross-platform. Runtime classpath or JavaScript plugin discovery, mutable
processor freezing, and ecosystem-specific domain types are outside this intent.

Parent: [0007 Multi-frontend Morphir transformation pipeline](/0007-multi-frontend-morphir-transformation-pipeline.md).
