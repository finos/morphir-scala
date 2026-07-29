---
type: Intent
title: Incremental builds and checkpoints
description: Use prior checkpoints and source changes to invalidate affected work and choose full or incremental pipeline paths.
state: Backlog
kind: performance
breaking: false
created: 2026-07-28
state_since: 2026-07-28
issue: 932
tags: [buildkit, incremental, cache]
---

# 0015 — Incremental builds and checkpoints

Use prior checkpoints and source changes to invalidate affected work and choose full or incremental pipeline paths.

## Problem

Rebuilding every source, dependent module, IR distribution, and backend artifact after any edit makes the pipeline
too expensive for developer loops and long-running tools. Hiding incremental state inside a frontend would also
prevent the standard pipeline from making consistent invalidation decisions.

## Approach

Model prior build state as an explicit checkpoint supplied with `BuildRequest` and returned with `BuildOutcome`.
Snapshot inputs, determine inserts, updates, and deletions, invalidate affected dependents, and select a full or
incremental frontend path.

Checkpoint persistence remains an interpreter concern. A missing, incompatible, or corrupt checkpoint falls back
through an explicit policy rather than silently reusing partial state.

Depends on [0012 Workspace and manifest normalization](/0012-workspace-and-manifest-normalization.md), the relevant
frontend adapters, and [0009 Standard Morphir build pipeline](/0009-standard-morphir-build-pipeline.md).
