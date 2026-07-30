---
type: Intent
title: Standard Morphir build pipeline
description: "Assemble one cross-platform Morphir pipeline from pluggable frontend, transformation, backend, workspace, package, checkpoint, and artifact capabilities."
state: Backlog
kind: feature
breaking: false
created: 2026-07-28
state_since: 2026-07-28
issue: 932
tags: [buildkit, pipeline, morphir]
---

# 0009 — Standard Morphir build pipeline

Assemble one cross-platform Morphir pipeline from pluggable frontend, transformation, backend, workspace, package, checkpoint, and artifact capabilities.

## Problem

Even with a reusable graph, Morphir entry points could assemble workspace loading, dependency resolution, frontend
compilation, IR processing, and generation differently. That would reproduce today's split between direct library
calls, test drivers, and CLI subprocesses rather than create one transformation pipeline.

## Approach

Create the cross-platform root `morphir/buildkit` module on
[buildkit core](/0008-buildkit-core-task-graph.md). Define `BuildRequest`, `BuildOutcome`, and extension points for
frontends, IR transformations, backends, workspace access, package management, checkpoints, and artifact outputs.

Provide one standard assembly describing the phase order while leaving policy in interpreters. It must not depend on
Elm or perform filesystem, network, process, or console operations directly.

Publish that assembly as an immutable preset built from typed frontend, inspection, transformation, and backend
plugins. A consumer can derive a definition, extend or explicitly replace compatible plugins, seal it into a
validated execution plan, and execute it repeatedly without mutating the shared preset.

Parsing, inspection, transformation, and generation remain independently invokable. The complete build is their
standard composition, not a separate implementation. Representation-preserving inspections may add diagnostics or
run metadata without replacing their typed value; transformations and backends declare their typed output
representation.

Parent: [0007 Multi-frontend Morphir transformation pipeline](/0007-multi-frontend-morphir-transformation-pipeline.md).

Depends on [0018 Typed pipeline plugins and immutable presets](/0018-typed-pipeline-plugins-and-immutable-presets.md).
