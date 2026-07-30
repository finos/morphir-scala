---
type: Intent
title: Buildkit core task graph
description: "Provide a Morphir-agnostic, Kyo-native task graph for composing and executing typed cross-platform pipelines."
state: Backlog
kind: feature
breaking: false
created: 2026-07-28
state_since: 2026-07-28
issue: 932
tags: [buildkit, pipeline, kyo]
---

# 0008 — Buildkit core task graph

Provide a Morphir-agnostic, Kyo-native task graph for composing and executing typed cross-platform pipelines.

## Problem

Pipeline composition currently exists as a generic `Stage[I, O, S]` inside the Elm compiler API. Its location makes
an Elm artifact the accidental owner of functionality that does not mention Elm or Morphir, while other library
users have no neutral place to construct their own Kyo pipelines.

## Approach

Create the cross-platform `morphir/buildkit/core` module with no dependency on Morphir domain types. Move or
compatibly expose `Stage` there and add an immutable typed `Pipeline` graph supporting sequence, fork, join, runtime
collection fan-out, and conditional branches.

Ship a deterministic sequential executor first. Stable node identity, structured progress, explicit skipped nodes,
and deterministic result collation are part of the initial contract so parallel execution can arrive through a new
interpreter without changing pipeline definitions.

Keep graph values immutable and separate graph definition from execution. The core must support a public typed
composition layer without embedding runtime plugin discovery, Morphir domain types, untyped option maps, or mutable
processor state. Definition sealing validates duplicate node identities, missing dependencies, invalid joins, and
incompatible stage placement before an execution plan can run.

Parent: [0007 Multi-frontend Morphir transformation pipeline](/0007-multi-frontend-morphir-transformation-pipeline.md).

The public plugin and preset composition API is delivered by
[0018 Typed pipeline plugins and immutable presets](/0018-typed-pipeline-plugins-and-immutable-presets.md).
