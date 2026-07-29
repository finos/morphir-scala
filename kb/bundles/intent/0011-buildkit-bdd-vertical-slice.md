---
type: Intent
title: Buildkit BDD vertical slice
description: Run an Elm source-to-generated-artifact scenario through the standard Morphir pipeline using in-memory interpreters.
state: Backlog
kind: test
breaking: false
created: 2026-07-28
state_since: 2026-07-28
issue: 932
tags: [buildkit, bdd, itest]
---

# 0011 — Buildkit BDD vertical slice

Run an Elm source-to-generated-artifact scenario through the standard Morphir pipeline using in-memory interpreters.

## Problem

Unit tests of the graph and individual adapters cannot prove that consumers can assemble the proposed boundaries
into one frontend-to-backend transformation. Wiring the CLI first would mix architecture validation with production
I/O and command-line design.

## Approach

Make `langkit.itest`'s `TestDriver` the first consumer of the
[standard pipeline](/0009-standard-morphir-build-pipeline.md), using the
[Elm frontend adapter](/0010-elm-frontend-buildkit-adapter.md) and in-memory interpreters.

The first scenario carries Elm source through Morphir IR into a deterministic generated artifact set. Additional
scenarios cover accumulated diagnostics, dependency cycles, and selecting full versus incremental execution. The
driver submits `BuildRequest` and inspects `BuildOutcome`; it does not reconstruct pipeline wiring.

Contract scenarios also prove that deriving a preset does not mutate its parent, invalid plugin combinations fail
during sealing, repeated runs do not share run state, and executing the complete preset is observably equivalent to
composing its independently invokable phases. Tree-facing scenarios verify stable traversal, preserved source spans,
and positionless generated nodes through the existing unist projection boundary.

Parent: [0007 Multi-frontend Morphir transformation pipeline](/0007-multi-frontend-morphir-transformation-pipeline.md).
