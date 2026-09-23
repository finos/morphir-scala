---
type: Intent
title: Move MicroKanren library to knowledge.logic
description: Publish the MicroKanren library as morphir-knowledge-logic under the knowledge module family.
state: InProgress
kind: refactor
breaking: true
created: 2026-09-23
state_since: 2026-09-23
tags: [knowledge, logic, modules]
---

# 0040 — Move MicroKanren library to knowledge.logic

Publish the MicroKanren library as morphir-knowledge-logic under the knowledge module family.

## Problem

The published MicroKanren library sits under `morphir/contrib/knowledge`, although its public Scala package is
`morphir.knowledge.logic`. The build path makes it look like a contributed knowledge module and gives it the artifact
name `morphir-contrib-knowledge`. That conflicts with the first-class module layout established by
[0027](0027-stop-using-contrib-for-first-class-work.md) and makes the library hard to find beside OKF.

## Approach

Move the module to `morphir/knowledge/logic`, under the existing `morphir/knowledge` Mill module. Align the shared
source directory with the unchanged `morphir.knowledge.logic` package. Keep its JVM and Scala.js variants, public API,
dependencies, and tests. Update CI selectors and module documentation. The new published artifact is
`morphir-knowledge-logic`; consumers must change their dependency coordinate from `morphir-contrib-knowledge`.
