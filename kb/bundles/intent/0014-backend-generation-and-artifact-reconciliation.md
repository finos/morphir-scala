---
type: Intent
title: Backend generation and artifact reconciliation
description: Generate desired artifact sets from Morphir IR and reconcile them deterministically through pluggable output interpreters.
state: Backlog
kind: feature
breaking: false
created: 2026-07-28
state_since: 2026-07-28
issue: 932
tags: [buildkit, backend, artifacts]
---

# 0014 — Backend generation and artifact reconciliation

Generate desired artifact sets from Morphir IR and reconcile them deterministically through pluggable output interpreters.

## Problem

Producing Morphir IR is only the midpoint of the transformation pipeline. Backends may emit many files, fail on
unsupported IR, require target-specific options, and remove outputs that are no longer desired. Letting each backend
write directly makes generation platform-specific and difficult to compose or test.

## Approach

Define a backend contract from Morphir IR and target options to a desired artifact set. Backends remain pure with
respect to output storage; artifact interpreters compare desired and existing state and apply deterministic inserts,
updates, and removals.

Allow independent selected backends to fan out through the task graph and join their artifact sets with explicit
collision handling. Supply an in-memory backend and artifact interpreter for the BDD vertical slice before production
filesystem interpreters.

Depends on [0009 Standard Morphir build pipeline](/0009-standard-morphir-build-pipeline.md).
