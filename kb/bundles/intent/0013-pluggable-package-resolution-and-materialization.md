---
type: Intent
title: Pluggable package resolution and materialization
description: Resolve package coordinates independently from locations and materialize dependencies through interchangeable cross-platform interpreters.
state: Backlog
kind: feature
breaking: false
created: 2026-07-28
state_since: 2026-07-28
issue: 932
tags: [buildkit, package-management, dependencies]
---

# 0013 — Pluggable package resolution and materialization

Resolve package coordinates independently from locations and materialize dependencies through interchangeable cross-platform interpreters.

## Problem

Frontend stages need dependency modules, but resolving directly from a registry or a language-specific cache couples
the pipeline to one ecosystem and prevents reproducible offline, vendored, mirrored, and test builds.

## Approach

Separate package coordinates, version constraints, resolved identities, and physical locations. Define a small
cross-platform effect for version discovery, dependency metadata, resolution, materialization, module enumeration,
and source reading.

Provide interchangeable interpreters for the launch techniques chosen during refinement, including a git-file index
and local directory. Preserve room for registry, git, path, vendored, cache, offline, and test implementations.
Resolution must be pinnable and mirrorable.

Depends on [0012 Workspace and manifest normalization](/0012-workspace-and-manifest-normalization.md) and the
[standard pipeline](/0009-standard-morphir-build-pipeline.md).
