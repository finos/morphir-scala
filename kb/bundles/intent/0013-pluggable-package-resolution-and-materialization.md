---
type: Intent
title: Pluggable package resolution and materialization
description: Resolve package coordinates independently from locations and materialize dependencies through interchangeable cross-platform interpreters.
state: Refinement
kind: feature
breaking: false
created: 2026-07-28
state_since: 2026-08-06
issue: 932
tags: [buildkit, package-management, dependencies]
---

# 0013 — Pluggable package resolution and materialization

Resolve package coordinates independently from locations and materialize dependencies through interchangeable cross-platform interpreters.

## Problem

Frontend stages need dependency modules, but resolving directly from a registry or a language-specific cache couples
the pipeline to one ecosystem and prevents reproducible offline, vendored, mirrored, and test builds.

## Approach

Use Package URL as the canonical package identity and interchange model. Exact requirements use versioned purls;
ranged requirements use the standard `vers` qualifier and expose its Package VERS value through a typed API. Keep
resolved identities, immutable source revisions, content digests, credentials, and physical locations distinct.
Define a small cross-platform effect for version discovery, dependency metadata, resolution, locking,
materialization, module enumeration, and source reading.

Provide interchangeable interpreters for the launch techniques chosen during refinement, including a git-file index
and local directory. Preserve room for registry, git, path, vendored, cache, offline, and test implementations.
Resolution must be pinnable and mirrorable.

The [MoonBit registry reference](../morphir/morphir-scala/design/moonbit-package-management.md) is architectural
evidence for a Git-distributed per-package line-delimited index, a small extensible resolution record, and verified
source materialization. Morphir does not copy its AGPL implementation or adopt its index format normatively.

Treat Morphir packages as source distributions in the initial design. Registry archives, immutable Git commits,
vendored trees, and local workspace snapshots may materialize the same logical package identity. Frontend-produced IR
and backend artifacts remain derived outputs, while publishable locks replace mutable source selectors with immutable
revisions and normalized content digests.

The evolving [Package URL-centered package-management design](../morphir/morphir-scala/design/package-url-package-management.md)
records the current proposal, commit-pinned research, capability-focused acceptance criteria, and the questions that
must settle before an immutable Decision Record is accepted. Its Elm acceptance target evaluates a direct unpublished
package whose result depends on a symbol implemented only by a transitive unpublished package. Preseeded runs through
both launch backends deny external package egress across resolution, materialization, and compilation; Elm cache and
shelm mechanics remain private to a replaceable compiler adapter.

Depends on [0012 Workspace and manifest normalization](/0012-workspace-and-manifest-normalization.md) and the
[standard pipeline](/0009-standard-morphir-build-pipeline.md).
