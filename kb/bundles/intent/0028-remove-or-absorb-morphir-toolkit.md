---
type: Intent
title: Remove or absorb morphir/toolkit
description: "Delete or absorb the unwired morphir/toolkit directory so it is not revived as a published-library namespace."
state: Backlog
kind: removal
breaking: false
created: 2026-08-14
state_since: 2026-08-14
tags: [toolkit, modules]
---

# 0028 — Remove or absorb morphir/toolkit

Delete or absorb the unwired morphir/toolkit directory so it is not revived as a published-library namespace.

## Problem

`morphir/toolkit` holds unwired `MirFileSupport` and is not a mill module. The name sits next to `kit`, `langkit`,
and `buildkit`, which are real families. A later session can easily treat `toolkit` as the place for connectors or
appkits. The directory should go, or its contents should move into a module that actually builds, before that
happens.

## Approach

Decide whether `MirFileSupport` still earns a home (for example under `interop` or `runtime.classic`) or can be
deleted. Then remove the `morphir/toolkit` directory. Do not add mill wiring that publishes a `morphir-toolkit`
artifact as a way to keep the name.

This intent is removal, not a new family. It does not block [0020](/0020-github-graphql-connector.md) through
[0022](/0022-okf-knowledge-library.md).

The family rule is [decision 0013](../morphir/morphir-scala/decisions/0013-published-library-families.md).
