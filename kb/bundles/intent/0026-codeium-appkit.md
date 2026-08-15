---
type: Intent
title: Codeium appkit
description: "Publish morphir-appkit-codeium for integrating Morphir with Codeium."
state: Backlog
kind: feature
breaking: false
created: 2026-08-14
state_since: 2026-08-14
tags: [appkit, codeium]
---

# 0026 — Codeium appkit

Publish morphir-appkit-codeium for integrating Morphir with Codeium.

## Problem

Integrating Morphir with Codeium is host-application work: Morphir runs inside another product's surface. That does
not belong in `kit` or `connector`. Without an `appkit` leaf, the integration has no mill path a later session can
grab.

## Approach

When this intent leaves the backlog, add `morphir/appkit/codeium` publishing as `morphir-appkit-codeium`. Platforms
are per host. The reserved `morphir/appkit` container and README claim the name until then.

This intent does not specify the Codeium integration mechanism. That design happens in Refinement.

The narrative home is the
[published library families Design Note](../morphir/morphir-scala/design/published-library-families.md).
The family rule is [decision 0013](../morphir/morphir-scala/decisions/0013-published-library-families.md).
