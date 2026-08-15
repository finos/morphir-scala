---
type: Intent
title: Electron appkit
description: "Publish morphir-appkit-electron for running Morphir inside an Electron host using Scala."
state: Backlog
kind: feature
breaking: false
created: 2026-08-14
state_since: 2026-08-14
tags: [appkit, electron]
---

# 0025 — Electron appkit

Publish morphir-appkit-electron for running Morphir inside an Electron host using Scala.

## Problem

Morphir tooling that should run as a desktop application needs a place to live that is neither a library bridge
(`kit`) nor an external-system client (`connector`). Electron is a host application. Integrating Morphir into it
is host-app work, and today there is no mill namespace that says so.

## Approach

When this intent leaves the backlog, add `morphir/appkit/electron` publishing as `morphir-appkit-electron`. Platforms
are per host. The reserved `morphir/appkit` container and README claim the name until then.

This intent does not specify the Electron embedding mechanism. That design happens in Refinement.

The narrative home is the
[published library families Design Note](../morphir/morphir-scala/design/published-library-families.md).
The family rule is [decision 0013](../morphir/morphir-scala/decisions/0013-published-library-families.md).
