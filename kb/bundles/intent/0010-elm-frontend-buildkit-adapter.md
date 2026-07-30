---
type: Intent
title: Elm frontend buildkit adapter
description: Expose Elm source-to-IR compilation as a frontend that the standard Morphir build pipeline can invoke.
state: Backlog
kind: feature
breaking: false
created: 2026-07-28
state_since: 2026-07-28
issue: 932
tags: [buildkit, elm, frontend]
---

# 0010 — Elm frontend buildkit adapter

Expose Elm source-to-IR compilation as a frontend that the standard Morphir build pipeline can invoke.

## Problem

Elm parsing and compiler behavior are currently available through Elm-specific facades and effects, but the standard
Morphir pipeline has no frontend implementation with which to prove its language extension boundary.

## Approach

Implement the standard buildkit frontend contract in the Elm langkit. The adapter owns Elm options, manifests,
sources, diagnostics, project-level module ordering, and source-to-IR stages; buildkit owns when the frontend runs and
how its IR participates in a workspace build.

Expose Elm parsing, inspection, lowering, and source-to-IR compilation as independently invokable typed phases as
well as one frontend plugin for the standard preset. Elm CST and AST values remain their native types; generic tree
processing uses `QueryableTree` and explicit projections instead of converting them to a buildkit-owned node model.

Decide during refinement whether `ElmParse` becomes a specialization of a shared reporting effect or remains an Elm
effect used behind the adapter. Neither choice may make the standard buildkit depend on Elm.

Depends on [0009 Standard Morphir build pipeline](/0009-standard-morphir-build-pipeline.md).
