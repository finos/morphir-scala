---
type: Intent
title: Workspace and manifest normalization
description: "Normalize Morphir and ecosystem manifests into one workspace and project model with explicit discovery, precedence, and merge rules."
state: Backlog
kind: feature
breaking: false
created: 2026-07-28
state_since: 2026-07-28
issue: 932
tags: [buildkit, workspace, configuration]
---

# 0012 — Workspace and manifest normalization

Normalize Morphir and ecosystem manifests into one workspace and project model with explicit discovery, precedence, and merge rules.

## Problem

Morphir workspaces can span projects from different source ecosystems, but their manifests describe overlapping
facts in different formats. Treating `morphir.toml`, `morphir.json`, `elm.json`, and future manifests independently
would leave discovery, precedence, and conflicts to each frontend or entry point.

## Approach

Define one internal workspace/project model and adapter contracts for Morphir and ecosystem manifests. Specify
workspace discovery, member selection, source roots, frontend selection, output requests, precedence, and merge
errors explicitly.

Reconcile the implementation with the existing draft `morphir.toml` workspace, project, task, workflow, and
toolchain model rather than introducing a parallel configuration concept.

Depends on [0009 Standard Morphir build pipeline](/0009-standard-morphir-build-pipeline.md).
