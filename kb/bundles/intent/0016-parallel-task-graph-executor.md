---
type: Intent
title: Parallel task-graph executor
description: Execute independent pipeline nodes concurrently without changing deterministic results or diagnostic ordering.
state: Backlog
kind: performance
breaking: false
created: 2026-07-28
state_since: 2026-07-28
issue: 932
tags: [buildkit, parallelism, pipeline]
---

# 0016 — Parallel task-graph executor

Execute independent pipeline nodes concurrently without changing deterministic results or diagnostic ordering.

## Problem

Large workspaces contain independent dependency fetches, projects, modules, transformations, and backend targets.
Executing every ready node serially leaves that available parallelism unused.

## Approach

Add a parallel interpreter for the graph defined by
[0008 Buildkit core task graph](/0008-buildkit-core-task-graph.md). Schedule independent ready nodes concurrently,
respect dependency and cancellation boundaries, and bound concurrency through interpreter policy.

The parallel executor must satisfy the same contract suite as the sequential executor and return the same externally
observable value, status, diagnostic, and progress ordering. Timing and scheduling traces may differ.

This work is deliberately separate from the first buildkit release so graph semantics can stabilize before execution
is optimized.
