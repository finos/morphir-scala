---
type: Intent
title: Intent management in the knowledge base
description: "Record features, enhancements and bugs as prose in kb/, with a lifecycle and a SQLite index."
state: Released
kind: feature
breaking: false
created: 2026-07-28
state_since: 2026-07-28
tags: [intent, kb]
capability: morphir/morphir-scala:/knowledge-base-tooling.md
artifacts: [pkg:maven/org.finos.morphir/morphir-core@0.4.0]
---

# 0001 — Intent management in the knowledge base

Record features, enhancements and bugs as prose in kb/, with a lifecycle and a SQLite index.

## Problem

Work gets decided in issues, chat and pull requests, and the reasoning evaporates. Six months later the code shows
*what* was built but not *why*, and the knowledge base — if it says anything at all — describes an intention rather
than a fact.

The second-order problem is rot. A knowledge base that records planned work goes stale the moment the work ships,
because nothing forces it to learn what changed.

## Approach

Record intent as prose in `kb/bundles/intent/`, with a lifecycle: Backlog, Refinement, InProgress, then Released,
Cancelled or Superseded. Records never move — the state is a field, and the index is regenerated from it.

Releasing an Intent requires linking the Capability it produced. That single obligation is the anti-rot mechanism:
the knowledge base cannot fall behind the code without `intent check` failing.

GitHub Issues remains the inbox and the public conversation. Intent is the curated record from backlog onward.

See ADRs 0001 to 0003 for the decisions that were not obvious.
