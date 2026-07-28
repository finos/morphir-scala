---
type: Intent
title: Capability coverage for morphir-scala
description: "Describe what morphir-scala does today, so released intent has capabilities to link to."
state: InProgress
kind: docs
breaking: false
created: 2026-07-28
state_since: 2026-07-28
tags: [kb]
---

# 0003 — Capability coverage for morphir-scala

Describe what morphir-scala does today, so released intent has capabilities to link to.

## Problem

The knowledge base documents upstream Morphir — `finos/morphir` and `finos/morphir-elm` — but says nothing about what
morphir-scala itself does. Released intent therefore has nothing to link to, and the obligation that keeps the
knowledge base current cannot be met.

## Approach

Grow `kb/bundles/morphir/morphir-scala/` as `type: Capability` concepts, present-tense, one per meaningful thing the
project does. Coverage arrives incrementally: every Released Intent adds or updates the capability it touched, so the
bundle fills in as work ships rather than in one documentation push.

Knowledge Base Tooling is the first entry, written as the capability that intent 0001 produced.
