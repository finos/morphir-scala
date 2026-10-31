---
type: Intent
title: Deprecate the classic ZIO-based morphir library
description: "Announce in one released milestone that the classic ZIO-based library family will stop being published, and name what replaces each artifact."
state: InProgress
kind: deprecation
breaking: false
created: 2026-10-31
state_since: 2026-10-31
tags: [classic, zio, kyo, publishing]
---

# 0042 — Deprecate the classic ZIO-based morphir library

Announce in one released milestone that the classic ZIO-based library family will stop being published, and name what replaces each artifact.

## Problem

When someone depends on the classic ZIO-based morphir library (`morphir_3` and the six artifacts built on it), they
need to learn that it is being retired, and what to use instead, before a release stops publishing it. Otherwise
their next upgrade fails with no warning and no route forward.

The project means to replace that family with the Kyo-based modules. Decision 0005 forbids bridging the two, yet ZIO
still reaches about a fifth of the build through the classic root.

## Approach

One milestone, 0.5.0-M12, carries the notice while still publishing every classic artifact:

- each classic POM description starts with "Deprecated:" and names the replacement, which Maven Central shows on
  the artifact page;
- each module's entry-point scaladoc and the root README say the same;
- the CHANGELOG has a `Deprecated` entry listing every artifact and its replacement, or "no replacement" for the
  evaluator (`morphir-runtime-classic`) and `morphir-tests`.

The same milestone ships the replacement for the one capability a Kyo consumer still needed from classic: reading v3
`morphir-ir.json`, now `V3Reader` in `morphir-model-compat-v3`. Removal is
[0043](/0043-remove-the-classic-zio-based-morphir-library.md). The plan of record is the OpenSpec change
`retire-classic-zio-morphir`.
