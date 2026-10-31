---
type: Intent
title: Remove the classic ZIO-based morphir library
description: Stop building and publishing the classic ZIO-based library family once its deprecation has shipped and the Kyo v3 reader passes the shared MCK v3 cases.
state: Backlog
kind: removal
breaking: true
created: 2026-10-31
state_since: 2026-10-31
tags: [classic, zio, kyo, publishing]
---

# 0043 — Remove the classic ZIO-based morphir library

Stop building and publishing the classic ZIO-based library family once its deprecation has shipped and the Kyo v3 reader passes the shared MCK v3 cases.

## Problem

When a contributor works on morphir-scala, they want one effect stack and one IR model, so a change does not have
to be paid for twice and decision 0005's rule against bridging ZIO and Kyo holds. The classic family keeps ZIO, a
second IR model, an evaluator, two CI jobs and their fixtures alive after its consumers have a replacement.

## Approach

After [0042](/0042-deprecate-the-classic-zio-based-morphir-library.md) has shipped, stop building and publishing
`morphir`, `morphir-interop-zio-json`, `morphir-interop-borer`, `morphir-model-lowering`, `morphir-runtime-classic`,
`morphir-tools` and `morphir-tests` on the JVM and Scala.js, and delete the unpublished `testing.zio` and
`testing.generators`, the classic-only CI jobs and tasks, `examples/morphir-elm-projects`, the ZIO version pins, and
the unwired leftovers under `morphir/meta`, `morphir/elm/facade` and `morphir/toolkit` (absorbing
[0028](/0028-remove-or-absorb-morphir-toolkit.md)). Released versions stay on Maven Central.

It starts only when two more conditions hold: the shared `@version:3` reading cases are in the Morphir
Compatibility Kit upstream in finos/morphir ([finos/morphir#1116](https://github.com/finos/morphir/pull/1116)), and the pinned Morphir CLI ships them with morphir-scala's adapter
passing. Scala evaluation of Morphir IR has no replacement and ends here. The plan of record is the OpenSpec change
`retire-classic-zio-morphir`.
