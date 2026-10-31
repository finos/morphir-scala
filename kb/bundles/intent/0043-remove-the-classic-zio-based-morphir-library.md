---
type: Intent
title: Remove the classic ZIO-based morphir library
description: Stop building and publishing the classic ZIO-based library family once its deprecation has shipped and the Kyo v3 reader passes the shared MCK v3 cases.
state: Released
kind: removal
breaking: true
created: 2026-10-31
state_since: 2026-10-31
tags: [classic, zio, kyo, publishing]
capability: morphir/morphir-scala:/v3-ir-reading.md
artifacts: [pkg:maven/org.finos.morphir/morphir-model-compat-v3_3@0.5.0-M13, pkg:maven/org.finos.morphir/morphir-model-compat-v3_sjs1_3@0.5.0-M13]
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

## Gate

All three start conditions held on 2026-10-31:

- **Deprecation shipped.** 0.5.0-M12 is on Maven Central. Each classic POM description starts with "Deprecated:" and
  names the replacement, and `morphir-model-compat-v3` is published for the JVM and Scala.js.
- **Upstream cases merged.** [finos/morphir#1116](https://github.com/finos/morphir/pull/1116) added the `@version:3`
  reading cases to the kit. One case, distributions-0016, stays pending until the Rust reader reports
  `unsupported_format_version_major`.
- **Pinned CLI ships them.** Morphir CLI `0.4.0-beta.11` embeds that kit. Against it the adapter passes 91 cases, fails
  none, and skips the v4 cases it does not claim.
