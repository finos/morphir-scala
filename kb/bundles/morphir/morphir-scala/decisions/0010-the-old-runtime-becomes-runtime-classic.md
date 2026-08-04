---
type: Decision Record
title: The old runtime becomes runtime.classic; its package rename is deferred
description: "The existing ZIO runtime moved to morphir/runtime/classic intact, so the new runtime can take the good module path without a flag-day cutover."
state: Accepted
decided: 2026-07-29
tags: [runtime, modules, migration, publishing]
status: stable
---

# 0010 — The old runtime becomes `runtime.classic`; its package rename is deferred

The existing ZIO-based runtime moved from `morphir/runtime/` to `morphir/runtime/classic/`, source unchanged beyond
what the move forced. `morphir/runtime` becomes the home of the new Kyo-based runtime. The same pairing applies to
values: the new value model takes `org.finos.morphir.datamodel`, and the pre-existing MDM types were renamed to
`org.finos.morphir.datamodel.classic`.

**The classic runtime's Scala package is still `org.finos.morphir.runtime.*`.** Renaming it to
`...runtime.classic.*` is deferred, not abandoned.

## Why

Moving the old runtime rather than deleting or rewriting it avoids a flag-day cutover: the new runtime can be built
and compared against a working implementation instead of replacing it in one step. The good module path belongs to
the runtime that has a future, which is why the new one takes `morphir/runtime` rather than being parked somewhere
like `morphir/vm`.

The package rename was costed at roughly 81 files, not the ~40 first assumed. Its only purpose is to free
`org.finos.morphir.runtime` for the new runtime — and slice 1 puts no code in that module, so the need is not yet
live. Landing 81 files of pure churn ahead of the work that needs it would conflict with everything else in flight
for no benefit.

The mechanical renames that *were* done — the code model package, `datamodel.classic`, the naming extraction — were
deliberately landed as isolated commits *before* the substantive work, on the same reasoning in reverse: they touch
well over a hundred files, so doing them after would mean rebasing real work across them.

## Consequences

`org.finos.morphir:morphir-runtime_3` silently stops being published. `morphir/runtime/package.mill.yaml` is now an
empty container and the code publishes as `morphir-runtime-classic_3` instead. Nothing catches this class of change:
`mimaPreviousArtifacts` is not configured for these modules and `mimaReportBinaryIssues` is not among the globs CI
runs. Downstream consumers have to move, and only a release note will tell them.

The deferred package rename becomes necessary the moment the new runtime puts code in `morphir/runtime` — two
modules cannot both own `org.finos.morphir.runtime` cleanly. That is the trigger to revisit, and it is expected
rather than hypothetical.

The classic runtime's test suite does not currently run: 4400 lines across six files sit in
`morphir/runtime/classic/test/jvm/src/`, but Mill's cross-platform layout reads `test/src` and `test/src-jvm`, so
none of it compiles. This is pre-existing and was inherited by the move, not caused by it — but it matters here,
because the argument for keeping the classic runtime as a differential oracle assumes that suite runs, and it does
not.
