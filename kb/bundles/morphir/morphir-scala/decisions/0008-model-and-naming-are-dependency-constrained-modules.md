---
type: Decision Record
title: morphir/model and morphir/naming are dependency-constrained modules
description: "The code model and the naming vocabulary were extracted into modules whose dependency closures are constrained by construction, not by convention."
state: Accepted
decided: 2026-07-29
tags: [modules, build, dependencies, codemodel, naming]
status: stable
---

# 0008 — `morphir/model` and `morphir/naming` are dependency-constrained modules

The v4 code model was extracted out of core `morphir` into a new `morphir/model` module, which depends on
`morphir/naming` and Kyo **and nothing else**. `org.finos.morphir.naming` was in turn extracted into
`morphir/naming`, whose dependency closure imports nothing but `scala.annotation.tailrec`.

The v3 → v4 lowering is a third module, `morphir/model/lowering`, rather than part of `morphir/model`.

## Why

The code model is the thing every future consumer needs: a runtime, a code generator, an analysis tool, a language
server. While it lived in core `morphir`, taking a dependency on it meant taking ZIO, zio-json, zio-prelude, spire,
magnolia and enumeratum along with it. For a module whose entire content is data definitions, that is a poor trade,
and it is the kind of thing that quietly makes a library unusable in contexts nobody anticipated.

The naming extraction fell out of the model extraction rather than being planned: the code model references
`FQName`, `Name`, `Path` and friends, so either those came along or the model kept its dependency on core. Extracting
naming turned out to be nearly free — the closure is twelve files that import one thing from the standard library.

**The constraint is structural, not aspirational.** `morphir/model/lowering` is a separate module specifically so
that the constraint survives contact with v3: the lowering genuinely needs v3 types from core, so if it lived in
`morphir/model` the module would depend on core and the property would be gone within one commit. Splitting it means
the build fails if anyone reintroduces the dependency, rather than the property decaying unnoticed.

## Consequences

`org.finos.morphir` now spans two published artifacts (`morphir` and `morphir-naming`), and
`org.finos.morphir.runtime` spans `morphir` and `morphir-runtime-classic`. Maven and Coursier consumers are
unaffected, since transitive resolution makes split packages invisible at that layer, but JPMS `module-info` and OSGi
bundling are foreclosed for those packages.

One inversion is worth knowing about: `org.finos.morphir.datamodel` is owned by `morphir-model`, while its child
package `org.finos.morphir.datamodel.classic` is owned by `morphir`, which does not depend on `morphir-model`. The
parent/child reading is conceptually right — `.classic` is genuinely the older sibling — but someone tracing the
child package upward expecting to land in the `morphir-model` jar will look in the wrong artifact.

`morphir/naming` has no test module of its own; its specs live in `morphir/tests`, which depends on all of core
`morphir`. So the one module whose entire selling point is an empty dependency closure currently cannot be tested
without building everything it was extracted to avoid.
