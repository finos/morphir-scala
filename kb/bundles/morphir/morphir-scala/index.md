---
okf_version: "0.2"
title: morphir-scala
description: "Morphir-scala capabilities, decisions, design notes, and research."
---

# morphir-scala

This bundle covers morphir-scala capabilities, decisions, design notes, and research.

Capabilities describe current behavior. Immutable [Decision Records](/decisions/index.md) explain accepted choices.
Draft Design Notes and research are provisional.

## Design Notes

* [Package URL-centered package management](/design/package-url-package-management.md) - A design for PURL identities, reproducible materialization, and packages outside ecosystem registries.
* [Multi-frontend pipeline and workspace boundaries](/design/pipeline-workspace-boundaries.md) - A design for shared buildkit phases, workspace normalization, frontend isolation, and issue #930.
* [Mill Morphir plugin architecture](/design/mill-morphir-plugin-architecture.md) - Design for publishable Mill plugins that acquire tools and compose Morphir generation with host-language builds.

## Package-management research

* [MoonBit registry, resolution, and source materialization](/design/moonbit-package-management.md) - How MoonBit resolves and materializes source packages from its Git-backed JSONL registry.

## Orientation

* [Knowledge Base Tooling](/knowledge-base-tooling.md) - The kb skill manages the OKF knowledge base and the intent recorded in it, from the command line.
* [Continuous Integration](/continuous-integration.md) - GitHub Actions runs linting, cross-platform tests and knowledge base checks on pull requests targeting supported branches.
* [Build System](/build-system.md) - Mill drives the build from per-directory package.mill.yaml files, with mise as the task runner.
* [Cross-Platform Targets](/cross-platform-targets.md) - Modules compile to the JVM, ScalaJS, WebAssembly and Scala Native from one shared source layout.

## Decisions

Full list, grouped, in [decisions/index.md](/decisions/index.md).

* [Released intent stays; capabilities are separate documents](/decisions/0001-released-intent-stays-capabilities-are-separate.md) - Intent records are never moved on release; a Released intent must link to a separate present-tense Capability document.
* [Intent tooling lives in the kb skill](/decisions/0002-intent-tooling-lives-in-the-kb-skill.md) - Intent management is implemented as `kb intent …` subcommands inside the kb skill, not as a separate skill with its own code.
* [Two identifier schemes for intent, deliberately](/decisions/0003-two-identifier-schemes-for-intent.md) - Documents are addressed as `bundle-label:/path.md` and published software by Package URL; neither scheme is legacy.
* [Decision Records are a third register in the knowledge base](/decisions/0004-decision-records-are-a-third-register.md) - Architectural decisions are recorded as `type: Decision Record` concepts that are superseded rather than edited, alongside Intent and Capability.
* [Bridge nothing between ZIO and Kyo](/decisions/0005-bridge-nothing-between-zio-and-kyo.md) - New code is written against Kyo, existing ZIO code is left untouched, and no ZIO-to-Kyo adapter is permitted at the boundary.
* [Runtime values are one serializable Val built on kyo-schema](/decisions/0006-runtime-values-are-val-on-kyo-schema.md) - The new runtime has a single value type, datamodel.Val, whose structured case is kyo-schema's Structure.Value and whose closures serialize.
* [The runtime reuses the code model's type language and targets v4](/decisions/0007-runtime-reuses-the-code-model-type-language.md) - Runtime types are codemodel.Type rather than a second type language, and the new runtime targets the v4 code model with a one-way lowering from v3.
* [morphir/model and morphir/naming are dependency-constrained modules](/decisions/0008-model-and-naming-are-dependency-constrained-modules.md) - The code model and the naming vocabulary were extracted into modules whose dependency closures are constrained by construction, not by convention.
* [Expressions are Expr, values are Val — diverging from Morphir's Elm-inherited vocabulary](/decisions/0009-expressions-are-expr-values-are-val.md) - The code model's expression type is renamed from Value to Expr, so that the word value is free for what an expression evaluates to.
* [The old runtime becomes runtime.classic; its package rename is deferred](/decisions/0010-the-old-runtime-becomes-runtime-classic.md) - The existing ZIO runtime moved to morphir/runtime/classic intact, so the new runtime can take the good module path without a flag-day cutover.
* [Runtime closures retain parameter patterns](/decisions/0011-runtime-closures-retain-parameter-patterns.md) - Val.Closure stores each remaining parameter as a code-model Pattern, preserving destructuring lambdas in the serializable runtime value.
