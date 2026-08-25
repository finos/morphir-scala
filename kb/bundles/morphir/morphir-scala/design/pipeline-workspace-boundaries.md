---
type: Design Note
title: Multi-frontend pipeline and workspace boundaries
description: "A design for shared buildkit phases, workspace normalization, frontend isolation, and issue #930."
tags: [buildkit, pipeline, workspace, frontends, elm]
status: draft
stale_after: 2026-10-05
sources:
  - id: issue-932
    title: Multi-frontend Morphir transformation pipeline
    resource: https://github.com/finos/morphir-scala/issues/932
    last_modified: 2026-07-28
  - id: issue-930
    title: Resolve Elm operator fixities from dependency sources
    resource: https://github.com/finos/morphir-scala/issues/930
  - id: morphir-elm
    title: finos/morphir-elm capability benchmark
    resource: https://github.com/finos/morphir-elm/tree/1956c36d3715851a2f215775a45395690746d801
  - id: elm-parse
    title: Current ElmParse effect
    resource: https://github.com/finos/morphir-scala/blob/d2abf88838da641fb7944c8d1569c9068eebdf4c/morphir/langkit/elm/core/src/morphir/langkit/elm/ElmParse.scala
  - id: stage
    title: Current Elm compiler Stage abstraction
    resource: https://github.com/finos/morphir-scala/blob/d2abf88838da641fb7944c8d1569c9068eebdf4c/morphir/langkit/elm/compiler/api/src/morphir/langkit/elm/compiler/Stage.scala
  - id: configuration-overview
    title: Morphir configuration overview
    resource: https://github.com/finos/morphir-scala/blob/d2abf88838da641fb7944c8d1569c9068eebdf4c/kb/bundles/morphir/morphir-configuration/overview.md
  - id: configuration-workspace
    title: Morphir workspace and project configuration
    resource: https://github.com/finos/morphir-scala/blob/d2abf88838da641fb7944c8d1569c9068eebdf4c/kb/bundles/morphir/morphir-configuration/workspace-and-project.md
  - id: configuration-workflows
    title: Morphir tasks and workflows
    resource: https://github.com/finos/morphir-scala/blob/d2abf88838da641fb7944c8d1569c9068eebdf4c/kb/bundles/morphir/morphir-configuration/tasks-and-workflows.md
  - id: unified
    title: unified processor architecture
    resource: https://github.com/unifiedjs/unified/tree/ba1af683ba597228b736566752668e7132295d38
  - id: unist
    title: Universal Syntax Tree specification
    resource: https://github.com/syntax-tree/unist/tree/8b10b6113c1463113b879f423d605547e04efd0d
---

# Multi-frontend pipeline and workspace boundaries

Morphir buildkit should provide language-neutral phase and workspace contracts while each frontend retains its own
effects, native manifests, syntax trees, and compiler accommodations. `morphir.toml` and `morphir.yaml` own workspace
orchestration. Manifest adapters normalize ecosystem projects. Package resolution supplies frontend-readable sources
without making an Elm cache or compiler sandbox part of the shared model.

This is a mutable Design Note for [intent 0007](../../../intent/0007-multi-frontend-morphir-transformation-pipeline.md).
The boundaries below are settled enough to guide refinement, but implementation feedback has not yet made their
alternatives and consequences stable enough for an immutable Decision Record. Package identity, ranges, locks,
registries, and materialization evolve separately in the
[Package URL-centered package-management Design Note](/design/package-url-package-management.md).

## Observed facts

- `ElmParse` is an Elm-specific Kyo effect. Its operations expose Elm parse options and frontend-internal diagnostic,
  reporting, and fatality policy; it is not currently a language-neutral phase contract.
- The reusable `Stage` abstraction lived under the Elm compiler API even though sequencing typed input and output
  values is not intrinsically Elm-specific. It has since moved to `morphir/buildkit/core` (package
  `morphir.buildkit`).
- Existing Morphir configuration knowledge assigns workspace members, tasks, workflows, outputs, and toolchain policy
  to one model with TOML and YAML serializations. The normative merge model has six sources. The pinned Rust
  implementation loads a smaller subset and handles daemon projects separately. The
  [configuration overview](../../morphir-configuration/overview.md) and the
  [Morphir Rust configuration reference](../../morphir-configuration/morphir-rust-configuration-cdfa6c63.md)
  document this difference.
  This configuration does not erase `elm.json`, `morphir.json`, or future native manifests.
- Morphir transforms projects rather than isolated files. A frontend needs normalized project inputs and resolved
  dependency modules before it can produce Morphir IR.
- Issue #930 needs dependency source to resolve Elm operator fixities. That need is narrower than delivering the
  complete standard pipeline, executor, or package-management implementation.
- unified demonstrates reusable processor composition, while unist demonstrates structural interoperability without
  requiring every language to share one concrete tree representation.

These are source observations and constraints. They do not by themselves decide Morphir's public API.

## Current refinement proposals

Buildkit is proposed as two dependency-directed layers:

- `morphir/buildkit/core` is a Morphir-agnostic, Kyo-native, cross-platform task-graph toolkit. It owns typed stages,
  immutable pipeline definitions, validation into execution plans, run-scoped context, deterministic outcomes, and
  an initial sequential executor whose graph still represents potential parallelism.
- `morphir/buildkit` assembles the standard Morphir pipeline. It orders workspace loading, dependency resolution,
  frontend compilation, IR transformation, backend generation, checkpoints, and artifact reconciliation.

The standard assembly exposes typed `Frontend`, `Transformation`, and `Backend` plugins. Language CSTs, ASTs, and
frontend effects remain in their langkits. Each phase surfaces typed outcomes and diagnostics at its adapter boundary;
the shared pipeline propagates and aggregates those values, schedules ready stages, and applies its configured
stop-or-continue policy between phase results. Generic inspection uses explicit structural capabilities such as
`QueryableTree` and `UnistProjection`; buildkit does not introduce one universal mutable node model.

Pipeline definitions contain reusable configuration. Per-run inputs, provenance, progress, diagnostics, checkpoint
data, and outputs live in a run request and outcome, allowing one validated definition to execute repeatedly without
mutable shared state.

## Settled-enough refinement boundaries

The following boundaries are the current refinement direction. They guide dependent work, but remain in this Design
Note until implementation proves that their alternatives, consequences, and revisit conditions are stable enough for
an accepted Decision Record.

### Parse and compile contracts

`ElmParse` remains an Elm frontend effect handled inside the Elm adapter. Shared `Parse[I, O, D]` and
`Compile[I, O, D]` names describe independently invokable buildkit phase contracts over typed stages; they are not
global effects that every frontend must implement internally. A frontend decides how its own conditions become
diagnostics, how they are reported internally, and which are fatal before returning a typed phase result. At the
boundary, buildkit propagates and aggregates the returned diagnostics, schedules subsequent phases, and applies
shared stop-or-continue policy without reinterpreting frontend semantics.

The generic `Stage` has moved to buildkit core. Its contract must remain cross-platform and must not acquire an Elm,
filesystem, process, or network dependency.

Buildkit core now also carries the first `Pipeline` slice: a sealed trait whose public variants are a buildable
`PipelineDef` and a validated `SealedPipeline`, with path-structured node identities assigned at seal time
(explicit id, else label slug, else position), whole-chain error accumulation into a `MorphirException`-rooted
`SealErrors`, and a deterministic sequential executor that emits `PipelineEvent`s and scopes a provenance `Local`
per node. Fork, join, fan-out and conditional branches remain open design, and nothing in the linear slice
prejudges the join representation.

The graph shapes landed as structured combinators rather than a general DAG: `par` forks heterogeneously with
Zippable tuple-flattening (package `morphir`), `fanOut` runs a nested sealed pipeline per element of a runtime
Chunk with element-indexed id paths, and `branch`/`when` conditionals emit `Skipped` for the untaken arm's nodes.
Joins are ordinary stages consuming the tuple or Chunk a shape yields. The sequential executor now guarantees
event balance via `kyo.kernel.Effect.catching`: every `NodeStarted` closes with a `NodeFinished`, for non-fatal
panics and normal completion alike, and every node on an untaken arm emits a standalone `Skipped` without ever
starting. Sealed pipelines render deterministic mermaid flowcharts through `toMermaid`. Typed halting is now
closed by the report executor on this branch; parallel execution remains open.

### Workspace ownership and normalization

The shared Morphir configuration model, serialized as `morphir.toml` or `morphir.yaml`, owns workspace discovery,
members, task and workflow policy, outputs, and toolchain selection. Native manifests remain authoritative for
ecosystem semantics and normalize through adapters into a shared project model. Explicit request values override
Morphir configuration. Morphir configuration overrides adapter defaults. Contradictory native claims produce
diagnostics rather than being silently merged.

This arrangement makes Morphir orchestration explicit without inventing a second representation of every ecosystem's
package and compiler rules.

### Package-management insulation

Buildkit and frontend contracts consume canonical package requirements, resolved identities, and materialized source
views. They do not expose registry implementation details, cache layouts, credentials, or compiler sandboxes. The
package-management Design Note owns the still-evolving choices around Package URL types, VERS semantics, locks,
integrity, source descriptors, and launch backends.

Unresolved package choices do not block task-graph, runtime, or Morphir-Elm restoration work whose contracts depend
only on the boundary above.

### Issue #930 seam

Issue #930 consumes shared package requirements, resolution, materialization, and source-reading capabilities through
`ElmProject`, which turns resolved Elm sources into the operator information the frontend needs. It can land with the
minimal required interpreters before the complete standard pipeline exists.

That seam receives resolved sources. It does not depend on `ELM_HOME`, Elm cache paths, `registry.dat`, or the
Elm compiler-sandbox adapter. A cache or sandbox may help an interpreter or compiler invocation privately, but neither
is part of the issue #930 API.

## Open questions

1. What is the smallest public `Stage` outcome that preserves typed values, diagnostics, provenance, and halting
   without forcing every frontend onto one internal effect?
2. Which validation belongs in an immutable pipeline definition, and which checks require per-run workspace data?
3. How do plugin repetition, replacement, and ordering remain explicit without an untyped option-merging model?
4. What normalized project fields are genuinely shared across ecosystems, and which must remain adapter-owned?
5. How should contradictory information from Morphir configuration, `morphir.json`, and a native manifest be
   reported when no source has universal authority?
6. Which minimal package interpreters does issue #930 require before the full package-management intent is delivered?
7. Which of the settled-enough boundaries need separate Decision Records rather than one combined record?

## Decision and implementation gates

A pipeline/workspace Decision Record is ready only when a working vertical slice validates the phase contracts,
workspace precedence, frontend dependency direction, and issue #930 seam, and when rejected alternatives and revisit
conditions can be stated precisely. Until then this note changes with research and implementation feedback.

Independent work may proceed against the settled-enough boundaries. A dependent issue must not assume an answer to
an open question unless it records that assumption and keeps it behind the relevant contract.
