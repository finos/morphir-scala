---
type: Intent
title: Multi-frontend Morphir transformation pipeline
description: "Design a shared transformation pipeline for mixed-language workspaces with pluggable, reproducible package management."
state: Refinement
kind: feature
breaking: false
created: 2026-07-28
state_since: 2026-07-28
issue: 932
tags: [pipeline, workspace, package-management]
sources:
  - id: morphir-elm
    title: finos/morphir-elm capability benchmark
    resource: https://github.com/finos/morphir-elm/tree/1956c36d3715851a2f215775a45395690746d801
  - id: unified
    title: unified processor architecture
    resource: https://github.com/unifiedjs/unified/tree/ba1af683ba597228b736566752668e7132295d38
  - id: unist
    title: Universal Syntax Tree specification
    resource: https://github.com/syntax-tree/unist/tree/8b10b6113c1463113b879f423d605547e04efd0d
---

# 0007 — Multi-frontend Morphir transformation pipeline

Design a shared transformation pipeline for mixed-language workspaces with pluggable, reproducible package management.

## Problem

Morphir-scala has the beginning of a transformation pipeline, but only as an Elm-specific implementation. `ElmParse`
already centralises parse options, diagnostics, and halting, while the language-neutral `Severity` and `Reported[D]`
types live in `langkit/core`. There is no shared abstraction that lets another frontend participate in the same
source-to-IR process without recreating that plumbing and its policies.

The missing boundary is larger than parsing. Morphir transforms projects, not isolated files: it must discover source,
interpret both Morphir and language-native manifests, resolve dependencies, make their modules available to frontend
stages, and produce distributable packages. Without a shared workspace and package-management layer, each langkit is
pushed toward its own resolver and cache assumptions. Issue #930 is the immediate example: resolving Elm operator
fixities requires dependency source, but an Elm-only resolver would deepen the split this work is intended to remove.

The absence is already constraining names and design. `Parse` and `Compile` remain reserved for a future general
pipeline, while `ElmParse` cannot be confidently generalised or established as an instance until that pipeline owns
the semantics. The decisions and their rationale therefore need to be made explicitly before more frontend-specific
infrastructure hardens around them.

The issue's workspace discussion also needs to be reconciled with context already present in the knowledge base. The
draft Morphir configuration model gives `morphir.toml` responsibility for workspace discovery, member projects, tasks,
workflows, and an intrinsic `morphir.pipeline.compile` action; see [Configuration Overview](https://github.com/finos/morphir-scala/blob/d2abf88838da641fb7944c8d1569c9068eebdf4c/kb/bundles/morphir/morphir-configuration/overview.md),
[Workspace and Project](https://github.com/finos/morphir-scala/blob/d2abf88838da641fb7944c8d1569c9068eebdf4c/kb/bundles/morphir/morphir-configuration/workspace-and-project.md), and
[Tasks and Workflows](https://github.com/finos/morphir-scala/blob/d2abf88838da641fb7944c8d1569c9068eebdf4c/kb/bundles/morphir/morphir-configuration/tasks-and-workflows.md). The pipeline design must decide how that
model relates to `morphir.json`, `elm.json`, and future ecosystem manifests rather than silently creating a competing
workspace model. Those configuration documents are draft, so alignment is a design input, not an assertion that their
current shape is final.

## Approach

Treat [issue #932](https://github.com/finos/morphir-scala/issues/932) as an umbrella design effort and decompose it
after the shared boundaries are settled. The design should preserve these constraints:

- One language-neutral orchestration model carries a project through frontend-specific stages from source text to CST,
  AST, and Morphir IR. Stage implementations remain language-specific; diagnostic collection, halting, and execution
  policy are shared.
- A workspace may contain projects from different frontend ecosystems. Adapters interpret `morphir.toml`,
  `morphir.json`, `elm.json`, and future native manifests into one internal workspace/project model, with explicit
  discovery, precedence, and merge rules.
- Package identity is separate from package location. Resolution produces a concrete, reproducible location for a
  coordinate, after which packages can be materialised into a local cache and read without embedding registry policy
  in frontend stages.
- Package-management operations form a small shared capability—candidate shape: a Kyo effect—with interpreters for
  the techniques chosen at launch and room for git, path, registry, vendored, cache, offline, and test implementations.
  Resolution must be pinnable and mirrorable so CI, developer machines, and air-gapped environments run the same
  pipeline under different policy interpreters.
- Morphir packages have a distributable form without depending on one registry implementation. A git-file index and
  local-directory registry are launch candidates because they support offline and service-free operation.
- Dependency coordinates use an ecosystem-spanning vocabulary if one fits. Package URL plus the companion version
  range specification is the leading candidate, but Elm's missing registered purl type and the boundary between
  coordinate qualifiers and separate location records require an explicit decision.

Refinement owes a written design or ADR covering the pipeline effect, workspace model, package-management operations
and interpreters, and package distribution. It must survey shelm's coordinate/location split, MoonBit's registry
interface and git index, Elm's cache layout, and the existing `ElmPackages` resolution ladder. MoonBit is AGPL-3.0, so
its architecture may inform the design but its code must not be copied or adapted.

The design must also decide whether `ElmParse` becomes the shared effect or remains Elm's instance of a new one, what
claims the `Parse` and `Compile` names, which package backends ship first, and how issue #930 consumes shared resolution
without forcing the rest of the pipeline to be delivered at once. The result should be a set of independently
implementable follow-up issues, not a single implementation change.

## Buildkit direction

The shared pipeline belongs under `morphir/buildkit/`, split by dependency direction:

- `morphir/buildkit/core` is a published, Morphir-agnostic, Kyo-native, cross-platform task-graph toolkit. It owns
  typed stages and pipelines, stable node identity, sequence, fork, join, runtime collection fan-out, conditional
  branches, deterministic outcomes, and the initial sequential executor. The generic `Stage` currently in the Elm
  compiler API moves here.
- `morphir/buildkit` is the published standard Morphir assembly. It depends on `core` and owns the ordering of
  workspace loading, dependency resolution, frontend compilation, IR transformation, backend generation, checkpoint
  production, and artifact reconciliation.

The root buildkit defines extension points for `Frontend`, `Transformation`, and `Backend` without depending on Elm
or any other frontend. Elm supplies an adapter from its own module, preserving the dependency direction
`Elm -> buildkit`. Both graph definitions and the standard Morphir assembly remain cross-platform; filesystem,
process, network, cache, and artifact persistence live behind effects and platform-specific interpreters.

The graph represents potential parallelism from the beginning, even though the first executor may run ready nodes
one at a time. Later parallel execution changes the interpreter, not user-authored pipeline definitions.

## Standard data flow

The standard assembly accepts a `BuildRequest` and:

1. Loads and normalizes workspace and ecosystem configuration.
2. Resolves and materializes dependencies.
3. Snapshots sources and compares them with an optional prior checkpoint.
4. Selects full or incremental frontend processing.
5. Builds project and module dependency graphs.
6. Fans out independent work while preserving dependency order.
7. Produces and validates Morphir IR distributions.
8. Applies selected IR-to-IR transformations.
9. Fans out selected backend generators.
10. Joins their desired artifact sets and reconciles them through output interpreters.
11. Returns a structured `BuildOutcome` and the next checkpoint.

A frontend maps ecosystem sources and manifests to Morphir IR. A transformation maps Morphir IR to Morphir IR. A
backend maps Morphir IR and target options to a desired artifact set; it does not write files itself. This keeps the
same pipeline usable with in-memory BDD interpreters, browser or WASM hosts, and production CLI interpreters.

## Outcomes and execution semantics

A node may succeed, report diagnostics while retaining a value, or halt and prevent dependent nodes from running.
Independent branches continue when their inputs remain valid. Blocked dependents are marked skipped with the
blocking node recorded instead of repeating the same failure downstream. Joins state whether they require every
branch or collect the successful subset.

The executor returns values, node statuses, diagnostics, progress events, timings, and checkpoint metadata.
Diagnostics and results are deterministic: graph declaration or topological order is primary, with domain location
as a secondary key. Every future executor must preserve the sequential executor's observable ordering.

Domain failures remain typed values. Unexpected interpreter defects may use Kyo failure effects internally, but
library users, BDD drivers, and CLI commands consume `BuildOutcome` rather than thrown exceptions or printed output.

## Validation against morphir-elm

The design was checked against `finos/morphir-elm` at commit
[`1956c36d`](https://github.com/finos/morphir-elm/tree/1956c36d3715851a2f215775a45395690746d801)
as a capability benchmark, not a compatibility target. That implementation requires all-source parsing with error
accumulation, module dependency sorting and cycle detection, tree shaking, name and type resolution, full and
incremental compilation, concurrent dependency loading, backend selection, multi-file generation, and output
reconciliation.

Sequence, fork, and join alone were not enough to represent those capabilities. Runtime fan-out, conditional
full-versus-incremental paths, prior checkpoints, progress events, partial failure, backend extension points, and
artifact-set reconciliation are therefore explicit requirements above.

## Lessons from unified and unist

The design was also compared with
[`unified`](https://github.com/unifiedjs/unified/tree/ba1af683ba597228b736566752668e7132295d38) and the
[`unist` specification](https://github.com/syntax-tree/unist/tree/8b10b6113c1463113b879f423d605547e04efd0d).
Their value is architectural rather than API compatibility.

From unified, buildkit adopts the separation between reusable pipeline configuration and per-run execution, the
ability to invoke parsing, transformation, and generation phases independently, and reusable processor templates
that consumers can derive and extend. Buildkit expresses these ideas through public typed plugins, immutable presets,
and a validated execution plan. Sealing a definition is the immutable equivalent of freezing a unified processor:
it validates the assembled graph without introducing mutable shared state. Plugin repetition and replacement are
explicit typed operations; buildkit does not adopt untyped option merging or runtime JavaScript plugin discovery.

The root Morphir assembly distinguishes frontend plugins, representation-preserving inspection plugins, typed
representation-changing transformation plugins, and backend plugins. Pipeline configuration belongs to the immutable
definition. Per-run inputs, provenance, diagnostics, progress, results, and checkpoint data belong to the build
request and outcome so derived presets can be reused safely across runs.

From unist, buildkit adopts the principle that interoperability needs a small structural protocol, not one universal
tree representation. Morphir's existing `QueryableTree` and `UnistProjection` already provide that direction:
language CSTs, ASTs, Morphir IR, and backend models remain strongly typed, while explicit adapters can expose stable
children, leaf text, named fields, and optional half-open source positions to generic tooling. Generated nodes may
have no position. Node-specific metadata remains in domain models or separate typed capabilities rather than an
untyped buildkit data map.

Typed values continue to flow between stages. Run-scoped context carries configuration, provenance, diagnostics,
progress, and outputs. The existing tooling-layer `VFile` is not promoted into buildkit core because its
filesystem-shaped contents and generic attribute maps are not a neutral representation for workspaces, IR, and
generated artifacts. A universal document envelope remains deferred until a concrete consumer demonstrates that
typed stage values plus run context are insufficient.

The general mechanisms and the fact-to-guidance reasoning are documented separately in the
[Programming Language Tooling](https://github.com/finos/morphir-scala/tree/main/kb/bundles/programming-language-tooling)
bundle, especially
[transformation pipelines](https://github.com/finos/morphir-scala/blob/main/kb/bundles/programming-language-tooling/transformation-pipelines.md)
and
[guidance for a Morphir toolchain](https://github.com/finos/morphir-scala/blob/main/kb/bundles/programming-language-tooling/morphir-toolchain-guidance.md).

## Child intents

- [0008 Buildkit core task graph](/0008-buildkit-core-task-graph.md)
- [0009 Standard Morphir build pipeline](/0009-standard-morphir-build-pipeline.md)
- [0010 Elm frontend buildkit adapter](/0010-elm-frontend-buildkit-adapter.md)
- [0011 Buildkit BDD vertical slice](/0011-buildkit-bdd-vertical-slice.md)
- [0012 Workspace and manifest normalization](/0012-workspace-and-manifest-normalization.md)
- [0013 Pluggable package resolution and materialization](/0013-pluggable-package-resolution-and-materialization.md)
- [0014 Backend generation and artifact reconciliation](/0014-backend-generation-and-artifact-reconciliation.md)
- [0015 Incremental builds and checkpoints](/0015-incremental-builds-and-checkpoints.md)
- [0016 Parallel task-graph executor](/0016-parallel-task-graph-executor.md)
- [0017 Morphir CLI buildkit integration](/0017-morphir-cli-buildkit-integration.md)
- [0018 Typed pipeline plugins and immutable presets](/0018-typed-pipeline-plugins-and-immutable-presets.md)
- [0019 Typed tree-processing interoperability](/0019-typed-tree-processing-interoperability.md)
