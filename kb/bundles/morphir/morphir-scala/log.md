# Log

## 2026-08-13

* **Update**: Recorded the `morphir/buildkit/core` implementation of the seven current-position entries in
  [Buildkit task-graph capability](/design/buildkit-task-graph.md), including deviations from the settled
  positions and the still-unreachable `SkipReason.ConditionFalse`.
* **Creation**: Added [Buildkit task-graph capability](/design/buildkit-task-graph.md).

## 2026-08-08

* **Update**: Implemented the five publishable Mill Morphir plugins and a fresh-consumer `SNAPSHOT` acceptance test.
* **Update**: Restored all eight Elm-to-Morphir IR builds and the generated classic-runtime test path.
* **Update**: Proved generated Scala composition without replacing the host module's `compile` task.
* **Update**: Made Morphir module identity an opaque, parsed value with compile-time literal support.
* **Update**: Unified JavaScript tooling in one plugin and made validation errors source-located exceptions.

## 2026-08-07

* **Update**: Shortened bundle and design descriptions and converted dense enumerations into scannable lists.
* **Update**: CI now validates pull requests into `develop` and, after the aggregate gate, publishes traceable
  `develop` snapshots as described in [Continuous Integration](/continuous-integration.md).
* **Creation**: Added the evolving [multi-frontend pipeline and workspace Design Note](/design/pipeline-workspace-boundaries.md), separating observed facts, current proposals, settled-enough refinement boundaries, and open questions without prematurely creating a Decision Record.
* **Update**: Reconciled pipeline intent around Elm-specific `ElmParse`, frontend-internal diagnostic policy, shared phase propagation and scheduling, `morphir.toml` workspace ownership, and the dependency-source seam for issue #930.
* **Update**: Strengthened unpublished-package acceptance to prove a used transitive symbol and a preseeded, externally network-denied run through both launch backends.
* **Update**: Represented the pinned Mooncakes registry snapshot as an explicit Git remote and commit scope rather than an HTTP fragment.
* **Update**: Marked draft Design Notes and research as non-authoritative and corrected the Package URL component list and MoonBit checksum qualification.
* **Creation**: Added [Mill Morphir plugin architecture](/design/mill-morphir-plugin-architecture.md).

## 2026-08-06

* **Creation**: Added the evolving [Package URL-centered package-management Design Note](/design/package-url-package-management.md), including standards research and unpublished Elm package acceptance criteria.
* **Update**: Scoped Package URL work to proving the provisional Morphir type; upstream proposals and ownership of an Elm type are explicitly excluded.
* **Creation**: Added the [MoonBit registry, resolution, and source materialization reference](/design/moonbit-package-management.md), including the Git-backed line-delimited index and source-package boundary.
* **Update**: Expanded the MoonBit reference with annotated, schema-faithful examples of a per-module `.index` history and dashboard `data.jsonl` output.
* **Update**: Refined the package-management design around first-class Morphir source distributions with location-independent identity and immutable materialization.
* **Creation**: Accepted [Runtime closures retain parameter patterns](/decisions/0011-runtime-closures-retain-parameter-patterns.md), resolving the pre-release closure wire format.

## 2026-07-28

* **Creation**: Bundle created.
* **Creation**: Added [Knowledge Base Tooling](/knowledge-base-tooling.md).
* **Creation**: Added [Continuous Integration](/continuous-integration.md).
* **Creation**: Added [Build System](/build-system.md).
* **Creation**: Added [Cross-Platform Targets](/cross-platform-targets.md).
