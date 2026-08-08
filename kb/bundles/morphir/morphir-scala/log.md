# Log

## 2026-08-07

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

## 2026-07-28

* **Creation**: Bundle created.
* **Creation**: Added [Knowledge Base Tooling](/knowledge-base-tooling.md).
* **Creation**: Added [Continuous Integration](/continuous-integration.md).
* **Creation**: Added [Build System](/build-system.md).
* **Creation**: Added [Cross-Platform Targets](/cross-platform-targets.md).
