# Log

## 2026-08-15

* **Update**: Recorded that GitHub list paging takes opaque `PageSize`, that `GitHubException` has a safe `Render`,
  and that appkit `SecretException` extends `MorphirException`, in
  [Published library families](/design/published-library-families.md) and
  [GitHub token providers and appkit secrets](/design/github-token-providers-and-appkit-secrets.md).
* **Update**: Recorded that process auth for `gh` and `security` uses shared `kyo.Command` spawn with concurrent
  stream drains on JVM, Node, and Scala Native, in
  [GitHub token providers and appkit secrets](/design/github-token-providers-and-appkit-secrets.md).
* **Update**: Recorded that the GitHub client failure ADT is `GitHubException` and extends `MorphirException`, in
  [Published library families](/design/published-library-families.md).
* **Update**: Recorded that `TokenProvider.gitHubCli` and `SecretStore.macOsKeychain` spawn `gh` and
  `security` on Scala Native, in
  [GitHub token providers and appkit secrets](/design/github-token-providers-and-appkit-secrets.md).
* **Update**: Recorded that GitHub public models have `Render` instances, in
  [Published library families](/design/published-library-families.md).
* **Update**: Recorded that GitHub issue, pull request, and discussion numbers form the `GithubNumber` union, with
  `@targetName` overloads of `fold`, in
  [Published library families](/design/published-library-families.md).
* **Update**: Recorded that GitHub issue, pull request, and discussion numbers, connection cursors, and discussion
  comment ids are opaque types, in
  [Published library families](/design/published-library-families.md).
* **Update**: Recorded that nested GitHub comments are a `ConnectionPage`, with `listIssueComments`,
  `listPullRequestComments`, and `listDiscussionComments`, in
  [Published library families](/design/published-library-families.md).

## 2026-08-14

* **Update**: Hosted CI mill invocations pass `--ticker false`. See [Continuous Integration](/continuous-integration.md).
* **Update**: The publish job now invokes Mill `ci.publish` (destination fan-out over `ci.sonatype.*`) rather than `mise run publish:sonatype`. The Release step converts Morphir `GPG_*` names through `ci.sonatype.writeMillEnv` before that mill. Live Central upload is the first `develop` publish job after merge. See [Continuous Integration](/continuous-integration.md).
* **Creation**: Added [Keep compiling Mill Morphir plugins into the metabuild](/decisions/0012-keep-source-metabuild-for-mill-morphir-plugins.md).
* **Update**: Sonatype publication now derives its module set from Mill
  (`__.publishSonatypeCentral`) rather than a hand-maintained script list, and publishes the Mill Morphir plugin
  family (`org.finos.morphir.mill`) alongside the library modules. Pre-publish verification: `mill-libs-scalalib_3`
  for the pinned Mill version is on Maven Central; plugin `publishArtifacts` succeed; generated POMs declare
  `mill-libs-scalalib` as `provided`; `integration` is absent from the resolve inventory. Live Central SNAPSHOT
  resolution is confirmed by the next `develop` publish job after merge. See
  [Continuous Integration](/continuous-integration.md).
* **Update**: Recorded that GitHub listing methods return `ConnectionPage` and take `after` / `first`, in
  [Published library families](/design/published-library-families.md).
* **Update**: Recorded that extra artifacts from a versioned suite are YAML `mvnDeps: !append` without a
  version, pinned by Mill `depManagement` on `MorphirSuiteBom` (`Deps.managedSuites`), in
  [Build System](/build-system.md).
* **Creation**: Added [Published library families are kit, connector, appkit, langkit, and knowledge](/decisions/0013-published-library-families.md).
* **Creation**: Added [Published library families](/design/published-library-families.md).
* **Creation**: Added [GitHub token providers and appkit secrets](/design/github-token-providers-and-appkit-secrets.md).

## 2026-08-13

* **Update**: CI now publishes branch snapshots from `main` and `develop` after the aggregate gate, as described in
  [Continuous Integration](/continuous-integration.md); `0.4.x` and tags keep VCS-derived milestone and release
  publishing.
* **Update**: Recorded the `morphir/buildkit/core` implementation of the seven current-position entries in
  [Buildkit task-graph capability](/design/buildkit-task-graph.md), including deviations from the settled
  positions and the still-unreachable `SkipReason.ConditionFalse`.
* **Creation**: Added [Buildkit task-graph capability](/design/buildkit-task-graph.md).
* **Update**: Renamed `StageEvent`/`Entered`/`Exited` to the shipped `PipelineEvent`/`NodeStarted`/`NodeFinished`
  names throughout the [multi-frontend pipeline and workspace Design Note](/design/pipeline-workspace-boundaries.md),
  and recorded that typed halting is now closed by the report executor on this branch, with parallel execution the
  remaining open item.

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
