# Log

## 2026-10-31

* **Creation**: [0042](/0042-deprecate-the-classic-zio-based-morphir-library.md) deprecates the classic ZIO-based library
  family (InProgress, shipping in 0.5.0-M12) and [0043](/0043-remove-the-classic-zio-based-morphir-library.md)
  removes it later; 0043 supersedes [0028](/0028-remove-or-absorb-morphir-toolkit.md).

## 2026-10-30

* **Update**: Released [0020](/0020-github-graphql-connector.md) in 0.5.0-M11, linking the
  [GitHub GraphQL Connector](../morphir/morphir-scala/github-graphql-connector.md) Capability. M11 is the first
  milestone with its Scala.js and Scala Native artifacts on Maven Central.
* **Creation**: [0041](/0041-live-github-http-on-scala-native.md), live GitHub HTTP on Scala Native, split out of
  [0020](/0020-github-graphql-connector.md). It waits on a Kyo Native artifact with working kqueue bindings.
* **Update**: Moved [0037](/0037-morphir-scala-elm-frontend-extension.md) back to Backlog. The provider shipped,
  but three acceptance criteria are unmet: the `elm_extension` host suite, schema validation and IR equivalence with
  `morphir-elm`.
* **Update**: Kept [0020](/0020-github-graphql-connector.md) InProgress. Its Scala.js and Scala Native artifacts never
  reached Maven Central; it releases once a milestone publishes them.
* **Update**: Moved the Kyo 1.0.0-RC6 claims in [0020](/0020-github-graphql-connector.md),
  [0033](/0033-markdown-compilation.md) and [0037](/0037-morphir-scala-elm-frontend-extension.md) to RC7 after
  re-checking them. The 0037 document-version gap is closed by Kyo PR 1920, released in RC7.

## 2026-09-23

* **Update**: Cancelled [0017](/0017-morphir-cli-buildkit-integration.md) and
  [0038](/0038-publish-the-morphir-scala-cli-to-github-releases.md) after retirement of the Scala general CLI.
  The Rust CLI in `finos/morphir` owns human-facing commands. Native MEP extension packaging remains part of
  [0037](/0037-morphir-scala-elm-frontend-extension.md).

## 2026-08-11

* **Update**: Moved [intent 0008](/0008-buildkit-core-task-graph.md) into InProgress and delivered its first slice —
  the cross-platform `morphir/buildkit/core` module, the generic `Stage` relocated out of the Elm compiler API, and
  the jvm, js and native variants wired to publish as `morphir-buildkit-core`. The `Pipeline` graph, sealing
  validation and the sequential executor remain outstanding.

## 2026-08-07

* **Update**: Reworked dense intent summaries into short paragraphs and lists without changing their design status or
  meaning.
* **Update**: Linked intents 0007 and 0010 to the mutable pipeline/workspace Design Note and recorded the current Elm-specific `ElmParse`, frontend-internal diagnostic policy, shared phase propagation, workspace-ownership, and issue #930 boundaries without prematurely accepting a Decision Record.
* **Update**: Strengthened intent 0013 acceptance around a used transitive unpublished symbol and an externally egress-denied resolution, materialization, and compilation run through both launch backends.

## 2026-08-06

* **Update**: Moved [intent 0013](/0013-pluggable-package-resolution-and-materialization.md) into Refinement and linked the evolving Package URL package-management design without blocking independent buildkit work.
* **Update**: Clarified [intent 0007](/0007-multi-frontend-morphir-transformation-pipeline.md) around Package URL identity, VERS requirements, and the independent packaging research lane.
* **Update**: Linked intents 0007 and 0013 to the MoonBit registry/source-materialization reference and scoped Package URL work to proving the Morphir convention without upstream proposals or Elm type ownership.
* **Update**: Refined intent 0013 around source-distributed Morphir packages, location-independent identity, and immutable locked materialization.

## 2026-07-28

* **Creation**: Intent bundle created.
