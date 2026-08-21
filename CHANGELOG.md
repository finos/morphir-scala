# Changelog

All notable changes to the Morphir libraries are recorded here. The topmost **undated** heading is
the shape of the next release, and it is what CI stamps on every build. Dated headings are history.

`## [Unreleased]` is optional and carries no build meaning; use it for entries not yet assigned to
a release.

## [Unreleased]

### Changed
- The repository moved to trunk-based development. Pull requests target `main` and merge into it; the `develop`
  integration branch, its promotion pull request and its back-migration are retired. Snapshots publish from `main`
  alone, and `squire branch refresh` now requires `--target`. See decision 0014.

## [0.5.0-M05]

The first release cut through the independent version streams, and the first since this changelog
existed. It carries roughly seventy merged pull requests; the notable ones are below, and the GitHub
release notes hold the full list, which release-drafter builds from the pull requests themselves.

### Added
- The Morphir desktop application: an Electron shell hosting morphir-ui, with Scala.js in both
  processes over a kyo-jsonrpc seam. It is packaged for macOS, Linux and Windows and published to a
  GitHub Release and to Maven Central, each asset carrying a `.sha256` sidecar (#986, #987, #988).
- A knowledge base under `kb/`, with the kb tooling, Decision Records as a third register, and an
  intent lifecycle that CI checks (#936, #939, #942, #948).
- Buildkit core: a Morphir-agnostic typed task graph with the outcome executor, alongside
  morphir-prelude (#966, #971).
- A GitHub connector, and the published library families (#983).
- The Mill Morphir plugins, dogfooded by this repository (#955).
- Mirrored Morphir IR sources, validated against the schemas (#945).
- Independent version streams: the libraries, the Mill plugins and the desktop application each take
  their version from their own changelog and tag stream (#991).
- A Kyo runtime data foundation, moving to kyo 1.0.0-RC6 (#950).
- Elm port and effect module metadata, carried through lowering (#937).

### Changed
- Snapshot coordinates now count toward the release the changelog names next, rather than away from
  the last one that shipped. `0.5.0-M05-12-SNAPSHOT` means twelve commits into work that will become
  `0.5.0-M05` (#991).
- SNAPSHOTs publish from `develop` as well as `main` (#970).
- Squire's tooling is Scala rather than Python (#956).
- Durable task tracking moved to beads (#941).

### Fixed
- Closure parameter patterns are retained in the model (#952).
- Sonatype publishing is serialized, so it no longer crashes in SubstituteLogger (#958).
- A breaking-change label no longer drafts a 1.0.0 release (#951).

## [0.5.0-M04] - 2026-04-22

### Added
- Released before this changelog existed; see the GitHub releases for detail.
