# Changelog

All notable changes to the Morphir libraries are recorded here. The topmost **undated** heading is
the shape of the next release, and it is what CI stamps on every build. Dated headings are history.

`## [Unreleased]` is optional and carries no build meaning; use it for entries not yet assigned to
a release.

## [0.5.0-M08]

### Changed
- `morphir-scala-elm` workspace discovery accepts an explicit package name in Elm or Morphir spelling and reports its
  normal form as the project name: the name splits into segments on `/` and `.`, each segment splits into words as
  morphir-elm `Name.fromString` does, and the words join with `-`. `My.Package`, `My/Package` and `my/package` all
  report `my/package`. A name that has no segments, or a segment that has no letters or digits, is refused with
  `workspace.project-name.invalid`. This is the contract of finos/morphir#917, which the other Elm providers follow.

## [0.5.0-M07] - 2026-09-23

### Added
- `morphir-scala-elm` serves workspace discovery, so `morphir compile --input Example.elm --extension
  morphir-scala-elm` works with the one compile route of the `morphir` CLI. It declares the `workspace` capability with
  protocol `0.1.0-draft.1` and answers `morphir.workspace.discover` for a selection of Elm files with the same project
  name, exposed modules and failure codes as the other Elm providers. An extension index record for this release must
  list the `workspace` capability, because the host refuses a session whose capabilities differ from the record
  (#1070).

## [0.5.0-M06] - 2026-09-19

### Added
- The Elm MEP frontend executable `morphir-scala-elm` on the GitHub release: a GraalVM Native Image
  for macOS ARM64 and x64, Linux ARM64 and x64, and Windows x64, each with a `.sha256` sidecar, staged
  and verified with the CLI packages. It is a Morphir Extension Protocol 0.1 process that compiles the
  supported single-file Elm subset to Morphir IR v3, and it reports the release version as its
  provider version. The `morphir` CLI from finos/morphir runs it as the `morphir-scala-elm` extension
  (#1043).

### Changed
- CI and release promotion run on Java 26 (Temurin `26.0.2+10`), and the test matrices moved from
  Java 25 to Java 26 with them (#1060). The native CLI and MEP executables still build with GraalVM
  Community 25, because GraalVM Community has no JDK 26 release.

### Removed
- The Morphir desktop application (the Electron shell hosting morphir-ui) and its `appkit-electron`
  secret-storage integration. The Morphir desktop/web UI now lives at
  [finos/morphir-ui](https://github.com/finos/morphir-ui). `morphir-ui` remains in this repository
  as the shared library behind the local web host (`morphir server`), and `morphir-scala` continues
  as the Scala implementation and capability provider (markdown tooling, the GitHub connector).

## [0.5.0-M05] - 2026-08-27

The first release cut through the independent version streams, and the first since this changelog
existed. It carries roughly seventy merged pull requests; the notable ones are below, and the GitHub
release notes hold the full list, generated from the pull requests themselves.

### Added
- Native CLI packages on the GitHub release: GraalVM Native Image archives for macOS ARM64 and x64,
  Linux ARM64 and x64, and Windows x64, plus an executable JVM assembly as the portable package and
  the Windows ARM64 path. Every asset carries a `.sha256` sidecar and is verified again after upload
  (#1038).
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
- The local browser host command is now `morphir server`; the earlier `morphir serve` spelling is
  removed (#1036).
- Releases run in two phases: pushing a release tag stages a draft GitHub release with verified
  assets, and publishing that draft is what promotes the release to Maven Central (#1038).
- The repository moved to trunk-based development. Pull requests target `main` and merge into it;
  the `develop` integration branch, its promotion pull request and its back-migration are retired.
  Snapshots publish from `main` alone, and `squire branch refresh` now requires `--target`. See
  decision 0014.
- Snapshot coordinates now count toward the release the changelog names next, rather than away from
  the last one that shipped. `0.5.0-M05-12-SNAPSHOT` means twelve commits into work that will become
  `0.5.0-M05` (#991).
- Squire's tooling is Scala rather than Python (#956).
- Durable task tracking moved to beads (#941).

### Removed
- `Concept.splitFrontmatter` and its `closingFence` helper are gone from the public API of
  `morphir-knowledge-okf` on JVM, JS and Native, with no deprecation shim. Where a frontmatter fence
  ends is a syntax question the Markdown parser now answers: parse with a frontmatter-enabled profile
  (`MdProfile.commonmark.withYamlFrontmatter`) and read `Root.frontmatter` (#1025).

### Fixed
- Closure parameter patterns are retained in the model (#952).
- Sonatype publishing is serialized, so it no longer crashes in SubstituteLogger (#958).
- A breaking-change label no longer drafts a 1.0.0 release (#951).

## [0.5.0-M04] - 2026-04-22

### Added
- Released before this changelog existed; see the GitHub releases for detail.
