# Changelog

The Mill Morphir plugin family (toolchain, javascript, elm-tooling, core and elm) versions and
releases together, independently of the Morphir libraries. Tags are `mill-plugins/vX`.

The topmost **undated** heading is the shape of the next release, and it is what CI stamps on every
build. Dated headings are history. `## [Unreleased]` is optional and carries no build meaning; use
it for entries not yet assigned to a release.

## [0.5.0-M05]

### Changed
- The plugins now version independently of the libraries. `0.5.0-M05` is chosen to sit above
  `0.5.0-M04` below, the last version tagged in the repository's previously shared stream, rather
  than to continue a published series. The plugin family has never been published on its own.

## [0.5.0-M04] - 2026-04-22

### Note
- Not a plugin-family release. `mill-plugins/` did not exist at the `v0.5.0-M04` tag, and
  `org.finos.morphir.mill` has no artifacts on Maven Central. The plugin family has never been
  published. This heading records only where the family's floor comes from: `0.5.0-M04` was the
  last version tagged in the repository's shared stream before the plugins gained their own tag
  namespace, so `startingVersion` here protects against regressing below that shared history even
  though nothing under `org.finos.morphir.mill` was ever released at it.
