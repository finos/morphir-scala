---
type: Intent
title: Independent version streams
description: "Version the libraries, the Mill plugin family and the desktop application from their own changelogs and tag streams."
state: Released
kind: feature
breaking: false
created: 2026-08-18
state_since: 2026-08-18
tags: [versioning, release, build]
capability: morphir/morphir-scala:/packaging-and-release.md
---

# 0032 — Independent version streams

Version the libraries, the Mill plugin family and the desktop application from their own changelogs and tag streams.

## Problem

Every published artifact in this repository took its version from one place: `SnapshotVersion.select` over
`VcsVersion.vcsState()`, which resolves the nearest git tag of any shape. One tag stream served the libraries, the
Mill plugin family and the desktop application alike, so cutting a release for one meant a tag that the others'
nearest-tag lookup could also see. The desktop application's release cadence has nothing to do with the libraries',
and the Mill plugins' consumers are other people's Mill builds. A plugin fix should not require cutting a library
release to reach them. The publish machinery already separates these destinations (`ci.publish` distinguishes
`PublishKind` `Libraries` from `Plugins`; `ci.sonatype.libraries` and `ci.sonatype.plugins` are separate commands);
only the version was still shared.

## Approach

Each independently versioned area now composes its own version from two sources: a **changelog** supplies the
release line, whose topmost undated heading is the number a build is heading toward, and **git** supplies
everything after it (commit distance, branch, revision, dirty state). Three areas exist: the repository root
(the libraries, tagged `v*`), the Mill plugin family under `mill-plugins/morphir` (tagged `mill-plugins/v*`), and
`morphir/desktop` (tagged `desktop/v*`). An area is declared by mixing `MorphirVersionedModule` into the module that
owns it; its `versionNamespace`, `startingVersion` and `changelog` are Task-valued members so a `package.mill.yaml`
can set them as literals. Release routing follows the tag's namespace rather than a single "any tag" lookup: Mill's
own `VcsVersion` has no `--match`, so pattern-filtered tag resolution (`GitStream`, `TagStream`) replaced it for
every stream, not only the new ones. Without that, the first `desktop/v*` tag would have become the libraries'
nearest tag too, and broken their version derivation. `SnapshotVersion` and its tests are retired now that nothing
composes a version through them.

The library snapshot coordinates changed meaning as a result. Under the old scheme, `0.5.0-M04-12-SNAPSHOT` meant
twelve commits past the `0.5.0-M04` tag that had already shipped. Under the new scheme, a coordinate names the
release line the build is heading *toward*: `0.6.0-M01-12-SNAPSHOT` means twelve commits into work that will become
`0.6.0-M01`. Anyone explaining an old library coordinate needs this distinction, because the number after the base version
no longer counts forward from the last release, it counts down to the next one. The Mill plugin family's own
floor starts at `0.5.0-M04` rather than `0.1.0`, the last version tagged in the repository's previously shared
stream, not a version the plugin family itself ever published; the family has never been published on its own.
The floor just keeps its eventual first release from reading as a regression against that shared history, even
though the plugins now version independently of the libraries.

See the [Packaging and release Capability](../morphir/morphir-scala/packaging-and-release.md) for the mechanics:
the Versions section, the release-routing table and Figure 1. The three streams are wired in
`ci/MorphirCi.mill`, `build.mill` and `MorphirVersionedModule`.
