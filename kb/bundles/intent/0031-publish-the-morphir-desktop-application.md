---
type: Intent
title: Publish the morphir-desktop application
description: "Ship the Electron desktop app from CI to GitHub Releases and Maven Central, with per-asset checksums and Mill-style platform naming."
state: InProgress
kind: feature
breaking: false
created: 2026-08-16
state_since: 2026-08-16
tags: [desktop, release, publishing]
---

# 0031 — Publish the morphir-desktop application

Ship the Electron desktop app from CI to GitHub Releases and Maven Central, with per-asset checksums and Mill-style platform naming.

## Problem

When a user wants to run the Morphir desktop application, they want to download and launch it, so they
can explore a workspace without building anything. The application built by
[`0030`](/0030-morphir-desktop-electron-app.md) has no distribution at all: running it needs a source
checkout, a JVM, a Scala.js link and a Node toolchain. Nothing is published, and there is no way to
check that a downloaded file is the one that was built.

## Approach

Publish the same binaries to two hosts, so an installation can come from whichever one a consumer
already trusts — a person reaching for a GitHub Release, an automated build reaching for Maven Central.

The naming is what makes that work. Both hosts carry the identical filename tail, following the layout
Mill uses for its own native launcher, so a downloader swaps only the base URL:

```
morphir-desktop-<os>-<arch>-<version>.<zip|tar.gz>
```

The tokens are Mill's — `mac-aarch64`, `mac-amd64`, `linux-amd64`, `linux-aarch64` — plus `win-amd64`,
which Mill's own launcher does not support. Maven Central gets one artifactId per platform,
`org.finos.morphir:morphir-desktop-<os>-<arch>`, with the archive as the main artifact rather than a jar.

This was verified rather than assumed: Mill 1.0.5 publishes `mill-dist-native-mac-aarch64` as an `.exe`
plus a POM declaring `<packaging>jar</packaging>`, with no sources jar and no javadoc jar, and Sonatype
Central accepted it. The desktop modules publish the same shape.

Integrity travels with every download. Each release asset carries a `.sha256` sidecar in `sha256sum`
format, so verification needs no special tool, and one `checksums.txt` covers the whole release under a
detached GPG signature made with the key that already signs the Maven artifacts.

Native installers — dmg, NSIS, AppImage, deb — go to the GitHub Release only. Maven Central carries the
portable archives, which are what an automated consumer can extract; an installer is not something a
build can act on.

Packaging runs on a matching operating system, so the release is assembled in two stages: native runners
produce raw electron-builder output, and a single runner canonicalizes the names, computes the digests
and drives both uploads. The naming and inventory rules are ordinary Scala in the Mill plugin, unit
tested, rather than shell spread across five runners.

Both destinations fire on tags and published releases only. Library snapshot publishing on `main` and
`develop` is unchanged, and the desktop modules are deliberately excluded from that sweep.

The same contract is meant to carry the Morphir CLI later, packaged with GraalVM native-image instead of
electron-builder. That work is not part of this intent.
