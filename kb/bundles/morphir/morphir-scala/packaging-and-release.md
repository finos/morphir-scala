---
type: Capability
title: Packaging and Release
description: "CI publishes Scala libraries and Mill plugins to Sonatype Central, and the desktop app to both Sonatype Central and GitHub Releases."
tags: [ci, release, packaging, desktop]
status: stable
generated:
  by: human:damreev
  at: 2026-08-17T00:00:00Z
---

# Packaging and Release

This page maps everything CI publishes from this repository: where each artifact lands, what triggers
the publish, and the exact steps each path runs. Read it before touching a release job, or before asking
why something did not show up where you expected it.

## What ships where

| What | Where | Coordinates / assets |
| --- | --- | --- |
| Scala libraries | Sonatype Central | `org.finos.morphir:*` |
| Mill Morphir plugins | Sonatype Central | `org.finos.morphir.mill:*` |
| Desktop application archives | Sonatype Central | `org.finos.morphir:morphir-desktop-<os>-<arch>` |
| Desktop archives, installers and checksums | GitHub Releases | `morphir-desktop-<os>-<arch>-<version>.<ext>` |
| CLI | Coursier channel | `org.finos.morphir:morphir-main_3`, resolved as `latest.release` |

Two paths reach these destinations. The library and plugin path runs from ordinary pushes and needs
nothing beyond the `ci` gate. The desktop path only starts once a GitHub Release is published. Figure 1
shows both, end to end.

```mermaid
flowchart TD
    PR[Pull request] --> Gate[ci aggregate gate: lint, tests, kb checks]
    Push[Push to main, 0.4.x or develop] --> Gate
    Rel[GitHub Release published]
    Rel --> Gate
    Dispatch[Manual dispatch] --> Gate

    Gate -->|main or develop: snapshot coordinate| Publish[publish job: ci.publish]
    Gate -->|0.4.x or a tag: milestone/release coordinate| Publish
    Publish -->|serial upload, one module at a time| Sonatype[(Sonatype Central)]

    Rel -->|five platform runners, needs the ci gate| Package[desktop-package matrix]
    Package -->|raw electron-builder output, one workflow artifact per platform| Canon[desktop-release: canonicalize]
    Canon -->|renamed assets, .sha256 sidecars, checksums.txt| Sign[sign checksums.txt with the PGP key]
    Sign -->|checksums.txt.asc| Verify[verify: seven checks]
    Verify -->|all checks pass| GhRelease[githubRelease: upload every asset]
    Verify -->|all checks pass| DesktopSonatype[sonatype: five coordinates, one deployment]
    GhRelease -->|archives, installers, checksums| GhAssets[(GitHub Release assets)]
    DesktopSonatype -->|archives only| Sonatype

    Sonatype -.->|coursier resolves latest.release| Cli[CLI install]
```

**Figure 1:** Two independent triggers feed two independent destinations. The library path runs on
every qualifying push; the desktop path only starts from a published GitHub Release, fans out to five
packaging runners, then funnels back through one runner that signs, verifies, and only then uploads.

## Triggers

| Event | GitHub Actions trigger | Condition | What runs |
| --- | --- | --- | --- |
| Pull request | `pull_request` | into `main`, `0.4.x`, `develop` | `ci` gate only; nothing publishes |
| Push | `push` | to `main`, `0.4.x`, `develop` | `ci` gate, then `publish` once it passes |
| Release published | `release`, `types: [published]` | not scoped to a branch | `ci` gate, then `publish`, `desktop-package` and `desktop-release` |
| Manual dispatch | `workflow_dispatch` | whichever ref is chosen | the same jobs that ref would otherwise trigger |

The workflow has no `push: tags:` trigger. A bare `git push --tags` never runs anything on its own.
Publishing a release, not pushing a tag, is what starts the flow: that is the routine path for both the
library releases and the desktop release.

## Publishing libraries and plugins

| Step | Task | What happens |
| --- | --- | --- |
| 1 | `ci` gate | lint, cross-platform tests and knowledge base checks all pass |
| 2 | `ci.publish` | resolves `__.publishSonatypeCentral`, dropping modules whose path matches `excludedModuleSubstrings` |
| 3 | Upload | one module at a time (`uploadJobs: 1`); parallel upload hits an SLF4J failure (morphir-scala#957) |
| 4 | Version | `SnapshotVersion.select` stamps the coordinate from `MORPHIR_PUBLISH_MODE` and `MORPHIR_PUBLISH_BRANCH` |

`excludedModuleSubstrings` in `ci/package.mill.yaml` drops `.integration.` (test-only) and
`.desktop.dist.`: the desktop archives publish through the separate `ci.desktop` destination described
below, because there is no archive to publish on an ordinary snapshot run. Snapshots publish from `main`
and `develop`; milestones and releases publish from `0.4.x` and tags. See
[Continuous Integration](/continuous-integration.md) for the exact coordinate formats, which this page
reuses rather than restating.

## Publishing the desktop app

Five platform tokens cover the desktop application, each packaged on a runner that matches its target:

| Token | Runner | Archive | Installers |
| --- | --- | --- | --- |
| `mac-aarch64` | `macos-14` | zip | dmg |
| `mac-amd64` | `macos-15-intel` | zip | dmg |
| `linux-amd64` | `ubuntu-24.04` | tar.gz | AppImage, deb |
| `linux-aarch64` | `ubuntu-24.04-arm` | tar.gz | AppImage, deb |
| `win-amd64` | `windows-latest` | zip | exe |

The release then runs as one ordered sequence:

| # | Step | Runs on | What it does |
| --- | --- | --- | --- |
| 1 | `desktop-package` (matrix) | each of the five runners above | links Scala.js with `fullLinkJS`, runs `electron-builder` through `morphir/desktop/scripts/package.sh`, uploads the raw output as a workflow artifact |
| 2 | `canonicalize` | `desktop-release`, one Linux runner | renames staged output to canonical names, writes a `.sha256` sidecar per asset and one `checksums.txt` |
| 3 | Sign `checksums.txt` | same runner | GPG detached signature over `checksums.txt`, producing `checksums.txt.asc` |
| 4 | `verify` | same runner | runs seven named checks against the release directory |
| 5 | `githubRelease --tag` | same runner | uploads every file in the release directory to that tag's release |
| 6 | `sonatype` | same runner | uploads all five archives to Sonatype Central in one deployment |

`canonicalize` and `verify` are pure preparation: they read and write files, and make no network call.
`verify` exists because the digests are computed once, during canonicalization, on the runner that staged
the assets, and the assets then cross a job boundary (uploaded as workflow artifacts, downloaded again)
before anything publishes them. Recomputing each digest from the bytes on disk after that crossing is what
catches corruption in transit.

`verify` reports every problem it finds rather than stopping at the first:

| Check | Confirms |
| --- | --- |
| `expected-assets-present` | every platform's archive and each of its installers exists under its canonical name |
| `no-empty-assets` | no asset is a zero-byte file |
| `archive-magic-numbers` | zip and tar.gz assets start with the right magic bytes, not a truncated download or an HTML error page |
| `version-in-asset-names` | every asset name embeds the version being published |
| `sidecar-digests-match` | each asset's recomputed sha256 matches its `.sha256` sidecar, and the sidecar's own filename matches |
| `checksums-covers-assets` | `checksums.txt` lists every asset exactly once, and nothing that is not an asset |
| `signature-present` | `checksums.txt.asc` exists, is non-empty, and begins with the PGP signature header |

`githubRelease` uploads with `--clobber`. If no release exists yet for the tag, it creates one as a
**draft** first and uploads into that. Publishing the release itself stays a human action, not something
this step does automatically.

Maven Central receives archives only; the native installers (dmg, NSIS `.exe`, AppImage, `.deb`) go to the
GitHub Release only, because an automated consumer can act on a portable archive but not on an installer.
The Maven layout is the archive plus a POM, with no sources jar and no javadoc jar, mirroring
`com.lihaoyi:mill-dist-native-mac-aarch64`.

`sonatype` uploads all five archives as **one** deployment under a single bundle name
(`morphir-desktop-<version>`), so nothing releases unless all five validate together. Sonatype Central's
`AUTOMATIC` publishing type releases a deployment irrevocably on success; five separate deployments could
leave a partial release with no way to re-run it if, say, the third of five failed after the first two had
already gone public. One deployment is what makes this step safely retriable.

The word bundle describes the upload, not the artifacts. Maven Central still receives five separate
coordinates, each with its own POM, archive and signatures, and a consumer resolves
`org.finos.morphir:morphir-desktop-mac-aarch64` exactly as it would any other artifact. A Sonatype
deployment bundle is simply the unit Central validates and releases: one zip in Maven repository layout
that can carry many coordinates at once.

## Versions

Every artifact's version, including the desktop app's, comes from
`SnapshotVersion.select(VcsVersion.vcsState(), env)`, driven by `MORPHIR_PUBLISH_MODE` and
`MORPHIR_PUBLISH_BRANCH`. See [Continuous Integration](/continuous-integration.md) for the exact
coordinate formats. One version stream covers the whole repository today: the desktop app moves
in lockstep with the libraries. Independent version streams for the desktop app are recorded as future
intent, not built yet.

## Signing keys

Two roles stay apart, on purpose:

| Role | Secrets | Used by | Populated today |
| --- | --- | --- | --- |
| PGP artifact signing | `ORG_MORPHIR_CI_GPG_PRIVATE_KEY`, `ORG_MORPHIR_CI_GPG_PASSPHRASE` | library publish, desktop `checksums.txt` signing, desktop Sonatype upload | yes |
| Platform code signing | `ORG_MORPHIR_CSC_LINK`, `ORG_MORPHIR_CSC_KEY_PASSWORD`, `ORG_MORPHIR_APPLE_ID`, `ORG_MORPHIR_APPLE_APP_SPECIFIC_PASSWORD`, `ORG_MORPHIR_APPLE_TEAM_ID` | `desktop-package` only | no |

The PGP key is deliberately reused: it is the same key that already signs the Maven artifacts, so a
desktop release needs no new key material. Platform code signing is a separate concern with its own
secrets, read only by the packaging runners. electron-builder signs a binary when it finds those
certificates and produces an unsigned build when it does not, so desktop binaries ship unsigned today.

## Retriability

A failed release run is safe to re-run. Packaging is idempotent: running `desktop-package` again for the
same version produces the same files. `canonicalize` and `verify` are pure preparation with no side
effect beyond writing files. `githubRelease` uploads with `--clobber`, so a re-run overwrites rather than
duplicating. `sonatype` uploads one atomic bundle, so a failed attempt publishes nothing and a retry starts
from a clean slate. Nothing in the desktop path can leave a release half-published.

## Installing the CLI

The CLI is an ordinary Scala library, `org.finos.morphir:morphir-main_3`, published through the library
and plugin path above. It has no separate publish job. Consumers install it with
[Coursier](https://get-coursier.io/), pointed at the channel declared in `coursier-channel.json` at the
repository root: `morphir-cli` resolves `org.finos.morphir:morphir-main_3:latest.release` from three
repository aliases (`central`, `sonatype:releases`, `typesafe:ivy-releases`), and `morphir-insiders-cli`
adds `sonatype:snapshots` for pre-release builds. `morphir-cli-install.sh` runs `cs bootstrap` against
that coordinate and drops the launcher into the Coursier bin directory.

Unverified: whether the `sonatype:releases` and `typesafe:ivy-releases` aliases in `coursier-channel.json`
still resolve anything now that publishing targets Sonatype Central's portal directly rather than the
legacy OSSRH staging repository `sonatype:releases` names. The `central` alias is enough on its own once
an artifact reaches Maven Central.

## Where to go next

[Continuous Integration](/continuous-integration.md) covers every CI job, including `publish`,
`desktop-package` and `desktop-release`, alongside the jobs that are not about releasing at all.
[Build System](/build-system.md) covers Mill and mise mechanics this page assumes. The desktop app's own
story, still in progress, lives in [intent 0030](../../intent/0030-morphir-desktop-electron-app.md) and
[intent 0031](../../intent/0031-publish-the-morphir-desktop-application.md).
