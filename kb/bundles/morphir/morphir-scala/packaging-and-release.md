---
type: Capability
title: Packaging and Release
description: "CI publishes Scala libraries and Mill plugins to Sonatype Central, plus CLI and desktop packages to GitHub Releases."
tags: [ci, release, packaging, cli, desktop]
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
| Native CLI packages and checksums | GitHub Releases | `morphir-cli-<os>-<arch>-<version>.tar.gz` or `.zip` |
| Executable JVM CLI and checksum | GitHub Releases | `morphir-cli-jvm-<version>.jar` |
| CLI library | Sonatype Central and Coursier channel | `org.finos.morphir:morphir-main_3` |

Two aggregate gates stand between a trigger and anything leaving the repository: `ci` for lint, tests
and knowledge base checks, and `packaging` for the CLI and desktop builds. Packaging runs on ordinary
pushes and pull requests as well as release tags. A release itself runs in two phases across two
workflows: a tag push *stages* a draft GitHub release through `ci.yml`, and publishing that draft
*promotes* it to Maven Central through `release-publish.yml`. Figure 1 shows every path, end to end.

```mermaid
flowchart TD
    PR[Pull request] --> Gate[ci gate: lint, tests, kb checks]
    Push[Push to main or 0.4.x] --> Gate
    TagPush[Tag push: v*, mill-plugins/v*, desktop/v*] --> Gate
    Dispatch[Manual dispatch] --> Gate

    Gate -->|branch push only: every area snapshots or milestones together, each from its own changelog| PublishAll[publish: ci.publish]
    PublishAll -->|serial upload, one module at a time| Sonatype[(Sonatype Central)]

    Gate --> CliMatrix[cli-matrix: five platforms on a v* tag; three-OS smoke matrix on a pull request or branch push]
    CliMatrix --> CliNative[cli-package-native: GraalVM Native Image]
    CliMatrix --> CliJvm[cli-package-jvm: executable assembly]
    CliNative --> CliVerify[cli-verify: assets and SHA-256 checksums]
    CliJvm --> CliVerify
    CliVerify --> Packaging
    CliVerify -->|refs/tags/v* only| CliRelease[cli-release: create or reuse the draft release, upload assets, re-download and verify]
    CliRelease --> GhAssets

    Gate --> Matrix[desktop-matrix: five platforms on a branch push or a desktop/v* tag; three-OS smoke matrix on a pull request]
    Matrix --> Package[desktop-package: one runner per platform]
    Package -->|raw electron-builder output, one artifact per platform| CiVerify[desktop-verify: canonicalize and verify, no signing, no upload]
    Matrix --> Packaging[packaging gate]
    Package --> Packaging
    CiVerify --> Packaging

    Packaging -->|refs/tags/desktop/v* only| Canon[desktop-release: canonicalize]
    Canon -->|renamed assets, .sha256 sidecars, checksums.txt| Sign[sign checksums.txt with the PGP key]
    Sign -->|checksums.txt.asc| Verify[verify: seven checks]
    Verify -->|all checks pass| GhRelease[githubRelease: upload every asset to the draft]
    GhRelease -->|archives, installers, checksums| GhAssets[(Draft GitHub Release assets)]

    GhAssets -->|a human publishes the draft| RelPub[release-publish.yml: target resolves the tag]
    RelPub -->|v*| PromoteLib[publish-libraries: verify CLI assets, then ci.sonatype.libraries]
    RelPub -->|mill-plugins/v*| PromotePlugins[publish-plugins: ci.sonatype.plugins]
    RelPub -->|desktop/v*| PromoteDesktop[publish-desktop: download and re-verify staged assets, then ci.desktop.sonatype]
    PromoteLib --> Sonatype
    PromotePlugins --> Sonatype
    PromoteDesktop -->|archives only| Sonatype

    Sonatype -.->|coursier resolves latest.release| Cli[CLI install]
```

**Figure 1:** Two gates stand in the staging flow, and every path into `Sonatype` except the branch
snapshot runs through the published-release promotion in `release-publish.yml`. Everything above
`packaging` runs on ordinary pull requests and pushes, so broken CLI or desktop packaging surfaces
where it was introduced. The three promotion jobs are each guarded on their own tag namespace. See
[Release routing](#release-routing) for why a single tag can never satisfy two of them.

## Triggers

| Event | GitHub Actions trigger | Condition | What runs |
| --- | --- | --- | --- |
| Pull request | `pull_request` | into `main`, `0.4.x` | `ci` gate; nothing publishes. CLI and desktop packaging each run one runner per operating system, unless their switches turn them off |
| Branch push | `push` | to `main`, `0.4.x` | `ci` gate, then `publish` (`ci.publish`, every area) once it passes. CLI packaging runs its three-OS smoke matrix; desktop packaging runs all five platforms; either switch turns its half off |
| Tag push | `push` | tags `v*`, `mill-plugins/v*`, `desktop/v*` | phase one of a release: `ci` gate, then full packaging and verification, then a **draft** GitHub release is created (or reused) with the assets attached. No Maven Central upload happens on a tag push |
| Release published | `release`, `types: [published]` on `release-publish.yml` | any published release | phase two: the tag namespace routes to one promotion job, which re-verifies the staged assets and uploads to Sonatype — unless the Maven Central switches stand it down |
| Manual dispatch of CI | `workflow_dispatch` on `ci.yml` | whichever ref is chosen | the same jobs that ref would otherwise trigger. Choosing a release tag re-runs that tag's staging phase (the retry path for a failed build or upload) |
| Manual dispatch of promotion | `workflow_dispatch` on `release-publish.yml` | `tag` input | re-runs phase two for that tag: the retry path for a failed Sonatype upload, or late promotion of a release published while the switches were off |

A release runs in two phases. **Phase one — staging** — is the tag push: tests, packaging,
verification, and a draft GitHub release holding the verified assets. Nothing irreversible happens;
a bad draft is deleted, the tag moved or removed, and nothing shipped. **Phase two — promotion** —
is a human publishing that draft: the `release: published` event fires `release-publish.yml`, a slim
workflow that re-verifies the staged assets and uploads to Maven Central. Splitting the phases
across two workflows is what keeps the publish button from re-running the whole test and packaging
pipeline, and what keeps the irrevocable Sonatype upload behind a human gate.

Creating a GitHub release with a **new** tag through the UI fires the tag-push event, so staging
still runs — but it also fires the release event immediately, and promotion will fail its
verification until staging finishes; re-run promotion through its dispatch afterwards. The routine
path is: push the tag, wait for staging to go green, review the draft, publish it.

### Skipping Maven Central

Two switches make a release target GitHub Releases only:

- The repository variable `MORPHIR_RELEASE_MAVEN_CENTRAL` — unset or anything other than `false`
  means enabled, matching the `MORPHIR_CI_PACKAGE_*` switches. Set to `false` it stands down every
  Sonatype upload — the three promotion jobs in `release-publish.yml` and the snapshot `publish`
  job in `ci.yml` — from repository settings without a commit.
- The `maven_central` input on a manual dispatch of `release-publish.yml` — uncheck it and the
  promotion run verifies without uploading. (The same input on `ci.yml` stands down the snapshot
  publish for one run.)

With either switch off, publishing the draft still publishes the GitHub release — assets were
staged in phase one — and Maven Central simply receives nothing until someone re-runs promotion
with the switches on.

### Release routing

Three independently versioned areas exist: the libraries, the Mill plugin family, and the desktop
application. Each releases through its own tag namespace, and the tag's shape is what routes a tag
push to the right destination; nothing else about the event distinguishes them, since `github.ref`
is the only thing that differs.

| Tag shape | Stages (tag push, `ci.yml`) | Promotes (release published, `release-publish.yml`) |
| --- | --- | --- |
| `v0.6.0-M01` | CLI packages and checksums on a draft release, via `cli-release` → `ci.cli.githubRelease` | `publish-libraries`: verify the CLI assets, then `ci.sonatype.libraries` |
| `mill-plugins/v0.1.0` | Nothing beyond the `ci` gate — plugins carry no GitHub release assets | `publish-plugins`: `ci.sonatype.plugins` |
| `desktop/v0.3.0` | Archives, installers, signed checksums on a draft release, via `desktop-release` | `publish-desktop`: download and re-verify the staged assets, then `ci.desktop.sonatype` |
| Anything else | Nothing, visibly: no job matches | Nothing: no promotion job matches |

Staging guards use `startsWith(github.ref, 'refs/tags/<namespace>/v')`; promotion guards apply the
same prefixes to the tag the `target` job resolved. Both are mutually exclusive by construction: a
single tag can only ever start one of `v`, `desktop/v` or `mill-plugins/v`, so a release never
routes to two paths at once. Snapshot and milestone publishing from a branch push is unaffected by
this table: `publish` still runs `ci.publish` on `main` and `0.4.x`, publishing every area
together, each stamped from its own changelog. Only the release path routes by tag.

### Desktop packaging in ordinary CI

Desktop packaging is no longer release-only. It also runs from a pull request and from a push, so a change
that breaks `electron-builder`, the Scala.js link, or canonicalization is caught where it was introduced
rather than at release time. Four jobs carry this:

| Job | Does |
| --- | --- |
| `desktop-matrix` | Computes the platform set: all five tokens on a `desktop/v*` tag or a push to `main` or `0.4.x`; the three-OS smoke matrix everywhere else (a pull request, or another namespace's tag). Outputs both the matrix JSON and the same set as a comma-separated token list. |
| `desktop-package` | The same packaging matrix described below, now sized from `desktop-matrix`'s output instead of always covering all five. |
| `desktop-verify` | Downloads the packaged artifacts, normalizes staging the way `desktop-release` does, then runs `ci.desktop.canonicalize` and `ci.desktop.verify` over exactly that subset, with the signature check relaxed because nothing signs `checksums.txt` here and a pull request carries no GPG secret. No signing and no upload happen in this job, or anywhere in ordinary CI. |
| `packaging` | Aggregates the three above, the way `ci` aggregates lint and the test jobs. It reads `desktop-matrix` first, because a skip on its own is ambiguous: `desktop-package` also skips when `ci` fails upstream. If `desktop-matrix` was skipped the switch is off and every member must be skipped together; if it ran, packaging was expected to run and only success will do. |

The repository variable `MORPHIR_CI_PACKAGE_DESKTOP` is the switch: unset, or set to anything other than
`false`, packaging runs; set to `false`, it does not. It is a repository variable, not a workflow `env:`,
because GitHub Actions does not expose the `env` context inside a job's `if:` condition. An `env`-based
switch would evaluate empty there and the gated jobs would never run. A maintainer flips it from repository
settings, no commit required. A `desktop/v*` tag ignores it entirely: turning off ordinary-CI
packaging must never weaken a real release. `desktop-release` depends on `packaging` rather than on
`desktop-package` directly, exactly as `publish` depends on the whole `ci` gate rather than one test job.

## Publishing libraries and plugins

| Step | Task | What happens |
| --- | --- | --- |
| 1 | `ci` gate (snapshots) or the staged, published release (promotion) | a snapshot publishes only after lint, cross-platform tests and knowledge base checks pass; a release publishes only after its tag-push run went green and a human published the draft |
| 2 | `ci.publish` (branch push) or `ci.sonatype.libraries` / `ci.sonatype.plugins` (promotion, routed by tag; see [Release routing](#release-routing)) | resolves `__.publishSonatypeCentral`, dropping modules whose path matches `excludedModuleSubstrings` |
| 3 | Upload | one module at a time (`uploadJobs: 1`); parallel upload hits an SLF4J failure (morphir-scala#957) |
| 4 | Version | each area's `streamVersion` stamps its own coordinate; see [Versions](#versions) |

`excludedModuleSubstrings` in `ci/package.mill.yaml` drops `.integration.` (test-only) and
`.desktop.dist.`: the desktop archives publish through the separate `ci.desktop` destination described
below, because there is no archive to publish on an ordinary snapshot run. Snapshots publish from
`main`; milestones and releases publish by promoting a tag's draft release. Promotion checks out the
tag itself, so `streamVersion` resolves the released version: HEAD sits at distance zero on the
stream's tag and the tag agrees with the changelog's release line. See
[Continuous Integration](/continuous-integration.md) for the exact coordinate formats, which this page
reuses rather than restating.

## Publishing the CLI

The root library version stream also versions the CLI. A root `v*` tag stages six CLI packages on
its draft release:

| Token | Runner | Package |
| --- | --- | --- |
| `mac-aarch64` | `macos-14` | `morphir-cli-mac-aarch64-<version>.tar.gz` |
| `mac-amd64` | `macos-15-intel` | `morphir-cli-mac-amd64-<version>.tar.gz` |
| `linux-amd64` | `ubuntu-24.04` | `morphir-cli-linux-amd64-<version>.tar.gz` |
| `linux-aarch64` | `ubuntu-24.04-arm` | `morphir-cli-linux-aarch64-<version>.tar.gz` |
| `win-amd64` | `windows-latest` | `morphir-cli-win-amd64-<version>.zip` |
| JVM, platform independent | `ubuntu-latest` | `morphir-cli-jvm-<version>.jar` |

Each native runner uses GraalVM Native Image with `--no-fallback` and `-march=compatibility`. The Native
Image classpath is the CLI assembly rather than its expanded dependency graph. This avoids the Windows
command-line limit and ensures the native compiler sees the same application bytes as the JVM package.
The Windows archive retains the DLLs emitted next to the executable. Unix archives mark `morphir` as
executable.

The JVM package is Mill's assembly output. Mill adds a shell and batch launcher to the JAR, in the same
style as Mill's own executable distribution, while it remains valid input to `java -jar`.

Every package command smoke-tests `version`, the top-level command list, and `server --help` before it
writes an archive. `cli-verify` then checks the complete platform set, rejects missing, empty, unexpected,
or corrupted assets, and writes `checksums.txt` from the per-asset SHA-256 sidecars. A root `v*` tag runs
the verifier again before `ci.cli.githubRelease` creates the GitHub release as a **draft** when none
exists yet (with generated notes, against the pushed tag) and uploads with `--clobber`, making a failed
upload safe to retry by dispatching the workflow on the same tag. After the upload, `ci.cli.verifyRelease`
downloads every asset fresh from the release and verifies it against the staged `checksums.txt`, so a
truncated or mislabeled upload fails the staging run rather than a user's install; the same task runs
again in `release-publish.yml` before the Maven Central upload, so promotion re-proves what staging
proved. The workflow does not create or upload to a GitHub Release from a pull request or branch push.
Only a root `v*` tag ref — pushed, or chosen for a manual dispatch — receives the job's
`contents: write` token.

GraalVM does not provide Native Image for Windows ARM64. That platform uses the JVM package with a native
ARM64 Java 25 runtime. An x64 Windows package can also run through Windows emulation, but it is not an
ARM64 native image. `CliRelease.Platform.fromHost` rejects a claimed `win-aarch64` build so an emulated
toolchain cannot be mislabeled.

Ordinary CI uses `MORPHIR_CI_PACKAGE_CLI` as its switch. Unset, or any value other than `false`, enables
the jobs. Pull requests and branch pushes exercise the three-OS smoke matrix — `linux-amd64`,
`win-amd64`, and `mac-aarch64` — each leg covering toolchain machinery the others do not, while five
GraalVM builds per ordinary merge would be paid for assets nothing publishes. A root `v*` tag always
exercises all five and ignores the switch.

## Publishing the desktop app

Five platform tokens cover the desktop application, each packaged on a runner that matches its target:

| Token | Runner | Archive | Installers |
| --- | --- | --- | --- |
| `mac-aarch64` | `macos-14` | zip | dmg |
| `mac-amd64` | `macos-15-intel` | zip | dmg |
| `linux-amd64` | `ubuntu-24.04` | tar.gz | AppImage, deb |
| `linux-aarch64` | `ubuntu-24.04-arm` | tar.gz | AppImage, deb |
| `win-amd64` | `windows-latest` | zip | exe |

On a `desktop/v*` tag, all five package and the staging phase then runs as one ordered sequence,
with the Sonatype upload deferred to promotion:

| # | Step | Runs on | What it does |
| --- | --- | --- | --- |
| 1 | `desktop-package` (matrix) | each of the five runners above | links Scala.js with `fullLinkJS`, runs `electron-builder` through `morphir/desktop/scripts/package.sh`, uploads the raw output as a workflow artifact |
| 2 | `canonicalize` | `desktop-release`, one Linux runner | renames staged output to canonical names, writes a `.sha256` sidecar per asset and one `checksums.txt` |
| 3 | Sign `checksums.txt` | same runner | GPG detached signature over `checksums.txt`, producing `checksums.txt.asc` |
| 4 | `verify` | same runner | runs seven named checks against the release directory |
| 5 | `githubRelease --tag` | same runner | uploads every file in the release directory to that tag's draft release |
| 6 | `publish-desktop` | `release-publish.yml`, after a human publishes the draft | downloads the staged assets back off the release, re-runs `verify` (signature included), then uploads all five archives to Sonatype Central in one deployment |

Outside a release, [desktop packaging in ordinary CI](#desktop-packaging-in-ordinary-ci) runs steps 1, 2 and
4 only, restricted to whatever subset `desktop-matrix` computed, with step 4's signature check relaxed and
no step 3, 5 or 6. See that section for the job breakdown.

`canonicalize` and `verify` both take a `--platforms` option: a comma-separated list of the tokens above,
defaulting to all five. `DesktopRelease.canonicalize` fails, naming every missing platform, if any requested
token has no staged directory. That is correct for a release, where every platform must be present, and it
is why the subset option exists: a pull request stages only the one platform `desktop-matrix` chose. An
unrecognized or blank token in the list is also rejected, naming the valid tokens.

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
**draft** first and uploads into that. Publishing the release itself stays a human action, not
something this step does automatically — and pressing that publish button is exactly what starts
promotion: `release-publish.yml` fires, downloads the staged assets, re-verifies them, and runs the
Sonatype upload. That upload stands down when either
[Maven Central switch](#skipping-maven-central) is off; the GitHub side of the release is
unaffected.

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

Three areas version independently, each through the same `MorphirVersionedModule` trait
(`build.mill`) configured with its own namespace, changelog and (for the plugin family and the
desktop app) a floor below which the release line may not regress:

| Area | Namespace | Changelog | Tag pattern | Coordinates |
| --- | --- | --- | --- | --- |
| Libraries | `None` (the root stream) | `/CHANGELOG.md` | `v*` | `org.finos.morphir:*` |
| Mill plugin family | `mill-plugins` | `mill-plugins/morphir/CHANGELOG.md` | `mill-plugins/v*` | `org.finos.morphir.mill:*` |
| Desktop application | `desktop` | `morphir/desktop/CHANGELOG.md` | `desktop/v*` | `org.finos.morphir:morphir-desktop-*` |

Each area's own `streamVersion` task composes its coordinate from two sources. The changelog supplies
the **release line**, whose topmost *undated* heading is the number a build is heading toward. Git
supplies everything after it: commit distance from the area's nearest matching tag, branch, revision,
dirty state. `MORPHIR_PUBLISH_MODE` and `MORPHIR_PUBLISH_BRANCH` still drive the choice between a
release and a snapshot, the same as before; what changed is where the release line comes from. See
[Continuous Integration](/continuous-integration.md) for the exact coordinate formats.

Git tag resolution had to change to make this possible. Mill's own `VcsVersion` runs `git describe
--abbrev=0 --tags` with no `--match`, returning the nearest tag of any shape. That works only while the
whole repository shares one tag stream. The moment a second namespace exists, that stops being safe: an
unfiltered lookup would let the first `desktop/v0.3.0` tag become the "nearest tag" for a library
build too, and reject it outright as not a semantic version. So every stream, including the original
library one, now resolves its nearest tag with `git describe --match '<pattern>'` (`GitStream` /
`TagStream`). The library stream needed that fix as much as the two new ones did, simply to keep
working once a second namespace could exist.

`ci.desktop.version` reads the desktop stream's version directly from one
`morphir.desktop.dist.platform[<token>]` dist module rather than recomputing it, so the packaging jobs
(`canonicalize`, `verify`, `githubRelease`, `sonatype`) and the dist modules that actually publish can
never disagree about what version is being built. Which token it reads from is arbitrary: every
`platform[<token>]` module shares the same namespace, changelog and tag stream, so all five resolve to
the same version.

**Library snapshot coordinates changed meaning.** Under the previous scheme the number after the base
version counted commits *past* the last release: `0.5.0-M04-12-SNAPSHOT` meant twelve commits past the
`0.5.0-M04` tag that had already shipped. Under this scheme it counts commits *into* the release the
changelog names next: `0.6.0-M01-12-SNAPSHOT` means twelve commits toward `0.6.0-M01`, which has not
shipped yet. Anyone explaining an old coordinate needs this distinction: both the starting point and
the direction of the count changed.

The Mill plugin family's own numbering **starts its floor at `0.5.0-M04`** rather than `0.1.0`. No
plugin release ever shipped at that number; it is the last version tagged in the repository's
previously shared stream. The plugin family has never been published on its own:
`mill-plugins/` did not exist at that tag, and `org.finos.morphir.mill` carries no artifacts on Maven
Central. The floor exists so the family's eventual first release cannot read as a regression against
that shared history, even though the plugins now version independently of the libraries.

Squire operationalizes the convention with four commands, all calling the same code the build calls:
`squire changelog check` (validated in the `squire-policy` CI job), `squire changelog show`, `squire
release prepare --area <name>`, and `squire release status`. Tagging itself stays a human act: these
commands print the `git tag` command or stage the release, and never run it.

See [intent 0032](../../intent/0032-independent-version-streams.md) for why this replaced the single
shared stream.

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

For a native install, download the archive matching the operating system and architecture from the
root GitHub Release, verify it against `checksums.txt`, extract it, and place `morphir` or `morphir.exe`
on `PATH`.

The JVM package is the fallback for every supported operating system and the primary package for Windows
ARM64. With Java 25 or newer installed, run:

```text
java -jar morphir-cli-jvm-<version>.jar version
java -jar morphir-cli-jvm-<version>.jar server --help
```

On macOS or Linux, the same file can be made executable with `chmod +x` and invoked directly.

The existing [Coursier](https://get-coursier.io/) channel remains available. `morphir-cli` resolves
`org.finos.morphir:morphir-main_3:latest.release`, while `morphir-insiders-cli` also admits snapshots.
`morphir-cli-install.sh` bootstraps that coordinate into the Coursier bin directory. This route consumes
the Maven-published library; the GitHub Release packages are a separate distribution of the same CLI.

Unverified: whether the `sonatype:releases` and `typesafe:ivy-releases` aliases in `coursier-channel.json`
still resolve anything now that publishing targets Sonatype Central's portal directly rather than the
legacy OSSRH staging repository `sonatype:releases` names. The `central` alias is enough on its own once
an artifact reaches Maven Central.

## Where to go next

[Continuous Integration](/continuous-integration.md) covers every CI job, including `publish`,
`publish-plugins`, `desktop-package` and `desktop-release`, alongside the jobs that are not about
releasing at all. [Build System](/build-system.md) covers Mill and mise mechanics this page assumes.
The desktop app's own story, still in progress, lives in
[intent 0030](../../intent/0030-morphir-desktop-electron-app.md) and
[intent 0031](../../intent/0031-publish-the-morphir-desktop-application.md); the three-stream
versioning convention itself is [intent 0032](../../intent/0032-independent-version-streams.md).
