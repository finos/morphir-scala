---
type: Capability
title: Continuous Integration
description: "GitHub Actions runs linting, cross-platform tests and knowledge base checks on pull requests targeting supported branches."
tags: [ci, build]
status: stable
---

# Continuous Integration

GitHub Actions runs linting, cross-platform tests and knowledge base checks on pull requests targeting supported
branches.

## Jobs

| Job | Runs |
| --- | ---- |
| `lint` | Mill `ci.lint`: scalafmt check over resolved `morphir.__.checkFormat` modules. `--exclude` drops matching module paths. |
| `squire-policy` | `mise run test:squire`. Squire and release-policy gates. |
| `knowledge-base` | `kb check` and `kb intent check` |
| `test-jvm` | JVM tests, including the Cucumber/JUnit5 `langkit.itest` suite |
| `test-js` | ScalaJS tests, including the WebAssembly link variants |
| `test-native` | Scala Native tests |
| `publish` | Sonatype publication via Mill `ci.publish`. Branch snapshots on `main` and `develop`; VCS milestones and releases on `0.4.x` and tags. The publish set is whatever Mill resolves for `__.publishSonatypeCentral`, including the Mill Morphir plugin family (`org.finos.morphir.mill`); the test-only `integration` module is not a publish module and is not uploaded. Destination tasks live under `ci.sonatype.*`. `ci.githubReleases.*` is reserved and not built yet. |
| `ci` | Aggregate gate — depends on lint, knowledge-base and all three test jobs |

CI runs on pull requests into `main`, `0.4.x`, and `develop`; pushes to those same branches; published releases; and
manual dispatch. Older runs of the same pull request are cancelled automatically. Hosted mill invocations pass
`--ticker false`. That includes the workflow, the local `lint` mise wrapper, and `test:jvm-platform`. The GitHub
log is then a linear task trace rather than a replayed progress ticker.

The Release step runs `ci.sonatype.writeMillEnv` first, with Morphir `GPG_*` and `SONATYPE_*` names in that mill.
It sources the written file and then starts `./mill --ticker false -i ci.publish`. Mill snapshots `Task.env` at process start, so
conversion has to happen in an earlier mill. Live Central upload is the first `develop` publish job after merge.

## Branch snapshots

A push or merge to `main` or `develop` must pass the full aggregate `ci` gate before publishing. On `main`, the
exact coordinate is `$releaseLine-$distance-SNAPSHOT`, for example `0.5.0-M04-57-SNAPSHOT` or `0.5.0-57-SNAPSHOT`.
On `develop`, the coordinate is `$releaseLine-$branch.$distance.g$abbrev-SNAPSHOT`, for example
`0.5.0-M04-develop.57.gbd4cd2-SNAPSHOT` or `0.5.0-develop.57.gbd4cd2-SNAPSHOT`: the release line may have a
qualifier, and the coordinate records `develop`, the distance from the nearest version tag, and a six-character Git
abbreviation before the terminal `SNAPSHOT` marker.

Only non-PR runs in the canonical `finos/morphir-scala` repository can reach publication and its credentials. Pull
requests validate without publishing, and contributors do not receive publication credentials locally. Consumers
add `https://central.sonatype.com/repository/maven-snapshots` and select the exact coordinate; resolution and
availability follow the snapshot repository's behavior. The revision-bearing logical version is traceable, but its
`-SNAPSHOT` artifact is mutable and may be overwritten. Sonatype says snapshots are
[currently cleaned up after 90 days](https://central.sonatype.org/publish/publish-portal-snapshots/), so the coordinate
must not be treated as an immutable, reproducible-release lock. Publication from `0.4.x` and tags keeps the ordinary
VCS-derived milestone and release flow, with no snapshot environment.

## The knowledge-base job

It needs a JVM and nothing else — the kb skill is a self-contained Mill script, so there is no build file to resolve
and no mise setup to perform.

Provenance checks are skipped with `--no-provenance`. They compare commit-pinned sources against reference checkouts
under `.refs/`, which is gitignored and therefore absent on a runner; running them there would report every source as
unverifiable rather than telling anyone anything.

Errors fail the job; warnings do not. That split is deliberate — obligations are errors, staleness is a warning, and a
warning that fails the build is a warning people route around.

## Locally

```bash
mise run kb:check
mise run test:squire
```

These run the knowledge-base checks and the Squire/release-policy tests with the same exit codes used by CI.
`mise run ci:local` includes `test:squire` in the full local aggregate workflow.
