---
type: Capability
title: Continuous Integration
description: "GitHub Actions runs linting, cross-platform tests and knowledge base checks on every pull request."
tags: [ci, build]
status: stable
---

# Continuous Integration

GitHub Actions runs linting, cross-platform tests and knowledge base checks on every pull request.

## Jobs

| Job | Runs |
| --- | ---- |
| `lint` | `mise run lint` plus `mise run test:squire` — scalafmt and Squire/release-policy gates |
| `knowledge-base` | `kb check` and `kb intent check` |
| `test-jvm` | JVM tests, including the Cucumber/JUnit5 `langkit.itest` suite |
| `test-js` | ScalaJS tests, including the WebAssembly link variants |
| `test-native` | Scala Native tests |
| `publish` | Sonatype publication on `main`, `0.4.x`, tags, and `develop` snapshots |
| `ci` | Aggregate gate — depends on lint, knowledge-base and all three test jobs |

CI runs on pull requests into `main`, `0.4.x`, and `develop`; pushes to those same branches; published releases; and
manual dispatch. Older runs of the same pull request are cancelled automatically.

## Develop snapshots

A push or merge to `develop` must pass the full aggregate `ci` gate before publishing. The resulting exact coordinate
is branch-qualified and traceable, for example `0.5.0-M04-develop.57.gbd4cd2-SNAPSHOT` or
`0.5.0-develop.57.gbd4cd2-SNAPSHOT`: the release line may have a qualifier, and the coordinate records `develop`, the
distance from the nearest version tag, and a six-character Git abbreviation before the terminal `SNAPSHOT` marker.

Only non-PR runs in the canonical `finos/morphir-scala` repository can reach publication and its credentials. Pull
requests validate without publishing, and contributors do not receive publication credentials locally. Consumers
add `https://central.sonatype.com/repository/maven-snapshots` and select the exact coordinate; resolution and
replacement follow the snapshot repository's behavior. Publication from `main`, `0.4.x`, and tags keeps the ordinary
VCS-derived milestone and release flow, with no snapshot environment on `main` or tags.

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
