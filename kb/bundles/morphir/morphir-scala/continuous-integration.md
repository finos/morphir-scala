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
| `lint` | `mise run lint` — scalafmt check across the Scala sources |
| `knowledge-base` | `kb check` and `kb intent check` |
| `test-jvm` | JVM tests, including the Cucumber/JUnit5 `langkit.itest` suite |
| `test-js` | ScalaJS tests, including the WebAssembly link variants |
| `test-native` | Scala Native tests |
| `publish` | Sonatype publication, on main and tags only |
| `ci` | Aggregate gate — depends on lint, knowledge-base and all three test jobs |

CI runs on pull requests, pushes to `main` and `0.4.x`, published releases, and manual dispatch. Older runs of the
same pull request are cancelled automatically.

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
```

Same checks, same exit codes.
