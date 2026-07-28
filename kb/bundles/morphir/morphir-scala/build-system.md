---
type: Capability
title: Build System
description: "Mill drives the build from per-directory package.mill.yaml files, with mise as the task runner."
tags: [build, mill]
status: stable
---

# Build System

Mill drives the build from per-directory package.mill.yaml files, with mise as the task runner.

## Shape

Mill is the build tool. Modules are configured per-directory in `package.mill.yaml` files — YAML is the default, and
a `.mill` file is the escape hatch for what YAML cannot express (currently only `Cross[...]` declarations). Anything
needing a `Task` — computed source paths, `forkArgs`, BuildInfo members — belongs in a named trait in `build.mill`
that the YAML then names in its `extends:`.

One consequence worth knowing: `moduleDeps:` in YAML *replaces* the inherited value rather than adding to it. Inside a
nested `object test:`, use `moduleDeps: !append [...]` to keep the implicit dependency on the enclosing module.

## Versions live in the build, not in prose

The sources of truth are `mill-build/src/millbuild/deps.scala` (Scala, Scala.js and Scala Native versions in
`ScalaVersions`; library versions in `Versions`), `.mill-version`, and `.scalafmt.conf`. Documentation deliberately
names no version numbers so it cannot drift out of step.

## mise as the task runner

```bash
mise run lint
```

```bash
mise run test:jvm
```

Tasks live in `.config/mise/tasks/`, one executable script per task, and cover setup, formatting, linting, the three
test platforms, the knowledge base check, and local CI.

## Dependencies

Declared with the `mvn""` interpolator — `ivy""` is deprecated in Mill 1.x. For JS and Native dependencies the
double-colon form (`group::artifact::version`) is required; a single colon cross-builds only by Scala version and
silently resolves the JVM jar.
