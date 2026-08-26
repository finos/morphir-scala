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

## Windows ARM64 build JVM

Scala.js linking runs inside Mill's build JVM. Mill normally resolves that JVM through Coursier instead of using
`JAVA_HOME`. On Windows ARM64, the default managed JDK may resolve to `win_x64`. Windows can run that binary under
emulation, but Closure linking becomes much slower.

Windows ARM64 contributors select a native Microsoft OpenJDK and put `system` in `.mill-jvm-version`. The ignored
local file tells Mill to use the `java` selected by `JAVA_HOME` and `PATH`:

```powershell
scoop reset microsoft-lts-jdk
Set-Content .mill-jvm-version system
.\mill.bat --no-server --version
```

The version output must report `os.arch: aarch64`. The `--no-server` check avoids reusing an x64 daemon started
before the override existed. Mill owns Node acquisition for the build, so this workaround does not add a global
Node prerequisite.

## Dependencies

Declare dependencies with the `mvn""` interpolator. Mill 1.x deprecates `ivy""`. For JS and Native dependencies the
double-colon form (`group::artifact::version`) is required; a single colon cross-builds only by Scala version and
silently resolves the JVM jar.

A library suite is two or more artifacts that share one pin in `Versions`. Kyo, ZIO core, zio-json,
zio-prelude, zio-config, zio-schema, fs2, upickle, borer, metaconfig and scala-java-time are suites. Mill
`depManagement` on `MorphirSuiteBom` applies those pins. None of those publishers are imported as a Maven BOM
with `bomMvnDeps`.

A module that already extends `MorphirJVMModule` (or `MorphirJSModule` / `MorphirNativeModule`) adds extra suite
artifacts in YAML without a version:

```yaml
mvnDeps: !append
- io.getkyo::kyo-config
- dev.zio::zio-json
```

Do not add a new `Morphir*MvnDeps` trait per artifact. Fetch traits still name a bundle that always travels
together (kyo-core plus kyo-prelude, kyo-test, and so on). When a suite gains a member, add it to that object's
`managed` in `mill-build/src/millbuild/deps.scala`.

A one-off library that is not a suite names its version in YAML. Still use `mvnDeps: !append`, because a bare
`mvnDeps:` replaces inherited deps.
