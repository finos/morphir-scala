---
type: Capability
title: Cross-Platform Targets
description: "Modules compile to the JVM, ScalaJS, WebAssembly and Scala Native from one shared source layout."
tags: [build, cross-platform]
status: stable
---

# Cross-Platform Targets

Modules compile to the JVM, ScalaJS, WebAssembly and Scala Native from one shared source layout.

## Layout

The project uses a custom cross-platform source layout. For a module at `morphir/foo/`:

| Directory | Compiled for |
| --------- | ------------ |
| `src/` | Every platform and Scala version |
| `jvm/src/` | JVM only |
| `js/src/` | ScalaJS only |
| `src-3/` | Scala 3.x, every platform |
| `jvm/src-3/` | JVM and Scala 3.x |

The nesting matters: the platform is a directory *containing* `src`, not a suffix on it — `jvm/src-3`, never
`src-3-jvm`. `millbuild.crossplatform.CrossPlatformScalaModule` derives the paths.

## Targets

- **JVM** — the primary target for every module.
- **ScalaJS** — plus a WebAssembly link variant, exercised by the `test-js` job.
- **Scala Native** — currently scoped to the `langkit` and `kit` modules rather than the whole project.

## A pin that moves in pairs

The Scala.js version is fixed in two places that must change together: `ScalaVersions.scalaJSVersion`, and the
`org.scala-js:scalajs-linker_2.13` entry in the `//|` metabuild header of `build.mill`. Mixing in
`MorphirWasmLinker` makes `scalaJSVersion` final at whatever the linker dependency provides, so a mismatch surfaces as
a confusing build failure rather than a version conflict.

## Testing across platforms

The library modules under `morphir/` test with **kyo-test**. A test class extends `kyo.test.Test[Any]`, and the test
module mixes in the per-platform trait (`millbuild.KyoTest`, `KyoTestJS`, `KyoTestNative` or `KyoTestWasm`). The Mill
plugins and `millbuildHelpers` run on the JVM only and use uTest.
