---
type: Intent
title: Publish the morphir-scala CLI to GitHub Releases
description: Publish GraalVM native CLI archives and a portable executable JVM assembly from the library release stream.
state: InProgress
kind: build
breaking: false
created: 2026-08-26
state_since: 2026-08-26
tags: [cli, release, graalvm]
---

# 0038: Publish the morphir-scala CLI to GitHub Releases

Publish GraalVM native CLI archives and a portable executable JVM assembly from the library release stream.

## Problem

The CLI is published to Maven Central as `morphir-main_3`, with its Mill assembly available under an
`assembly` classifier. A user still needs Coursier or a JVM-aware Maven download to find it. GitHub Releases carry
no morphir-scala CLI assets, and the project has no tested path for producing a native executable. This leaves the
main Morphir distribution unable to acquire the Scala backend as a normal command-line tool.

## Approach

Keep the CLI on the root library version stream and publish its distributable files when a root `v*` tag runs CI.
Do not create a fourth version namespace for another representation of `morphir-main_3`.

Build GraalVM Native Image archives on the operating system and architecture they target. Native Image is not a
cross-compiler. The supported matrix is macOS ARM64 and x64, Linux ARM64 and x64, and Windows x64. Windows ARM64 has
no GraalVM distribution, so it uses the JVM package until GraalVM adds that host. The Windows archive must include
the runtime DLLs emitted beside the executable because the CLI uses `java.awt.Desktop` to open `morphir server`.

Use Mill's executable assembly as both the portable JVM asset and the single Native Image classpath input. The
assembly is an uber JAR with Mill's universal shell and batch prefix. Passing it as one input also avoids Windows'
command-line length limit, which the expanded runtime classpath exceeds. Every package runs `version`, top-level
help, and `server --help` before upload so AOT reachability cannot silently remove the server command.

Pull requests build one native target per operating system plus the JVM package. Mainline and release builds run all
five native targets. A verification job recomputes every SHA-256 digest after workflow artifact transfer and writes
one `checksums.txt`. Root release tags create the GitHub Release when needed and upload changed assets with clobber
semantics, so a failed run can be retried.
