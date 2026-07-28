---
okf_version: "0.2"
title: morphir-scala
description: "What morphir-scala does today — the Scala bindings, JVM tooling and build for Morphir."
---

# morphir-scala

What morphir-scala does today — the Scala bindings, JVM tooling and build for Morphir.

## Orientation

* [Knowledge Base Tooling](/knowledge-base-tooling.md) - The kb skill manages the OKF knowledge base and the intent recorded in it, from the command line.
* [Continuous Integration](/continuous-integration.md) - GitHub Actions runs linting, cross-platform tests and knowledge base checks on every pull request.
* [Build System](/build-system.md) - Mill drives the build from per-directory package.mill.yaml files, with mise as the task runner.
* [Cross-Platform Targets](/cross-platform-targets.md) - Modules compile to the JVM, ScalaJS, WebAssembly and Scala Native from one shared source layout.
