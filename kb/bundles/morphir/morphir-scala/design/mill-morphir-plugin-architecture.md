---
type: Design Note
title: Mill Morphir plugin architecture
description: Design for publishable Mill plugins that acquire tools and compose Morphir generation with host-language builds.
tags: [mill, plugins, elm, morphir-elm, build]
status: draft
stale_after: 2026-11-07
sources:
  - id: current-plugin
    resource: https://github.com/finos/morphir-scala/blob/a4eb4a6072a7b6de5ff1a51339ac00334256c2c7/morphir/build/package.mill
    title: Current mill-morphir-elm build module
  - id: mill-plugin-testing
    resource: https://github.com/com-lihaoyi/mill/blob/4300a0101ca1d244a2436dd51067e6036abd5744/mill-build/src/millbuild/MillJavaModule.scala
    title: Mill local plugin test repositories
  - id: mill-publish-test
    resource: https://github.com/com-lihaoyi/mill/blob/4300a0101ca1d244a2436dd51067e6036abd5744/mill-build/src/millbuild/MillPublishJavaModule.scala
    title: Mill SNAPSHOT test publication
  - id: mill-scalapb
    resource: https://github.com/com-lihaoyi/mill/blob/4300a0101ca1d244a2436dd51067e6036abd5744/contrib/scalapblib/src/mill/contrib/scalapblib/ScalaPBModule.scala
    title: Mill generated-source composition
---

# Mill Morphir plugin architecture

Morphir build integration is a family of publishable Mill plugins under `org.finos.morphir.mill`. The plugins
acquire their own tools, expose typed compilation tasks, and compose generated code with the host language instead of
replacing its compiler lifecycle.

This is an evolving design. The current slice restores Elm-to-Morphir IR generation and runtime tests. The same
boundaries leave room for other frontends and code-generation backends. The broader transformation model remains in
the [pipeline and workspace Design Note](/design/pipeline-workspace-boundaries.md). Package identity and source
materialization remain in the
[Package URL package-management Design Note](/design/package-url-package-management.md).

This work advances the
[multi-frontend pipeline intent](../../../intent/0007-multi-frontend-morphir-transformation-pipeline.md) and its
[standard pipeline slice](../../../intent/0009-standard-morphir-build-pipeline.md). It provides Mill integration for
those capabilities; it does not implement the buildkit pipeline itself.

## Goals

- Make verified tool acquisition a Mill capability rather than a Mise prerequisite.
- Develop the plugins beside morphir-scala and dogfood their published boundary.
- Keep JavaScript runtimes/package managers, Elm, and Morphir Elm concerns in focused packages and artifacts.
- Preserve Mill's task graph, caching, generated-source, BSP, and testing behavior.
- Support Morphir packages whose sources are not published to an ecosystem registry.

The first slice does not:

- remove every Mise task;
- implement the future Morphir package manager;
- file an upstream Package URL proposal; or
- require generated Scala to live in a separate module.

## Plugin family

The source lives under `mill-plugins/morphir/`; `mill` is already the repository's launcher file. All artifacts
initially share one release version.

| Artifact | Package | Responsibility |
| --- | --- | --- |
| `mill-morphir-toolchain` | `org.finos.morphir.mill.toolchain` | Verified acquisition, platform selection, and safe extraction |
| `mill-morphir-javascript` | `org.finos.morphir.mill.javascript` | JavaScript runtime/package-manager contracts with provisioned Node and locked npm support |
| `mill-morphir-elm-tooling` | `org.finos.morphir.mill.elm` | Elm tools and Elm-language compilation |
| `mill-morphir-core` | `org.finos.morphir.mill` | Frontend-neutral Morphir project, IR artifact, and generation contracts |
| `mill-morphir-elm` | `org.finos.morphir.mill.elm.morphir` | Elm-project-to-Morphir-IR compilation |

The existing `mill-morphir-elm` shell and the working metabuild implementation move into this family. They do not
remain as parallel implementations. Plugin artifacts depend on Mill APIs and lower plugin layers, not on the
morphir-scala application modules being built.

`mill-morphir-javascript` is one deployable plugin jar. Its public contracts describe runtime execution, project and
lock inputs, installation, and local package-binary invocation without naming npm or assuming one file of each type.
Node and npm are the first implementations. Yarn, pnpm, Bun, and Deno can be added to the same jar without changing
consumers or creating one artifact per tool.

The metabuild and published modules compile the same source tree. Source sharing solves the first-release bootstrap
without preserving a private implementation under `mill-build`.

`mill-plugins/morphir/integration` is test-only. It publishes the five artifacts to a task-local repository and runs
a fresh consumer build against them.

## Compilation task ownership

Host-language modules own `compile`. A Morphir capability that may mix into `ScalaModule` must not override or
replace `ScalaModule.compile` merely to force ordering.

The phases instead have distinct typed tasks:

1. `MorphirElmModule.morphirIR` compiles Elm project inputs into a `MorphirIrArtifact`.
2. A backend-specific generation task consumes that artifact and returns generated sources.
3. A Scala adapter appends those sources through `super.generatedSources()`.
4. `ScalaModule.compile` remains the task that compiles Scala.

`MorphirElmModule.make` remains as a compatibility alias. An Elm language module may own its own `compile` task;
the Morphir Elm adapter does not.

The current Scala integration generates sources into the same module. Tests prove that handwritten code compiles
against them and that same-path IR changes invalidate generation and compilation. Generation results retain enough
identity and location information for a future adapter to expose them as a distinct downstream module.

## Acquisition and caching

Acquisition uses pinned versions and checksums. Archives are verified before safe extraction, and tools are invoked
through explicit executable paths rather than ambient `PATH` lookup.

A machine-level content-addressed cache is enabled by default:

- Entries are keyed by cryptographic digest and promoted atomically after verification.
- Concurrent workspaces may share it safely.
- Its location can be overridden and it can be disabled.
- Corrupt entries are rejected and reacquired.
- Offline mode succeeds only when verified content is already available.

The machine cache is an optimization, not a project requirement. Removing it cannot change build results. npm cache
paths, `ELM_HOME`, `elm-stuff`, tool homes, and sandbox layouts remain private implementation details. GitHub Actions
may cache these verified entries without adding CI concepts to the public plugin API.

## Project execution

Each Morphir project keeps configuration in its colocated `package.mill.yaml`. Declared sources, `elm.json`,
`morphir.json`, typed upstream IR dependencies, options, and tool versions are task inputs.

Compilation stages an isolated workspace under `Task.dest`. Source projects are not modified; generated IR, hashes,
dependency views, npm installations, Elm state, and generated sources remain under Mill-controlled output paths.
Content identity must invalidate downstream tasks even when a generated artifact retains the same path.

Source-level dependencies may be materialized into the sandbox without being published to Elm's package manager.
The frontend consumes the resulting source or typed IR view; it does not expose an Elm cache workaround as the
package-management contract.

## Typed module identity

Public APIs use an opaque `ModuleId`, not a raw string:

- Computed IDs use one parser for portable, lower-case, dot-separated segments.
- Static IDs use a `moduleId"..."` interpolator with the same validation.
- Invalid IDs return a typed `Error` that is also a source-located exception.
- Raw text is exposed only for serialization, diagnostics, commands, and bounded sandbox paths.

There is no public unchecked constructor.

## Failure contract

Failures name their layer and corrective action: unsupported platform, verification failure, invalid lockfile,
project or dependency error, compiler diagnostic, or missing output. Process execution records the tool version,
working directory, command, and captured diagnostics in the Mill task log without exposing credentials.

Public validation errors remain typed ADTs, also extend an exception type, and carry source-location data where the
caller can act on it.

External processes receive an explicit environment. Proxy, registry, and credential settings enter through
documented inputs rather than unrestricted ambient environment propagation.

## Dogfood and development paths

Normal clean checkouts compile the current `mill-plugins/morphir/` sources into the metabuild and need no bootstrap
command.
The publishable modules compile that same source tree, so there is one implementation.

The acceptance path uses two Mill evaluations:

1. Publish the current plugin family as `SNAPSHOT` artifacts to a task-local repository.
2. Start a fresh consumer evaluation that selects that repository and plugin version.
3. Generate project IR and compile dependent Scala through the resolved artifacts.

Plugin version and repository selection are tracked metabuild inputs. The fast path uses direct module tests,
`UnitTester`, and selected integration fixtures without publication. It shortens iteration but never replaces the
artifact-based acceptance path.

After the first Mill 1.x plugin release, the project may switch normal builds to a pinned artifact. That choice does
not weaken the local-SNAPSHOT acceptance path and should be made from measured maintenance and bootstrap experience.
The decision for now is recorded in
[0012 — Keep compiling Mill Morphir plugins into the metabuild](/decisions/0012-keep-source-metabuild-for-mill-morphir-plugins.md):
publication is enabled; the metabuild stays source-compiled until the revisit conditions there are met.

Mill uses the same basic pattern: its tests publish current modules into local test repositories and inject those
repositories into isolated consumer runs. It also separates fast and packaged integration launchers.

## Worker policy

Elm and Morphir Elm compilation begin as normal cached tasks that invoke external processes. A Mill worker is added
only when:

- a supported reusable compiler, classloader, or daemon protocol exists;
- measurement shows a meaningful gain;
- concurrent access, invalidation, and cleanup are defined; and
- worker and clean-daemon runs produce equivalent results.

An in-JVM Morphir backend may meet these conditions earlier than Elm or Morphir Elm. The public task contract does
not change if a worker is introduced internally.

## Verification

Scala tests cover pure logic and Mill modules. `IntegrationTester` exercises isolated builds that resolve locally
published plugin artifacts.

Required acceptance cases include:

- cold, warm, disabled, corrupt, and offline acquisition-cache behavior;
- locked npm installation without system Node or npm;
- Elm and Morphir Elm compilation without ambient tools;
- cache invalidation for sources, configuration, dependencies, lockfiles, and tool versions;
- concurrent machine-cache use;
- a used source dependency that is not published to Elm's package manager; and
- generated Scala discovered through `generatedSources`, compiled normally, and invalidated by same-path IR changes.

Focused CI jobs are `mill-morphir-unit`, `mill-morphir-integration`, `morphir-elm-projects`,
`runtime-generated-fixtures`, and `runtime-tests`. Runtime verification asserts required test-class discovery so a
green job cannot hide suites that Mill no longer discovers.

## Guidance and evolution

`mill-plugins/morphir/AGENTS.md` carries only implementation invariants and links here for reasoning. Its sibling
`CLAUDE.md` contains `@AGENTS.md`. Squire documents the Scala/Mill dogfood and fast paths and diagnoses bootstrap,
repository, cache, and metabuild invalidation problems; it does not duplicate this design or implement the workflow
in Python.

The current vertical slice validates compile ownership and generated-source composition. A separate generated module
should be added only when a real consumer needs independently addressable code, different compiler settings, or reuse
by multiple downstream modules. This note remains the evolving record until that boundary is stable enough for an
immutable Decision Record.
