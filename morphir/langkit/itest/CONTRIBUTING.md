# Contributing to `langkit itest`

The namespace guide [`morphir/langkit/CONTRIBUTING.md`](../CONTRIBUTING.md) governs here, along with the root
[CONTRIBUTING.md](../../../CONTRIBUTING.md) and [AGENTS.md](../../../AGENTS.md). This file carries only what is true of
`itest`.

## This module is not like the others

Three ways it departs from every other module under `langkit`, all of which have caught people out:

- **It is JVM-only and not cross-built.** Its `package.mill.yaml` extends `build.MorphirCommonModule`, not
  `MorphirJVMModule` — the cross-platform base resolves sources relative to the *parent* directory, which made it read
  `morphir/langkit/` instead of `morphir/langkit/itest/`.
- **Its task is `testCached`, not `test`,** and it is not matched by `morphir.__.jvm.__`. CI names it explicitly in
  `.config/mise/tasks/test/jvm`; a new selector that is meant to cover it has to do the same.
- **It is not published.** It has no `MorphirPublishModule` and should not gain one.

## Adding a scenario

Feature files go in [`resources/features`](./resources/features), step definitions in
[`src/morphir/langkit/itest/steps`](./src/morphir/langkit/itest/steps). Cucumber finds them through
[`resources/junit-platform.properties`](./resources/junit-platform.properties), which pins the glue package — a step
class outside `morphir.langkit.itest.steps` is silently never registered.

Prefer expressing new coverage as scenarios over new step verbs. The existing verb set is deliberately generic ("the
CST is queried with", "capture X of match N has text Y"), so most new cases are a feature-file edit and no Scala at
all. Scenario state is shared between step classes through `TestDriver`, injected by the cucumber-scala DI container —
put new state there rather than in a step class.

Fixtures under [`resources/fixtures`](./resources/fixtures) are vendored verbatim from `finos/morphir-elm` and
`finos/morphir-examples`. Keep them verbatim: their value is that nobody wrote them for this parser. Reference them
with the `Given the Elm fixture {string}` verb rather than pasting excerpts inline.

## Backend parity

`compiler-api.feature` is a scenario outline over backends, and the point is that every backend produces identical
output for identical input. A new backend is a row in the `Examples:` table plus a case in `TestDriver.invoke` and its
`supportedBackends` set — an unlisted backend name fails with an explicit assertion rather than a silent skip.

Do not weaken a shared scenario to accommodate one backend. If a backend genuinely cannot satisfy a case, that is a
finding about the backend.

## The Wasm tests

`ChicorySupportedCompilerHarness` and `ChicoryCompilerWasmCompatibilityTest` look similar and test opposite things:

- The **harness** builds a small WAT module whose only import calls back into the JVM `AbiEntryPoint`. It never loads
  the linked compiler; it exercises the ABI's byte contract through linear memory. It needs no build wiring.
- The **compatibility test** reads the real linked `main.wasm`, via the path Mill injects as
  `-Dmorphir.langkit.elm.compiler.api.wasm.dir` (see `MorphirLangkitItestWasmArtifacts` in `build.mill`). Taking that
  task dependency is also what forces the wasm module to link before the suite runs — removing it does not just drop
  the property, it stops the artifact being built.

The harness caps operation names, input, and output at fixed offsets in a single page of linear memory and raises an
explicit error when a payload exceeds them. A scenario with a large source will hit that ceiling; raise the constants
deliberately rather than shrinking the fixture to fit.

## Expected noise

A passing run prints `RuntimeException: Test class io.cucumber.junit.platform.engine.CucumberTestEngine is not
enclosed by ...` and a `TestExecutionListener ... threw exception` warning. Both come from Mill running each test
class through the JUnit Platform suite engine and are harmless. The line that matters is the final
`Test run finished: 0 failed`.
