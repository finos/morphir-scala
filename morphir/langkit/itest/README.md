# langkit itest

The integration suite for the langkits: Cucumber scenarios describing parser and query behaviour in Gherkin, plus
JUnit 5 tests covering the compiler ABI through a WebAssembly runtime.

This module is not published. It sits at the `langkit` level rather than under `elm` because it is the suite for all
langkits, and only happens to exercise Elm today.

## Running

```bash
./mill morphir.langkit.itest.testCached
```

Note the task name: this is a plain JVM module rather than a `<module>.jvm` one, and it exposes `testCached` rather
than `test`, so the usual `morphir.__.jvm.__` selector does not reach it. `mise run test:jvm` names it explicitly.

## Feature files

Scenarios live in [`resources/features`](./resources/features) and read as a description of the language surface:

```gherkin
Scenario: CST query surfaces a single value declaration
  Given the Elm source:
    """
    module M exposing (..)

    main = 42
    """
  When the CST is queried with "(CstValueDeclaration name: (CstName) @n)"
  Then the query matches exactly 1 time
  And capture "n" of match 1 is a "CstName"
  And capture "n" of match 1 has text "main"
```

| Feature | Covers |
| --- | --- |
| `module-parsing.feature` | Module headers, qualified names, imports |
| `declarations.feature` | Value, type alias, and custom type declarations |
| `expressions.feature` | Expressions appearing as declaration bodies |
| `patterns.feature` | Patterns in function parameter lists |
| `comments.feature` | Comment scanning and doc-comment attachment |
| `lowering.feature` | CST → AST lowering |
| `query.feature` | The tree query DSL, including canonical rendering |
| `compiler-api.feature` | The compiler ABI, per backend |
| `morphir-corpus.feature` | Real Elm modules from `finos/morphir-elm` and `finos/morphir-examples` |

Step definitions are in [`src/morphir/langkit/itest/steps`](./src/morphir/langkit/itest/steps), sharing scenario state
through `TestDriver` via the cucumber-scala DI container.

Fixtures under [`resources/fixtures`](./resources/fixtures) are real Elm sources vendored from the Morphir projects,
so `morphir-corpus.feature` exercises the parser against code nobody wrote for it.

## Compiler backends

`compiler-api.feature` runs each scenario against every supported backend, so they are held to identical behaviour:

- **`jvm`** — calls `InvokeCompiler.invoke` directly.
- **`chicory`** — drives a hand-written WebAssembly module through the [Chicory](https://chicory.dev) runtime, whose
  single host import calls back into the JVM `AbiEntryPoint`. This exercises the ABI's byte-level contract — UTF-8 in,
  UTF-8 out, through linear memory — without needing a linked Wasm build of the compiler.

`ChicoryCompilerWasmCompatibilityTest` is the other half: it reads the *real* linked `main.wasm` off disk and asserts
what the Scala.js WebAssembly backend emits — that it imports `__scalaJSHelpers:JSTag` and the `wasm:js-string:`
builtins, and exports nothing directly. Mill injects the artifact path via
`-Dmorphir.langkit.elm.compiler.api.wasm.dir`, which is also what makes the wasm module link before the suite runs.

Krueger additionally carried a `scalajs-node` backend that shelled out to Node against a linked Scala.js facade. That
facade has no counterpart here yet — it arrives with the kyo-ui based Morphir web page — so the backend and its
scenarios are left out until then.
