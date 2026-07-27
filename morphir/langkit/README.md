# morphir-langkit

Language toolkits: the machinery for reading a source language into a tree, querying that tree, and reporting
diagnostics against the original text.

A *langkit* is everything needed to take one language from source text to a queryable syntax tree. The Elm langkit is
the first; the namespace is laid out so later ones sit beside it and reuse the shared pieces rather than growing their
own copies.

## Modules

| Module | Artifact | What it holds |
| --- | --- | --- |
| [`core`](./core) | `morphir-langkit-core` | Language-agnostic source positions and diagnostic rendering — `Span`, `SourceSpan`, `SourceOffsets`, `SourceSnippetBuilder` |
| [`trees`](./trees) | `morphir-langkit-trees` | The `QueryableTree[T]` typeclass and a tree-sitter-inspired query DSL, generic over any tree |
| [`elm`](./elm) | `morphir-langkit-elm-core`, `morphir-langkit-elm-compiler-api` | The Elm langkit: lexer, parser, CST/AST, diagnostics, and a Kyo-backed compiler API |
| [`itest`](./itest) | *(not published)* | Cucumber/JUnit5 integration suite covering the langkits end to end |

`core` and `trees` are the shared layer: neither knows anything about Elm, and a second langkit should depend on both
rather than reimplementing spans, snippets, or queries.

`itest` sits here rather than under `elm` for the same reason — it is the suite for *all* langkits, and only happens to
exercise Elm today.

## Platforms

Everything here cross-builds for the JVM, Scala.js, and Scala Native. `elm/compiler/api` additionally has a `wasm`
variant: the same sources as `js`, linked through Scala.js' WebAssembly backend to emit a `main.wasm` binary alongside
an ES-module loader.

```bash
./mill morphir.langkit.__.test              # every platform
./mill morphir.langkit.elm.core.native.test # one module, one platform
./mill morphir.langkit.itest.testCached     # the integration suite
```

Note that `itest` is JVM-only and exposes `testCached` rather than `test`, so the usual `morphir.__.jvm.__` selector
does not reach it.

## Attribution

This namespace was ported from [Eleven19/krueger](https://github.com/Eleven19/krueger), an Elm compiler written in
Scala. The `io.eleven19.krueger` packages became `morphir.langkit.elm`, and `io.eleven19.krueger.trees` became
`morphir.langkit.trees`.
