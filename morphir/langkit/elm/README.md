# morphir-langkit-elm

The Elm langkit: source text in, queryable syntax tree out, with diagnostics that point back at the original text.

## Modules

| Module | Artifact | What it holds |
| --- | --- | --- |
| [`core`](./core) | `morphir-langkit-elm-core` | Lexer, parser, CST and AST, Elm diagnostics, and the `Elm` facade |
| [`compiler/api`](./compiler/api) | `morphir-langkit-elm-compiler-api` | A Kyo-backed compiler surface and a JSON ABI for calling it across a language boundary |

## Parsing

`Elm` is the entry point. Both methods return `parsley.Result`, so a failure is a value rather than an exception:

```scala
import morphir.langkit.elm.Elm
import parsley.{Success, Failure}

Elm.parseCst("module M exposing (..)\n\nmain = 42\n") match
  case Success(cst)        => cst.declarations
  case Failure(diagnostic) => println(diagnostic.message)
```

- **`parseCst`** produces a concrete syntax tree — every token, plus comments and their attachment to declarations.
  This is the tree for formatters, editors, and anything that has to reproduce the original text.
- **`parseAst`** lowers that CST into an abstract syntax tree, dropping trivia. This is the tree for analysis.

Both take an `ElmParseOptions`, defaulting to `ElmParseOptions.elm` — canonical Elm semantics. Binary operator chains
come out shaped by Elm's precedence and associativity rules, so `1 + 2 * 3` parses as `1 + (2 * 3)` and
`a :: b :: rest` as `a :: (b :: rest)`, and a chain Elm refuses to group (`a == b == c`) is a diagnostic rather than a
guess:

```scala
import morphir.langkit.elm.{Elm, ElmParseOptions}

Elm.parseCst(source)                             // canonical Elm
Elm.parseCst(source, ElmParseOptions.lenient)    // best-effort tree for tooling
```

To see everything a parse found rather than the first problem, ask for the diagnoses instead — a module with four
unresolvable operator chains describes all four:

```scala
val outcome = Elm.diagnoseCst(source)
outcome.messages   // every diagnostic, in source order
outcome.errors     // the ones that stopped it
outcome.value      // the tree, if one survived
```

Both are the same pipeline: stages composed in the `ElmParse` effect, with `ElmParse.run` interpreting them. Compose
the stages directly to run parsing alongside your own Kyo effects, or interpret them your own way — see
[The pipeline is an effect](./CONTRIBUTING.md#the-pipeline-is-an-effect-and-policy-lives-in-the-interpreter).

```scala
import morphir.langkit.elm.{ElmParse, ElmParseOptions}

val pipeline = ElmParse.cst(source)              // CstModule < ElmParse
ElmParse.run(ElmParseOptions.elm)(pipeline)      // ElmParse.Outcome[CstModule]
```

See [Operator fixity is a second pass](./CONTRIBUTING.md#operator-fixity-is-a-second-pass) for what the parser knows
about operators declared elsewhere, and
[the conformance plan](../../../.dev/.sdlc/elm-parser-conformance/PLAN.md) for the divergences still being worked
through.

Both trees have `QueryableTree` instances, so the [query DSL](../trees) works against either:

```scala
import morphir.langkit.elm.cst.CstQueryableTree.given
import morphir.langkit.elm.ast.AstQueryableTree.given
```

They also project to [unist](https://github.com/syntax-tree/unist) nodes, and both offer cursor and visitor traversal
(`CstCursor`/`AstCursor`, `CstVisitor`/`AstVisitor`, plus Kyo-effectful `KyoCstVisitor`/`KyoAstVisitor`).

## Diagnostics

Parse failures carry a `ParseDiagnostic`: a stable code, a `SourceSpan`, a formatted message, the expected tokens, an
optional suggestion, and the surrounding source lines.

```
-- PARSE ERROR ------------------------------------------------ 3:7

I was partway through parsing a value declaration when I hit the end of the input.

Expected: an expression

1| module M exposing (..)
2|
3| main =
         ^
```

Codes are `ELM-P###` for parse errors and `ELM-T###` for tokenizer errors, validated at construction by the
`DiagnosticCode` newtype. Rendering builds on [`langkit-core`](../core)'s snippet machinery.

## Compiler API

`compiler/api` wraps the parser in a `CompilerComponent[Ctx]`, whose operations return Kyo effects rather than raw
results, so parse failures, query failures, and internal errors all surface as structured `CompileError` values in one
envelope:

```scala
trait CompilerComponent[Ctx]:
  def parseCst(source: String): CompileEff[Ctx, CstModule]
  def parseAst(source: String): CompileEff[Ctx, AstModule]
  def parseQuery(q: String): CompileEff[Ctx, Query]
  def runQuery[T](q: Query, root: T)(using QueryableTree[T]): CompileEff[Ctx, List[MatchView]]
  def prettyQuery(q: Query): String
```

`ElmCompiler.defaultCompiler` is the ready-made instance; `ElmCompiler.compiler[Ctx]` threads a caller-chosen context
for composition with your own stateful effects.

The `abi` package puts a JSON envelope around that surface for callers that are not Scala — `InvokeCompiler.invoke`
takes an operation name and a JSON request string and returns a JSON response, and `AbiEntryPoint.invokeUtf8` does the
same over raw UTF-8 bytes. Responses are deterministic: the same request yields byte-identical JSON.

| Operation | Request |
| --- | --- |
| `parseCst`, `parseAst` | `{"source": "..."}` |
| `parseQuery`, `prettyQuery` | `{"query": "..."}` |
| `runQuery` | `{"query": "...", "source": "...", "treeKind": "cst" \| "ast"}` |

`runQuery` names its tree by source rather than accepting a pre-serialized one: nothing in the langkit deserializes a
CST or AST from JSON.

## Platforms

`core` and `compiler/api` both build for the JVM, Scala.js, and Scala Native. `compiler/api` additionally has a `wasm`
variant — the same sources as `js`, linked through Scala.js' WebAssembly backend to emit `main.wasm` plus an ES-module
loader:

```bash
./mill morphir.langkit.elm.compiler.api.wasm.fullLinkJS
```

The `wasm` variant is not published; its Maven coordinate would collide with `js`.

## Artifacts

`org.finos.morphir::morphir-langkit-elm-core` and `org.finos.morphir::morphir-langkit-elm-compiler-api`.
