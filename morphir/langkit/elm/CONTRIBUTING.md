# Contributing to `morphir-langkit-elm`

The namespace guide [`morphir/langkit/CONTRIBUTING.md`](../CONTRIBUTING.md) governs here, along with the root
[CONTRIBUTING.md](../../../CONTRIBUTING.md) and [AGENTS.md](../../../AGENTS.md). This file carries only what is true of
the Elm langkit.

## Check whether it is actually Elm-specific

This module is where language-agnostic code accumulates by accident, because the original port renamed a whole
upstream namespace into it at once. Two clusters have already had to move out — the Kyo `Log` ↔ scribe bridge to
[`morphir/kit`](../../kit), and positions and snippet rendering to [`langkit/core`](../core).

Before adding a type here, ask whether a second langkit would want it unchanged. If yes, it belongs in `core` or
`trees`.

The line currently runs through the diagnostic types: `DiagnosticCode` and `DiagnosticMessageFormatter` are Elm's
because the codes are `ELM-`-prefixed and the prose is Elm-flavoured; `SourceOffsets`, `SourceSnippetBuilder`,
`DiagnosticContextLine`, `Span`, and `SourceSpan` are shared and live in `core`.

## Keep the CST and AST in step

The two trees are not independent: `CstLowering.lowerModule` maps one onto the other, and both carry `QueryableTree`
instances, unist projections, cursors, and visitors. A new node type is not one change but a set:

1. The CST node, and its parser production.
2. The AST node, if the construct survives lowering, and the `CstLowering` case that produces it.
3. `CstQueryableTree` / `AstQueryableTree` — remembering that every value in `fields` must also appear in `children`
   (see [`trees/CONTRIBUTING.md`](../trees/CONTRIBUTING.md)).
4. The unist projections and the visitors, which are exhaustive over node types.

`nodeType` is the raw case-class simple name and query text names it directly, so renaming a CST or AST case class
breaks every query and feature file mentioning it.

## Trivia is part of the CST contract

The CST keeps comments and attaches them to declarations — `CommentScanner` finds them and `TriviaAssociator` binds
them. That association is positional and uses `span.offset` of the preceding element as a lower bound, so a change to
how spans are assigned during parsing can silently re-attach doc comments to the wrong declaration. The lowering path
drops trivia entirely, so an AST-only test will not catch it.

## Conformance is the standard, and departures are configured

The target is `elm/compiler`: what it accepts, we accept; what it rejects, we reject. Where a caller could reasonably
want something else, it is a field on `ElmParseOptions` that defaults to Elm's behaviour — never a quiet allowance in
the parser. `ElmParseOptions.elm` is the default everywhere; `ElmParseOptions.lenient` is for tooling that wants a
tree out of text that does not compile.

When you find a divergence, the choices are to fix it or to record it as a known one in
[`conformance.html`](./conformance.html) with a test that pins the current behaviour and says what Elm does instead.
Do not leave it undocumented, and do not "fix" it by loosening something else.

Upstream behaviour is checked against `elm/compiler`'s parser rather than intuition — `Parse/Symbol.hs` for the
operator character set and reserved sequences, `Parse/Expression.hs` for negation and flat operator chains,
`Canonicalize/Expression.hs` and `Reporting/Error/Canonicalize.hs` for how chains resolve and how conflicts read.

## The pipeline is an effect, and policy lives in the interpreter

Parsing is several passes — syntax, operator re-association, comment association, with layout and lexical checks to
come — and they share three needs: the options in force, somewhere to put what they found, and a way to give up.
`ElmParse` is a Kyo `ArrowEffect` carrying exactly those, as `ElmParseOp.Options`, `ElmParseOp.Report` and
`ElmParseOp.Halt`.

A stage's signature is `CstModule < ElmParse`. It has no options parameter, no accumulator, and no early-return
plumbing: it asks for what it needs and says what it found. Whether a report is fatal, whether reporting one stops
the pipeline, and where the options come from are the *interpreter's* business.

`ElmParse.run` is the interpreter this module ships: it collects every report and withholds the value if any was an
error. That is why a module with four unresolvable operator chains now describes all four rather than the first — a
`Report` resumes the stage, while a `Halt` drops the continuation, because a stage that halted has nothing to hand
on. A different interpreter is a legitimate thing to write instead; `ParseEffectSpec` has one that keeps the tree the
shipped interpreter would withhold, over identical stages and options.

The name is Elm's on purpose. A general Morphir parse and compile pipeline is coming, and this is meant to become its
Elm instance rather than its definition, so `Parse` and `Compile` stay unclaimed. The parts that are already
language-neutral have moved down to [`langkit/core`](../core): `Severity`, and `Reported[D]` generic in each
langkit's own diagnostic type.

Three working rules:

- A new pass belongs in `ElmParse` as a stage, not as a function taking options and returning `Either`.
- Purity inside a stage is fine and often better. `OperatorReassociator` is an ordinary recursive function over the
  tree that hands back what it found; suspending inside a tree walk that makes no requests would only add noise. The
  effect boundary is the stage, not every function it calls.
- Anything a second langkit would need unchanged — severity, reporting, positions — belongs in `core`, per the first
  section of this file. The effect itself does not, until the general pipeline exists to hold it.

`Elm` remains the plain façade for callers who want a tree or a diagnostic: `parseCst` / `parseAst` report the first
problem, `diagnoseCst` / `diagnoseAst` report everything.

## Every divergence is written down

Where the parser differs from `elm/compiler`, there is a row in the gap ledger of
[`conformance.html`](./conformance.html) — the convergence tracker for this module — and, wherever the divergence can
be written as "this valid Elm does not parse", an assertion pinning that in a `KnownGapsSpec`.

The tracker lives beside the module rather than under `.dev/`, which is gitignored: a ledger nobody else can open is
not a ledger. It also carries the workstream status, the `elm/compiler` sources each rule was checked against, and
the diagnostic codes this work introduced.

Closing a gap therefore fails the suite: the pin says the construct is rejected, and it no longer is. That is
deliberate. A gap cannot be closed without deleting its row and its pin in the same commit, and cannot be forgotten
while the row is still there. `KnownGapsSpec` does not exist at the moment because W6 and W7 emptied it — recreate it
for the next divergence rather than leaving one in a comment.

The ledger's numbering does not reuse: a closed gap keeps its number retired, so a reference to G3 in a commit or an
issue keeps meaning what it meant.

## Layout is grammar, not formatting

Elm is indentation-sensitive, and the indentation is what tells the parser where things end. A top-level declaration
begins in column 1; the bindings of a `let` line up with each other; the branches of a `case` line up with each
other. `ElmLexer` states these as `atTopLevel`, `atColumn` and `aligned`, and the productions that need them say so.

Get one wrong and you do not get a formatting complaint — you get one declaration's tail silently attached to the
next, or a `case` that swallows the declaration below it. `LayoutSpec` exists to catch exactly that, which is why its
assertions are about how many declarations and branches came out rather than about parse success.

A layout violation halts rather than reports: the pipeline cannot carry on when it does not know where the block
ended. That is the difference from an operator chain it can describe and keep going past.

Expression continuation is still the approximation `sameLineOrIndentedPast`, measured from the expression's first
token, where Elm threads a real indentation context. It agrees with Elm on everything currently covered; replacing it
is G5 in the [conformance tracker](./conformance.html).

## Tokens know whether they touch

Elm's grammar keeps asking whether two tokens are adjacent: `a.b` is field access while `a . b` is an error,
`List.map` is a qualified name while `List . map` is not, and `f -1` applies `f` to `-1` while `f - 1` subtracts. A
token that consumes its own trailing whitespace has thrown that information away.

So `ElmLexer.raw` exposes non-lexeme tokens — they stop at their last character — and the expression atoms are built
from those, with one explicit `whiteSpace` at the end of `postfixAtom` or of a whole application. `ModuleParser`
follows the same split: `rawLowerName` / `lowerName`, `rawQualifiedName` / `qualifiedName`. Productions above the atom
level keep using the lexeme parsers.

The rule when adding a production: if Elm cares whether your token touches its neighbour, build it from `raw` and
consume `whiteSpace` yourself at the one place whitespace is allowed. Otherwise use the lexeme parsers and do not
think about it.

## Operator fixity is a second pass

`ExpressionParser.expression` builds a flat, left-leaning chain, and `OperatorReassociator` — run by `Elm.parseCst`
before `TriviaAssociator` — rebuilds it, spans included, from the fixities in `OperatorTable`. This is not a
workaround: `elm/compiler` also parses chains flat (`Src.Binops`) and resolves precedence during canonicalisation,
because an operator's fixity comes from an `infix` declaration that may appear anywhere in the module or in a
dependency.

The pass refuses what Elm refuses:

- An operator whose fixity nothing in scope declares is an error (`ELM-P005`), not an assumed precedence.
  `OperatorTable.wellKnown` bundles the official packages that declare operators — `elm/core`, `elm/parser`,
  `elm/url` — and the parsed module's own `infix` declarations are overlaid on top. Fixities are matched by operator
  name, not by resolving imports: the parser has no dependency source to resolve against, so a caller that does
  resolve them should supply a fuller table through `ElmParseOptions.operators`.
- Adjacent operators of equal precedence that cannot be grouped — a non-associative operator chained (`a == b == c`),
  or a left- and a right-associative operator mixed (`a |> f <| g`) — are an error (`ELM-P004`), worded as Elm words
  it.

Both are `Leniency.Accept`-able through `ElmParseOptions` for callers that would rather have a guessed tree.

Changing a fixity changes tree *shape* rather than parse success, so the coverage for it — `OperatorPrecedenceSpec`
and the precedence scenarios in [`itest`](../itest)'s `expressions.feature` — asserts on shape. A "does it parse"
assertion cannot catch a regression here.

## Diagnostics are snapshot-tested

`ParseDiagnosticMessageSnapshotSpec` asserts on rendered diagnostic text, gutter alignment and caret position
included. Changing message wording, snippet width, or code assignment means updating those snapshots deliberately —
they are the closest thing this module has to a spec for its user-facing output.

Every `ParseDiagnostic` should carry a real `DiagnosticCode`; the newtype refuses anything outside `ELM-P###` and
`ELM-T###`, so a new failure mode needs a new code constant rather than a reused one.

## The wasm variant

`compiler/api` has a fourth build target beside jvm/js/native: `wasm`, which compiles the *same sources* as `js` and
differs only in linker configuration. Its `moduleDeps` therefore point at the `js` modules of `core` and `trees`.

Two things to keep in mind:

- [`compiler/wasm/Main.scala`](./compiler/api/src/morphir/langkit/elm/compiler/wasm/Main.scala) is a no-op `main`
  that exists solely so the linker has a reachable entry point and emits `main.js` and `main.wasm`. It looks like dead
  code and is not.
- The WebAssembly backend treats `@JSExport*` differently from the JS linker: `@JSExport` on members is dropped, and
  `@JSExportTopLevel` on an `object` yields an export with no members. Only `@JSExportTopLevel` on a top-level `val`
  is honoured. Any FFI surface added here has to use the `val` form to survive the wasm link.

The variant is deliberately not a publish module — its coordinate carries the same `_sjs1_3` suffix as `js`.

## Tests

kyo-test, per the namespace guide, on all platforms including `wasm`. The wasm test block mixes in
`build.MorphirWasmTests`: without it, Mill's `ScalaJSTests` links the tests as plain JS and they silently duplicate the
`js` run rather than exercising the Wasm output.

End-to-end behaviour is covered by [`langkit/itest`](../itest) rather than here.
