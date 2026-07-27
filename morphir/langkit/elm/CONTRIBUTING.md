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

## Operator fixity is a second pass

`ExpressionParser.expression` deliberately builds a flat, left-leaning chain: an operator's precedence and
associativity come from an `infix` declaration that may sit anywhere in the module, so the shape is not decidable
while parsing. `OperatorReassociator`, run by `Elm.parseCst` before `TriviaAssociator`, flattens each chain and
rebuilds it — spans included — from the fixities in `OperatorTable`.

Two consequences worth knowing before trusting a tree:

- `OperatorTable.builtin` carries the `infix` declarations of `elm/core` (`Basics` plus `List`'s `(::)`), overlaid
  with the parsed module's own. An operator declared in *another* module and imported — `(|=)` from `elm/parser`,
  `(</>)` from `elm/url` — is invisible here, because resolving it needs that dependency's source. It falls back to
  `OperatorTable.unknownFixity`: precedence 9, left-associative. Anything that resolves imports should re-run the pass
  with a fuller table rather than assume the parser got it right.
- Chaining a non-associative operator (`a == b == c`) is a compile error in Elm, but this pass accepts it and groups
  it to the left. Rejecting it belongs to a semantic pass with real name resolution, not to the parser.

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
