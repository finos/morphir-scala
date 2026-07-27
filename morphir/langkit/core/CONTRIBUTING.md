# Contributing to `morphir-langkit-core`

The namespace guide [`morphir/langkit/CONTRIBUTING.md`](../CONTRIBUTING.md) governs here, along with the root
[CONTRIBUTING.md](../../../CONTRIBUTING.md) and [AGENTS.md](../../../AGENTS.md). This file carries only what is true of
`langkit-core`.

## The bar for adding something here

This module has **no main dependencies** — not kyo, not parsley, not neotype. Its `package.mill.yaml` declares
`mvnDeps` only inside the `object test:` blocks, for kyo-test itself. Every other langkit module sits above this one,
so a dependency added to the main blocks is a dependency added under all of them. That is a stack-wide decision, not a
module-local one.

Two questions before adding a type:

1. **Is it language-agnostic?** Anything naming a specific language, or validating a language's conventions, belongs in
   that langkit instead. `DiagnosticCode` is the worked example: it looks generic, but its newtype validates
   `ELM-P###`/`ELM-T###`, so it stayed in `elm`.
2. **Would a second langkit want it unchanged?** If it would need a parameter or a tweak per language, it is probably
   not ready to live here yet.

## Do not add a second span encoding

`Span` deliberately exposes `offset`/`length` *and* `start`/`end` for one underlying range. This module previously
shipped one span type per encoding, and consolidating them was a deliberate change — see the note in
[`Span.scala`](./src/morphir/langkit/core/Span.scala).

If a caller wants a different construction shape, add a factory (`Span.fromStartEnd` is the precedent), not a type.
If it wants extra information alongside the range, wrap a `Span` the way `SourceSpan` does rather than restating
`start` and `end` as loose fields.

## Positions are zero-based, line/column are one-based

`Span` offsets are zero-based character indices. `SourceSpan.line` and `.column` are one-based, because that is what
editors and compiler output use. `SourceOffsets.offsetAt` requires `line >= 1 && column >= 1` and will throw
otherwise; `lineColumnAt` clamps its offset into range rather than throwing.

This asymmetry is easy to get wrong in both directions, so any new position helper should state which convention it
takes and hold to this split.

## Tests

`SourceSnippetBuilderSpec` and `SpanSpec` are kyo-test specs and run on all three platforms. Snippet rendering is
whitespace- and alignment-sensitive — the gutter width is derived from the highest line number shown — so assert on
exact expected strings rather than on substrings.
