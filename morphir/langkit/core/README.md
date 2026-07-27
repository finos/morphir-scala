# morphir-langkit-core

Source positions and diagnostic rendering, shared by every langkit.

This module is the bottom of the langkit stack. It has no dependencies beyond the standard library, knows nothing
about any particular language, and holds the two things every parser needs regardless of what it parses: a way to
name a region of source text, and a way to show that region back to a human.

## Positions

`Span` is a half-open range `[offset, offset + length)` of zero-based characters, exposing both encodings of that
range:

```scala
import morphir.langkit.core.Span

val s = Span(offset = 4, length = 3)
s.start  // 4  — alias for offset
s.end    // 7
Span.fromStartEnd(4, 7)          // same span, built from boundaries
Span.between(Span(2, 3), Span(10, 2))  // Span(2, 10) — first start through last end
```

Syntax nodes tend to build spans by extending a known start, so they want `offset`/`length`; diagnostics report the
boundaries a caller should underline, so they want `start`/`end`. Both are views on one type — an earlier split into
two span types carried no information beyond the arithmetic between them.

`SourceSpan` adds what a diagnostic knows and a bare span does not: the 1-based line and column its range begins at.

```scala
final case class SourceSpan(range: Span, line: Int, column: Int):
  export range.{end, start}
```

`SourceOffsets` converts between the two coordinate systems — `offsetAt(source, line, column)` and
`lineColumnAt(source, offset)`.

## Snippets

`SourceSnippetBuilder` renders the familiar gutter-and-caret excerpt, given source text and an error position:

```scala
import morphir.langkit.core.SourceSnippetBuilder

val snippet = SourceSnippetBuilder.build(
  source = "module M exposing (..)\n\nmain =\n",
  errorLine = 3,
  column = 7,
  errorWidth = 1
)
println(snippet.rendered)
```

```
1| module M exposing (..)
2|
3| main =
         ^
```

It returns both the rendered string and the structured `contextLines` behind it, so a caller that wants to render its
own way — a language server, a JSON envelope — does not have to parse the text back apart. Two lines of context
before and one after by default, both overridable.

## What is not here

Diagnostic *codes* and *messages* are language-specific and live in their langkit. The Elm langkit's `DiagnosticCode`
validates `ELM-P###`/`ELM-T###`, and its `DiagnosticMessageFormatter` writes Elm-flavoured prose; both build on the
snippets and positions defined here.

## Artifact

`org.finos.morphir::morphir-langkit-core` — JVM, Scala.js, and Scala Native.
