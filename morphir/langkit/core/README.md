# morphir-langkit-core

Source positions and diagnostic rendering, shared by every langkit.

This module is the bottom of the langkit stack. It depends on Morphir Prelude and Kyo Core, knows nothing about any
particular language, and holds what every parser needs regardless of what it parses: a way to name a region of source
text, a way to show that region back to a human, and a way to say how much a finding matters.

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

## Budgeted source scanning

`SourceScanner` gives parsers a scan-local cursor with deterministic ceilings for input length, work, nesting, and
emitted nodes. The default entry point uses conservative safe limits, and a caller can supply narrower typed limits:

A scanner session is mutable, single-owner state. It is not thread-safe and must not be shared across threads; keep
all scanner use inside its `scan` callback on one execution path.

```scala
import kyo.Result
import morphir.langkit.core.scanner.*

val defaultScan = SourceScanner.scan(source) { scanner =>
  while !scanner.isAtEnd do scanner.advance()
  scanner.metrics
}

val compiledBudget = ScanBudget.limited(
  maxInputLength = InputSize.megabytes(1),
  maxWork = WorkUnits(8L * 1024L * 1024L),
  maxNestingDepth = NestingDepth(128),
  maxOutputNodes = NodeCount(100000L)
)

compiledBudget match
  case Result.Success(budget) =>
    val limitedScan = SourceScanner.scan(source, budget) { scanner =>
      // Parser work goes here.
      scanner.metrics
    }
  case Result.Failure(error) =>
    // Report invalid caller-supplied configuration; no scan occurs.
  case Result.Panic(error) =>
    // Handle an unexpected panic; no scan occurs.
```

Literal measures (`InputSize.codeUnits(16)`, `WorkUnits(8L * 1024L * 1024L)`, and the other compile-time constructors)
are checked by an inline macro; an invalid constant fails compilation. Dynamic values go through `fromCodeUnits`,
`fromMegabytes` (and the other `from*` size constructors), and `from`, and return `Result`. Invalid budget ceilings
still return a typed `ScanBudgetError` in
`Result.Failure`; they do not throw. Keep caller-supplied configuration in this result-handling path rather than
unwrapping it. Zero is a valid measure and an invalid ceiling: `InputSize.codeUnits(0)` is a valid size, and
`ScanBudget.limited` then fails with `NonPositiveInputLength`.

Cursor movement and lookahead consume work. Use `chargeWork` for deterministic work performed outside cursor
movement, `withNesting` around recursive descent, and `chargeOutputNodes` immediately before emitting syntax nodes.
`metrics` returns an immutable snapshot of work, output nodes, and maximum nesting depth; like every scanner
operation, it is available only during the `scan` callback.

Checkpoints restore the cursor but never refund work already consumed by speculation. Source offsets, input lengths,
and view boundaries count UTF-16 code units, matching Scala `String` indices on every supported platform.

An explicit opt-out exists for callers that independently control both input size and execution isolation:

```scala
val unsafeScan = SourceScanner.scan(source, ScanBudget.UnsafeUnbounded) { scanner =>
  // The caller, not SourceScanner, now owns resource containment.
  scanner.metrics
}
```

`UnsafeUnbounded` removes all resource ceilings. Use it only when trusted upstream constraints guarantee bounded
input and bounded parser behavior, and the execution environment can contain a faulty or unexpectedly expensive
consumer. Cursor bounds, checkpoint ownership, progress checks, and callback lifetime rules still apply.

## Severity

How much a finding matters is a property of the options a pipeline runs under, not of the finding itself: the same
unresolvable operator chain is an error to a compiler and a remark to an editor. `Reported` pairs the two, generic in
whatever diagnostic type the langkit uses:

```scala
import morphir.langkit.core.{Reported, Severity}

Reported.error(diagnostic)     // the result cannot stand
Reported.advisory(diagnostic)  // worth saying; the caller wanted a result anyway
```

The Elm langkit's parse pipeline reports in these terms, and its interpreter decides what a collection of them means.

## What is not here

Diagnostic *codes* and *messages* are language-specific and live in their langkit. The Elm langkit's `DiagnosticCode`
validates `ELM-P###`/`ELM-T###`, and its `DiagnosticMessageFormatter` writes Elm-flavoured prose; both build on the
snippets and positions defined here.

The parse pipeline itself is also not here yet. `ElmParse` is Elm's, deliberately, until there is a general Morphir
parse and compile pipeline for this module to hold.

## Artifact

`org.finos.morphir::morphir-langkit-core` — JVM, Scala.js, and Scala Native.
