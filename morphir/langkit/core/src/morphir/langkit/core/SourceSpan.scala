package morphir.langkit.core

/**
 * A diagnostic's source location: a character [[Span]] plus the 1-based line and column its range starts at.
 *
 * The line/column pair is what distinguishes this from a bare [[Span]] — it is derived from the source text (see
 * [[SourceOffsets]]) and carried alongside the range so diagnostics can be reported without the source in hand.
 */
final case class SourceSpan(range: Span, line: Int, column: Int) derives CanEqual:
  export range.{end, start}

object SourceSpan:
  /** Build a source span from range boundaries, the form parsers and tokenizers report errors in. */
  def fromStartEnd(start: Int, end: Int, line: Int, column: Int): SourceSpan =
    SourceSpan(Span.fromStartEnd(start, end), line, column)
