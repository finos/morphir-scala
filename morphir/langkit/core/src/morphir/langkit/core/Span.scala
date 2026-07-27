package morphir.langkit.core

/**
 * A half-open range `[offset, offset + length)` of zero-based characters in a source file.
 *
 * Both encodings of that range are available as accessors — `offset`/`length` for syntax nodes, which build spans by
 * extending a known start, and `start`/`end` for diagnostics, which report the boundaries a caller should underline.
 * They describe the same range, so the type is deliberately singular: the earlier split into an offset/length span for
 * nodes and a start/end span for diagnostics carried no information beyond the arithmetic between them.
 */
case class Span(offset: Int, length: Int) derives CanEqual:
  /** Alias for [[offset]], for callers that think in `[start, end)` terms. */
  def start: Int = offset

  def end: Int = offset + length

object Span:
  val zero: Span = Span(0, 0)

  /** Build a span from its boundaries rather than from its extent. */
  def fromStartEnd(start: Int, end: Int): Span = Span(start, end - start)

  def between(start: Span, end: Span): Span =
    val s = start.offset
    val e = end.offset + end.length
    Span(s, e - s)
