package morphir.langkit.elm

/** A source location represented as a zero-based offset and length. */
case class Span(offset: Int, length: Int) derives CanEqual:
  def end: Int = offset + length

object Span:
  val zero: Span = Span(0, 0)

  def between(start: Span, end: Span): Span =
    val s = start.offset
    val e = end.offset + end.length
    Span(s, e - s)
