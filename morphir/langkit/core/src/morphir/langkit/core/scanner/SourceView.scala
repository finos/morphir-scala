package morphir.langkit.core.scanner

import morphir.langkit.core.Span

sealed abstract case class SourceView private (source: String, span: Span) derives CanEqual:
  /** Inclusive start of this half-open source range, measured in UTF-16 code units. */
  // unsafe: fromSpan already validated span.offset >= 0.
  def start: SourceOffset = SourceOffset.unsafe(span.offset)

  /** Exclusive end of this half-open source range, measured in UTF-16 code units. */
  // unsafe: fromSpan already validated span.offset >= 0 and span.length >= 0, so span.end cannot be negative.
  def end: SourceOffset = SourceOffset.unsafe(span.end)

  // unsafe: fromSpan already validated span.length >= 0.
  def length: CodeUnitCount = CodeUnitCount.unsafe(span.length)

  def isEmpty: Boolean = span.length == 0

  def nonEmpty: Boolean = !isEmpty

  def charAt(relativeOffset: CodeUnitCount): Char =
    val relativeIndex = relativeOffset.toInt
    if relativeIndex >= span.length then
      throw new IndexOutOfBoundsException(s"relative offset $relativeIndex outside view of length ${span.length}")
    source.charAt(span.offset + relativeIndex)

  def text: String = source.substring(span.offset, span.end)

object SourceView:
  def fromSpan(source: String, span: Span): SourceView =
    require(span.offset >= 0, "span offset must be non-negative")
    require(span.length >= 0, "span length must be non-negative")
    require(
      span.offset.toLong + span.length.toLong <= source.length.toLong,
      "span end must not exceed source length"
    )
    new SourceView(source, span) {}
