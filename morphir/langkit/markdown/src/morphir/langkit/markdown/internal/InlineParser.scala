package morphir.langkit.markdown.internal

import kyo.*
import morphir.langkit.core.Span
import morphir.langkit.markdown.*

/**
 * Splits a block's raw prose into [[Inline]] nodes.
 *
 * Internal on purpose: blocks reach callers already carrying inline content, and no caller runs this itself. Each
 * construct this learns to recognise turns text that used to be literal into a typed node, so the set of cases grows
 * while the entry point does not.
 */
private[markdown] object InlineParser:

  /**
   * Parse `text` into inline nodes.
   *
   * @param text
   *   the block's prose, with its marker and surrounding whitespace already removed
   * @param sourceOffsetAt
   *   maps an index in `text` to its offset in the original source. A block whose text was joined from several lines
   *   supplies a mapping that accounts for the line endings it dropped, so spans stay true even when the joined text is
   *   shorter than the source it came from.
   */
  def parse(text: String, sourceOffsetAt: Int => Int): Chunk[Inline] =
    val nodes        = List.newBuilder[Inline]
    val pending      = StringBuilder()
    var pendingStart = 0
    var index        = 0

    def flushPending(): Unit =
      if pending.nonEmpty then
        val value = pending.toString
        nodes += Inline.Text(value, spanOf(pendingStart, index, sourceOffsetAt))
        pending.clear()

    while index < text.length do
      if text.charAt(index) == '`' then
        val runLength = backtickRun(text, index)
        closingRun(text, index + runLength, runLength) match
          case Present(closeStart) =>
            flushPending()
            val content = normalize(text.substring(index + runLength, closeStart))
            val end     = closeStart + runLength
            nodes += Inline.CodeSpan(content, spanOf(index, end, sourceOffsetAt))
            index = end
            pendingStart = index
          case Absent =>
            // No closer of equal length: the run is ordinary text, and rescanning inside it would be wrong.
            if pending.isEmpty then pendingStart = index
            pending.append(text.substring(index, index + runLength))
            index += runLength
      else
        if pending.isEmpty then pendingStart = index
        pending.append(text.charAt(index))
        index += 1
    end while
    flushPending()
    Chunk.from(nodes.result())
  end parse

  private def spanOf(start: Int, end: Int, sourceOffsetAt: Int => Int): Span =
    Span.fromStartEnd(sourceOffsetAt(start), sourceOffsetAt(end))

  /** The length of the backtick run beginning at `start`. */
  private def backtickRun(text: String, start: Int): Int =
    var end = start
    while end < text.length && text.charAt(end) == '`' do end += 1
    end - start

  /**
   * Where the closing backtick run of exactly `length` begins, searching from `from`.
   *
   * A run of a different length cannot close the span and is skipped whole, which is what makes ``` ``foo`bar`` ``` one
   * span rather than two.
   */
  private def closingRun(text: String, from: Int, length: Int): Maybe[Int] =
    var index  = from
    var result = Maybe.empty[Int]
    while result.isEmpty && index < text.length do
      if text.charAt(index) == '`' then
        val run = backtickRun(text, index)
        if run == length then result = Present(index)
        else index += run
      else index += 1
    result

  /**
   * CommonMark's code-span content rules.
   *
   * Line endings become spaces. Then, if the result begins and ends with a space but is not all spaces, one space is
   * removed from each end — one, not all, so `` `  `` ` `` keeps the inner pair. "Space" here means U+0020 only, so a
   * non-breaking space is content and never stripped.
   */
  private def normalize(content: String): String =
    val spaced = content.replace("\r\n", " ").replace('\n', ' ').replace('\r', ' ')
    if spaced.length >= 2 && spaced.startsWith(" ") && spaced.endsWith(" ") && spaced.exists(_ != ' ') then
      spaced.substring(1, spaced.length - 1)
    else spaced
end InlineParser
