package morphir.langkit.markdown

import kyo.*
import morphir.langkit.core.Span

/**
 * Stub parser for ATX headings and paragraphs.
 *
 * This is not a CommonMark parser. It exists so tests run on JVM, JS, and Native while the production parser is
 * still an open question. Do not take `commonmark-java` here. ATX headings still terminate at the end of their
 * line, so a heading is never merged with the following block when only a single newline separates them.
 */
object Parser:

  def parse(source: String): Result[ParseError, Document] =
    val blocks = parseBlocks(source)
    Result.succeed(Document(blocks, Span(0, source.length)))

  private def parseBlocks(source: String): Chunk[Block] =
    val chunks = splitBlocks(source)
    Chunk.from(chunks.iterator.map(parseChunk).toList)

  private def parseChunk(chunk: SourceChunk): Block =
    val trimmed = chunk.text.trim
    headingPrefix(trimmed) match
      case Present((level, rest)) =>
        Block.Heading(level, rest.trim, Span(chunk.offset, chunk.text.length))
      case Absent =>
        Block.Paragraph(trimmed, Span(chunk.offset, chunk.text.length))

  private def headingPrefix(text: String): Maybe[(Int, String)] =
    val hashes = text.takeWhile(_ == '#')
    if hashes.nonEmpty && hashes.length <= 6 && text.length > hashes.length && text.charAt(hashes.length) == ' '
    then Present((hashes.length, text.drop(hashes.length + 1)))
    else Absent

  private final case class SourceChunk(text: String, offset: Int)

  /** Split into block chunks. Blank lines separate paragraphs; an ATX heading line is always its own chunk. */
  private def splitBlocks(source: String): List[SourceChunk] =
    val result           = List.newBuilder[SourceChunk]
    var paragraphStart   = -1
    var paragraphEnd     = -1
    var lineStart        = 0
    var i                = 0

    def flushParagraph(): Unit =
      if paragraphStart >= 0 then
        val piece = source.substring(paragraphStart, paragraphEnd)
        if piece.trim.nonEmpty then result += SourceChunk(piece, paragraphStart)
        paragraphStart = -1
        paragraphEnd = -1

    def emitLine(start: Int, end: Int): Unit =
      val text = source.substring(start, end)
      if text.trim.isEmpty then flushParagraph()
      else if !headingPrefix(text.trim).isEmpty then
        flushParagraph()
        result += SourceChunk(text, start)
      else
        if paragraphStart < 0 then paragraphStart = start
        paragraphEnd = end

    while i <= source.length do
      if i == source.length || source.charAt(i) == '\n' then
        emitLine(lineStart, i)
        i += 1
        lineStart = i
      else i += 1

    flushParagraph()
    result.result()
