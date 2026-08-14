package morphir.langkit.markdown

import kyo.*
import morphir.langkit.core.Span

/**
 * Stub parser for ATX headings and paragraphs.
 *
 * This is not a CommonMark parser. It exists so tests run on JVM, JS, and Native while the production parser is
 * still an open question. Do not take `commonmark-java` here.
 */
object Parser:

  def parse(source: String): Result[ParseError, Document] =
    val blocks = parseBlocks(source)
    Result.succeed(Document(blocks, Span(0, source.length)))

  private def parseBlocks(source: String): Chunk[Block] =
    val chunks = splitParagraphs(source)
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

  private def splitParagraphs(source: String): List[SourceChunk] =
    val result = List.newBuilder[SourceChunk]
    var start  = 0
    var i      = 0
    while i < source.length do
      if i + 1 < source.length && source.charAt(i) == '\n' && source.charAt(i + 1) == '\n' then
        val piece = source.substring(start, i)
        if piece.trim.nonEmpty then result += SourceChunk(piece, start)
        i += 2
        start = i
      else i += 1
    val last = source.substring(start)
    if last.trim.nonEmpty then result += SourceChunk(last, start)
    result.result()
