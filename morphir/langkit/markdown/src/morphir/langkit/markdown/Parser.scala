package morphir.langkit.markdown

import kyo.*
import morphir.langkit.core.Span

/**
 * A CommonMark subset parser: ATX headings, paragraphs, fenced code, unordered lists, and thematic breaks.
 *
 * This is not a full CommonMark implementation. `commonmark-java` must not enter this module. Inlines stay raw text
 * until an inline parser is added.
 */
object Parser:

  def parse(source: String): Result[ParseError, Document] =
    val normalized = source.replace("\r\n", "\n")
    Result.succeed(Document(parseBlocks(normalized), Span(0, normalized.length)))

  private final case class Line(offset: Int, text: String)

  private def parseBlocks(source: String): Chunk[Block] =
    val lines  = splitLines(source)
    val blocks = List.newBuilder[Block]
    var i      = 0
    while i < lines.length do
      val line = lines(i)
      if line.text.trim.isEmpty then i += 1
      else
        headingPrefix(line.text.trim) match
          case Present((level, rest)) =>
            blocks += Block.Heading(level, rest.trim, Span(line.offset, line.text.length))
            i += 1
          case Absent =>
            fenceOpen(line.text) match
              case Present((marker, info)) =>
                val (block, next) = readFencedCode(lines, i, marker, info)
                blocks += block
                i = next
              case Absent =>
                if isThematicBreak(line.text) then
                  blocks += Block.ThematicBreak(Span(line.offset, line.text.length))
                  i += 1
                else
                  unorderedItem(line.text) match
                    case Present(_) =>
                      val (block, next) = readUnorderedList(lines, i)
                      blocks += block
                      i = next
                    case Absent =>
                      val (block, next) = readParagraph(lines, i)
                      blocks += block
                      i = next
    Chunk.from(blocks.result())

  private def readFencedCode(lines: Vector[Line], start: Int, marker: Char, info: String): (Block, Int) =
    val opening = lines(start)
    var i       = start + 1
    val body    = StringBuilder()
    var closed  = false
    while i < lines.length && !closed do
      val line = lines(i)
      if isClosingFence(line.text, marker) then closed = true
      else
        if body.nonEmpty then body.append('\n')
        body.append(line.text)
        i += 1
    val endLine = if closed then lines(i) else lines(lines.length - 1)
    val end     = endLine.offset + endLine.text.length
    val content =
      if closed then
        if body.nonEmpty then body.append('\n')
        body.toString
      else body.toString
    val next = if closed then i + 1 else i
    (Block.FencedCode(info, content, Span.fromStartEnd(opening.offset, end)), next)

  private def readParagraph(lines: Vector[Line], start: Int): (Block, Int) =
    val first = lines(start)
    var i     = start + 1
    val text  = StringBuilder(first.text)
    while i < lines.length && continuesParagraph(lines(i)) do
      text.append('\n').append(lines(i).text)
      i += 1
    val last = lines(i - 1)
    (Block.Paragraph(text.toString.trim, Span.fromStartEnd(first.offset, last.offset + last.text.length)), i)

  private def continuesParagraph(line: Line): Boolean =
    line.text.trim.nonEmpty &&
      fenceOpen(line.text).isEmpty &&
      headingPrefix(line.text.trim).isEmpty &&
      unorderedItem(line.text).isEmpty &&
      !isThematicBreak(line.text)

  private def isThematicBreak(text: String): Boolean =
    val compact = text.filterNot(_.isWhitespace)
    compact.length >= 3 && (
      compact.forall(_ == '-') || compact.forall(_ == '*') || compact.forall(_ == '_')
    )

  private def readUnorderedList(lines: Vector[Line], start: Int): (Block, Int) =
    val first = lines(start)
    var i     = start
    val items = List.newBuilder[String]
    var done  = false
    while i < lines.length && !done do
      unorderedItem(lines(i).text) match
        case Present(item) =>
          items += item
          i += 1
        case Absent => done = true
    val last = lines(i - 1)
    (
      Block.UnorderedList(Chunk.from(items.result()), Span.fromStartEnd(first.offset, last.offset + last.text.length)),
      i
    )

  private def unorderedItem(text: String): Maybe[String] =
    val trimmed = text.stripLeading
    if trimmed.length >= 2 && (trimmed.startsWith("- ") || trimmed.startsWith("* ") || trimmed.startsWith("+ "))
    then Present(trimmed.drop(2).trim)
    else Absent

  private def splitLines(source: String): Vector[Line] =
    if source.isEmpty then Vector.empty
    else
      val result = Vector.newBuilder[Line]
      var start  = 0
      var i      = 0
      while i < source.length do
        if source.charAt(i) == '\n' then
          result += Line(start, source.substring(start, i))
          i += 1
          start = i
        else i += 1
      if start < source.length || source.charAt(source.length - 1) == '\n' then
        result += Line(start, source.substring(start))
      result.result()

  private def headingPrefix(text: String): Maybe[(Int, String)] =
    val hashes = text.takeWhile(_ == '#')
    if hashes.nonEmpty && hashes.length <= 6 && text.length > hashes.length && text.charAt(hashes.length) == ' '
    then Present((hashes.length, text.drop(hashes.length + 1)))
    else Absent

  private def fenceOpen(text: String): Maybe[(Char, String)] =
    val trimmed = text.stripLeading
    val marker  = trimmed.headOption.filter(c => c == '`' || c == '~')
    marker match
      case Some(ch) =>
        val run = trimmed.takeWhile(_ == ch)
        if run.length >= 3 && !trimmed.drop(run.length).contains(ch) then
          Present((ch, trimmed.drop(run.length).trim))
        else Absent
      case None => Absent

  private def isClosingFence(text: String, marker: Char): Boolean =
    val trimmed = text.stripLeading
    val run     = trimmed.takeWhile(_ == marker)
    run.length >= 3 && trimmed.drop(run.length).trim.isEmpty
