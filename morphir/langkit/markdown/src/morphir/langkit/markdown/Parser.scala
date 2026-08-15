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
    // Keep the caller's coordinate space: do not rewrite CRLF before measuring spans.
    Result.succeed(Document(parseBlocks(source), Span(0, source.length)))

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
              case Present(open) =>
                val (block, next) = readFencedCode(lines, i, open)
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

  private type FenceOpen = (marker: Char, length: Int, indentation: Int, info: String)

  private def readFencedCode(lines: Vector[Line], start: Int, open: FenceOpen): (Block, Int) =
    val opening = lines(start)
    var i       = start + 1
    val body    = StringBuilder()
    var closed  = false
    while i < lines.length && !closed do
      val line = lines(i)
      if isClosingFence(line.text, open.marker, open.length) then closed = true
      else
        if body.nonEmpty then body.append('\n')
        body.append(removeFenceIndentation(line.text, open.indentation))
        i += 1
    val endLine = if closed then lines(i) else lines(lines.length - 1)
    val end     = endLine.offset + endLine.text.length
    val content =
      if closed then
        if body.nonEmpty then body.append('\n')
        body.toString
      else body.toString
    val next = if closed then i + 1 else i
    (Block.FencedCode(FenceInfo.parse(open.info), content, Span.fromStartEnd(opening.offset, end)), next)

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
          val end = if i > start && source.charAt(i - 1) == '\r' then i - 1 else i
          result += Line(start, source.substring(start, end))
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

  private def fenceOpen(text: String): Maybe[FenceOpen] =
    fenceIndent(text).flatMap { case (indentation = indentation, rest = trimmed) =>
      val marker = trimmed.headOption.filter(c => c == '`' || c == '~')
      marker match
        case Some(ch) =>
          val run  = trimmed.takeWhile(_ == ch)
          val info = trimmed.drop(run.length)
          if run.length >= 3 && (ch != '`' || !info.contains(ch)) then
            Present((
              marker = ch,
              length = run.length,
              indentation = indentation,
              info = trimSpacesOrTabs(info)
            ))
          else Absent
        case None => Absent
    }

  private def isClosingFence(text: String, marker: Char, openingLength: Int): Boolean =
    fenceIndent(text).exists { case (rest = trimmed) =>
      val run = trimmed.takeWhile(_ == marker)
      run.length >= openingLength && isSpacesOrTabs(trimmed.drop(run.length))
    }

  private def fenceIndent(text: String): Maybe[(indentation: Int, rest: String)] =
    val indent = text.takeWhile(_ == ' ').length
    if indent <= 3 then Present((indentation = indent, rest = text.drop(indent))) else Absent

  private def removeFenceIndentation(text: String, indentation: Int): String =
    text.drop(math.min(indentation, text.takeWhile(_ == ' ').length))

  private def isSpacesOrTabs(text: String): Boolean =
    text.forall(char => char == ' ' || char == '\t')

  private def trimSpacesOrTabs(text: String): String =
    val start = text.indexWhere(char => char != ' ' && char != '\t')
    if start == -1 then ""
    else
      val end = text.lastIndexWhere(char => char != ' ' && char != '\t')
      text.substring(start, end + 1)
