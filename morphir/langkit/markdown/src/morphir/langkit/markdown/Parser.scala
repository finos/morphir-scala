package morphir.langkit.markdown

import kyo.*
import morphir.langkit.core.Span
import morphir.langkit.core.scanner.*
import morphir.langkit.markdown.internal.InlineParser

/**
 * A CommonMark subset parser: ATX headings, paragraphs, fenced code, unordered lists, and thematic breaks.
 *
 * This is not a full CommonMark implementation. `commonmark-java` must not enter this module. Inlines stay raw text
 * until an inline parser is added.
 */
object Parser:

  private val BlocksPhase = ScanPhase("markdown.blocks")
  private val LinesPhase  = ScanPhase("markdown.lines")

  def parse(source: String): Result[ParseError, Document] =
    parse(source, ScanBudget.default)

  def parse(source: String, budget: ScanBudget): Result[ParseError, Document] =
    parseWithMetrics(source, budget).map(_._1)

  private[markdown] def parseWithMetrics(
      source: String,
      budget: ScanBudget
  ): Result[ParseError, (Document, ScanMetrics)] =
    SourceScanner.scan(source, budget, phase = Present(BlocksPhase)) { scanner =>
      scanner.chargeOutputNodes(NodeCount.one)
      val blocks = parseBlocks(scanner)
      // Keep the caller's coordinate space: do not rewrite CRLF before measuring spans.
      val document = Document(blocks, Span(0, source.length))
      (document, scanner.metrics)
    } match
      case ScanResult.Success(value) => Result.succeed(value)
      case ScanResult.Failure(error) => Result.fail(ParseError.Scan(error))

  private final case class Line(view: SourceView, text: String, terminatedByLf: Boolean):
    def offset: Int = view.start.toInt
    def end: Int    = view.end.toInt
    def length: Int = view.length.toInt

  private def parseBlocks(scanner: SourceScanner): Chunk[Block] =
    val blocks = List.newBuilder[Block]
    while !scanner.isAtEnd do
      scanner.requireProgress(BlocksPhase) {
        val line = readLine(scanner)
        if !isBlank(scanner, line) then
          val block =
            // Four spaces of indentation beats every other block opener: an indented `# foo` is code, not a heading.
            if isIndentedCode(scanner, line) then readIndentedCode(scanner, line)
            else
              headingPrefix(scanner, line) match
                case Present((level, rest)) =>
                  Block.Heading(
                    level,
                    inlineOfLine(line, rest),
                    Span(line.offset, line.length)
                  )
                case Absent =>
                  fenceOpen(scanner, line) match
                    case Present(open) => readFencedCode(scanner, line, open)
                    case Absent        =>
                      if isThematicBreak(scanner, line) then Block.ThematicBreak(Span(line.offset, line.length))
                      else
                        unorderedItem(scanner, line) match
                          case Present(item) => readUnorderedList(scanner, line, item)
                          case Absent        => readParagraph(scanner, line)
          scanner.chargeOutputNodes(NodeCount.one)
          blocks += block
      }
    Chunk.from(blocks.result())

  /** A line of four or more leading spaces, which CommonMark reads as code rather than as whatever it looks like. */
  private def isIndentedCode(scanner: SourceScanner, line: Line): Boolean =
    inspectLine(scanner, line)(text => text.length >= 4 && text.take(4).forall(_ == ' '))

  /**
   * Read an indented code block.
   *
   * Blank lines belong to the block when more indented content follows, which is what keeps the gaps in a multi-chunk
   * block; blank lines at the end do not, so the block stops at the last indented line.
   */
  private def readIndentedCode(scanner: SourceScanner, first: Line): Block =
    val lines = List.newBuilder[String]
    lines += stripIndent(first.text)
    var last    = first
    var pending = List.newBuilder[String]
    var done    = false
    while !scanner.isAtEnd && !done do
      val checkpoint = scanner.checkpoint()
      val line       = readLine(scanner)
      if isIndentedCode(scanner, line) then
        lines ++= pending.result()
        pending = List.newBuilder[String]
        lines += stripIndent(line.text)
        last = line
      else if isBlank(scanner, line) then pending += stripIndent(line.text)
      else
        scanner.restore(checkpoint)
        done = true

    val content = lines.result().mkString("", "\n", "\n")
    scanner.chargeWork(WorkUnits.from(content.length.toLong).getOrThrow)
    Block.IndentedCode(content, Span.fromStartEnd(first.offset, last.end))

  /** Remove up to four leading spaces, which is the indentation the block form spends rather than content. */
  private def stripIndent(text: String): String =
    var removed = 0
    while removed < 4 && removed < text.length && text.charAt(removed) == ' ' do removed += 1
    text.substring(removed)

  private def readLine(scanner: SourceScanner): Line =
    scanner.requireProgress(LinesPhase) {
      val start                 = scanner.mark
      var previous: Maybe[Char] = Absent
      var terminatedByLf        = false
      var acquisitionRunning    = true
      while acquisitionRunning do
        scanner.peek() match
          case Present(char) =>
            scanner.advance()
            if char == '\n' then
              terminatedByLf = true
              acquisitionRunning = false
            else previous = Present(char)
          case Absent => acquisitionRunning = false

      val rawEnd  = scanner.offset.toInt - (if terminatedByLf then 1 else 0)
      val textEnd =
        if terminatedByLf && previous.contains('\r') then rawEnd - 1
        else rawEnd
      val view = scanner.view(Span.fromStartEnd(start.toInt, textEnd))
      scanner.chargeWork(WorkUnits.from(view.length.toInt.toLong).getOrThrow)
      Line(view, view.text, terminatedByLf)
    }

  private type FenceOpen = (marker: Char, length: Int, indentation: Int, info: String)

  private def readFencedCode(scanner: SourceScanner, opening: Line, open: FenceOpen): Block =
    val body            = StringBuilder()
    var closed          = false
    var closingEnd      = opening.end
    var bodyEndedWithLf = false
    while !scanner.isAtEnd && !closed do
      val line = readLine(scanner)
      if isClosingFence(scanner, line, open.marker, open.length) then
        closed = true
        closingEnd = line.end
      else
        if body.nonEmpty then body.append('\n')
        body.append(removeFenceIndentation(scanner, line, open.indentation))
        bodyEndedWithLf = line.terminatedByLf

    val end     = if closed then closingEnd else scanner.offset.toInt
    val content =
      if closed then
        if body.nonEmpty then body.append('\n')
        body.toString
      else
        if body.nonEmpty && bodyEndedWithLf then body.append('\n')
        body.toString

    // The budgeted FenceInfo path reserves deterministic work and output before token materialization.
    Block.FencedCode(FenceInfo.parseBudgeted(open.info, scanner), content, Span.fromStartEnd(opening.offset, end))

  private def readParagraph(scanner: SourceScanner, first: Line): Block =
    val segments = List.newBuilder[(Int, String)]
    segments += ((first.offset, first.text))
    var last        = first
    var interrupted = false
    while !scanner.isAtEnd && !interrupted do
      val checkpoint = scanner.checkpoint()
      val line       = readLine(scanner)
      if continuesParagraph(scanner, line) then
        segments += ((line.offset, line.text))
        last = line
      else
        scanner.restore(checkpoint)
        interrupted = true

    val lines   = Chunk.from(segments.result())
    val raw     = lines.map(_._2).mkString("\n")
    val trimmed = raw.trim
    val leading = raw.length - raw.stripLeading.length
    scanner.chargeWork(WorkUnits.from(raw.length.toLong).getOrThrow)
    Block.Paragraph(
      InlineParser.parse(trimmed, index => sourceOffsetOf(lines, index + leading)),
      Span.fromStartEnd(first.offset, last.end)
    )

  /**
   * Map an index in a paragraph's joined text back to its offset in the source.
   *
   * The join uses a single `\n` between lines, but the source may have used `\r\n`, and each line carries its own
   * offset. Walking the lines rather than adding a constant keeps inline spans true on both.
   */
  private def sourceOffsetOf(lines: Chunk[(Int, String)], index: Int): Int =
    var remaining = index
    var cursor    = 0
    var result    = Absent: Maybe[Int]
    while result.isEmpty && cursor < lines.size do
      val (offset, text) = lines(cursor)
      if remaining <= text.length then result = Present(offset + remaining)
      else remaining -= text.length + 1 // the '\n' the join introduced
      cursor += 1
    val (lastOffset, lastText) = lines(lines.size - 1)
    result.getOrElse(lastOffset + lastText.length)

  private def continuesParagraph(scanner: SourceScanner, line: Line): Boolean =
    !isBlank(scanner, line) &&
      fenceOpen(scanner, line).isEmpty &&
      headingPrefix(scanner, line).isEmpty &&
      unorderedItem(scanner, line).isEmpty &&
      !isThematicBreak(scanner, line)

  private def isBlank(scanner: SourceScanner, line: Line): Boolean =
    inspectLine(scanner, line)(_.trim.isEmpty)

  private def isThematicBreak(scanner: SourceScanner, line: Line): Boolean =
    inspectLine(scanner, line) { text =>
      val compact = text.filterNot(_.isWhitespace)
      compact.length >= 3 && (
        compact.forall(_ == '-') || compact.forall(_ == '*') || compact.forall(_ == '_')
      )
    }

  private def readUnorderedList(scanner: SourceScanner, first: Line, firstItem: String): Block =
    val items = List.newBuilder[ListItem]
    items += listItem(first, firstItem)
    var last = first
    var done = false
    while !scanner.isAtEnd && !done do
      val checkpoint = scanner.checkpoint()
      val line       = readLine(scanner)
      unorderedItem(scanner, line) match
        case Present(item) =>
          items += listItem(line, item)
          last = line
        case Absent =>
          scanner.restore(checkpoint)
          done = true
    Block.UnorderedList(Chunk.from(items.result()), Span.fromStartEnd(first.offset, last.end))

  private def listItem(line: Line, content: String): ListItem =
    ListItem(inlineOfLine(line, content), contentSpan(line, content))

  /** Inline content for a block whose prose came from one line, so offsets are the line's plus an index. */
  private def inlineOfLine(line: Line, content: String): Chunk[Inline] =
    val base = contentSpan(line, content).offset
    InlineParser.parse(content, index => base + index)

  /**
   * Where a block's extracted content sits in the source.
   *
   * The content is the line with its marker and surrounding whitespace removed, so locating it in the raw line recovers
   * the offset. A content string the line does not contain verbatim cannot happen for the forms parsed here, and falls
   * back to the whole line rather than to a negative offset.
   */
  private def contentSpan(line: Line, content: String): Span =
    val start = line.text.indexOf(content)
    if start >= 0 then Span(line.offset + start, content.length)
    else Span(line.offset, line.length)

  private def unorderedItem(scanner: SourceScanner, line: Line): Maybe[String] =
    inspectLine(scanner, line)(unorderedItem)

  private def unorderedItem(text: String): Maybe[String] =
    val trimmed = text.stripLeading
    if trimmed.length >= 2 && (trimmed.startsWith("- ") || trimmed.startsWith("* ") || trimmed.startsWith("+ "))
    then Present(trimmed.drop(2).trim)
    else Absent

  private def headingPrefix(scanner: SourceScanner, line: Line): Maybe[(HeadingLevel, String)] =
    inspectLine(scanner, line) { text =>
      headingPrefix(text.trim).map { case (level, rest) => (level, rest.trim) }
    }

  // The one-to-six bound lives in HeadingLevel.fromInt rather than in a guard here, so a run of seven
  // or more hashes falls through to the paragraph branch exactly as CommonMark requires.
  private def headingPrefix(text: String): Maybe[(HeadingLevel, String)] =
    val hashes = text.takeWhile(_ == '#')
    if hashes.nonEmpty && text.length > hashes.length && text.charAt(hashes.length) == ' ' then
      HeadingLevel.fromInt(hashes.length).map(level => (level, text.drop(hashes.length + 1)))
    else Absent

  private def fenceOpen(scanner: SourceScanner, line: Line): Maybe[FenceOpen] =
    inspectLine(scanner, line)(fenceOpen)

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

  private def isClosingFence(
      scanner: SourceScanner,
      line: Line,
      marker: Char,
      openingLength: Int
  ): Boolean =
    inspectLine(scanner, line)(text => isClosingFence(text, marker, openingLength))

  private def isClosingFence(text: String, marker: Char, openingLength: Int): Boolean =
    fenceIndent(text).exists { case (rest = trimmed) =>
      val run = trimmed.takeWhile(_ == marker)
      run.length >= openingLength && isSpacesOrTabs(trimmed.drop(run.length))
    }

  private def fenceIndent(text: String): Maybe[(indentation: Int, rest: String)] =
    val indent = text.takeWhile(_ == ' ').length
    if indent <= 3 then Present((indentation = indent, rest = text.drop(indent))) else Absent

  private def removeFenceIndentation(scanner: SourceScanner, line: Line, indentation: Int): String =
    inspectLine(scanner, line)(text => removeFenceIndentation(text, indentation))

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

  private def inspectLine[A](scanner: SourceScanner, line: Line)(operation: String => A): A =
    scanner.chargeWork(WorkUnits.from(line.length.toLong).getOrThrow)
    operation(line.text)
