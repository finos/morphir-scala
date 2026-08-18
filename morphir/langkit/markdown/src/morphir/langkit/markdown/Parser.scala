package morphir.langkit.markdown

import kyo.*
import morphir.langkit.core.Span
import morphir.langkit.core.scanner.*

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
    parseWithMetrics(source, budget) match
      case Result.Success((document, _)) => Result.succeed(document)
      case Result.Failure(error)         => Result.fail(error)

  private[markdown] def parseWithMetrics(
      source: String,
      budget: ScanBudget
  ): Result[ParseError, (Document, ScanMetrics)] =
    SourceScanner.scan(source, budget, phase = Some(BlocksPhase)) { scanner =>
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
            headingPrefix(scanner, line) match
              case Present((level, rest)) =>
                Block.Heading(level, rest, Span(line.offset, line.length))
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

  private def readLine(scanner: SourceScanner): Line =
    scanner.requireProgress(LinesPhase) {
      val start              = scanner.mark
      var previous           = Option.empty[Char]
      var terminatedByLf     = false
      var acquisitionRunning = true
      while acquisitionRunning do
        scanner.peek() match
          case Some(char) =>
            scanner.advance()
            if char == '\n' then
              terminatedByLf = true
              acquisitionRunning = false
            else previous = Some(char)
          case None => acquisitionRunning = false

      val rawEnd  = scanner.offset.toInt - (if terminatedByLf then 1 else 0)
      val textEnd =
        if terminatedByLf && previous.contains('\r') then rawEnd - 1
        else rawEnd
      val view = scanner.view(Span.fromStartEnd(start.toInt, textEnd))
      scanner.chargeWork(WorkUnits(view.length.toInt.toLong))
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
    val text        = StringBuilder(first.text)
    var last        = first
    var interrupted = false
    while !scanner.isAtEnd && !interrupted do
      val checkpoint = scanner.checkpoint()
      val line       = readLine(scanner)
      if continuesParagraph(scanner, line) then
        text.append('\n').append(line.text)
        last = line
      else
        scanner.restore(checkpoint)
        interrupted = true

    scanner.chargeWork(WorkUnits(text.length.toLong))
    Block.Paragraph(text.toString.trim, Span.fromStartEnd(first.offset, last.end))

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
    val items = List.newBuilder[String]
    items += firstItem
    var last = first
    var done = false
    while !scanner.isAtEnd && !done do
      val checkpoint = scanner.checkpoint()
      val line       = readLine(scanner)
      unorderedItem(scanner, line) match
        case Present(item) =>
          items += item
          last = line
        case Absent =>
          scanner.restore(checkpoint)
          done = true
    Block.UnorderedList(Chunk.from(items.result()), Span.fromStartEnd(first.offset, last.end))

  private def unorderedItem(scanner: SourceScanner, line: Line): Maybe[String] =
    inspectLine(scanner, line)(unorderedItem)

  private def unorderedItem(text: String): Maybe[String] =
    val trimmed = text.stripLeading
    if trimmed.length >= 2 && (trimmed.startsWith("- ") || trimmed.startsWith("* ") || trimmed.startsWith("+ "))
    then Present(trimmed.drop(2).trim)
    else Absent

  private def headingPrefix(scanner: SourceScanner, line: Line): Maybe[(Int, String)] =
    inspectLine(scanner, line) { text =>
      headingPrefix(text.trim).map { case (level, rest) => (level, rest.trim) }
    }

  private def headingPrefix(text: String): Maybe[(Int, String)] =
    val hashes = text.takeWhile(_ == '#')
    if hashes.nonEmpty && hashes.length <= 6 && text.length > hashes.length && text.charAt(hashes.length) == ' '
    then Present((hashes.length, text.drop(hashes.length + 1)))
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
    scanner.chargeWork(WorkUnits(line.length.toLong))
    operation(line.text)
