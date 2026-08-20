package morphir.langkit.markdown

import kyo.*
import morphir.langkit.core.Span
import morphir.langkit.core.scanner.*
import morphir.langkit.markdown.internal.{InlineParser, LinkDefinition}

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
      val definitions = scala.collection.mutable.Map.empty[String, LinkDefinition]
      val blocks      = parseBlocks(scanner, definitions)
      // Keep the caller's coordinate space: do not rewrite CRLF before measuring spans.
      val document = Document(blocks, Span(0, source.length))
      (document, scanner.metrics)
    } match
      case ScanResult.Success(value) => Result.succeed(value)
      case ScanResult.Failure(error) => Result.fail(ParseError.Scan(error))

  /**
   * A block whose inline content has not been parsed yet.
   *
   * Link reference definitions may appear after the text that uses them, so no prose can be parsed until the whole
   * document has been read. A block with no prose resolves to itself.
   */
  private final case class Deferred(resolve: Map[String, LinkDefinition] => Block)

  private object Deferred:
    def ready(block: Block): Deferred                                = Deferred(_ => block)
    def prose(build: Map[String, LinkDefinition] => Block): Deferred = Deferred(build)

  private final case class Line(view: SourceView, text: String, terminatedByLf: Boolean):
    def offset: Int = view.start.toInt
    def end: Int    = view.end.toInt
    def length: Int = view.length.toInt

  /**
   * What a line starts, decided in one pass.
   *
   * The parser used to ask each line six to ten separate questions, every one of them re-reading and re-charging the
   * whole line. Classifying once is the same decision made from one read, and it is the step the open-blocks loop
   * needs: a container has to know what a line is before it can decide whether the line belongs to it.
   */
  private enum LineKind derives CanEqual:
    case Blank
    case IndentedCode
    case Heading(level: HeadingLevel, text: String)
    case Fence(open: FenceOpen)
    case ThematicBreak
    case Html(kind: HtmlBlockKind)
    case BulletItem(content: String)
    case OrderedItem(marker: OrderedMarker)
    case Text

  /**
   * A classified line.
   *
   * `setext` is carried alongside `kind` rather than folded into it because the same characters mean different things
   * by position: `---` starts a thematic break at the top of a block and closes a setext heading under a paragraph.
   * Deciding both here keeps it to one read.
   */
  private final case class Classified(kind: LineKind, setext: Maybe[HeadingLevel])

  /** Precedence follows the spec: indentation first, then the leaf openers, then list markers, then prose. */
  private def classify(scanner: SourceScanner, line: Line): Classified =
    inspectLine(scanner, line) { text =>
      val setext  = setextUnderlineOf(text)
      val trimmed = text.trim
      val kind    =
        if trimmed.isEmpty then LineKind.Blank
        else if text.length >= 4 && text.take(4).forall(_ == ' ') then LineKind.IndentedCode
        else
          headingPrefix(text.trim) match
            case Present((level, rest)) => LineKind.Heading(level, rest.trim)
            case Absent                 =>
              fenceOpen(text) match
                case Present(open) => LineKind.Fence(open)
                case Absent        =>
                  if isThematicBreakText(text) then LineKind.ThematicBreak
                  else
                    htmlBlockStart(text) match
                      case Present(html) => LineKind.Html(html)
                      case Absent        =>
                        unorderedItem(text) match
                          case Present(item) => LineKind.BulletItem(item)
                          case Absent        =>
                            orderedItem(text) match
                              case Present(marker) => LineKind.OrderedItem(marker)
                              case Absent          => LineKind.Text
      Classified(kind, setext)
    }

  /** The setext level a line would close a paragraph with, judged on its characters alone. */
  private def setextUnderlineOf(text: String): Maybe[HeadingLevel] =
    val trimmed = text.trim
    val indent  = text.length - text.stripLeading.length
    if indent >= 4 || trimmed.isEmpty then Absent
    else if trimmed.forall(_ == '=') then Present(HeadingLevel.One)
    else if trimmed.forall(_ == '-') then Present(HeadingLevel.Two)
    else Absent

  /**
   * Whether a classified line continues the paragraph above it.
   *
   * The same list as before, read off one classification instead of re-asking the line. An indented line continues a
   * paragraph rather than starting code, a numbered item interrupts only when it starts at 1, and the any-tag HTML
   * condition never interrupts.
   */
  private def continues(classified: Classified): Boolean =
    classified.kind match
      case LineKind.Blank                        => false
      case LineKind.Text | LineKind.IndentedCode => true
      case LineKind.OrderedItem(marker)          => marker.number != 1
      case LineKind.Html(HtmlBlockKind.AnyTag)   => true
      case _                                     => false

  private def parseBlocks(
      scanner: SourceScanner,
      definitions: scala.collection.mutable.Map[String, LinkDefinition]
  ): Chunk[Block] =
    val blocks = List.newBuilder[Deferred]
    while !scanner.isAtEnd do
      scanner.requireProgress(BlocksPhase) {
        val line       = readLine(scanner)
        val classified = classify(scanner, line)
        if classified.kind != LineKind.Blank then
          val block = classified.kind match
            case LineKind.IndentedCode         => Present(Deferred.ready(readIndentedCode(scanner, line)))
            case LineKind.Heading(level, rest) =>
              val headingSpan = Span(line.offset, line.length)
              val base        = contentSpan(line, rest).offset
              Present(Deferred.prose { defs =>
                Block.Heading(level, InlineParser.parse(rest, index => base + index, defs), headingSpan)
              })
            case LineKind.Fence(open)   => Present(Deferred.ready(readFencedCode(scanner, line, open)))
            case LineKind.ThematicBreak =>
              Present(Deferred.ready(Block.ThematicBreak(Span(line.offset, line.length))))
            case LineKind.Html(_)               => Present(Deferred.ready(readHtmlBlock(scanner, line)))
            case LineKind.BulletItem(item)      => Present(readUnorderedList(scanner, line, item))
            case LineKind.OrderedItem(marker)   => Present(readOrderedList(scanner, line, marker))
            case LineKind.Text | LineKind.Blank => readParagraph(scanner, line, definitions)
          scanner.chargeOutputNodes(NodeCount.one)
          block.foreach(blocks += _)
      }
    // Second phase: every definition in the document is known now, so prose can resolve references that point
    // forward as well as back.
    Chunk.from(blocks.result().map(_.resolve(definitions.toMap)))

  /** Tag names that open an HTML block on sight, from the spec's condition-6 list. */
  private val HtmlBlockTags = Set(
    "address",
    "article",
    "aside",
    "base",
    "basefont",
    "blockquote",
    "body",
    "caption",
    "center",
    "col",
    "colgroup",
    "dd",
    "details",
    "dialog",
    "dir",
    "div",
    "dl",
    "dt",
    "fieldset",
    "figcaption",
    "figure",
    "footer",
    "form",
    "frame",
    "frameset",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "head",
    "header",
    "hr",
    "html",
    "iframe",
    "legend",
    "li",
    "link",
    "main",
    "menu",
    "menuitem",
    "nav",
    "noframes",
    "ol",
    "optgroup",
    "option",
    "p",
    "param",
    "search",
    "section",
    "summary",
    "table",
    "tbody",
    "td",
    "tfoot",
    "th",
    "thead",
    "title",
    "tr",
    "track",
    "ul"
  )

  /**
   * Which of the spec's HTML block start conditions a line meets, if any.
   *
   * Each condition carries its own end condition, which is why the kind is kept rather than a boolean: conditions one
   * to five end on a closing marker, and six and seven end on a blank line.
   */
  private enum HtmlBlockKind derives CanEqual:
    case ScriptLike, Comment, ProcessingInstruction, Declaration, CData, KnownTag, AnyTag

  private def htmlBlockStart(scanner: SourceScanner, line: Line): Maybe[HtmlBlockKind] =
    inspectLine(scanner, line)(htmlBlockStart)

  private def htmlBlockStart(text: String): Maybe[HtmlBlockKind] =
    val trimmed = text.stripLeading
    val indent  = text.length - trimmed.length
    if indent >= 4 || !trimmed.startsWith("<") then Absent
    else
      val lower = trimmed.toLowerCase
      if Seq("<script", "<pre", "<style", "<textarea").exists(lower.startsWith) then Present(HtmlBlockKind.ScriptLike)
      else if lower.startsWith("<!--") then Present(HtmlBlockKind.Comment)
      else if lower.startsWith("<?") then Present(HtmlBlockKind.ProcessingInstruction)
      else if lower.startsWith("<![cdata[") then Present(HtmlBlockKind.CData)
      else if trimmed.length > 2 && trimmed.charAt(1) == '!' && trimmed.charAt(2).isLetter then
        Present(HtmlBlockKind.Declaration)
      else
        val name = tagNameOf(trimmed)
        if name.nonEmpty && HtmlBlockTags.contains(name) then Present(HtmlBlockKind.KnownTag)
        else if isCompleteTagLine(trimmed) then Present(HtmlBlockKind.AnyTag)
        else Absent

  private def tagNameOf(trimmed: String): String =
    val body = if trimmed.startsWith("</") then trimmed.drop(2) else trimmed.drop(1)
    val name = body.takeWhile(char => char.isLetterOrDigit || char == '-')
    if name.isEmpty then ""
    else
      val rest = body.drop(name.length)
      if rest.isEmpty || rest.startsWith(">") || rest.startsWith("/>") || rest.charAt(0).isWhitespace then
        name.toLowerCase
      else ""

  /**
   * Condition seven: a complete, syntactically valid open or closing tag, alone on its line.
   *
   * "Valid" is load-bearing and was the source of a regression when this merely looked for a `>`. `<a h*#ref="hi">` has
   * no valid attribute name, so it is not a tag at all and stays escaped text in a paragraph; so does
   * `<a href='bar'title=title>`, whose attributes do not separate, and `</a href="foo">`, since a closing tag takes no
   * attributes.
   */
  private def isCompleteTagLine(trimmed: String): Boolean =
    completeTagEnd(trimmed).exists(end => trimmed.drop(end).isBlank)

  /** Where a valid tag beginning at index 0 ends, or [[kyo.Absent]] if the text does not open one. */
  private def completeTagEnd(text: String): Maybe[Int] =
    if !text.startsWith("<") then Absent
    else if text.startsWith("</") then closingTagEnd(text)
    else openTagEnd(text)

  private def closingTagEnd(text: String): Maybe[Int] =
    var index = 2
    val start = index
    while index < text.length && (text.charAt(index).isLetterOrDigit || text.charAt(index) == '-') do index += 1
    if index == start || !text.charAt(start - 1 + 1 - 1).isLetter then Absent
    else
      index = skipSpaces(text, index)
      if index < text.length && text.charAt(index) == '>' then Present(index + 1) else Absent

  private def openTagEnd(text: String): Maybe[Int] =
    var index = 1
    if index >= text.length || !text.charAt(index).isLetter then Absent
    else
      while index < text.length && (text.charAt(index).isLetterOrDigit || text.charAt(index) == '-') do index += 1
      var valid = true
      var done  = false
      while valid && !done do
        val afterSpaces = skipSpaces(text, index)
        if afterSpaces >= text.length then valid = false
        else if text.charAt(afterSpaces) == '>' then
          index = afterSpaces + 1
          done = true
        else if text.charAt(afterSpaces) == '/' &&
          afterSpaces + 1 < text.length && text.charAt(afterSpaces + 1) == '>'
        then
          index = afterSpaces + 2
          done = true
        else if afterSpaces == index then valid = false // attributes must be separated by whitespace
        else
          attributeEnd(text, afterSpaces) match
            case Present(next) => index = next
            case Absent        => valid = false
      if valid && done then Present(index) else Absent

  private def attributeEnd(text: String, from: Int): Maybe[Int] =
    var index = from
    if index >= text.length then Absent
    else if !(text.charAt(index).isLetter || text.charAt(index) == '_' || text.charAt(index) == ':') then Absent
    else
      while index < text.length &&
        (text.charAt(index).isLetterOrDigit ||
          "_.:-".indexOf(text.charAt(index).toInt) >= 0)
      do index += 1
      val afterName = skipSpaces(text, index)
      if afterName >= text.length || text.charAt(afterName) != '=' then Present(index)
      else
        val valueStart = skipSpaces(text, afterName + 1)
        if valueStart >= text.length then Absent
        else
          val quote = text.charAt(valueStart)
          if quote == '"' || quote == '\'' then
            val close = text.indexOf(quote.toInt, valueStart + 1)
            if close < 0 then Absent else Present(close + 1)
          else
            var cursor = valueStart
            while cursor < text.length && !text.charAt(cursor).isWhitespace &&
              "\"'=<>`".indexOf(text.charAt(cursor).toInt) < 0
            do cursor += 1
            if cursor == valueStart then Absent else Present(cursor)

  private def skipSpaces(text: String, from: Int): Int =
    var index = from
    while index < text.length && text.charAt(index).isWhitespace do index += 1
    index

  /**
   * Read a raw HTML block.
   *
   * Conditions one to five run until their closing marker appears; six and seven run until a blank line. Either way the
   * lines are kept verbatim, because the content is HTML rather than Markdown.
   */
  private def readHtmlBlock(scanner: SourceScanner, first: Line): Block =
    val kind  = htmlBlockStart(scanner, first).getOrElse(HtmlBlockKind.AnyTag)
    val lines = List.newBuilder[String]
    lines += first.text
    var last = first
    var done = closesHtmlBlock(kind, first.text, opening = true)

    while !scanner.isAtEnd && !done do
      val checkpoint = scanner.checkpoint()
      val line       = readLine(scanner)
      val blank      = isBlank(scanner, line)
      if endsOnBlankLine(kind) && blank then
        scanner.restore(checkpoint)
        done = true
      else
        lines += line.text
        last = line
        if closesHtmlBlock(kind, line.text, opening = false) then done = true

    // No trailing newline: the document separator supplies the one between blocks, and adding another here would
    // double it. A code block differs because its closing tag ends the content.
    val content = lines.result().mkString("\n")
    scanner.chargeWork(WorkUnits.from(content.length.toLong).getOrThrow)
    Block.HtmlBlock(content, Span.fromStartEnd(first.offset, last.end))

  private def endsOnBlankLine(kind: HtmlBlockKind): Boolean =
    kind == HtmlBlockKind.KnownTag || kind == HtmlBlockKind.AnyTag

  private def closesHtmlBlock(kind: HtmlBlockKind, text: String, opening: Boolean): Boolean =
    val lower = text.toLowerCase
    kind match
      case HtmlBlockKind.ScriptLike =>
        Seq("</script>", "</pre>", "</style>", "</textarea>").exists(lower.contains)
      case HtmlBlockKind.Comment               => lower.contains("-->") && (!opening || lower.indexOf("-->") >= 4)
      case HtmlBlockKind.ProcessingInstruction => lower.contains("?>")
      case HtmlBlockKind.Declaration           => lower.contains(">")
      case HtmlBlockKind.CData                 => lower.contains("]]>")
      case _                                   => false

  /**
   * A setext underline: a run of `=` or `-` beneath a paragraph, which turns it into a heading.
   *
   * Only meaningful after paragraph text, which is why it is checked here rather than as a block opener. A run of `-`
   * with nothing above it stays a thematic break.
   */
  private def setextUnderline(scanner: SourceScanner, line: Line): Maybe[HeadingLevel] =
    inspectLine(scanner, line) { text =>
      val trimmed = text.trim
      val indent  = text.length - text.stripLeading.length
      if indent >= 4 || trimmed.isEmpty then Absent
      else if trimmed.forall(_ == '=') then Present(HeadingLevel.One)
      else if trimmed.forall(_ == '-') then Present(HeadingLevel.Two)
      else Absent
    }

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

  private def readParagraph(
      scanner: SourceScanner,
      first: Line,
      definitions: scala.collection.mutable.Map[String, LinkDefinition]
  ): Maybe[Deferred] =
    val segments = List.newBuilder[(Int, String)]
    segments += ((first.offset, first.text))
    var last        = first
    var interrupted = false
    var setext      = Absent: Maybe[HeadingLevel]
    while !scanner.isAtEnd && !interrupted do
      val checkpoint = scanner.checkpoint()
      val line       = readLine(scanner)
      val classified = classify(scanner, line)
      classified.setext match
        case Present(level) if classified.kind != LineKind.IndentedCode =>
          // An underline turns everything gathered so far into a heading, and is consumed with it.
          setext = Present(level)
          last = line
          interrupted = true
        case _ =>
          if continues(classified) then
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

    // A paragraph may open with link reference definitions. They are not content: they are consumed here and only
    // what follows them, if anything, becomes a paragraph.
    val consumed = takeDefinitions(trimmed, definitions)
    val body     = trimmed.substring(consumed)
    if body.isBlank then Absent
    else
      val bodyStart = leading + consumed
      val span      = Span.fromStartEnd(first.offset, last.end)
      Present(Deferred.prose { defs =>
        val content = InlineParser.parse(body.trim, index => sourceOffsetOf(lines, index + bodyStart), defs)
        setext match
          case Present(level) => Block.Heading(level, content, span)
          case Absent         => Block.Paragraph(content, span)
      })
  end readParagraph

  /**
   * Consume `[label]: destination "title"` definitions from the front of a paragraph's text.
   *
   * Returns how many characters were taken. A definition contributes no block; it is recorded and the text that follows
   * becomes the paragraph, which is what lets a document open with its link definitions.
   */
  private def takeDefinitions(
      text: String,
      definitions: scala.collection.mutable.Map[String, LinkDefinition]
  ): Int =
    var consumed = 0
    var scanning = true
    while scanning do
      linkDefinitionAt(text, consumed) match
        case Present((end, label, destination, title)) =>
          val key = InlineParser.normalizeLabel(label)
          // First definition wins, which is what the spec says about duplicates.
          if !definitions.contains(key) then definitions(key) = LinkDefinition(destination, title)
          consumed = end
        case Absent => scanning = false
    consumed

  /** One definition beginning at `from`, or [[kyo.Absent]] if the text does not start with one. */
  private def linkDefinitionAt(
      text: String,
      from: Int
  ): Maybe[(Int, String, String, Maybe[String])] =
    var index = from
    while index < text.length && (text.charAt(index) == ' ' || text.charAt(index) == '\n') do index += 1
    if index >= text.length || text.charAt(index) != '[' then Absent
    else
      InlineParser.labelEndOf(text, index + 1) match
        case Absent         => Absent
        case Present(close) =>
          if close + 1 >= text.length || text.charAt(close + 1) != ':' then Absent
          else
            val label = text.substring(index + 1, close)
            InlineParser.definitionTarget(text, close + 2) match
              case Absent                             => Absent
              case Present((end, destination, title)) => Present((end, label, destination, title))

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
      !interruptsParagraph(scanner, line) &&
      !htmlBlockStart(scanner, line).exists(_ != HtmlBlockKind.AnyTag) &&
      !isThematicBreak(scanner, line)

  /**
   * Whether a numbered item may break the paragraph above it.
   *
   * Only a list starting at 1 can. Example 304 is the reason: "The number of windows in my house is / 14. The number of
   * doors is 6." is one paragraph, not a paragraph and a list starting at 14.
   */
  private def interruptsParagraph(scanner: SourceScanner, line: Line): Boolean =
    orderedItem(scanner, line).exists(_.number == 1)

  private def isBlank(scanner: SourceScanner, line: Line): Boolean =
    inspectLine(scanner, line)(_.trim.isEmpty)

  private def isThematicBreak(scanner: SourceScanner, line: Line): Boolean =
    inspectLine(scanner, line)(isThematicBreakText)

  private def isThematicBreakText(text: String): Boolean =
    val compact = text.filterNot(_.isWhitespace)
    compact.length >= 3 && (
      compact.forall(_ == '-') || compact.forall(_ == '*') || compact.forall(_ == '_')
    )

  private def readUnorderedList(scanner: SourceScanner, first: Line, firstItem: String): Deferred =
    val items = List.newBuilder[DeferredItem]
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
    val collected = Chunk.from(items.result())
    val listSpan  = Span.fromStartEnd(first.offset, last.end)
    Deferred.prose(defs => Block.UnorderedList(collected.map(_.resolve(defs)), listSpan))

  private final case class DeferredItem(resolve: Map[String, LinkDefinition] => ListItem)

  private def listItem(line: Line, content: String): DeferredItem =
    val span = contentSpan(line, content)
    DeferredItem(defs => ListItem(InlineParser.parse(content, index => span.offset + index, defs), span))

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

  private type OrderedMarker = (number: Int, delimiter: Char, content: String)

  /** A numbered list marker: up to nine digits, then `.` or `)`, then a space. */
  private def orderedItem(scanner: SourceScanner, line: Line): Maybe[OrderedMarker] =
    inspectLine(scanner, line)(orderedItem)

  private def orderedItem(text: String): Maybe[OrderedMarker] =
    val trimmed = text.stripLeading
    val digits  = trimmed.takeWhile(_.isDigit)
    if digits.isEmpty || digits.length > 9 then Absent
    else if trimmed.length <= digits.length + 1 then Absent
    else
      val delimiter = trimmed.charAt(digits.length)
      if (delimiter == '.' || delimiter == ')') && trimmed.charAt(digits.length + 1) == ' ' then
        Present((number = digits.toInt, delimiter = delimiter, content = trimmed.drop(digits.length + 2).trim))
      else Absent

  /**
   * Read consecutive numbered items as one list.
   *
   * A change of delimiter ends the list, because `1.` and `1)` are different lists rather than one; example 302 renders
   * two.
   */
  private def readOrderedList(scanner: SourceScanner, first: Line, firstItem: OrderedMarker): Deferred =
    val items = List.newBuilder[DeferredItem]
    items += listItem(first, firstItem.content)
    var last = first
    var done = false
    while !scanner.isAtEnd && !done do
      val checkpoint = scanner.checkpoint()
      val line       = readLine(scanner)
      orderedItem(scanner, line) match
        case Present(marker) if marker.delimiter == firstItem.delimiter =>
          items += listItem(line, marker.content)
          last = line
        case _ =>
          scanner.restore(checkpoint)
          done = true

    val collected = Chunk.from(items.result())
    val listSpan  = Span.fromStartEnd(first.offset, last.end)
    Deferred.prose(defs => Block.OrderedList(firstItem.number, collected.map(_.resolve(defs)), listSpan))

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
