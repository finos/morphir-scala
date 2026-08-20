package morphir.langkit.markdown

import kyo.*
import scala.annotation.tailrec
import morphir.langkit.core.Span
import morphir.langkit.core.scanner.*
import morphir.langkit.markdown.internal.{ContainerCursor, ContainerPrefix, InlineParser, Line, LinkDefinition}

/**
 * A CommonMark subset parser: ATX headings, paragraphs, fenced code, unordered lists, and thematic breaks.
 *
 * This is not a full CommonMark implementation. `commonmark-java` must not enter this module. Inlines stay raw text
 * until an inline parser is added.
 */
object Parser:

  private val BlocksPhase = ScanPhase("markdown.blocks")

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
      val blocks      = parseBlocks(ContainerCursor.top(scanner), definitions)
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
    case BlockQuote
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
        else if isBlockQuoteStart(text) then LineKind.BlockQuote
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
      cursor: ContainerCursor,
      definitions: scala.collection.mutable.Map[String, LinkDefinition]
  ): Chunk[Block] =
    val deferred = parseDeferred(cursor, definitions)
    // Second phase: every definition in the document is known now, so prose can resolve references that point
    // forward as well as back.
    Chunk.from(deferred.map(_.resolve(definitions.toMap)))

  /**
   * Read every block the cursor's container holds, with prose left unresolved.
   *
   * The document is a container like any other, so this is the one block loop: the top level runs it over a cursor with
   * nothing open, and a block quote runs it over a cursor that takes the `>` off each line. What differs between them
   * is which lines the cursor offers, not what is done with them.
   */
  private def parseDeferred(
      cursor: ContainerCursor,
      definitions: scala.collection.mutable.Map[String, LinkDefinition]
  ): List[Deferred] =
    val scanner = cursor.scanner
    val blocks  = List.newBuilder[Deferred]
    while !cursor.isAtEnd do
      scanner.requireProgress(BlocksPhase) {
        val opening = cursor.checkpoint()
        cursor.readLine().foreach { line =>
          val classified = classify(scanner, line)
          if classified.kind != LineKind.Blank then
            val block = classified.kind match
              case LineKind.IndentedCode => Present(Deferred.ready(readIndentedCode(cursor, line)))
              case LineKind.BlockQuote   =>
                // A quote's own cursor has to see the marker, so the line goes back before the recursion reads it.
                cursor.restore(opening)
                Present(readBlockQuote(cursor, definitions))
              case LineKind.Heading(level, rest) =>
                val headingSpan = Span(line.offset, line.length)
                val base        = contentSpan(line, rest).offset
                Present(Deferred.prose { defs =>
                  Block.Heading(level, InlineParser.parse(rest, index => base + index, defs), headingSpan)
                })
              case LineKind.Fence(open)   => Present(Deferred.ready(readFencedCode(cursor, line, open)))
              case LineKind.ThematicBreak =>
                Present(Deferred.ready(Block.ThematicBreak(Span(line.offset, line.length))))
              case LineKind.Html(_)               => Present(Deferred.ready(readHtmlBlock(cursor, line)))
              case LineKind.BulletItem(item)      => Present(readUnorderedList(cursor, line, item))
              case LineKind.OrderedItem(marker)   => Present(readOrderedList(cursor, line, marker))
              case LineKind.Text | LineKind.Blank => readParagraph(cursor, line, definitions)
            scanner.chargeOutputNodes(NodeCount.one)
            block.foreach(blocks += _)
        }
      }
    blocks.result()

  /**
   * Read a block quote and everything it holds.
   *
   * Nothing here knows what a quote may contain: the content goes through the same loop the document does, so a quote
   * holds whatever a document can, another quote included. `withNesting` declares the depth to the scanner, which is
   * what keeps a file of ten thousand `>` characters inside the budget rather than inside a stack overflow.
   */
  private def readBlockQuote(
      cursor: ContainerCursor,
      definitions: scala.collection.mutable.Map[String, LinkDefinition]
  ): Deferred =
    val scanner = cursor.scanner
    val start   = scanner.offset.toInt
    val inner   = cursor.nested(ContainerPrefix.BlockQuote)
    val blocks  = scanner.withNesting(parseDeferred(inner, definitions))
    val span    = Span.fromStartEnd(start, cursor.consumedEnd)
    Deferred.prose(defs => Block.BlockQuote(Chunk.from(blocks.map(_.resolve(defs))), span))

  /** Whether a line carries a block quote marker: up to three spaces, then `>`. */
  private def isBlockQuoteStart(text: String): Boolean =
    !ContainerPrefix.BlockQuote.consume(text, 0).isEmpty

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
    val nameEnd = tagNameEnd(text, 2)
    if nameEnd == 2 || !text.charAt(2).isLetter then Absent
    else
      val afterSpaces = skipSpaces(text, nameEnd)
      // A closing tag takes no attributes: anything but whitespace before the `>` disqualifies it.
      if afterSpaces < text.length && text.charAt(afterSpaces) == '>' then Present(afterSpaces + 1) else Absent

  @tailrec private def tagNameEnd(text: String, from: Int): Int =
    if from < text.length && (text.charAt(from).isLetterOrDigit || text.charAt(from) == '-') then
      tagNameEnd(text, from + 1)
    else from

  private def openTagEnd(text: String): Maybe[Int] =
    if text.length < 2 || !text.charAt(1).isLetter then Absent
    else
      @tailrec def attributes(index: Int): Maybe[Int] =
        val afterSpaces = skipSpaces(text, index)
        if afterSpaces >= text.length then Absent
        else if text.charAt(afterSpaces) == '>' then Present(afterSpaces + 1)
        else if text.charAt(afterSpaces) == '/' && afterSpaces + 1 < text.length &&
          text.charAt(afterSpaces + 1) == '>'
        then Present(afterSpaces + 2)
        else if afterSpaces == index then Absent // attributes must be separated by whitespace
        else
          attributeEnd(text, afterSpaces) match
            case Present(next) => attributes(next)
            case Absent        => Absent
      attributes(tagNameEnd(text, 1))

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
            @tailrec def unquotedEnd(cursor: Int): Int =
              if cursor < text.length && !text.charAt(cursor).isWhitespace &&
                "\"'=<>`".indexOf(text.charAt(cursor).toInt) < 0
              then unquotedEnd(cursor + 1)
              else cursor
            val cursor = unquotedEnd(valueStart)
            if cursor == valueStart then Absent else Present(cursor)

  @tailrec private def skipSpaces(text: String, from: Int): Int =
    if from < text.length && text.charAt(from).isWhitespace then skipSpaces(text, from + 1) else from

  /**
   * Read a raw HTML block.
   *
   * Conditions one to five run until their closing marker appears; six and seven run until a blank line. Either way the
   * lines are kept verbatim, because the content is HTML rather than Markdown.
   */
  private def readHtmlBlock(cursor: ContainerCursor, first: Line): Block =
    val scanner = cursor.scanner
    val kind    = htmlBlockStart(scanner, first).getOrElse(HtmlBlockKind.AnyTag)
    val lines   = List.newBuilder[String]
    lines += first.text
    // Unlike the other readers this one can stop *after* accepting a line, because a closing marker belongs to the
    // block it closes. `done` is therefore carried forward rather than tested at the top.
    @tailrec def gather(last: Line, done: Boolean): Line =
      if done then last
      else
        val checkpoint = cursor.checkpoint()
        cursor.readLine() match
          case Absent        => last
          case Present(line) =>
            if endsOnBlankLine(kind) && isBlank(scanner, line) then
              cursor.restore(checkpoint)
              last
            else
              lines += line.text
              gather(line, closesHtmlBlock(kind, line.text, opening = false))

    val last = gather(first, closesHtmlBlock(kind, first.text, opening = true))

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

  /** A line of four or more leading spaces, which CommonMark reads as code rather than as whatever it looks like. */
  private def isIndentedCode(scanner: SourceScanner, line: Line): Boolean =
    inspectLine(scanner, line)(text => text.length >= 4 && text.take(4).forall(_ == ' '))

  /**
   * Read an indented code block.
   *
   * Blank lines belong to the block when more indented content follows, which is what keeps the gaps in a multi-chunk
   * block; blank lines at the end do not, so the block stops at the last indented line.
   */
  private def readIndentedCode(cursor: ContainerCursor, first: Line): Block =
    val scanner = cursor.scanner
    val lines   = List.newBuilder[String]
    lines += stripIndent(first.text)
    // Blank lines are held back rather than appended: they belong to the block only if indented content follows, so
    // a trailing run of them is dropped by simply never being flushed.
    @tailrec def gather(last: Line, pending: List[String]): Line =
      val checkpoint = cursor.checkpoint()
      cursor.readLine() match
        case Absent        => last
        case Present(line) =>
          if isIndentedCode(scanner, line) then
            lines ++= pending
            lines += stripIndent(line.text)
            gather(line, Nil)
          else if isBlank(scanner, line) then gather(last, pending :+ stripIndent(line.text))
          else
            cursor.restore(checkpoint)
            last

    val last = gather(first, Nil)

    val content = lines.result().mkString("", "\n", "\n")
    scanner.chargeWork(WorkUnits.from(content.length.toLong).getOrThrow)
    Block.IndentedCode(content, Span.fromStartEnd(first.offset, last.end))

  /** Remove up to four leading spaces, which is the indentation the block form spends rather than content. */
  private def stripIndent(text: String): String =
    @tailrec def removed(count: Int): Int =
      if count < 4 && count < text.length && text.charAt(count) == ' ' then removed(count + 1) else count
    text.substring(removed(0))

  /**
   * Consume lines for as long as `take` accepts them, and return the last one accepted.
   *
   * The scanner is restored to just before the first rejected line, so the caller's block ends where it should and the
   * next block starts by reading that line again. Four readers hand-rolled this with a `done` flag; naming it once
   * makes each of them say what it collects rather than how it stops.
   *
   * `take` is expected to have an effect -- appending to the caller's builder -- which is why it returns a plain
   * Boolean rather than an option: acceptance and accumulation are the same decision.
   */
  /**
   * Consume lines for as long as `take` accepts them, and return the last one accepted.
   *
   * The scanner is restored to just before the first rejected line, so the caller's block ends where it should and the
   * next block starts by reading that line again. Four readers hand-rolled this with a `done` flag; naming it once
   * makes each of them say what it collects rather than how it stops.
   *
   * `take` is expected to have an effect -- appending to the caller's builder -- which is why it returns a plain
   * Boolean rather than an option: acceptance and accumulation are the same decision.
   *
   * `inline`, with an `inline` predicate, so the caller's lambda is beta-reduced into the loop instead of becoming a
   * `Function1`. `Line => Boolean` is not one of the shapes `Function1` specialises, so every line would otherwise pay
   * a generic `apply` and a boxed `Boolean`. The recursion stays in a local `@tailrec` loop, because an `inline def`
   * cannot recurse. Two call sites, so the body is duplicated twice -- worth it here, and the reason this is private
   * rather than something callers can grow.
   */
  private inline def consumeWhile(cursor: ContainerCursor, last: Line)(inline take: Line => Boolean): Line =
    @tailrec def loop(current: Line): Line =
      val checkpoint = cursor.checkpoint()
      cursor.readLine() match
        case Present(line) if take(line) => loop(line)
        case Present(_)                  =>
          cursor.restore(checkpoint)
          current
        case Absent => current
    loop(last)

  private type FenceOpen = (marker: Char, length: Int, indentation: Int, info: String)

  private def readFencedCode(cursor: ContainerCursor, opening: Line, open: FenceOpen): Block =
    val scanner = cursor.scanner
    val body    = StringBuilder()
    // A fence is consumed whether or not it closes: an unterminated one runs to the end of its container, and whether
    // its last line ended in a newline decides the content's trailing newline.
    @tailrec def gather(closingEnd: Int, closed: Boolean, endedWithLf: Boolean): (Int, Boolean, Boolean) =
      if closed then (closingEnd, closed, endedWithLf)
      else
        cursor.readLine() match
          case Absent        => (closingEnd, closed, endedWithLf)
          case Present(line) =>
            if isClosingFence(scanner, line, open.marker, open.length) then (line.end, true, endedWithLf)
            else
              if body.nonEmpty then body.append('\n')
              body.append(removeFenceIndentation(scanner, line, open.indentation))
              gather(closingEnd, false, line.terminatedByLf)

    val (closingEnd, closed, bodyEndedWithLf) = gather(opening.end, false, false)

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
      cursor: ContainerCursor,
      first: Line,
      definitions: scala.collection.mutable.Map[String, LinkDefinition]
  ): Maybe[Deferred] =
    val scanner  = cursor.scanner
    val segments = List.newBuilder[(Int, String)]
    segments += segment(first)
    // A paragraph ends three ways, and the recursion says which: a setext underline promotes it to a heading and is
    // consumed with it, a line that does not continue it is put back, and end of input just stops.
    //
    // This is the one block that reads lazily -- `readContinued` rather than `readLine` -- because it is the one block
    // a line may continue without repeating its containers' markers. A lazy line is prose and nothing else: it can
    // neither open a block nor close a setext heading, which is what `matchedAll` guards below.
    @tailrec def gather(last: Line): (Line, Maybe[HeadingLevel]) =
      val checkpoint = cursor.checkpoint()
      cursor.readContinued() match
        case Absent             => (last, Absent)
        case Present(continued) =>
          val line       = continued.line
          val classified = classify(scanner, line)
          classified.setext match
            case Present(level) if continued.matchedAll && classified.kind != LineKind.IndentedCode =>
              (line, Present(level))
            case _ =>
              if continues(classified) then
                segments += segment(line)
                gather(line)
              else
                cursor.restore(checkpoint)
                (last, Absent)

    val (last, setext) = gather(first)

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
   * A paragraph line with its indentation removed, and the offset moved to match.
   *
   * CommonMark strips the leading whitespace of every line of a paragraph, not only the first, which is why an indented
   * continuation line is neither code nor a setext underline: `Foo` over `    ---` is one paragraph reading `Foo\n---`.
   * Moving the offset with the text is what keeps an inline span in that line pointing at the source.
   */
  private def segment(line: Line): (Int, String) =
    val stripped = line.text.stripLeading
    (line.offset + (line.text.length - stripped.length), stripped)

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
    @tailrec def take(consumed: Int): Int =
      linkDefinitionAt(text, consumed) match
        case Present((end, label, destination, title)) =>
          val key = InlineParser.normalizeLabel(label)
          // First definition wins, which is what the spec says about duplicates.
          if !definitions.contains(key) then definitions(key) = LinkDefinition(destination, title)
          take(end)
        case Absent => consumed
    take(0)

  /** One definition beginning at `from`, or [[kyo.Absent]] if the text does not start with one. */
  private def linkDefinitionAt(
      text: String,
      from: Int
  ): Maybe[(Int, String, String, Maybe[String])] =
    @tailrec def skipLeading(cursor: Int): Int =
      if cursor < text.length && (text.charAt(cursor) == ' ' || text.charAt(cursor) == '\n') then
        skipLeading(cursor + 1)
      else cursor
    val index = skipLeading(from)
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
    @tailrec def walk(cursor: Int, remaining: Int): Int =
      if cursor >= lines.size then
        val (lastOffset, lastText) = lines(lines.size - 1)
        lastOffset + lastText.length
      else
        val (offset, text) = lines(cursor)
        if remaining <= text.length then offset + remaining
        else walk(cursor + 1, remaining - (text.length + 1)) // the '\n' the join introduced
    walk(0, index)

  private def isBlank(scanner: SourceScanner, line: Line): Boolean =
    inspectLine(scanner, line)(_.trim.isEmpty)

  private def isThematicBreakText(text: String): Boolean =
    val compact = text.filterNot(_.isWhitespace)
    compact.length >= 3 && (
      compact.forall(_ == '-') || compact.forall(_ == '*') || compact.forall(_ == '_')
    )

  private def readUnorderedList(cursor: ContainerCursor, first: Line, firstItem: String): Deferred =
    val scanner = cursor.scanner
    val items   = List.newBuilder[DeferredItem]
    items += listItem(first, firstItem)
    val last = consumeWhile(cursor, first) { line =>
      unorderedItem(scanner, line) match
        case Present(item) =>
          items += listItem(line, item)
          true
        case Absent => false
    }
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
  private def readOrderedList(cursor: ContainerCursor, first: Line, firstItem: OrderedMarker): Deferred =
    val scanner = cursor.scanner
    val items   = List.newBuilder[DeferredItem]
    items += listItem(first, firstItem.content)
    val last = consumeWhile(cursor, first) { line =>
      orderedItem(scanner, line) match
        case Present(marker) if marker.delimiter == firstItem.delimiter =>
          items += listItem(line, marker.content)
          true
        case _ => false
    }

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

  // The one-to-six bound lives in HeadingLevel.fromInt rather than in a guard here, so a run of seven
  // or more hashes falls through to the paragraph branch exactly as CommonMark requires.
  private def headingPrefix(text: String): Maybe[(HeadingLevel, String)] =
    val hashes = text.takeWhile(_ == '#')
    if hashes.nonEmpty && text.length > hashes.length && text.charAt(hashes.length) == ' ' then
      HeadingLevel.fromInt(hashes.length).map(level => (level, text.drop(hashes.length + 1)))
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
