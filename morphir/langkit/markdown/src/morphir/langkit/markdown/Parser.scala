package morphir.langkit.markdown

import kyo.*
import scala.annotation.tailrec
import morphir.langkit.core.Span
import morphir.langkit.markdown.cst.CstCollector
import morphir.langkit.markdown.cst.CstFragment
import morphir.langkit.markdown.cst.InlineSlot
import morphir.langkit.core.scanner.*
import morphir.langkit.markdown.internal.{ContainerCursor, ContainerPrefix, HtmlTag, InlineParser, Line, LinkDefinition}

/**
 * A CommonMark parser, complete against the 0.31.2 specification.
 *
 * Every one of the 652 examples in that specification parses and renders byte for byte, which the conformance suite in
 * `morphir-langkit-markdown-scalatags` measures and defends. `commonmark-java` must not enter this module: the point of
 * writing the parser was to have one that runs on JVM, Scala.js and Scala Native alike.
 *
 * GitHub Flavored Markdown is a separate profile and is not implemented here. Tables, strikethrough, task lists and
 * autolink literals are all extensions to CommonMark rather than parts of it.
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
      val blocks      = parseBlocks(ContainerCursor.top(scanner), definitions, Absent)
      // Keep the caller's coordinate space: do not rewrite CRLF before measuring spans.
      val document = Document(blocks, Span(0, source.length))
      (document, scanner.metrics)
    } match
      case ScanResult.Success(value) => Result.succeed(value)
      case ScanResult.Failure(error) => Result.fail(ParseError.Scan(error))

  /**
   * The block phase again, reporting what it read as [[CstFragment]]s rather than as a document.
   *
   * Same loop, same budget discipline; the collector rides along the way `definitions` does. Container readers open a
   * child collector per container, so the fragments nest the way the blocks do.
   * [[morphir.langkit.markdown.cst.CstParser]] is the one caller.
   */
  private[markdown] def parseFragments(
      source: String,
      budget: ScanBudget = ScanBudget.default
  ): Result[ParseError, Chunk[CstFragment]] =
    SourceScanner.scan(source, budget, phase = Present(BlocksPhase)) { scanner =>
      scanner.chargeOutputNodes(NodeCount.one)
      val definitions = scala.collection.mutable.Map.empty[String, LinkDefinition]
      val collector   = CstCollector()
      val _           = parseBlocks(ContainerCursor.top(scanner), definitions, Present(collector))
      collector.fragments
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
    case BulletItem(marker: BulletMarker)
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
                          case Present(marker) => LineKind.BulletItem(marker)
                          case Absent          =>
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
      // A list interrupts a paragraph only when it starts at 1 and its first item holds something: `1.` alone under a
      // paragraph is that paragraph's text, and so is `14.` whatever follows it (example 304).
      case LineKind.OrderedItem(marker)        => marker.number != 1 || marker.empty
      case LineKind.BulletItem(marker)         => marker.empty
      case LineKind.Html(HtmlBlockKind.AnyTag) => true
      case _                                   => false

  /**
   * Whether a line that dropped a container's marker continues the paragraph above it.
   *
   * Stricter than [[continues]], and it has to be. A lazy line is only ever paragraph continuation text: it fell out of
   * a container, so anything that could open a block where it landed opens one there instead. `1. one` over `2. two` is
   * two items even though `2.` may not interrupt a paragraph -- the paragraph is not what the line fell out of, the
   * list is, and a list is happy to take another item.
   */
  private def continuesLazily(classified: Classified): Boolean =
    classified.kind match
      case LineKind.Text | LineKind.IndentedCode => true
      case _                                     => false

  private def parseBlocks(
      cursor: ContainerCursor,
      definitions: scala.collection.mutable.Map[String, LinkDefinition],
      cst: Maybe[CstCollector]
  ): Chunk[Block] =
    val run = parseDeferred(cursor, definitions, cst)
    // Second phase: every definition in the document is known now, so prose can resolve references that point
    // forward as well as back.
    Chunk.from(run.blocks.map(_.resolve(definitions.toMap)))

  /**
   * Read every block the cursor's container holds, with prose left unresolved.
   *
   * The document is a container like any other, so this is the one block loop: the top level runs it over a cursor with
   * nothing open, and a block quote runs it over a cursor that takes the `>` off each line. What differs between them
   * is which lines the cursor offers, not what is done with them.
   */
  /**
   * A run of blocks, and what the blank lines between them say about it.
   *
   * Only a list asks the last two questions, and only a list can answer them: CommonMark calls a list loose when its
   * items are separated by blank lines or when an item holds two blocks with a blank line between them, so the
   * information has to come out of the run that read the item rather than be recovered from the blocks afterwards.
   * `- a` over `  - b` is two blocks in one item and is tight; `- a`, blank, `  b` is two blocks and is not.
   */
  private final case class BlockRun(
      blocks: List[Deferred],
      contentEnd: Int,
      interiorBlank: Boolean,
      trailingBlank: Boolean
  )

  /**
   * A block that has just been read, and whether reading it consumed a blank line at its end.
   *
   * Only a container can consume one: a blank line matches a continuation prefix, so it is eaten inside the container
   * rather than left for the loop that opened it. A list has to know, because a blank that ended a nested container
   * still separated the blocks of the item holding it.
   */
  private final case class Opened(block: Maybe[Deferred], endedOnBlank: Boolean)

  private object Opened:
    def leaf(block: Block): Opened        = Opened(Present(Deferred.ready(block)), endedOnBlank = false)
    def deferred(block: Deferred): Opened = Opened(Present(block), endedOnBlank = false)

  private def parseDeferred(
      cursor: ContainerCursor,
      definitions: scala.collection.mutable.Map[String, LinkDefinition],
      cst: Maybe[CstCollector]
  ): BlockRun =
    val scanner       = cursor.scanner
    val blocks        = List.newBuilder[Deferred]
    var written       = 0
    var contentEnd    = cursor.consumedEnd
    var blankSince    = false
    var interiorBlank = false
    while !cursor.isAtEnd do
      scanner.requireProgress(BlocksPhase) {
        val opening = cursor.checkpoint()
        cursor.readLine().foreach { line =>
          val classified = classify(scanner, line)
          if classified.kind == LineKind.Blank then blankSince = true
          else
            val opened = classified.kind match
              case LineKind.IndentedCode => Opened.leaf(readIndentedCode(cursor, line, cst))
              case LineKind.BlockQuote   =>
                // A quote's own cursor has to see the marker, so the line goes back before the recursion reads it.
                cursor.restore(opening)
                readBlockQuote(cursor, definitions, cst)
              case LineKind.Heading(level, rest) =>
                val headingSpan = Span(line.offset, line.length)
                val base        = contentSpan(line, rest).offset
                val slot        = InlineSlot()
                cst.foreach(_.record(CstFragment.AtxHeading(level, headingSpan, Span(base, rest.length), slot)))
                Opened.deferred(Deferred.prose { defs =>
                  val content = InlineParser.parse(rest, index => base + index, defs)
                  slot.fill(content)
                  Block.Heading(level, content, headingSpan)
                })
              case LineKind.Fence(open)   => Opened.leaf(readFencedCode(cursor, line, open, cst))
              case LineKind.ThematicBreak =>
                val breakSpan = Span(line.offset, line.length)
                cst.foreach(_.record(CstFragment.ThematicBreak(breakSpan)))
                Opened.leaf(Block.ThematicBreak(breakSpan))
              case LineKind.Html(_)          => Opened.leaf(readHtmlBlock(cursor, line, cst))
              case LineKind.BulletItem(item) =>
                cursor.restore(opening)
                readUnorderedList(cursor, definitions, item, line.offset, cst)
              case LineKind.OrderedItem(marker) =>
                cursor.restore(opening)
                readOrderedList(cursor, definitions, marker, line.offset, cst)
              case LineKind.Text | LineKind.Blank =>
                Opened(readParagraph(cursor, line, definitions, cst), endedOnBlank = false)
            scanner.chargeOutputNodes(NodeCount.one)
            contentEnd = cursor.consumedEnd
            opened.block.foreach { deferred =>
              if written > 0 && blankSince then interiorBlank = true
              blocks += deferred
              written += 1
              // Only content clears the blank. A line that produced nothing -- a link reference definition, which is
              // recorded rather than rendered -- leaves the run still standing on the blank before it, which is what
              // makes example 317 a loose list.
              blankSince = false
            }
            // A container swallows the blank line at its end, because a blank matches its continuation prefix. The
            // blank still separated this run's blocks, so it is put back here: without it, examples 325 and 326 read
            // as tight lists.
            if opened.endedOnBlank then blankSince = true
        }
      }
    BlockRun(blocks.result(), contentEnd, interiorBlank, blankSince)

  /**
   * Read a block quote and everything it holds.
   *
   * Nothing here knows what a quote may contain: the content goes through the same loop the document does, so a quote
   * holds whatever a document can, another quote included. `withNesting` declares the depth to the scanner, which is
   * what keeps a file of ten thousand `>` characters inside the budget rather than inside a stack overflow.
   */
  private def readBlockQuote(
      cursor: ContainerCursor,
      definitions: scala.collection.mutable.Map[String, LinkDefinition],
      cst: Maybe[CstCollector]
  ): Opened =
    val scanner = cursor.scanner
    val start   = scanner.offset.toInt
    // The quote's own collector: its run records child fragments into it, and its cursor records the `>` marker span
    // of every line it reads. The quote fragment carries both, so nesting is recorded by construction.
    val child = cst.map(_ => CstCollector())
    val inner = cursor.nested(ContainerPrefix.BlockQuote, child)
    val run   = scanner.withNesting(parseDeferred(inner, definitions, child))
    val span  = Span.fromStartEnd(start, cursor.consumedEnd)
    for
      collector <- cst
      recorded  <- child
    do collector.record(CstFragment.BlockQuote(span, recorded.markers, recorded.fragments))
    // Not a container that can end on a blank line, unlike a list item: the quote's prefix needs a `>`, so the blank
    // lines it swallows are `>` with nothing after it. Those are blank *content*, not blank lines, and they do not
    // loosen the list holding the quote -- example 320 is `* a`, an indented quote ending in a bare `>`, then `* c`,
    // and it renders tight.
    Opened.deferred(Deferred.prose(defs => Block.BlockQuote(Chunk.from(run.blocks.map(_.resolve(defs))), span)))

  /** Whether a line carries a block quote marker: up to three spaces, then `>`. */
  private def isBlockQuoteStart(text: String): Boolean =
    !ContainerPrefix.BlockQuote.consume(text, 0, absoluteFrom = 0).isEmpty

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
      // Condition one names whole tags, so the name has to end where the name ends: `<scriptorium>` is not `<script>`
      // and must not open a block that runs to `</script>` -- which, absent one, is the rest of the document.
      if Seq("<script", "<pre", "<style", "<textarea").exists(prefix =>
          lower.startsWith(prefix) && endsTagName(lower, prefix.length)
        )
      then Present(HtmlBlockKind.ScriptLike)
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

  /** Whether a tag name ends at `at`: the spec allows whitespace, `>`, or the end of the line, and nothing else. */
  private def endsTagName(text: String, at: Int): Boolean =
    at >= text.length || text.charAt(at) == '>' || text.charAt(at).isWhitespace

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
    HtmlTag.tagEndOf(trimmed, 0).exists(end => trimmed.drop(end).isBlank)

  /**
   * Read a raw HTML block.
   *
   * Conditions one to five run until their closing marker appears; six and seven run until a blank line. Either way the
   * lines are kept verbatim, because the content is HTML rather than Markdown.
   */
  private def readHtmlBlock(cursor: ContainerCursor, first: Line, cst: Maybe[CstCollector]): Block =
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
    val span = Span.fromStartEnd(first.offset, last.end)
    cst.foreach(_.record(CstFragment.HtmlBlock(span)))
    Block.HtmlBlock(content, span)

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
  private def readIndentedCode(cursor: ContainerCursor, first: Line, cst: Maybe[CstCollector]): Block =
    val scanner = cursor.scanner
    val lines   = List.newBuilder[String]
    // Blank lines are held back rather than appended: they belong to the block only if indented content follows, so a
    // run of them at either end is dropped by simply never being flushed. Blankness is asked first, because a line of
    // exactly four spaces is both blank and indented and it is the blank that decides.
    if !isBlank(scanner, first) then lines += stripIndent(first.text)

    @tailrec def gather(last: Line, pending: List[String]): Line =
      val checkpoint = cursor.checkpoint()
      cursor.readLine() match
        case Absent        => last
        case Present(line) =>
          if isBlank(scanner, line) then gather(last, pending :+ stripIndent(line.text))
          else if isIndentedCode(scanner, line) then
            lines ++= pending
            lines += stripIndent(line.text)
            gather(line, Nil)
          else
            cursor.restore(checkpoint)
            last

    val last = gather(first, Nil)

    val content = lines.result().mkString("", "\n", "\n")
    scanner.chargeWork(WorkUnits.from(content.length.toLong).getOrThrow)
    val span = Span.fromStartEnd(first.offset, last.end)
    cst.foreach(_.record(CstFragment.IndentedCode(span)))
    Block.IndentedCode(content, span)

  /** Remove up to four leading spaces, which is the indentation the block form spends rather than content. */
  private def stripIndent(text: String): String =
    @tailrec def removed(count: Int): Int =
      if count < 4 && count < text.length && text.charAt(count) == ' ' then removed(count + 1) else count
    text.substring(removed(0))

  private type FenceOpen = (marker: Char, length: Int, indentation: Int, info: String)

  private def readFencedCode(cursor: ContainerCursor, opening: Line, open: FenceOpen, cst: Maybe[CstCollector]): Block =
    val scanner = cursor.scanner
    val body    = StringBuilder()
    // A fence is consumed whether or not it closes: an unterminated one runs to the end of its container, and whether
    // its last line ended in a newline decides the content's trailing newline.
    // `written`, not `body.nonEmpty`: a fence whose first line is blank writes nothing and would otherwise look like
    // it had written nothing at all, so the next line would join it and the blank line would vanish.
    var written = 0
    // Where the closing fence line starts, for the CST: the closing token leaf runs from here to the block's end.
    var closeStart: Maybe[Int] = Absent

    @tailrec def gather(closingEnd: Int, closed: Boolean, endedWithLf: Boolean): (Int, Boolean, Boolean) =
      if closed then (closingEnd, closed, endedWithLf)
      else
        cursor.readLine() match
          case Absent        => (closingEnd, closed, endedWithLf)
          case Present(line) =>
            if isClosingFence(scanner, line, open.marker, open.length) then
              closeStart = Present(line.offset)
              (line.end, true, endedWithLf)
            else
              if written > 0 then body.append('\n')
              body.append(removeFenceIndentation(scanner, line, open.indentation))
              written += 1
              gather(closingEnd, false, line.terminatedByLf)

    val (closingEnd, closed, bodyEndedWithLf) = gather(opening.end, false, false)

    val end     = if closed then closingEnd else scanner.offset.toInt
    val content =
      if closed then
        if written > 0 then body.append('\n')
        body.toString
      else
        if written > 0 && bodyEndedWithLf then body.append('\n')
        body.toString

    val span = Span.fromStartEnd(opening.offset, end)
    cst.foreach(_.record(CstFragment.FencedCode(span, opening.end, closeStart)))
    // The budgeted FenceInfo path reserves deterministic work and output before token materialization.
    Block.FencedCode(FenceInfo.parseBudgeted(open.info, scanner), content, span)

  private def readParagraph(
      cursor: ContainerCursor,
      first: Line,
      definitions: scala.collection.mutable.Map[String, LinkDefinition],
      cst: Maybe[CstCollector]
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
    @tailrec def gather(last: Line): (last: Line, setext: Maybe[HeadingLevel], beforeUnderline: Maybe[ScanCheckpoint]) =
      val checkpoint = cursor.checkpoint()
      cursor.readContinued() match
        case Absent             => (last = last, setext = Absent, beforeUnderline = Absent)
        case Present(continued) =>
          val line       = continued.line
          val classified = classify(scanner, line)
          classified.setext match
            case Present(level) if continued.matchedAll && classified.kind != LineKind.IndentedCode =>
              (last = line, setext = Present(level), beforeUnderline = Present(checkpoint))
            case _ =>
              if (if continued.matchedAll then continues(classified) else continuesLazily(classified)) then
                segments += segment(line)
                gather(line)
              else
                cursor.restore(checkpoint)
                (last = last, setext = Absent, beforeUnderline = Absent)

    val gathered = gather(first)
    val last     = gathered.last
    val setext   = gathered.setext

    val lines   = Chunk.from(segments.result())
    val raw     = lines.map(_._2).mkString("\n")
    val trimmed = raw.trim
    val leading = raw.length - raw.stripLeading.length
    scanner.chargeWork(WorkUnits.from(raw.length.toLong).getOrThrow)

    // A paragraph may open with link reference definitions. They are not content: they are consumed here and only
    // what follows them, if anything, becomes a paragraph.
    val (consumed = consumed, taken = taken) = takeDefinitions(trimmed, definitions)
    // Each definition is its own CST node, recorded where it was consumed. Indices in the trimmed text map back to
    // the source through the lines that built it, the same way inline spans do.
    cst.foreach { collector =>
      taken.foreach { case (start = defStart, end = defEnd) =>
        collector.record(CstFragment.LinkReferenceDefinition(Span.fromStartEnd(
          sourceOffsetOf(lines, defStart + leading),
          sourceOffsetOf(lines, defEnd + leading)
        )))
      }
    }
    val body = trimmed.substring(consumed)
    if body.isBlank then
      // Definitions and nothing else. A setext underline has no paragraph to close here, so it goes back to be read
      // on its own: `[foo]: /url` over `===` is a definition and then a paragraph beginning `===`, not a heading.
      gathered.beforeUnderline.foreach(cursor.restore)
      Absent
    else
      // Where the prose itself begins in the joined text: past the consumed definitions and past any whitespace the
      // body's own trim will strip, so the inline mapping and the CST fragment agree on the first content character.
      val contentIndex = leading + consumed + (body.length - body.stripLeading.length)
      val span         = Span.fromStartEnd(first.offset, last.end)
      // The CST fragment starts at the body, not at the run: definitions consumed off the front are their own nodes,
      // and a fragment overlapping them would lose the paragraph to a verbatim gap at materialization.
      val cstStart = if consumed == 0 then span.offset else sourceOffsetOf(lines, contentIndex)
      val cstSpan  = Span.fromStartEnd(cstStart, span.end)
      val slot     = InlineSlot()
      cst.foreach(_.record(setext match
        case Present(level) => CstFragment.SetextHeading(level, cstSpan, last.offset, slot)
        case Absent         => CstFragment.Paragraph(cstSpan, slot)))
      Present(Deferred.prose { defs =>
        val content = InlineParser.parse(body.trim, index => sourceOffsetOf(lines, index + contentIndex), defs)
        slot.fill(content)
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
    // The offset comes from the line rather than from the text's length, because a tab-expanded line counts columns
    // where the source counts characters: `\tfoo` has four columns of indentation and one character of it.
    (line.contentOffset, line.text.stripLeading)

  /**
   * Consume `[label]: destination "title"` definitions from the front of a paragraph's text.
   *
   * Returns how many characters were taken. A definition contributes no block; it is recorded and the text that follows
   * becomes the paragraph, which is what lets a document open with its link definitions.
   */
  private def takeDefinitions(
      text: String,
      definitions: scala.collection.mutable.Map[String, LinkDefinition]
  ): (consumed: Int, taken: List[(start: Int, end: Int)]) =
    val taken                             = List.newBuilder[(start: Int, end: Int)]
    @tailrec def take(consumed: Int): Int =
      linkDefinitionAt(text, consumed) match
        case Present(definition) =>
          val key = InlineParser.normalizeLabel(definition.label)
          // First definition wins, which is what the spec says about duplicates.
          if !definitions.contains(key) then
            definitions(key) = LinkDefinition(definition.destination, definition.title)
          taken += ((start = definition.start, end = definition.end))
          take(definition.end)
        case Absent => consumed
    val consumed = take(0)
    (consumed = consumed, taken = taken.result())

  /** A link reference definition: where it starts and ends, its label, and where it points. */
  private type Definition = (start: Int, end: Int, label: String, destination: String, title: Maybe[String])

  /** One definition beginning at `from`, or [[kyo.Absent]] if the text does not start with one. */
  private def linkDefinitionAt(text: String, from: Int): Maybe[Definition] =
    @tailrec def skipLeading(cursor: Int): Int =
      if cursor < text.length && (text.charAt(cursor) == ' ' || text.charAt(cursor) == '\n') then
        skipLeading(cursor + 1)
      else cursor
    val index = skipLeading(from)
    if index >= text.length || text.charAt(index) != '[' then Absent
    else
      // A definition's label follows the label rule, not the link-text rule: it ends at the first unescaped `]` and
      // holds no bare bracket, so `[[[foo]]]: /url` and `[]: /uri` define nothing and stay paragraphs.
      InlineParser.referenceLabelEndOf(text, index + 1) match
        case Absent         => Absent
        case Present(close) =>
          if close + 1 >= text.length || text.charAt(close + 1) != ':' then Absent
          else
            val label = text.substring(index + 1, close)
            if !InlineParser.isLabelOf(label) then Absent
            else
              InlineParser.definitionTarget(text, close + 2) match
                case Absent          => Absent
                case Present(target) =>
                  Present((
                    start = index,
                    end = target.end,
                    label = label,
                    destination = target.destination,
                    title = target.title
                  ))

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

  /**
   * Read a run of bullet items sharing one bullet character into one list.
   *
   * A change of bullet begins a new list rather than continuing this one, which is why example 281 renders two.
   */
  private def readUnorderedList(
      cursor: ContainerCursor,
      definitions: scala.collection.mutable.Map[String, LinkDefinition],
      first: BulletMarker,
      markerAt: Int,
      cst: Maybe[CstCollector]
  ): Opened =
    val start = cursor.scanner.offset.toInt
    val items = List.newBuilder[DeferredItem]
    // The list's own collector: each item records its fragment here, and the list fragment gathers them.
    val listCst = cst.map(_ => CstCollector())

    @tailrec def gather(
        marker: BulletMarker,
        at: Int,
        loose: Boolean
    ): (loose: Boolean, end: Int, endedOnBlank: Boolean) =
      val (item, run) = readItem(cursor, definitions, at, marker.columns, marker.empty, listCst)
      items += item
      val soFar  = loose || run.interiorBlank
      val resume = cursor.checkpoint()
      nextItem(cursor) match
        case Present((line = line, kind = LineKind.BulletItem(next), blankBefore = blankBefore))
            if next.bullet == marker.bullet =>
          gather(next, line.offset, soFar || run.trailingBlank || blankBefore)
        case _ =>
          cursor.restore(resume)
          // The list ends where its last item does, so it ended on a blank line exactly when that item did.
          (loose = soFar, end = run.contentEnd, endedOnBlank = run.trailingBlank)

    val gathered  = gather(first, markerAt, loose = false)
    val loose     = gathered.loose
    val collected = Chunk.from(items.result())
    val listSpan  = Span.fromStartEnd(start, gathered.end)
    for
      collector <- cst
      recorded  <- listCst
    do collector.record(CstFragment.BulletList(first.bullet, tight = !loose, listSpan, recorded.fragments))
    Opened(
      Present(Deferred.prose(defs => Block.UnorderedList(collected.map(_.resolve(defs)), !loose, listSpan))),
      gathered.endedOnBlank
    )

  /**
   * Read a run of numbered items sharing one delimiter into one list.
   *
   * A change of delimiter ends the list, because `1.` and `1)` are different lists rather than one; example 302 renders
   * two.
   */
  private def readOrderedList(
      cursor: ContainerCursor,
      definitions: scala.collection.mutable.Map[String, LinkDefinition],
      first: OrderedMarker,
      markerAt: Int,
      cst: Maybe[CstCollector]
  ): Opened =
    val start = cursor.scanner.offset.toInt
    val items = List.newBuilder[DeferredItem]
    // The list's own collector: each item records its fragment here, and the list fragment gathers them.
    val listCst = cst.map(_ => CstCollector())

    @tailrec def gather(
        marker: OrderedMarker,
        at: Int,
        loose: Boolean
    ): (loose: Boolean, end: Int, endedOnBlank: Boolean) =
      val (item, run) = readItem(cursor, definitions, at, marker.columns, marker.empty, listCst)
      items += item
      val soFar  = loose || run.interiorBlank
      val resume = cursor.checkpoint()
      nextItem(cursor) match
        case Present((line = line, kind = LineKind.OrderedItem(next), blankBefore = blankBefore))
            if next.delimiter == marker.delimiter =>
          gather(next, line.offset, soFar || run.trailingBlank || blankBefore)
        case _ =>
          cursor.restore(resume)
          // The list ends where its last item does, so it ended on a blank line exactly when that item did.
          (loose = soFar, end = run.contentEnd, endedOnBlank = run.trailingBlank)

    val gathered  = gather(first, markerAt, loose = false)
    val loose     = gathered.loose
    val collected = Chunk.from(items.result())
    val listSpan  = Span.fromStartEnd(start, gathered.end)
    for
      collector <- cst
      recorded  <- listCst
    do
      collector.record(
        CstFragment.OrderedList(first.number, first.delimiter, tight = !loose, listSpan, recorded.fragments)
      )
    Opened(
      Present(Deferred.prose(defs =>
        Block.OrderedList(first.number, collected.map(_.resolve(defs)), !loose, listSpan)
      )),
      gathered.endedOnBlank
    )

  /**
   * The next line that could open another item, classified, with the cursor left just before it.
   *
   * Blank lines are stepped over rather than read as the end of the list, because they are not: they separate items and
   * make the list loose, which is what `blankBefore` reports. An item that consumed its own trailing blanks reports the
   * same thing through its run, so both routes to a loose list are covered.
   *
   * The caller decides whether the line opens an item of *this* list, and puts the cursor back to its own checkpoint if
   * it does not -- the blanks belong to whatever follows the list in that case.
   */
  private def nextItem(cursor: ContainerCursor): Maybe[(line: Line, kind: LineKind, blankBefore: Boolean)] =
    @tailrec def skip(blank: Boolean): Maybe[(line: Line, kind: LineKind, blankBefore: Boolean)] =
      val before = cursor.checkpoint()
      cursor.readLine() match
        case Absent        => Absent
        case Present(line) =>
          val kind = classify(cursor.scanner, line).kind
          if kind == LineKind.Blank then skip(blank = true)
          else
            cursor.restore(before)
            Present((line = line, kind = kind, blankBefore = blank))
    skip(blank = false)

  /**
   * Read one list item, which holds blocks rather than prose.
   *
   * The item's content is read by the same loop the document is, through a cursor that spends `columns` characters of
   * every line: the marker on the item's own line, and the indentation that stands in for it on the rest. That is what
   * lets an item hold a code block, a quote, or another list without anything here enumerating the possibilities.
   */
  private def readItem(
      cursor: ContainerCursor,
      definitions: scala.collection.mutable.Map[String, LinkDefinition],
      markerAt: Int,
      columns: Int,
      startsEmpty: Boolean,
      cst: Maybe[CstCollector]
  ): (DeferredItem, BlockRun) =
    val scanner = cursor.scanner
    val start   = scanner.offset.toInt
    // The item's own collector: child fragments and the marker spans its cursor spends — the marker line's prefix and
    // each continuation line's indentation. The item fragment is recorded into the list's collector, `cst`.
    val child = cst.map(_ => CstCollector())
    val inner = cursor.nested(ContainerPrefix.ListItem(markerAt, columns), child)

    def item(run: BlockRun): (DeferredItem, BlockRun) =
      val span = Span.fromStartEnd(start, run.contentEnd)
      for
        collector <- cst
        recorded  <- child
      do collector.record(CstFragment.ListItem(span, recorded.markers, recorded.fragments))
      (DeferredItem(defs => ListItem(Chunk.from(run.blocks.map(_.resolve(defs))), span)), run)

    // An item must consume at least the line that opened it. Guarding that here rather than trusting it: an item that
    // consumed nothing would leave the list gathering the same line for ever, and because nothing was read there is no
    // work charged for the scan budget to notice.
    def read(): BlockRun =
      scanner.requireProgress(BlocksPhase)(scanner.withNesting(parseDeferred(inner, definitions, child)))

    if !startsEmpty then item(read())
    else
      // "A list item can begin with at most one blank line." An item whose marker line holds nothing and whose next
      // line is blank as well holds nothing at all, and that second blank belongs to whatever follows the list.
      inner.readLine()
      val markerEnd  = cursor.consumedEnd
      val checkpoint = cursor.checkpoint()
      val followed   = inner.readLine().exists(line => !isBlank(scanner, line))
      cursor.restore(checkpoint)
      if followed then item(read())
      else item(BlockRun(Nil, markerEnd, interiorBlank = false, trailingBlank = false))

  private final case class DeferredItem(resolve: Map[String, LinkDefinition] => ListItem)

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

  private type BulletMarker  = (bullet: Char, columns: Int, empty: Boolean)
  private type OrderedMarker = (number: Int, delimiter: Char, columns: Int, empty: Boolean)

  /**
   * How many characters a marker of `width` at `indent` spends before its content begins.
   *
   * The spaces after the marker are part of it, up to four. Beyond that the item holds indented code and only one space
   * is spent, which is what makes `1.      indented code` a code block rather than a very indented paragraph (example
   * 274). An item with nothing after its marker spends one space it does not have, and starts empty.
   */
  private def markerColumns(text: String, indent: Int, width: Int): (columns: Int, empty: Boolean) =
    val afterMarker = indent + width
    val spaces      = text.drop(afterMarker).takeWhile(_ == ' ').length
    val empty       = text.drop(afterMarker).isBlank
    if empty || spaces > 4 then (columns = afterMarker + 1, empty = empty)
    else (columns = afterMarker + spaces, empty = false)

  /** A numbered list marker: up to nine digits, then `.` or `)`, then a space or the end of the line. */
  private def orderedItem(text: String): Maybe[OrderedMarker] =
    val indent  = text.takeWhile(_ == ' ').length
    val trimmed = text.drop(indent)
    val digits  = trimmed.takeWhile(_.isDigit)
    if indent > 3 || digits.isEmpty || digits.length > 9 || trimmed.length <= digits.length then Absent
    else
      val delimiter = trimmed.charAt(digits.length)
      val width     = digits.length + 1
      if (delimiter != '.' && delimiter != ')') then Absent
      else if trimmed.length > width && trimmed.charAt(width) != ' ' then Absent
      else
        val (columns = columns, empty = empty) = markerColumns(text, indent, width)
        Present((number = digits.toInt, delimiter = delimiter, columns = columns, empty = empty))

  /** A bullet marker: `-`, `*` or `+`, then a space or the end of the line. */
  private def unorderedItem(text: String): Maybe[BulletMarker] =
    val indent  = text.takeWhile(_ == ' ').length
    val trimmed = text.drop(indent)
    if indent > 3 || trimmed.isEmpty then Absent
    else
      val bullet = trimmed.charAt(0)
      if bullet != '-' && bullet != '*' && bullet != '+' then Absent
      else if trimmed.length > 1 && trimmed.charAt(1) != ' ' then Absent
      else
        val (columns = columns, empty = empty) = markerColumns(text, indent, 1)
        Present((bullet = bullet, columns = columns, empty = empty))

  // The one-to-six bound lives in HeadingLevel.fromInt rather than in a guard here, so a run of seven
  // or more hashes falls through to the paragraph branch exactly as CommonMark requires.
  private def headingPrefix(text: String): Maybe[(HeadingLevel, String)] =
    val hashes = text.takeWhile(_ == '#')
    if hashes.isEmpty then Absent
    // A heading may say nothing: `#` on its own is an empty `h1`, and so is `## ` with only whitespace after it.
    else if text.length == hashes.length then HeadingLevel.fromInt(hashes.length).map(level => (level, ""))
    // A tab separates the hashes from the text as well as a space does. It is not indentation here -- the heading's
    // content is trimmed either way -- so it is taken as a separator rather than measured in columns.
    else if text.charAt(hashes.length) == ' ' || text.charAt(hashes.length) == '\t' then
      HeadingLevel.fromInt(hashes.length).map(level => (level, headingContent(text.drop(hashes.length + 1))))
    else Absent

  /**
   * A heading's text, with the closing run of hashes taken off.
   *
   * The closing run is optional and says nothing: `### foo ###` is the same heading as `### foo`. It counts as closing
   * only when whitespace precedes it or it is the whole of the content, so `# foo#` keeps its hash -- and whitespace
   * may follow it, which is what `### foo ###   ` relies on.
   */
  private def headingContent(text: String): String =
    val trimmed = text.trim
    val hashes  = trimmed.reverse.takeWhile(_ == '#').length
    if hashes == 0 then trimmed
    else if hashes == trimmed.length then ""
    else
      val before = trimmed.charAt(trimmed.length - hashes - 1)
      if before == ' ' || before == '\t' then trimmed.dropRight(hashes).trim else trimmed

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
