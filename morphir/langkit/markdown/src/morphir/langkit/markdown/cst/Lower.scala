package morphir.langkit.markdown.cst

import kyo.*
import scala.annotation.tailrec
import morphir.langkit.core.Span
import morphir.langkit.markdown.*
import morphir.langkit.markdown.internal.{InlineParser, LinkDefinition}

/**
 * Lowers a CST into the AST: `lower: MdcCstNode.Document => MdcNode.Root`, total.
 *
 * The CST records what was written; the AST is what it means. Lowering is where the distance between them is walked:
 * container marker bytes disappear (they are [[MdcCstNode.Token]] leaves, and tokens carry no content), escapes and
 * entities resolve to the characters they stand for, reference labels resolve against the definitions the document
 * declares, destinations normalise as URIs, and an image's inline label flattens to the text an `alt` attribute can
 * hold. Total because every CST has a meaning: a region no slice has structured is prose, and a reference the
 * definitions cannot answer lowers to an empty destination rather than to a failure.
 *
 * Inline text segmentation is not preserved node for node — adjacent literal runs may lower to differently split
 * [[MdcNode.Text]] chunks than the direct parse produces — but the rendered document is the same, which the conformance
 * suite measures over the whole CommonMark corpus.
 */
object Lower:

  def lower(document: MdcCstNode.Document): MdcNode.Root =
    val definitions = collectDefinitions(document)
    MdcNode.Root(blocks(document.children, definitions, document.span.end), MdcMeta.at(document.span))

  /** Every definition in the tree, first spelling of a label winning, in document order. */
  private def collectDefinitions(root: MdcCstNode): Map[String, LinkDefinition] =
    val definitions                  = scala.collection.mutable.Map.empty[String, LinkDefinition]
    def walk(node: MdcCstNode): Unit = node match
      case MdcCstNode.LinkReferenceDefinition(children, _) =>
        parseDefinition(contentText(children)).foreach { case (label, definition) =>
          val key = InlineParser.normalizeLabel(label)
          if !definitions.contains(key) then definitions(key) = definition
        }
      case _ => node.childNodes.foreach(walk)
    walk(root)
    definitions.toMap

  /** One `[label]: destination "title"` from a definition node's text, or nothing if it will not parse back. */
  private def parseDefinition(text: String): Maybe[(String, LinkDefinition)] =
    @tailrec def skipLeading(index: Int): Int =
      if index < text.length && text.charAt(index).isWhitespace then skipLeading(index + 1) else index
    val open = skipLeading(0)
    if open >= text.length || text.charAt(open) != '[' then Absent
    else
      InlineParser.referenceLabelEndOf(text, open + 1).flatMap { close =>
        if close + 1 >= text.length || text.charAt(close + 1) != ':' then Absent
        else
          InlineParser.definitionTarget(text, close + 2).map { target =>
            (text.substring(open + 1, close), LinkDefinition(target.destination, target.title))
          }
      }

  /** The non-token leaf text of `children`, concatenated: content with the container's marker bytes already gone. */
  private def contentText(children: Chunk[MdcCstNode]): String =
    val out                          = new StringBuilder
    def walk(node: MdcCstNode): Unit = node match
      case MdcCstNode.Token(_, _)               => ()
      case MdcCstNode.Text(text, _)             => out.append(text)
      case MdcCstNode.Verbatim(text, _)         => out.append(text)
      case MdcCstNode.PhantomIndent(columns, _) => out.append(" " * columns)
      case _                                    => node.childNodes.foreach(walk)
    children.foreach(walk)
    out.toString.replace("\r\n", "\n")

  /**
   * Code content from a code block's leaves, reconstructed in columns.
   *
   * Marker tokens contribute no text but advance the column — a tab in a marker reaches its stop — so a phantom indent
   * lands at the column the marker's claim ended and the content's own leading tabs expand from the column they
   * actually occupy. Leading whitespace becomes spaces, the way the parser expands structural tabs; content past the
   * first non-blank character is kept as written.
   */
  private def codeText(children: Chunk[MdcCstNode], startColumn: Int = 0, startPhantom: Int = 0): String =
    val out     = new StringBuilder
    var column  = startColumn
    var leading = true
    out.append(" " * startPhantom)
    def advance(char: Char): Unit =
      if char == '\t' then column = ((column / 4) + 1) * 4 else column += 1
    def walkText(raw: String): Unit =
      raw.replace("\r\n", "\n").foreach { char =>
        if char == '\n' then
          out.append('\n')
          column = 0
          leading = true
        else if leading && char == '\t' then
          val stop = ((column / 4) + 1) * 4
          while column < stop do
            out.append(' ')
            column += 1
        else
          if char != ' ' then leading = false
          out.append(char)
          column += 1
      }
    def walk(node: MdcCstNode): Unit = node match
      case MdcCstNode.Token(text, _) => text.foreach(advance)
      // The phantom columns sit inside the width the marker token already advanced past, so they add text, not width.
      case MdcCstNode.PhantomIndent(columns, _) => out.append(" " * columns)
      case MdcCstNode.Text(text, _)             => walkText(text)
      case MdcCstNode.Verbatim(text, _)         => walkText(text)
      case _                                    => node.childNodes.foreach(walk)
    children.foreach(walk)
    out.toString

  private def blocks(
      children: Chunk[MdcCstNode],
      definitions: Map[String, LinkDefinition],
      docEnd: Int
  ): Chunk[MdcNode.FlowContent] =
    // A block's first line began after whatever markers its container spent on it, and those leaves sit in the gap
    // before the block, not inside it. The running line state — column reached, phantom columns owed — is what an
    // indented code block needs to reconstruct its first line's indentation.
    var column                         = 0
    var phantom                        = 0
    def advanceGap(text: String): Unit =
      text.foreach { char =>
        if char == '\n' then
          column = 0
          phantom = 0
        else if char == '\t' then column = ((column / 4) + 1) * 4
        else column += 1
      }
    val out = Chunk.newBuilder[MdcNode.FlowContent]
    children.foreach { node =>
      node match
        case MdcCstNode.Token(text, _)                    => advanceGap(text)
        case MdcCstNode.Verbatim(text, _)                 => advanceGap(text)
        case MdcCstNode.PhantomIndent(columns, _)         => phantom += columns
        case MdcCstNode.ThematicBreak(_, span)            => out.addOne(MdcNode.ThematicBreak(MdcMeta.at(span)))
        case MdcCstNode.AtxHeading(level, children, span) =>
          out.addOne(MdcNode.Heading(level, inlines(children, definitions), MdcMeta.at(span)))
        case MdcCstNode.SetextHeading(level, children, span) =>
          out.addOne(MdcNode.Heading(level, inlines(children, definitions), MdcMeta.at(span)))
        case MdcCstNode.Paragraph(children, span) =>
          out.addOne(MdcNode.Paragraph(inlines(children, definitions), MdcMeta.at(span)))
        case MdcCstNode.FencedCode(children, span) =>
          out.addOne(loweredFence(children, span, docEnd))
        case MdcCstNode.IndentedCode(children, span) =>
          out.addOne(MdcNode.Code(
            FenceInfo.empty,
            indentedContent(codeText(children, column, phantom)),
            MdcMeta.at(span)
          ))
        case MdcCstNode.HtmlBlock(children, span) =>
          out.addOne(MdcNode.Html(contentText(children), MdcMeta.at(span)))
        case MdcCstNode.BlockQuote(children, span) =>
          out.addOne(MdcNode.Blockquote(blocks(children, definitions, docEnd), MdcMeta.at(span)))
        case MdcCstNode.BulletList(_, tight, children, span) =>
          out.addOne(MdcNode.List(
            ordered = false,
            start = Absent,
            spread = !tight,
            items(children, definitions, docEnd),
            MdcMeta.at(span)
          ))
        case MdcCstNode.OrderedList(start, _, tight, children, span) =>
          out.addOne(MdcNode.List(
            ordered = true,
            start = Present(start),
            spread = !tight,
            items(children, definitions, docEnd),
            MdcMeta.at(span)
          ))
        case _ =>
          // Link reference definitions contribute no block.
          ()
      node match
        case _: (MdcCstNode.Token | MdcCstNode.Verbatim | MdcCstNode.PhantomIndent) => ()
        case _                                                                      =>
          // A block consumed the rest of its line; the next gap starts a fresh one.
          column = 0
          phantom = 0
    }
    out.result()

  private def items(
      children: Chunk[MdcCstNode],
      definitions: Map[String, LinkDefinition],
      docEnd: Int
  ): Chunk[MdcNode.ListItem] =
    children.collect { case MdcCstNode.ListItem(itemChildren, span) =>
      MdcNode.ListItem(blocks(itemChildren, definitions, docEnd), MdcMeta.at(span))
    }

  /** Fence metadata from the opening token's info string; content from the text leaves, indentation removed. */
  private def loweredFence(children: Chunk[MdcCstNode], span: Span, docEnd: Int): MdcNode.Code =
    val opening     = children.collectFirst { case MdcCstNode.Token(text, _) => text }.getOrElse("")
    val indentation = opening.takeWhile(_ == ' ').length
    val afterIndent = opening.drop(indentation)
    val marker      = afterIndent.headOption.getOrElse('`')
    val run         = afterIndent.takeWhile(_ == marker).length
    val rawInfo     = afterIndent.drop(run).replace("\r", "")
    val info        = FenceInfo.parse(trimSpacesOrTabs(rawInfo))

    // Only the fence body is a Text leaf — the fences and any container markers are tokens — so the non-token text
    // of the whole node is exactly the raw content region.
    val raw = codeText(children)
    // Closed when the last child is the closing fence: a token of nothing but the fence character and blanks. A
    // container marker token can end an unterminated fence's children, but a marker never spells a fence.
    val closed = children.lastOption.exists {
      case MdcCstNode.Token(text, _) =>
        text.exists(_ == marker) && text.forall(char => char == marker || char == ' ' || char == '\t')
      case _ => false
    }
    val body = if raw.startsWith("\n") then raw.substring(1) else raw
    // A fence cut short by its container ending lost the line ending that closed its last line — the container's
    // span could not carry it — so an unterminated fence whose body does not end in one gets it back. A fence that
    // ran to the end of the input lost nothing: the document simply has no final line ending.
    val restored =
      if !closed && body.nonEmpty && !body.endsWith("\n") && span.end < docEnd then body + "\n" else body
    val content =
      if restored.isEmpty then restored
      else
        val trailing = restored.endsWith("\n")
        val lines    = (if trailing then restored.dropRight(1) else restored).split("\n", -1)
        lines.map(removeIndentation(_, indentation)).mkString("", "\n", if trailing then "\n" else "")
    MdcNode.Code(info, content, MdcMeta.at(span))

  /** Remove up to `indentation` leading spaces, which the opening fence spent rather than the content. */
  private def removeIndentation(line: String, indentation: Int): String =
    line.drop(math.min(indentation, line.takeWhile(_ == ' ').length))

  /** Indented-code content: each line loses the four columns the block form spent, structural tabs expanded first. */
  private def indentedContent(raw: String): String =
    val body  = if raw.endsWith("\n") then raw.dropRight(1) else raw
    val lines = body.split("\n", -1)
    lines.map(stripIndentColumns).mkString("", "\n", "\n")

  /** Up to four leading columns removed, a leading tab counting as the columns to its stop. */
  private def stripIndentColumns(line: String): String =
    var index  = 0
    var column = 0
    while column < 4 && index < line.length && (line.charAt(index) == ' ' || line.charAt(index) == '\t') do
      if line.charAt(index) == '\t' then column = ((column / 4) + 1) * 4
      else column += 1
      index += 1
    line.substring(index)

  private def trimSpacesOrTabs(text: String): String =
    val start = text.indexWhere(char => char != ' ' && char != '\t')
    if start == -1 then ""
    else
      val end = text.lastIndexWhere(char => char != ' ' && char != '\t')
      text.substring(start, end + 1)

  // --- inline lowering ---------------------------------------------------------------------------------------------

  /**
   * The inline content of a prose interior: its typed constructs lowered, its verbatim gaps as text.
   *
   * The trim happens on the raw verbatim edges before lowering, the way block parsing trims raw prose before the inline
   * grammar sees it: an entity or escape at the edge resolved to whitespace is content and survives, where raw edge
   * whitespace is layout and does not.
   */
  private def inlines(
      children: Chunk[MdcCstNode],
      definitions: Map[String, LinkDefinition]
  ): Chunk[MdcNode.PhrasingContent] =
    Chunk.from(trimVerbatimEnds(children).flatMap(loweredInline(_, definitions)))

  private def trimVerbatimEnds(children: Chunk[MdcCstNode]): Chunk[MdcCstNode] =
    // Tokens at the edges — an ATX marker, a setext underline — carry no content and sit outside the prose, so the
    // trim looks past them for the outermost verbatim pieces.
    var items    = children.toVector
    var trimming = true
    while trimming do
      trimming = false
      val head = items.indexWhere(!_.isInstanceOf[MdcCstNode.Token])
      if head >= 0 then
        items(head) match
          case MdcCstNode.Verbatim(text, span) if text.stripLeading != text =>
            val trimmed = text.stripLeading
            if trimmed.isEmpty then
              items = items.patch(head, Nil, 1)
              trimming = true
            else items = items.updated(head, MdcCstNode.Verbatim(trimmed, span))
          case _ => ()
    trimming = true
    while trimming do
      trimming = false
      val last = items.lastIndexWhere(!_.isInstanceOf[MdcCstNode.Token])
      if last >= 0 then
        items(last) match
          case MdcCstNode.Verbatim(text, span) if text.stripTrailing != text =>
            val trimmed = text.stripTrailing
            if trimmed.isEmpty then
              items = items.patch(last, Nil, 1)
              trimming = true
            else items = items.updated(last, MdcCstNode.Verbatim(trimmed, span))
          case _ => ()
    Chunk.from(items)

  private def loweredInline(
      node: MdcCstNode,
      definitions: Map[String, LinkDefinition]
  ): Chunk[MdcNode.PhrasingContent] =
    node match
      case MdcCstNode.Token(_, _) =>
        Chunk.empty

      case MdcCstNode.Verbatim(text, span) =>
        val value = proseValue(text)
        if value.isEmpty then Chunk.empty else Chunk(MdcNode.Text(value, MdcMeta.at(span)))

      case MdcCstNode.Escape(children, span) =>
        Chunk(MdcNode.Text(children.collect { case MdcCstNode.Text(text, _) => text }.mkString, MdcMeta.at(span)))

      case MdcCstNode.Entity(children, span) =>
        val raw = children.collect { case MdcCstNode.Token(text, _) => text }.mkString
        Chunk(MdcNode.Text(InlineParser.resolveEscapes(raw), MdcMeta.at(span)))

      case MdcCstNode.HardBreak(_, span) =>
        Chunk(MdcNode.Break(MdcMeta.at(span)))

      case MdcCstNode.CodeSpan(children, span) =>
        Chunk(MdcNode.InlineCode(InlineParser.codeSpanValueOf(contentText(children)), MdcMeta.at(span)))

      case MdcCstNode.Autolink(children, span) =>
        val inner       = contentText(children)
        val destination = InlineParser.autolinkDestinationOf(inner).getOrElse(InlineParser.normalizeUriOf(inner))
        Chunk(MdcNode.Link(destination, Absent, Chunk(MdcNode.Text(inner, MdcMeta.at(span))), MdcMeta.at(span)))

      case MdcCstNode.RawHtml(children, span) =>
        Chunk(MdcNode.InlineHtml(contentText(children), MdcMeta.at(span)))

      case MdcCstNode.Emphasis(_, strong, children, span) =>
        // Leftover delimiters of a partially consumed run sit verbatim outside the tokens; they are prose siblings,
        // not emphasis content, so they lower before and after the emphasis node itself.
        val firstToken = children.indexWhere(_.isInstanceOf[MdcCstNode.Token])
        val lastToken  = children.lastIndexWhere(_.isInstanceOf[MdcCstNode.Token])
        if firstToken < 0 || lastToken <= firstToken then Chunk.empty
        else
          val before                        = children.take(firstToken).flatMap(loweredInline(_, definitions))
          val interior                      = inlines(children.slice(firstToken + 1, lastToken), definitions)
          val after                         = children.drop(lastToken + 1).flatMap(loweredInline(_, definitions))
          val node: MdcNode.PhrasingContent =
            if strong then MdcNode.Strong(interior, MdcMeta.at(span)) else MdcNode.Emphasis(interior, MdcMeta.at(span))
          Chunk.from(before) ++ Chunk(node) ++ Chunk.from(after)

      case MdcCstNode.Link(form, destination, title, reference, children, span) =>
        val content             = inlines(linkContent(children), definitions)
        val (uri, loweredTitle) = resolveTarget(form, destination, title, reference, definitions)
        Chunk(MdcNode.Link(uri, loweredTitle, content, MdcMeta.at(span)))

      case MdcCstNode.Image(form, destination, title, reference, children, span) =>
        val content             = inlines(linkContent(children), definitions)
        val (uri, loweredTitle) = resolveTarget(form, destination, title, reference, definitions)
        Chunk(MdcNode.Image(uri, loweredTitle, InlineParser.altTextOf(content), MdcMeta.at(span)))

      case other =>
        // A block node cannot appear in an inline region; hold its text rather than lose it.
        val text = contentText(Chunk(other))
        if text.isEmpty then Chunk.empty else Chunk(MdcNode.Text(text, MdcMeta.at(other.span)))

  /** The children between the opening bracket token and the token that closes the link text. */
  private def linkContent(children: Chunk[MdcCstNode]): Chunk[MdcCstNode] =
    val afterOpen = children.dropWhile(!_.isInstanceOf[MdcCstNode.Token]).drop(1)
    afterOpen.takeWhile {
      case MdcCstNode.Token(text, _) => !text.startsWith("]")
      case _                         => true
    }

  /** The destination and title a link resolves to, from its own spelling or through its reference label. */
  private def resolveTarget(
      form: LinkForm,
      destination: Maybe[String],
      title: Maybe[String],
      reference: Maybe[String],
      definitions: Map[String, LinkDefinition]
  ): (String, Maybe[String]) =
    form match
      case LinkForm.Inline =>
        val uri = InlineParser.normalizeUriOf(InlineParser.resolveEscapes(destination.getOrElse("")))
        (uri, title.map(t => InlineParser.resolveEscapes(proseValue(t))))
      case _ =>
        reference.map(label => definitions.get(InlineParser.normalizeLabel(label))) match
          case Present(Some(definition)) =>
            (InlineParser.normalizeUriOf(definition.destination), definition.title)
          case _ => ("", Absent)

  /**
   * Prose text as the AST carries it: no carriage returns, and each line ending shorn of the horizontal whitespace
   * around it — a soft break's single trailing space is not content, and a continuation line's indentation was spent on
   * being a continuation.
   */
  private def proseValue(raw: String): String =
    val text = raw.replace("\r\n", "\n")
    if !text.contains('\n') then text
    else
      val out   = new StringBuilder
      var index = 0
      while index < text.length do
        val char = text.charAt(index)
        if char == '\n' then
          while out.nonEmpty && (out.last == ' ' || out.last == '\t') do out.setLength(out.length - 1)
          out.append('\n')
          index += 1
          while index < text.length && (text.charAt(index) == ' ' || text.charAt(index) == '\t') do index += 1
        else
          out.append(char)
          index += 1
      out.toString
