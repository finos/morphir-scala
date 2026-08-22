package morphir.langkit.markdown.internal

import kyo.*
import scala.annotation.tailrec
import morphir.langkit.core.Span
import morphir.langkit.markdown.*

/**
 * Lowers a CST into the AST: `lower: MdCstNode.Document => MdNode.Root`, total.
 *
 * The CST records what was written; the AST is what it means. Lowering is where the distance between them is walked:
 * container marker bytes disappear (they are [[MdCstNode.Token]] leaves, and tokens carry no content), escapes and
 * entities resolve to the characters they stand for, reference labels resolve against the definitions the document
 * declares, destinations normalise as URIs, and an image's inline label flattens to the text an `alt` attribute can
 * hold. Total because every CST has a meaning: a region no slice has structured is prose, and a reference the
 * definitions cannot answer lowers to an empty destination rather than to a failure.
 *
 * Inline text segmentation is not preserved node for node — adjacent literal runs may lower to differently split
 * [[MdNode.Text]] chunks than the direct parse produces — but the rendered document is the same, which the conformance
 * suite measures over the whole CommonMark corpus.
 */
private[markdown] object Lower:

  def lower(document: MdCstNode.Document)(using MdProfile): MdNode.Root =
    val definitions = collectDefinitions(document)
    // Frontmatter is a field on the root rather than a block, and it is only ever the first child — the parser
    // recognizes it at offset zero or not at all — so the head is the only place worth looking.
    val frontmatter = document.children.headOption.collect { case front: MdCstNode.Frontmatter => front }
    val children    = if frontmatter.isDefined then document.children.drop(1) else document.children
    MdNode.Root(
      blocks(children, definitions, document.span.end),
      Maybe.fromOption(frontmatter.map(loweredFrontMatter)),
      meta = MdMeta.at(document.span)
    )

  /**
   * The frontmatter node one CST block means: its raw value, undecoded.
   *
   * Only the interior is [[MdCstNode.Text]] — both delimiter lines are tokens — so the text leaf, when there is one, is
   * exactly the value region. A block whose value region is empty has no text leaf at all and means the empty document,
   * not a missing one. Which kind the delimiters spelled is not read back off the source: `---` is YAML by
   * construction, and a second kind arrives as its own case here alongside its own delimiter.
   */
  private def loweredFrontMatter(front: MdCstNode.Frontmatter): MdNode.FrontMatter =
    val raw = front.children.collectFirst { case MdCstNode.Text(text, _) => text }.getOrElse("")
    MdNode.FrontMatter.Yaml(YamlDocText(raw), MdMeta.at(front.span))

  /** Every definition in the tree, first spelling of a label winning, in document order. */
  private def collectDefinitions(root: MdCstNode): Map[String, LinkDefinition] =
    val definitions                 = scala.collection.mutable.Map.empty[String, LinkDefinition]
    def walk(node: MdCstNode): Unit = node match
      case MdCstNode.LinkReferenceDefinition(children, _) =>
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
  private def contentText(children: Chunk[MdCstNode]): String =
    val out                         = new StringBuilder
    def walk(node: MdCstNode): Unit = node match
      case MdCstNode.Token(_, _)               => ()
      case MdCstNode.Text(text, _)             => out.append(text)
      case MdCstNode.Verbatim(text, _)         => out.append(text)
      case MdCstNode.PhantomIndent(columns, _) => out.append(" " * columns)
      case _                                   => node.childNodes.foreach(walk)
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
  private def codeText(children: Chunk[MdCstNode], startColumn: Int = 0, startPhantom: Int = 0): String =
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
    def walk(node: MdCstNode): Unit = node match
      case MdCstNode.Token(text, _) => text.foreach(advance)
      // The phantom columns sit inside the width the marker token already advanced past, so they add text, not width.
      case MdCstNode.PhantomIndent(columns, _) => out.append(" " * columns)
      case MdCstNode.Text(text, _)             => walkText(text)
      case MdCstNode.Verbatim(text, _)         => walkText(text)
      case _                                   => node.childNodes.foreach(walk)
    children.foreach(walk)
    out.toString

  private def blocks(
      children: Chunk[MdCstNode],
      definitions: Map[String, LinkDefinition],
      docEnd: Int
  ): Chunk[MdNode.FlowContent] =
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
    val out = Chunk.newBuilder[MdNode.FlowContent]
    children.foreach { node =>
      node match
        case MdCstNode.Token(text, _)                    => advanceGap(text)
        case MdCstNode.Verbatim(text, _)                 => advanceGap(text)
        case MdCstNode.PhantomIndent(columns, _)         => phantom += columns
        case MdCstNode.ThematicBreak(_, span)            => out.addOne(MdNode.ThematicBreak(MdMeta.at(span)))
        case MdCstNode.AtxHeading(level, children, span) =>
          out.addOne(MdNode.Heading(level, inlines(children, definitions), MdMeta.at(span)))
        case MdCstNode.SetextHeading(level, children, span) =>
          out.addOne(MdNode.Heading(level, inlines(children, definitions), MdMeta.at(span)))
        case MdCstNode.Paragraph(children, span) =>
          out.addOne(MdNode.Paragraph(inlines(children, definitions), MdMeta.at(span)))
        case MdCstNode.FencedCode(children, span) =>
          out.addOne(loweredFence(children, span, docEnd))
        case MdCstNode.IndentedCode(children, span) =>
          out.addOne(MdNode.Code(
            FenceInfo.empty,
            indentedContent(codeText(children, column, phantom)),
            MdMeta.at(span)
          ))
        case MdCstNode.HtmlBlock(children, span) =>
          out.addOne(MdNode.Html(contentText(children), MdMeta.at(span)))
        case MdCstNode.BlockQuote(children, span) =>
          out.addOne(MdNode.Blockquote(blocks(children, definitions, docEnd), MdMeta.at(span)))
        case MdCstNode.BulletList(_, tight, children, span) =>
          out.addOne(MdNode.List(
            ordered = false,
            start = Absent,
            spread = !tight,
            items(children, definitions, docEnd),
            MdMeta.at(span)
          ))
        case MdCstNode.OrderedList(start, _, tight, children, span) =>
          out.addOne(MdNode.List(
            ordered = true,
            start = Present(start),
            spread = !tight,
            items(children, definitions, docEnd),
            MdMeta.at(span)
          ))
        case MdCstNode.Frontmatter(_, _) =>
          // Already lifted onto the root by `lower`; a frontmatter block is metadata, not flow content.
          ()
        case _ =>
          // Link reference definitions contribute no block.
          ()
      node match
        case _: (MdCstNode.Token | MdCstNode.Verbatim | MdCstNode.PhantomIndent) => ()
        case _                                                                   =>
          // A block consumed the rest of its line; the next gap starts a fresh one.
          column = 0
          phantom = 0
    }
    out.result()

  private def items(
      children: Chunk[MdCstNode],
      definitions: Map[String, LinkDefinition],
      docEnd: Int
  ): Chunk[MdNode.ListItem] =
    children.collect { case MdCstNode.ListItem(itemChildren, span) =>
      MdNode.ListItem(blocks(itemChildren, definitions, docEnd), MdMeta.at(span))
    }

  /** Fence metadata from the opening token's info string; content from the text leaves, indentation removed. */
  private def loweredFence(children: Chunk[MdCstNode], span: Span, docEnd: Int): MdNode.Code =
    val opening     = children.collectFirst { case MdCstNode.Token(text, _) => text }.getOrElse("")
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
      case MdCstNode.Token(text, _) =>
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
    MdNode.Code(info, content, MdMeta.at(span))

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
      children: Chunk[MdCstNode],
      definitions: Map[String, LinkDefinition]
  ): Chunk[MdNode.PhrasingContent] =
    Chunk.from(trimVerbatimEnds(children).flatMap(loweredInline(_, definitions)))

  private def trimVerbatimEnds(children: Chunk[MdCstNode]): Chunk[MdCstNode] =
    // Tokens at the edges — an ATX marker, a setext underline — carry no content and sit outside the prose, so the
    // trim looks past them for the outermost verbatim pieces.
    var items    = children.toVector
    var trimming = true
    while trimming do
      trimming = false
      val head = items.indexWhere(!_.isInstanceOf[MdCstNode.Token])
      if head >= 0 then
        items(head) match
          case MdCstNode.Verbatim(text, span) if text.stripLeading != text =>
            val trimmed = text.stripLeading
            if trimmed.isEmpty then
              items = items.patch(head, Nil, 1)
              trimming = true
            else items = items.updated(head, MdCstNode.Verbatim(trimmed, span))
          case _ => ()
    trimming = true
    while trimming do
      trimming = false
      val last = items.lastIndexWhere(!_.isInstanceOf[MdCstNode.Token])
      if last >= 0 then
        items(last) match
          case MdCstNode.Verbatim(text, span) if text.stripTrailing != text =>
            val trimmed = text.stripTrailing
            if trimmed.isEmpty then
              items = items.patch(last, Nil, 1)
              trimming = true
            else items = items.updated(last, MdCstNode.Verbatim(trimmed, span))
          case _ => ()
    Chunk.from(items)

  private def loweredInline(
      node: MdCstNode,
      definitions: Map[String, LinkDefinition]
  ): Chunk[MdNode.PhrasingContent] =
    node match
      case MdCstNode.Token(_, _) =>
        Chunk.empty

      case MdCstNode.Verbatim(text, span) =>
        val value = proseValue(text)
        if value.isEmpty then Chunk.empty else Chunk(MdNode.Text(value, MdMeta.at(span)))

      case MdCstNode.Escape(children, span) =>
        Chunk(MdNode.Text(children.collect { case MdCstNode.Text(text, _) => text }.mkString, MdMeta.at(span)))

      case MdCstNode.Entity(children, span) =>
        val raw = children.collect { case MdCstNode.Token(text, _) => text }.mkString
        Chunk(MdNode.Text(InlineParser.resolveEscapes(raw), MdMeta.at(span)))

      case MdCstNode.HardBreak(_, span) =>
        Chunk(MdNode.Break(MdMeta.at(span)))

      case MdCstNode.CodeSpan(children, span) =>
        Chunk(MdNode.InlineCode(InlineParser.codeSpanValueOf(contentText(children)), MdMeta.at(span)))

      case MdCstNode.Autolink(children, span) =>
        val inner       = contentText(children)
        val destination = InlineParser.autolinkDestinationOf(inner).getOrElse(InlineParser.normalizeUriOf(inner))
        Chunk(MdNode.Link(destination, Absent, Chunk(MdNode.Text(inner, MdMeta.at(span))), MdMeta.at(span)))

      case MdCstNode.RawHtml(children, span) =>
        Chunk(MdNode.InlineHtml(contentText(children), MdMeta.at(span)))

      case MdCstNode.Emphasis(_, strong, children, span) =>
        // Leftover delimiters of a partially consumed run sit verbatim outside the tokens; they are prose siblings,
        // not emphasis content, so they lower before and after the emphasis node itself.
        val firstToken = children.indexWhere(_.isInstanceOf[MdCstNode.Token])
        val lastToken  = children.lastIndexWhere(_.isInstanceOf[MdCstNode.Token])
        if firstToken < 0 || lastToken <= firstToken then Chunk.empty
        else
          val before                       = children.take(firstToken).flatMap(loweredInline(_, definitions))
          val interior                     = inlines(children.slice(firstToken + 1, lastToken), definitions)
          val after                        = children.drop(lastToken + 1).flatMap(loweredInline(_, definitions))
          val node: MdNode.PhrasingContent =
            if strong then MdNode.Strong(interior, MdMeta.at(span)) else MdNode.Emphasis(interior, MdMeta.at(span))
          Chunk.from(before) ++ Chunk(node) ++ Chunk.from(after)

      case MdCstNode.Link(form, destination, title, reference, children, span) =>
        val content             = inlines(linkContent(children), definitions)
        val (uri, loweredTitle) = resolveTarget(form, destination, title, reference, definitions)
        Chunk(MdNode.Link(uri, loweredTitle, content, MdMeta.at(span)))

      case MdCstNode.Image(form, destination, title, reference, children, span) =>
        val content             = inlines(linkContent(children), definitions)
        val (uri, loweredTitle) = resolveTarget(form, destination, title, reference, definitions)
        Chunk(MdNode.Image(uri, loweredTitle, InlineParser.altTextOf(content), MdMeta.at(span)))

      case other =>
        // A block node cannot appear in an inline region; hold its text rather than lose it.
        val text = contentText(Chunk(other))
        if text.isEmpty then Chunk.empty else Chunk(MdNode.Text(text, MdMeta.at(other.span)))

  /** The children between the opening bracket token and the token that closes the link text. */
  private def linkContent(children: Chunk[MdCstNode]): Chunk[MdCstNode] =
    val afterOpen = children.dropWhile(!_.isInstanceOf[MdCstNode.Token]).drop(1)
    afterOpen.takeWhile {
      case MdCstNode.Token(text, _) => !text.startsWith("]")
      case _                        => true
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
