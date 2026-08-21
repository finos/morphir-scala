package morphir.langkit.markdown

import kyo.*
import morphir.langkit.markdown.cst.CstParser
import morphir.langkit.markdown.cst.MdcCstNode

/**
 * Markdown text from an [[MdcNode]] tree, and the CST that text parses to.
 *
 * The contract is **structural fidelity**, not byte fidelity: `Parser.parse(write(tree))` succeeds and yields the tree
 * it was given, once positions are dropped and prose split by escapes is merged back. Which bytes carry that meaning is
 * the writer's business, and it spends them freely — the escaper over-escapes on purpose, because an unnecessary
 * backslash costs a byte while a missing one costs the tree.
 *
 * Spelling that [[MdcNode]] does not carry comes from an [[MdStyle]] in scope, overridden per node through the keys in
 * [[MdStyleKeys]]: the writer asks the node's own data first and falls back to the style. Nothing here consults the CST
 * — the AST is the input, and a tree the parser never saw writes exactly as well as one it did.
 *
 * One node has no faithful spelling and is documented rather than fixed: an [[MdcNode.InlineCode]] whose value is empty
 * writes as a single-space code span, because CommonMark cannot express a code span holding nothing. It is a node no
 * parse produces, and the alternative — an unbalanced backtick run, which reparses as prose — loses the code span
 * altogether. See [[writeInlineCode]].
 *
 * Blocks are written whole, each to its own string, and joined; container prefixes (`> `, list-item indentation) are
 * then applied line by line to what a container holds. Writing a prefix stack down through a streaming emitter would
 * save the intermediate strings and cost the clarity, and a Markdown document is not large enough for that trade.
 */
object MdWriter:

  /**
   * The document as Markdown text, ending in exactly one newline; the empty document writes nothing.
   *
   * @param root
   *   the tree to write
   */
  def write(root: MdcNode.Root)(using MdStyle): String =
    if root.children.isEmpty then ""
    else blocks(root.children, BlankSeparated) + "\n"

  /** The document's CST, obtained by writing it and parsing what was written. */
  def raise(root: MdcNode.Root)(using MdStyle): MdcCstNode.Document = CstParser.parse(write(root))

  /** Between blocks that may not touch: paragraphs, and everything at the top level or inside a quote. */
  private val BlankSeparated = "\n\n"

  /** Between blocks of a tight list item, which may not be separated by a blank line without loosening the list. */
  private val LineSeparated = "\n"

  /**
   * Punctuation a text node escapes wherever it appears.
   *
   * Every one of these opens or closes something somewhere, and the position that would make it harmless is not worth
   * proving: `<` is an autolink or a tag, `&` is a character reference, `#` at the head of a line is a heading, and a
   * lone `_` between two letters is nothing at all until the word moves. Escaping unconditionally keeps the escaper a
   * function of the character rather than of the surrounding grammar.
   */
  private val AlwaysEscaped: Set[Char] = Set('\\', '`', '*', '_', '[', ']', '<', '>', '&', '#', '!', '~', '|')

  /** Punctuation that means something only at the head of a line: two list markers and a setext underline. */
  private val LineLeadEscaped: Set[Char] = Set('-', '+', '=')

  /** A run of blocks, joined by whatever the run that holds them keeps between siblings. */
  private def blocks(nodes: Chunk[MdcNode.FlowContent], separator: String)(using MdStyle): String =
    nodes.map(block).mkString(separator)

  /** One block, with its own internal newlines and no trailing one. */
  private def block(node: MdcNode.FlowContent)(using style: MdStyle): String = node match
    case MdcNode.Paragraph(children, _)  => inlines(children, atLineStart = true)
    case heading: MdcNode.Heading        => writeHeading(heading)
    case code: MdcNode.Code              => writeCode(code)
    case MdcNode.Html(value, _)          => value
    case MdcNode.Blockquote(children, _) => prefixed(blocks(children, BlankSeparated), "> ", ">")
    case list: MdcNode.List              => writeList(list)
    // Three spellings mean one break; only what else they could be read as differs. A `-` bullet takes `***`, so a
    // break is never the `- - -` its own list could have written. Every other bullet takes `___`, which is neither a
    // list marker nor a setext underline. `---` is both, and the second is what rules it out: a tight list item
    // writes its blocks with no blank line between them, and a `---` under a paragraph there is that paragraph's
    // underline rather than a break — the break disappears and the paragraph becomes a heading.
    case MdcNode.ThematicBreak(_) => if style.bullet == '-' then "***" else "___"

  /**
   * A heading, ATX or setext.
   *
   * Setext is available only for depths one and two, and only for a heading that says something: an underline under
   * nothing is a paragraph of `===`. Everything else falls back to ATX, which every depth can spell.
   */
  private def writeHeading(heading: MdcNode.Heading)(using style: MdStyle): String =
    val body  = inlines(heading.children, atLineStart = true)
    val depth = heading.depth.toInt
    val kind  = heading.meta.get(MdStyleKeys.headingStyle).getOrElse(style.headingStyle)
    if kind == HeadingStyle.Setext && depth <= 2 && !body.isBlank then
      val underline = if depth == 1 then "===" else "---"
      s"$body\n$underline"
    else if body.isEmpty then "#" * depth
    else s"${"#" * depth} $body"

  /**
   * A fenced code block. The indented form is not written: a fence carries every body, including an empty info string.
   *
   * The fence outgrows its content — one character longer than the longest run of the fence character inside it — so a
   * body holding its own fence cannot close the block early. A backtick fence gives way to a tilde when the info string
   * holds a backtick, which CommonMark forbids outright.
   */
  private def writeCode(code: MdcNode.Code)(using style: MdStyle): String =
    val info      = code.info.raw
    val requested = code.meta.get(MdStyleKeys.fence).getOrElse(style.fence)
    val marker    = if requested == '`' && info.contains('`') then '~' else requested
    val fence     = marker.toString * math.max(3, longestRun(code.value, marker) + 1)
    val body      = if code.value.isEmpty || code.value.endsWith("\n") then code.value else code.value + "\n"
    s"$fence$info\n$body$fence"

  /**
   * A list, tight or spread.
   *
   * Tightness is the same fact at both levels: a tight list separates its items by a single newline *and* the blocks
   * within an item by one, because a blank line anywhere inside a list is what makes the parser call it loose.
   */
  private def writeList(list: MdcNode.List)(using style: MdStyle): String =
    val bullet    = list.meta.get(MdStyleKeys.bullet).getOrElse(style.bullet)
    val delimiter = list.meta.get(MdStyleKeys.orderedDelimiter).getOrElse(style.orderedDelimiter)
    val first     = list.start.getOrElse(1)
    val separator = if list.tight then LineSeparated else BlankSeparated
    list.children.zipWithIndex.map { case (item, index) =>
      val marker = if list.ordered then s"${first + index}$delimiter " else s"$bullet "
      val body   = blocks(item.children, separator)
      // An item that holds nothing spends no space after its marker: a trailing one would be whitespace no line needs.
      if body.isEmpty then marker.stripTrailing else marked(body, marker)
    }.mkString(separator)

  /** Every line of `text` behind `prefix`; a blank line takes `blankPrefix`, so no line ends in dead whitespace. */
  private def prefixed(text: String, prefix: String, blankPrefix: String): String =
    linesOf(text).map(line => if line.isEmpty then blankPrefix else prefix + line).mkString("\n")

  /** A list item's body: the marker on the first line, indentation of the marker's width on the rest. */
  private def marked(text: String, marker: String): String =
    val indent = " " * marker.length
    linesOf(text).zipWithIndex.map { case (line, index) =>
      if index == 0 then marker + line
      else if line.isEmpty then ""
      else indent + line
    }.mkString("\n")

  /** The lines of `text`, keeping empty ones at either end: `split` alone drops the trailing empties. */
  private def linesOf(text: String): Chunk[String] = Chunk.from(text.split("\n", -1).toIndexedSeq)

  /**
   * A run of phrasing content.
   *
   * The one piece of state is whether the next character lands at the head of a line, which the escaper needs and only
   * this fold knows: a hard break ends a line, and so does a soft break inside a text node.
   */
  private def inlines(nodes: Chunk[MdcNode.PhrasingContent], atLineStart: Boolean)(using MdStyle): String =
    val out       = new StringBuilder
    var lineStart = atLineStart
    nodes.foreach { node =>
      val piece = writeInline(node, lineStart)
      out.append(piece)
      if piece.nonEmpty then lineStart = piece.endsWith("\n")
    }
    out.toString

  private def writeInline(node: MdcNode.PhrasingContent, atLineStart: Boolean)(using style: MdStyle): String =
    node match
      case MdcNode.Text(value, _)       => escapeText(value, atLineStart)
      case MdcNode.InlineCode(value, _) => writeInlineCode(value)
      case MdcNode.InlineHtml(value, _) => value
      case link: MdcNode.Link   => s"[${inlines(link.children, atLineStart = false)}](${target(link.url, link.title)})"
      case image: MdcNode.Image =>
        s"![${escapeText(image.alt, atLineStart = false)}](${target(image.url, image.title)})"
      case emphasis: MdcNode.Emphasis =>
        val marker = emphasis.meta.get(MdStyleKeys.emphasisMarker).getOrElse(style.emphasisMarker)
        s"$marker${inlines(emphasis.children, atLineStart = false)}$marker"
      case strong: MdcNode.Strong =>
        val marker = strong.meta.get(MdStyleKeys.strongMarker).getOrElse(style.strongMarker).toString * 2
        s"$marker${inlines(strong.children, atLineStart = false)}$marker"
      case MdcNode.Break(_) =>
        style.hardBreak match
          case HardBreakStyle.Backslash => "\\\n"
          case HardBreakStyle.Spaces    => "  \n"

  /**
   * An inline code span.
   *
   * The backtick run outgrows the longest run inside the value, so the closer cannot land early. Padding spaces go on
   * when the value would otherwise touch the delimiter — a leading or trailing backtick — or when the value both begins
   * and ends with a space, which CommonMark strips off again on the way back in.
   *
   * An empty value is the one case with no faithful spelling, and it writes as a single-space span. CommonMark has no
   * way to say "a code span holding nothing": the padding rule removes a space from each end only when the interior
   * does *not* consist entirely of spaces, so `` ` ` `` is a span holding one space and `` `` `` is two backticks that
   * close nothing, which reparses as literal text. Between a span whose value is off by one space and prose that is no
   * longer code at all, the first keeps the node a code span, and the writer never emits an unbalanced run.
   */
  private def writeInlineCode(value: String): String =
    if value.isEmpty then "` `"
    else
      val ticks    = "`" * math.max(1, longestRun(value, '`') + 1)
      val touches  = value.startsWith("`") || value.endsWith("`")
      val stripped = value.startsWith(" ") && value.endsWith(" ") && !value.forall(_ == ' ')
      if touches || stripped then s"$ticks $value $ticks" else s"$ticks$value$ticks"

  /** A link or image target: the destination, and the title behind it when there is one. */
  private def target(url: String, title: Maybe[String]): String =
    val destination = writeDestination(url)
    title match
      case Present(value) => s"""$destination "${value.replace("\\", "\\\\").replace("\"", "\\\"")}""""
      case Absent         => destination

  /**
   * A link destination.
   *
   * The bare form cannot hold whitespace or an unbalanced parenthesis and cannot be empty, so anything that might goes
   * in angle brackets, where only `<`, `>` and the backslash itself need escaping.
   */
  private def writeDestination(url: String): String =
    val bracketed = url.isEmpty || url.exists(character => character.isWhitespace || Bracketing.contains(character))
    if bracketed then s"<${url.replace("\\", "\\\\").replace("<", "\\<").replace(">", "\\>")}>"
    else url.replace("\\", "\\\\")

  private val Bracketing: Set[Char] = Set('(', ')', '<', '>')

  /** The length of the longest run of `character` in `text`; zero when it does not appear. */
  private def longestRun(text: String, character: Char): Int =
    var longest = 0
    var current = 0
    text.foreach { at =>
      if at == character then
        current += 1
        if current > longest then longest = current
      else current = 0
    }
    longest

  /**
   * A text node's value, spelled so that a parse reads it back as itself.
   *
   * Two mechanisms, chosen by what the character needs. Punctuation takes a backslash, which is what CommonMark's
   * escape is for. Whitespace cannot: a space is not ASCII punctuation, so `\ ` is a backslash and a space rather than
   * an escaped one, and the space in question is exactly the space the parser strips — leading whitespace off every
   * line, trailing whitespace off a line ending, where two of them would have been a hard break instead. Those spaces
   * are written as `&#32;`, which is content the stripping never sees and which decodes back to the space it stands
   * for. Tabs go the same way, since a tab's meaning at the head of a line is measured in columns rather than bytes.
   *
   * `atLineStart` is the writer's position rather than the value's: a text node that follows a hard break begins a line
   * even though its own value does not start with a newline.
   */
  private[markdown] def escapeText(value: String, atLineStart: Boolean): String =
    val out       = new StringBuilder
    var lineStart = atLineStart
    var index     = 0
    while index < value.length do
      val character = value.charAt(index)
      if character == '\n' then
        out.append('\n')
        lineStart = true
        index += 1
      else if character == ' ' then
        val end = runEnd(value, index, ' ')
        // A space survives literally only in the middle of a line: at either end of one it is stripped, and two of
        // them at the end are a hard break. The end of the value counts as the end of a line, because whatever
        // follows may be nothing at all.
        val vulnerable = lineStart || end >= value.length || value.charAt(end) == '\n'
        out.append(if vulnerable then "&#32;" * (end - index) else " " * (end - index))
        lineStart = false
        index = end
      else if character == '\t' then
        out.append("&#9;")
        lineStart = false
        index += 1
      else if AlwaysEscaped.contains(character) || (lineStart && LineLeadEscaped.contains(character)) then
        out.append('\\').append(character)
        lineStart = false
        index += 1
      else if lineStart && character.isDigit then
        // A digit run at the head of a line is an ordered-list marker only when a delimiter closes it, so the escape
        // goes on the delimiter rather than on the digits: `1\.` rather than `\1.`, which is not an escape at all.
        val end = digitsEnd(value, index)
        out.append(value.substring(index, end))
        val delimited = end < value.length && (value.charAt(end) == '.' || value.charAt(end) == ')')
        if delimited then
          out.append('\\').append(value.charAt(end))
          index = end + 1
        else index = end
        lineStart = false
      else
        out.append(character)
        lineStart = false
        index += 1
    out.toString
  end escapeText

  private def runEnd(text: String, from: Int, character: Char): Int =
    var index = from
    while index < text.length && text.charAt(index) == character do index += 1
    index

  private def digitsEnd(text: String, from: Int): Int =
    var index = from
    while index < text.length && text.charAt(index).isDigit do index += 1
    index
end MdWriter
