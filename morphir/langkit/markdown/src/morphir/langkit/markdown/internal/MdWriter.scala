package morphir.langkit.markdown.internal

import kyo.*
import morphir.langkit.markdown.*

/**
 * Markdown text from an [[MdNode]] tree, and the CST that text parses to.
 *
 * The contract is **structural fidelity**, not byte fidelity: `Parser.parse(write(tree))` succeeds and yields the tree
 * it was given, once positions are dropped and prose split by escapes is merged back. Which bytes carry that meaning is
 * the writer's business, and it spends them freely — the escaper over-escapes on purpose, because an unnecessary
 * backslash costs a byte while a missing one costs the tree.
 *
 * Spelling that [[MdNode]] does not carry comes from an [[MdStyle]] in scope, overridden per node through the keys in
 * [[MdStyleKeys]]: the writer asks the node's own data first and falls back to the style. Nothing here consults the CST
 * — the AST is the input, and a tree the parser never saw writes exactly as well as one it did.
 *
 * Six shapes have no faithful spelling and are documented rather than fixed. Three more belong to the pipe table alone
 * and are described on [[writeTable]] rather than here.
 *
 * An [[MdNode.InlineCode]] whose value is empty writes as a single-space code span, because CommonMark cannot express a
 * code span holding nothing. It is a node no parse produces, and the alternative — an unbalanced backtick run, which
 * reparses as prose — loses the code span altogether. See [[writeInlineCode]].
 *
 * An ATX heading is exactly one physical line, so nothing inside one can carry a real line break. An [[MdNode.Break]]
 * (a hard break needs a second line to land on) and a soft break inside a [[MdNode.Text]] both write as `&#10;` instead
 * — content the ATX line's single-line rule does not see, which the parser resolves back to the same character but
 * never back to a break of its own. An embedded newline inside an [[MdNode.InlineCode]] or [[MdNode.InlineHtml]] value
 * has no such entity available to it — a character reference does not resolve inside a code span or raw HTML — so each
 * downgrades to a single space instead, the nearest either can spell one line at a time. Every one of these is again a
 * node no parse of an ATX heading produces (Setext is what a source spells for multi-line heading content), and the
 * alternative — writing the break literally — truncates the heading rather than merely losing one construct's own
 * meaning. See `oneLine` on [[inlines]] and [[escapeText]].
 *
 * A link or image destination is written as given: [[writeDestination]] adds angle brackets and escapes `\`, `<` and
 * `>` when the raw bytes demand them, but does not percent-encode anything. The parse pipeline this writer round-trips
 * against always yields a percent-encoded url, so a destination that came from a parsed tree already carries that form;
 * a hand-built destination holding a raw space or a raw `&`-entity is outside the reparse image this contract covers,
 * and an author supplying one should percent-encode it first.
 *
 * An [[MdNode.FrontMatter.Yaml]] value holding a line that reads exactly as its own closing delimiter (`---`) has no
 * faithful spelling either: [[write]] emits the value verbatim between the two delimiter lines it owns, and a reparse
 * closes the block at the first such line inside it rather than at the one the author meant, spilling the remainder
 * into a document body that was never there. A fenced code block escapes the same jeopardy by outgrowing the longest
 * run of its own fence character; a frontmatter value has no equivalent escape to grow, so this one is pinned by its
 * written text rather than round-tripped. See [[write]].
 *
 * Blocks are written whole, each to its own string, and joined; container prefixes (`> `, list-item indentation) are
 * then applied line by line to what a container holds. Writing a prefix stack down through a streaming emitter would
 * save the intermediate strings and cost the clarity, and a Markdown document is not large enough for that trade.
 */
private[markdown] object MdWriter:

  /**
   * The document as Markdown text, ending in exactly one newline; the empty document — no frontmatter, no children —
   * writes nothing.
   *
   * Frontmatter, when present, is written first: its delimiter line, its value, and the delimiter line again, each
   * ending in its own newline. A frontmatter-only document ends there, right after the closing delimiter line; one
   * followed by blocks gets the same blank line every other pair of blocks does, between the closing delimiter and the
   * first one.
   *
   * @param root
   *   the tree to write
   */
  def write(root: MdNode.Root)(using MdStyle): String =
    val body = if root.children.isEmpty then "" else blocks(root.children, BlankSeparated) + "\n"
    root.frontmatter match
      case Absent         => body
      case Present(front) =>
        val frontText = writeFrontMatter(front)
        if body.isEmpty then frontText else frontText + "\n" + body

  /**
   * The document's CST, obtained by writing it and parsing what was written.
   *
   * Read back under the YAML-frontmatter profile always, and one profile serves every tree because the profile is inert
   * on text that does not open with `---`. Nothing this writer spells opens that way by accident: a thematic break
   * writes as `***` or `___`, and text is escaped. So a document with no frontmatter raises to exactly the CST plain
   * CommonMark gives it, and a document with frontmatter raises to one that spells what was written.
   */
  def raise(root: MdNode.Root)(using MdStyle): MdCstNode.Document =
    CstParser.parse(write(root))(using MdProfile.commonmark.withYamlFrontmatter)

  /**
   * A frontmatter block: its kind's delimiter, its value, and the delimiter again, each ending its own line.
   *
   * The value gains a trailing newline when it lacks one, since a delimiter line always closes a line of its own — a
   * parse slices the value up to where the closing delimiter's own line begins, so a value that already ends in `\n` is
   * left alone. An empty value stays empty rather than becoming a single blank line, matching what an empty value
   * region parses back to.
   */
  private def writeFrontMatter(front: MdNode.FrontMatter): String = front match
    case MdNode.FrontMatter.Yaml(value, _) =>
      val delimiter = FrontMatterKind.Yaml.delimiter
      val raw       = value.unwrap
      val body      = if raw.isEmpty || raw.endsWith("\n") then raw else raw + "\n"
      s"$delimiter\n$body$delimiter\n"

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

  /**
   * A run of blocks, joined by whatever the run that holds them keeps between siblings.
   *
   * Two sibling [[MdNode.List]]s of the same `ordered`ness, one directly after the other with nothing but a block
   * separator between them, are how the AST represents what the source spelled as two lists rather than one loose one —
   * a bullet or a delimiter changed mid-run, which is the only thing that ends a list without ending the run of items.
   * The AST keeps no memory of which marker the source used, so on its own the writer would give both the same default
   * marker and a reparse would fuse them back into one. Tracking the immediately preceding list's marker here — reset
   * by any other block in between, since only *directly* adjacent same-kind lists are at risk of fusing — is what lets
   * [[writeList]] pick a different one when it must.
   */
  private def blocks(nodes: Chunk[MdNode.FlowContent], separator: String)(using MdStyle): String =
    val pieces                               = Chunk.newBuilder[String]
    var previousList: Maybe[(Boolean, Char)] = Absent
    nodes.foreach {
      case list: MdNode.List =>
        val avoid = previousList.flatMap { case (ordered, marker) =>
          if ordered == list.ordered then Present(marker) else Absent
        }
        val (written, marker) = writeList(list, avoid)
        pieces += written
        previousList = Present((list.ordered, marker))
      case other =>
        pieces += block(other)
        previousList = Absent
    }
    pieces.result().mkString(separator)

  /** One block, with its own internal newlines and no trailing one. */
  private def block(node: MdNode.FlowContent)(using style: MdStyle): String = node match
    case MdNode.Paragraph(children, _)  => inlines(children, atLineStart = true)
    case heading: MdNode.Heading        => writeHeading(heading)
    case code: MdNode.Code              => writeCode(code)
    case MdNode.Html(value, _)          => value
    case MdNode.Blockquote(children, _) => prefixed(blocks(children, BlankSeparated), "> ", ">")
    case list: MdNode.List              => writeList(list)._1
    case table: MdNode.Table            => writeTable(table)
    // Three spellings mean one break; only what else they could be read as differs. A `-` bullet takes `***`, so a
    // break is never the `- - -` its own list could have written. Every other bullet takes `___`, which is neither a
    // list marker nor a setext underline. `---` is both, and the second is what rules it out: a tight list item
    // writes its blocks with no blank line between them, and a `---` under a paragraph there is that paragraph's
    // underline rather than a break — the break disappears and the paragraph becomes a heading.
    case MdNode.ThematicBreak(_) => if style.bullet == '-' then "***" else "___"

  /**
   * A heading, ATX or setext.
   *
   * Setext is available only for depths one and two, and only for a heading that says something: an underline under
   * nothing is a paragraph of `===`. Everything else falls back to ATX, which every depth can spell.
   */
  private def writeHeading(heading: MdNode.Heading)(using style: MdStyle): String =
    val depth = heading.depth.toInt
    val kind  = heading.meta.get(MdStyleKeys.headingStyle).getOrElse(style.headingStyle)
    // Setext content spans one line per child line and tolerates an embedded soft break; only Setext's own
    // eligibility check (depth, blankness) needs it, so it is computed once and reused for the body itself.
    lazy val setextBody = inlines(heading.children, atLineStart = true)
    if kind == HeadingStyle.Setext && depth <= 2 && !setextBody.isBlank then
      val underline = if depth == 1 then "===" else "---"
      s"$setextBody\n$underline"
    else
      // ATX syntax is exactly one physical line: a heading whose content carries a soft break or a hard break —
      // reachable whenever Setext is unavailable, at depth three and up, or a two-line Setext candidate falls back
      // here for some other reason — cannot spell that break literally without splitting the line in two. `oneLine`
      // asks `inlines` for the nearest meaning instead: every line break, hard or soft, becomes `&#10;`, which reads
      // back as the same character without ending the ATX line early.
      val body = inlines(heading.children, atLineStart = true, oneLine = true)
      if body.isEmpty then "#" * depth else s"${"#" * depth} $body"

  /**
   * A fenced code block. The indented form is not written: a fence carries every body, including an empty info string.
   *
   * The fence outgrows its content — one character longer than the longest run of the fence character inside it — so a
   * body holding its own fence cannot close the block early. A backtick fence gives way to a tilde when the info string
   * holds a backtick, which CommonMark forbids outright.
   */
  private def writeCode(code: MdNode.Code)(using style: MdStyle): String =
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
  /**
   * @param avoid
   *   the marker an immediately preceding sibling list of the same `ordered`ness already used, when there is one — see
   *   [[blocks]]. Reusing it here would fuse the two back into one list on reparse, so this list takes the other marker
   *   in its two- (ordered) or three-way (bullet) choice instead, regardless of what the style in scope or a per-node
   *   override asked for.
   * @return
   *   the written list, and the bullet or delimiter character it settled on — what the next sibling list, if any, must
   *   avoid in turn
   */
  private def writeList(list: MdNode.List, avoid: Maybe[Char] = Absent)(using style: MdStyle): (String, Char) =
    val wantedBullet    = list.meta.get(MdStyleKeys.bullet).getOrElse(style.bullet)
    val wantedDelimiter = list.meta.get(MdStyleKeys.orderedDelimiter).getOrElse(style.orderedDelimiter)
    val bullet = if !list.ordered && avoid.contains(wantedBullet) then alternateBullet(wantedBullet) else wantedBullet
    val delimiter =
      if list.ordered && avoid.contains(wantedDelimiter) then alternateDelimiter(wantedDelimiter) else wantedDelimiter
    val first     = list.start.getOrElse(ListStart.One).toInt
    val separator = if list.tight then LineSeparated else BlankSeparated
    val written   = list.children.zipWithIndex.map { case (item, index) =>
      val marker = if list.ordered then s"${first + index}$delimiter " else s"$bullet "
      // The checkbox is content, not container prefix: it sits after the bullet on the item's own first line, the
      // same as any other text the item opens with, so continuation lines still indent by the bullet's width alone.
      val checkbox = item.checked match
        case Present(true)  => "[x] "
        case Present(false) => "[ ] "
        case Absent         => ""
      val body = checkbox + blocks(item.children, separator)
      // An item that holds nothing spends no space after its marker: a trailing one would be whitespace no line needs.
      if body.isEmpty then marker.stripTrailing else marked(body, marker)
    }.mkString(separator)
    (written, if list.ordered then delimiter else bullet)

  /**
   * A pipe table: the header row, a delimiter row built from the alignment, and the body rows.
   *
   * Every row is written with both outer pipes, whatever the source had, because they are the spelling that cannot be
   * misread. A cell's content goes through [[inlines]] with `oneLine` set, since a cell has nowhere to put a line
   * ending; a literal `|` inside one is escaped by [[escapeText]], which already treats the pipe as always-escaped.
   *
   * Two shapes have no faithful spelling here, both for the same reason -- a table's structure is decided by characters
   * that no escape reaches.
   *
   * A [[MdNode.InlineCode]] whose value holds a `|` writes as a code span with a bare pipe in it, which the row
   * splitter reads as a cell boundary. Escaping it instead would spell `\|`, which a code span reads literally (a
   * backslash escapes nothing inside one), so the value would come back with the backslash still in it. The pipe splits
   * the cell rather than corrupting the value, which is the smaller of the two losses and the one a reader can see.
   *
   * A table written as a non-first block of a *tight* list item is separated from the block before it by a single
   * newline rather than a blank line, so a paragraph directly above it becomes the table's header line -- or, when the
   * table comes first, the paragraph below it becomes another of its rows. Loose items and every other position write
   * the blank line that keeps the two apart.
   *
   * A table of no columns at all -- `align` empty, which no parse produces and only a hand-built tree can hold -- has
   * no spelling either: a row of no cells writes as `|  |`, whose delimiter row holds one empty cell and so is not a
   * delimiter row, and the whole thing reads back as a paragraph. GFM has no syntax for a table with no columns, so
   * there is nothing to write it as.
   */
  private def writeTable(table: MdNode.Table)(using style: MdStyle): String =
    val padded  = table.meta.get(MdStyleKeys.padTableColumns).getOrElse(style.padTableColumns)
    val minimum = math.max(1, table.meta.get(MdStyleKeys.tableDelimiterWidth).getOrElse(style.tableDelimiterWidth))
    val columns = table.align.size
    val written = (Chunk(table.header) ++ table.rows).map(row =>
      fittedCells(row.children.map(cell => inlines(cell.children, atLineStart = false, oneLine = true)), columns, "")
    )
    // The delimiter cell at its narrowest sets a floor on its column's width, so padding never shortens one below
    // what its own colons and minimum run of dashes need.
    val narrowest = table.align.map(delimiterCell(_, minimum, width = 0))
    val widths    =
      if !padded then Chunk.from(Seq.fill(columns)(0))
      else
        Chunk.from((0 until columns).map(column =>
          (narrowest(column).length +: written.map(_(column).length).toSeq).max
        ))
    val lines =
      Chunk(rowLine(written.head, widths)) ++
        Chunk(rowLine(table.align.zipWithIndex.map((a, c) => delimiterCell(a, minimum, widths(c))), widths)) ++
        written.drop(1).map(rowLine(_, widths))
    lines.mkString("\n")

  /** One row's cells between pipes, each padded out to its column's width. */
  private def rowLine(cells: Chunk[String], widths: Chunk[Int]): String =
    cells.zipWithIndex.map((cell, column) => cell.padTo(widths(column), ' ')).mkString("| ", " | ", " |")

  /** A delimiter cell: the colons the alignment asks for, and enough dashes to reach `width`. */
  private def delimiterCell(alignment: Maybe[ColumnAlignment], minimum: Int, width: Int): String =
    val (open, close) = alignment match
      case Absent                          => ("", "")
      case Present(ColumnAlignment.Left)   => (":", "")
      case Present(ColumnAlignment.Right)  => ("", ":")
      case Present(ColumnAlignment.Center) => (":", ":")
    s"$open${"-" * math.max(minimum, width - open.length - close.length)}$close"

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
   *
   * Every consecutive run of [[MdNode.Text]] siblings escapes as one string rather than one call per node. A parse
   * splits prose into a new node at every escape and every entity, so the digits of a line-leading ordered-list marker
   * can end one node while its delimiter opens the next — [[escapeText]]'s line-leading rules need to see both to
   * escape the delimiter, and a node boundary that fell between them by accident of the source's own escaping is not a
   * boundary this writer owes any respect to.
   *
   * @param oneLine
   *   when true, no line break in this run may reach the page literally — the run is headed for a context, such as an
   *   ATX heading, that is exactly one physical line. A soft or hard break spells as `&#10;` instead, which reads back
   *   as the same character without ending the line early; a code span or raw HTML's own embedded newline has no such
   *   escape available to it, so it downgrades to a space, the nearest either can spell one line at a time.
   */
  private def inlines(nodes: Chunk[MdNode.PhrasingContent], atLineStart: Boolean, oneLine: Boolean = false)(using
      MdStyle
  ): String =
    val out       = new StringBuilder
    var lineStart = atLineStart
    var index     = 0
    while index < nodes.length do
      nodes(index) match
        case MdNode.Text(_, _) =>
          val start = index
          while index < nodes.length && nodes(index).isInstanceOf[MdNode.Text] do index += 1
          val merged = nodes.slice(start, index).collect { case MdNode.Text(value, _) => value }.mkString
          val piece  = escapeText(merged, lineStart, oneLine)
          out.append(piece)
          if piece.nonEmpty then lineStart = piece.endsWith("\n")
        case node =>
          val piece = writeInline(node, lineStart, oneLine)
          out.append(piece)
          if piece.nonEmpty then lineStart = piece.endsWith("\n")
          index += 1
    out.toString

  private def writeInline(node: MdNode.PhrasingContent, atLineStart: Boolean, oneLine: Boolean)(using
      style: MdStyle
  ): String =
    node match
      case MdNode.Text(value, _)       => escapeText(value, atLineStart, oneLine)
      case MdNode.InlineCode(value, _) =>
        // A code span's own line endings are the parser's to turn into spaces on the way in (CommonMark strips line
        // endings from a code span's content), so doing the same here when this span cannot carry a line ending of
        // its own — a `oneLine` context — spells the nearest thing CommonMark itself would ever produce, at the cost
        // of the value's own embedded newline surviving as a space rather than as itself. A code span holding a raw
        // newline is a node no parse of a `oneLine` context produces, so this only fires for a hand-built tree.
        writeInlineCode(if oneLine then value.replace('\n', ' ') else value)
      case MdNode.InlineHtml(value, _) =>
        // Raw HTML is emitted verbatim everywhere else — escaping it would show the author their own markup — but a
        // `oneLine` context has nowhere to put a literal line ending at all, so it is downgraded to a space here, the
        // one substitution HTML's own whitespace handling treats as equivalent inside a tag.
        if oneLine then value.replace('\n', ' ') else value
      case link: MdNode.Link =>
        s"[${inlines(link.children, atLineStart = false, oneLine)}](${target(link.url, link.title)})"
      case image: MdNode.Image =>
        s"![${escapeText(image.alt, atLineStart = false, oneLine)}](${target(image.url, image.title)})"
      case emphasis: MdNode.Emphasis =>
        val marker = emphasis.meta.get(MdStyleKeys.emphasisMarker).getOrElse(style.emphasisMarker)
        // A sole child that is itself an Emphasis resolving to the same marker touches it on both sides: two
        // single-character delimiters run together into one two-character run, which a parse always reads as Strong
        // rather than as emphasis nested in emphasis — CommonMark prefers the longer match whenever both flanking
        // runs allow it, and a run of exactly two never has a leftover single character to spend on the outer level.
        val soleInnerEmphasis         = soleChildEmphasis(emphasis.children)
        val (outerMarker, childStyle) = resolveEmphasisClash(marker, soleInnerEmphasis)
        s"$outerMarker${inlines(emphasis.children, atLineStart = false, oneLine)(using childStyle)}$outerMarker"
      case strong: MdNode.Strong =>
        val marker = strong.meta.get(MdStyleKeys.strongMarker).getOrElse(style.strongMarker)
        // A sole child that is an Emphasis resolving to the same marker character touches it on the inside: the
        // outer's two-character run runs straight into the inner's one-character run, and a parse of the resulting
        // three-character run always prefers the longer match — reading emphasis nested in strong back as strong
        // nested in emphasis instead. Same clash, same resolution, as the Emphasis arm above.
        val soleInnerEmphasis         = soleChildEmphasis(strong.children)
        val (outerMarker, childStyle) = resolveEmphasisClash(marker, soleInnerEmphasis)
        val delimiter                 = outerMarker.toString * 2
        s"$delimiter${inlines(strong.children, atLineStart = false, oneLine)(using childStyle)}$delimiter"
      // Two tildes, always. Unlike emphasis, which `MdStyle` lets a caller spell with `*` or `_`, strikethrough has
      // exactly one spelling in GFM 0.29-gfm — so a style key here would have one legal value and no purpose.
      case strikethrough: MdNode.Delete =>
        s"~~${inlines(strikethrough.children, atLineStart = false, oneLine)}~~"
      case MdNode.Break(meta) =>
        if oneLine then "&#10;"
        else
          meta.get(MdStyleKeys.hardBreak).getOrElse(style.hardBreak) match
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
   *
   * A newline in the value is not always safe to write literally. The general rule is `lineStart`: whenever the line
   * about to be written is already known to be empty — whether because a previous character in this same value was a
   * raw newline, or because `atLineStart` said so on entry, as it does for text that follows a hard break — one more
   * bare newline would leave that line with nothing on it at all: a blank line, which is exactly what ends the block
   * this text belongs to, whether or not the author meant a break there. Every newline reached in that state spells as
   * `&#10;` instead, content the block scanner sees and the parser resolves back to the same character. `oneLine` asks
   * for the same treatment unconditionally, because its context — an ATX heading — has no second physical line to spend
   * even a single newline on.
   *
   * A run that GFM would read as an extended autolink — a bare `www.` host, a `http://`, `https://` or `ftp://` URL, an
   * email address — is broken the same way, by writing the one character that makes it a link as the reference for
   * itself: `www&#46;example.com`. A backslash cannot do this job, since `\w` is a backslash and a `w` rather than an
   * escape; a reference can, because it carries the character back through a parse and is a node of its own there, so
   * neither half of what it split reads as a destination. It runs whatever profile the text will be read under, for the
   * reason `~` and `|` sit in [[AlwaysEscaped]]: the writer spells one document, and it has to survive the widest
   * dialect that might read it.
   */
  private[markdown] def escapeText(value: String, atLineStart: Boolean, oneLine: Boolean = false): String =
    val out       = new StringBuilder
    var lineStart = atLineStart
    var index     = 0
    // Whether the character about to be written opens a text node of its own, as a reparse would see it: true at the
    // start of the value, and again after every character reference written below, because a reference is its own
    // node in a parse. It is a boundary an extended autolink may begin at, whatever precedes it in this value.
    var nodeStart = true
    // Where the character that has to be spelled as a reference to break a bare destination sits, or -1 for none.
    var breakAt = -1
    while index < value.length do
      val character = value.charAt(index)
      val fresh     = nodeStart
      nodeStart = false
      // A pending break is written before another is looked for, and the character that carries one is always past
      // the position the match began at, so the two never collide on one index.
      if breakAt < index then
        InlineParser.extendedAutolinkAt(value, index, fresh).foreach(matched => breakAt = matched.anchor)
      if index == breakAt then
        // The one character that makes the run a link -- the `.` of a `www.` host, a scheme's `:`, an address's `@`
        // -- written as the reference for itself. It carries the same character back through a parse, and a
        // reference is a node boundary there, so neither half of what it split can be read as a bare destination.
        out.append("&#").append(character.toInt).append(';')
        lineStart = false
        nodeStart = true
        index += 1
      else if character == '\n' then
        if oneLine || lineStart then
          // Either this line already holds nothing and another bare newline would leave it blank, or no line break
          // is allowed here at all: either way the newline's content survives, but not as a physical line ending.
          out.append("&#10;")
          lineStart = false
          nodeStart = true
        else
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
        nodeStart = vulnerable
        index = end
      else if character == '\t' then
        out.append("&#9;")
        lineStart = false
        nodeStart = true
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

  /** `children`'s sole member, when there is exactly one and it is an [[MdNode.Emphasis]]. */
  private def soleChildEmphasis(children: Chunk[MdNode.PhrasingContent]): Maybe[MdNode.Emphasis] =
    if children.size == 1 then
      children(0) match
        case inner: MdNode.Emphasis => Present(inner)
        case _                      => Absent
    else Absent

  /**
   * The marker an Emphasis or Strong delimiter should use, and the style its content should render under, given the
   * marker it would use before any clash resolution and its sole child when that child is itself an
   * [[MdNode.Emphasis]].
   *
   * A sole inner Emphasis whose resolved marker is a different character never touches the outer delimiter on reparse
   * and needs no adjustment. One resolving to the same character does: the two delimiter runs touch and a parse always
   * prefers the longer match, so the inner subtree's ambient style shadows to the other marker instead — unless the
   * inner marker is pinned by its own explicit [[MdStyleKeys.emphasisMarker]] override, which this writer must respect
   * rather than shadow, in which case the outer level takes the other marker so the two still differ.
   *
   * @param marker
   *   the character the outer delimiter would use before any clash resolution — the marker itself for Emphasis, or the
   *   character later doubled for Strong
   * @param soleInnerEmphasis
   *   the outer node's sole child, when it is itself an [[MdNode.Emphasis]] — see [[soleChildEmphasis]]
   */
  private def resolveEmphasisClash(marker: Char, soleInnerEmphasis: Maybe[MdNode.Emphasis])(using
      style: MdStyle
  ): (Char, MdStyle) =
    soleInnerEmphasis match
      case Present(inner) =>
        val innerOverride = inner.meta.get(MdStyleKeys.emphasisMarker)
        val innerResolved = innerOverride.getOrElse(style.emphasisMarker)
        if innerResolved != marker then (marker, style)
        else
          innerOverride match
            case Present(_) => (alternateMarker(marker), style)
            case Absent     => (marker, style.copy(emphasisMarker = alternateMarker(marker)))
      case Absent => (marker, style)

  /** The other emphasis marker CommonMark recognises, for the one place two of the same one cannot touch. */
  private def alternateMarker(marker: Char): Char = if marker == '*' then '_' else '*'

  /**
   * A bullet other than `marker`, from CommonMark's three (`-`, `+`, `*`); which one is unimportant, only that it
   * differs.
   */
  private def alternateBullet(marker: Char): Char = if marker == '-' then '+' else '-'

  /** The other ordered-list delimiter CommonMark recognises. */
  private def alternateDelimiter(delimiter: Char): Char = if delimiter == '.' then ')' else '.'

  private def runEnd(text: String, from: Int, character: Char): Int =
    var index = from
    while index < text.length && text.charAt(index) == character do index += 1
    index

  private def digitsEnd(text: String, from: Int): Int =
    var index = from
    while index < text.length && text.charAt(index).isDigit do index += 1
    index
end MdWriter
