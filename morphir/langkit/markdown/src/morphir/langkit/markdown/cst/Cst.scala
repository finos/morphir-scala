package morphir.langkit.markdown.cst

import kyo.*
import morphir.langkit.core.Span
import morphir.langkit.core.scanner.ScanBudget
import morphir.langkit.markdown.HeadingLevel
import morphir.langkit.markdown.MdProfile
import morphir.langkit.markdown.MdMeta
import morphir.langkit.markdown.MdNode
import morphir.langkit.markdown.internal.Parser

/**
 * A concrete syntax tree over a Markdown source, under one invariant: **leaf tiling**.
 *
 * Interior nodes own structure; only leaves own text. The leaves of a document, read in order, partition the source —
 * every byte belongs to exactly one leaf, with no gap and no overlap — so [[Cst.print]] reproduces the source exactly,
 * by construction rather than by effort. [[Cst.tilingErrors]] checks the invariant and the round-trip suite enforces it
 * over the whole vendored CommonMark corpus.
 *
 * Three leaf kinds, by what they claim. [[MdCstNode.Token]] is syntax the author spent — a marker run, a fence, an
 * underline. [[MdCstNode.Text]] is literal content in its final form — the body of a code fence. [[MdCstNode.Verbatim]]
 * is the graduation device: a region no slice has yet modelled, printed as itself so the round-trip invariant holds
 * while constructs move out of it one slice at a time (morphir-lc8.21 through lc8.26). A block whose interior is still
 * a single verbatim leaf — a paragraph before inlines graduate — is typed at the block level and unmodelled within,
 * which is exactly the state the slice plan names.
 */
enum MdCstNode derives CanEqual:

  /** The root. Its children, in source order, tile the whole input. */
  case Document(children: Chunk[MdCstNode], span: Span)

  /**
   * A frontmatter block, which only a profile that enables its kind produces. Spelled like a fenced code block: the
   * opening delimiter line as a [[Token]], the raw value as [[Text]] — undecoded, whatever the kind's grammar is — and
   * the closing delimiter line as a [[Token]]. Legal only as the document's first child.
   */
  case Frontmatter(children: Chunk[MdCstNode], span: Span)

  case ThematicBreak(children: Chunk[MdCstNode], span: Span)
  case AtxHeading(level: HeadingLevel, children: Chunk[MdCstNode], span: Span)
  case SetextHeading(level: HeadingLevel, children: Chunk[MdCstNode], span: Span)
  case FencedCode(children: Chunk[MdCstNode], span: Span)
  case IndentedCode(children: Chunk[MdCstNode], span: Span)
  case Paragraph(children: Chunk[MdCstNode], span: Span)

  /**
   * A quote. Its children interleave per-line `>` marker [[Token]]s with the blocks the quote holds; a marker that
   * falls inside a child's span — the middle of a two-line paragraph — appears as a token *inside* that child, because
   * the child spans the marker bytes without owning them.
   */
  case BlockQuote(children: Chunk[MdCstNode], span: Span)

  /** A run of bullet items sharing `bullet`. `tight` is the rendering evidence the parser gathered from blank lines. */
  case BulletList(bullet: Char, tight: Boolean, children: Chunk[MdCstNode], span: Span)

  /** A run of numbered items sharing `delimiter`, numbered from `start`. */
  case OrderedList(start: Int, delimiter: Char, tight: Boolean, children: Chunk[MdCstNode], span: Span)

  /** One item. Its first child is the marker [[Token]]; continuation-line indentation appears as tokens too. */
  case ListItem(children: Chunk[MdCstNode], span: Span)

  /** A raw HTML block. Its interior is [[Text]]: the content is HTML, not Markdown, and is already in final form. */
  case HtmlBlock(children: Chunk[MdCstNode], span: Span)

  /** A `[label]: destination "title"` definition, unresolved. Its interior stays verbatim until inlines graduate. */
  case LinkReferenceDefinition(children: Chunk[MdCstNode], span: Span)

  /** An inline code span: backtick-run [[Token]]s around the raw [[Text]] between them. */
  case CodeSpan(children: Chunk[MdCstNode], span: Span)

  /** An autolink, kept in its angle-bracket form: `<`, the literal destination, `>`. */
  case Autolink(children: Chunk[MdCstNode], span: Span)

  /** Inline raw HTML, taken whole as [[Text]]: its interior is HTML the inline grammar never re-reads. */
  case RawHtml(children: Chunk[MdCstNode], span: Span)

  /**
   * A link, in whichever of the four forms the author wrote. Its children keep the leaves in place — bracket and
   * parenthesis [[Token]]s, the link text as inline content, the destination and title as [[Text]] — and the fields
   * repeat the raw spellings with container marker bytes stripped, so a consumer need not reassemble them from leaves.
   * The AST's normalized URI is a resolution, not a spelling; resolution is lowering's job.
   *
   * `destination` and `title` are present only for the inline form; `reference` only for the full reference form.
   */
  case Link(
      form: LinkForm,
      destination: Maybe[String],
      title: Maybe[String],
      reference: Maybe[String],
      children: Chunk[MdCstNode],
      span: Span
  )

  /** An image. Its bracketed label is inline content here, where the AST flattens it to an `alt` string. */
  case Image(
      form: LinkForm,
      destination: Maybe[String],
      title: Maybe[String],
      reference: Maybe[String],
      children: Chunk[MdCstNode],
      span: Span
  )

  /**
   * Emphasis, keeping its delimiter char and strength. Its children are the delimiter-run [[Token]]s around inline
   * content; its span is what it owns — a partially consumed run's leftover delimiters fall outside it, into the
   * surrounding gaps.
   */
  case Emphasis(delimiter: Char, strong: Boolean, children: Chunk[MdCstNode], span: Span)

  /** A hard line break, spelled as the author wrote it: trailing spaces or a backslash, then the line ending. */
  case HardBreak(children: Chunk[MdCstNode], span: Span)

  /** A backslash escape: the backslash [[Token]] and the character it makes literal as [[Text]]. */
  case Escape(children: Chunk[MdCstNode], span: Span)

  /** A character reference — `&amp;`, `&#35;` — kept as written; its decoded value is a resolution, not a spelling. */
  case Entity(children: Chunk[MdCstNode], span: Span)

  /** Syntax the author spent: marker runs, fences, setext underlines. */
  case Token(text: String, span: Span)

  /** Literal content in final form, such as the raw body of a code fence. */
  case Text(text: String, span: Span)

  /** A region held as raw text because no slice has yet given it structure. */
  case Verbatim(text: String, span: Span)

  /**
   * Columns a container marker's final tab spent past what the container claimed. A tab is consumed by column but owned
   * by character, so the content after such a marker is owed indentation that exists as layout rather than as bytes.
   * Zero-width: it prints nothing and claims no source, which keeps the round-trip exact.
   */
  case PhantomIndent(columns: Int, span: Span)

  def span: Span

  /** The exact source text of a leaf; `Absent` for interior nodes. */
  def leafText: Maybe[String] = this match
    case Token(text, _)    => Present(text)
    case Text(text, _)     => Present(text)
    case Verbatim(text, _) => Present(text)
    case _                 => Absent

  /** Children of an interior node, in source order; empty for leaves. */
  def childNodes: Chunk[MdCstNode] = this match
    case Document(children, _)                => children
    case Frontmatter(children, _)             => children
    case ThematicBreak(children, _)           => children
    case AtxHeading(_, children, _)           => children
    case SetextHeading(_, children, _)        => children
    case FencedCode(children, _)              => children
    case IndentedCode(children, _)            => children
    case Paragraph(children, _)               => children
    case BlockQuote(children, _)              => children
    case BulletList(_, _, children, _)        => children
    case OrderedList(_, _, _, children, _)    => children
    case ListItem(children, _)                => children
    case HtmlBlock(children, _)               => children
    case LinkReferenceDefinition(children, _) => children
    case CodeSpan(children, _)                => children
    case Autolink(children, _)                => children
    case RawHtml(children, _)                 => children
    case Link(_, _, _, _, children, _)        => children
    case Image(_, _, _, _, children, _)       => children
    case Emphasis(_, _, children, _)          => children
    case HardBreak(children, _)               => children
    case Escape(children, _)                  => children
    case Entity(children, _)                  => children
    case PhantomIndent(_, _)                  => Chunk.empty
    case _: (Token | Text | Verbatim)         => Chunk.empty

object Cst:

  /** The source, reproduced from the tree's leaves in order. Byte-exact whenever the tree tiles. */
  def print(node: MdCstNode): String =
    val out                      = new StringBuilder
    def walk(n: MdCstNode): Unit = n.leafText match
      case Present(text) => out.append(text)
      case Absent        => n.childNodes.foreach(walk)
    walk(node)
    out.toString

  /**
   * Violations of the leaf-tiling invariant, empty when the tree is sound.
   *
   * Checks that the leaves, in tree order, cover `[0, sourceLength)` exactly: each leaf starts where the previous one
   * ended, the first at zero, the last ending at the length, and each carrying exactly as much text as its span claims.
   */
  def tilingErrors(node: MdCstNode, sourceLength: Int): Chunk[String] =
    val errors                   = Chunk.newBuilder[String]
    var cursor                   = 0
    def walk(n: MdCstNode): Unit = n.leafText match
      case Present(text) =>
        if n.span.offset != cursor then errors.addOne(s"leaf at ${n.span.offset} does not start at cursor $cursor")
        if text.length != n.span.length then
          errors.addOne(s"leaf at ${n.span.offset} carries ${text.length} chars but spans ${n.span.length}")
        cursor = n.span.end
      case Absent => n.childNodes.foreach(walk)
    walk(node)
    if cursor != sourceLength then errors.addOne(s"leaves end at $cursor, source ends at $sourceLength")
    errors.result()

/**
 * Parses a source into its CST.
 *
 * The block phase records a [[CstFragment]] at each graduated construction site; this object turns those fragments into
 * typed nodes by slicing the source, and holds every unclaimed region — blank runs, mostly — as [[MdCstNode.Verbatim]]
 * until its slice graduates it. A source the parser rejects (budget exhaustion) degrades to one verbatim leaf, so
 * parsing stays total and round-trip exact.
 *
 * Containers bring marker spans with them: the bytes their cursor spent on `>` prefixes and item indentation. Those
 * spans are punched out of gaps and leaf interiors as [[MdCstNode.Token]] leaves wherever they fall inside the region
 * being tiled — which is how a child that spans marker bytes ends up not owning them. A marker recorded on a peek past
 * the container's end falls outside every region and is clamped away by the same tiling.
 */
object CstParser:

  def parse(source: String)(using profile: MdProfile): MdCstNode.Document =
    Parser.parseFragments(source, ScanBudget.default, profile) match
      case Result.Success(fragments) => assembleDocument(source, fragments)
      case _                         => fallback(source)

  /** The document from an already-run block phase; [[Parser.parse]] uses this to lower in a single pass. */
  private[markdown] def assembleDocument(source: String, fragments: Chunk[CstFragment]): MdCstNode.Document =
    MdCstNode.Document(assembleRegion(source, Span(0, source.length), Chunk.empty, fragments), Span(0, source.length))

  private def fallback(source: String): MdCstNode.Document =
    val span = Span(0, source.length)
    if source.isEmpty then MdCstNode.Document(Chunk.empty, span)
    else MdCstNode.Document(Chunk(MdCstNode.Verbatim(source, span)), span)

  /**
   * The children of one region: fragments materialized in order, gaps tiled as [[MdCstNode.Verbatim]] with any marker
   * spans punched out as [[MdCstNode.Token]]s. The document is the region with no markers; containers pass their own.
   */
  private def assembleRegion(
      source: String,
      span: Span,
      markers: Chunk[Marker],
      fragments: Chunk[CstFragment]
  ): Chunk[MdCstNode] =
    val children              = Chunk.newBuilder[MdCstNode]
    var cursor                = span.offset
    def gap(until: Int): Unit =
      if until > cursor then
        children.addAll(tile(source, cursor, until, markers)(MdCstNode.Verbatim(_, _)))
        cursor = until
    fragments.foreach { fragment =>
      if fragment.span.offset >= cursor && fragment.span.end <= span.end then
        gap(fragment.span.offset)
        children.addOne(materialize(source, fragment, markers))
        cursor = fragment.span.end
    }
    gap(span.end)
    children.result()

  /**
   * A leaf over `[from, until)`, or nothing when the range is empty — empty leaves would break nothing but say nothing.
   */
  private def leaf(source: String, from: Int, until: Int)(make: (String, Span) => MdCstNode): Chunk[MdCstNode] =
    if until > from then Chunk(make(source.substring(from, until), Span.fromStartEnd(from, until)))
    else Chunk.empty

  /**
   * Leaves over `[from, until)` with every marker span that intersects the range cut out as a [[MdCstNode.Token]] and
   * the rest made by `make`. Markers arrive sorted by offset; ranges that overlap — the same prefix recorded at two
   * nesting depths — degrade to adjacent tokens rather than to a tiling violation.
   */
  private def tile(source: String, from: Int, until: Int, markers: Chunk[Marker])(
      make: (String, Span) => MdCstNode
  ): Chunk[MdCstNode] =
    if markers.isEmpty then leaf(source, from, until)(make)
    else
      val out    = Chunk.newBuilder[MdCstNode]
      var cursor = from
      markers.foreach { marker =>
        val start = math.max(marker.span.offset, cursor)
        val end   = math.min(marker.span.end, until)
        if start < end then
          out.addAll(leaf(source, cursor, start)(make))
          out.addAll(leaf(source, start, end)(MdCstNode.Token(_, _)))
          // The marker's final tab may have reached past the container's claim; the leftover columns belong to the
          // content and are kept as zero-width layout so the round-trip stays byte-exact.
          if marker.phantom > 0 && end == marker.span.end then
            out.addOne(MdCstNode.PhantomIndent(marker.phantom, Span(end, 0)))
          cursor = end
      }
      out.addAll(leaf(source, cursor, until)(make))
      out.result()

  /** `markers` are the enclosing container's, for punching out of this fragment's unstructured interiors. */
  private def materialize(source: String, fragment: CstFragment, markers: Chunk[Marker]): MdCstNode = fragment match
    // Laid out like a closed fence, and with no marker punching: frontmatter is recognized at offset zero, before any
    // container can be open, so there are never marker bytes inside it.
    case CstFragment.Frontmatter(span, openEnd, closeStart) =>
      val children =
        leaf(source, span.offset, openEnd)(MdCstNode.Token(_, _))
          ++ leaf(source, openEnd, closeStart)(MdCstNode.Text(_, _))
          ++ leaf(source, closeStart, span.end)(MdCstNode.Token(_, _))
      MdCstNode.Frontmatter(children, span)

    case CstFragment.ThematicBreak(span) =>
      MdCstNode.ThematicBreak(leaf(source, span.offset, span.end)(MdCstNode.Token(_, _)), span)

    case CstFragment.AtxHeading(level, span, content, inlines) =>
      val children =
        leaf(source, span.offset, content.offset)(MdCstNode.Token(_, _))
          ++ prose(source, content.offset, content.end, markers, inlines)
          ++ leaf(source, content.end, span.end)(MdCstNode.Token(_, _))
      MdCstNode.AtxHeading(level, children, span)

    case CstFragment.SetextHeading(level, span, underlineOffset, inlines) =>
      val children =
        prose(source, span.offset, underlineOffset, markers, inlines)
          ++ leaf(source, underlineOffset, span.end)(MdCstNode.Token(_, _))
      MdCstNode.SetextHeading(level, children, span)

    case CstFragment.FencedCode(span, openEnd, closeStart) =>
      val contentEnd = closeStart.getOrElse(span.end)
      val children   =
        leaf(source, span.offset, openEnd)(MdCstNode.Token(_, _))
          ++ tile(source, openEnd, contentEnd, markers)(MdCstNode.Text(_, _))
          ++ leaf(source, contentEnd, span.end)(MdCstNode.Token(_, _))
      MdCstNode.FencedCode(children, span)

    case CstFragment.IndentedCode(span) =>
      MdCstNode.IndentedCode(tile(source, span.offset, span.end, markers)(MdCstNode.Verbatim(_, _)), span)

    case CstFragment.Paragraph(span, inlines) =>
      MdCstNode.Paragraph(prose(source, span.offset, span.end, markers, inlines), span)

    case CstFragment.HtmlBlock(span) =>
      MdCstNode.HtmlBlock(tile(source, span.offset, span.end, markers)(MdCstNode.Text(_, _)), span)

    case CstFragment.LinkReferenceDefinition(span) =>
      MdCstNode.LinkReferenceDefinition(tile(source, span.offset, span.end, markers)(MdCstNode.Verbatim(_, _)), span)

    case CstFragment.BlockQuote(span, own, children) =>
      val all = merged(markers, own)
      MdCstNode.BlockQuote(assembleRegion(source, span, all, children), span)

    case CstFragment.BulletList(bullet, tight, span, items) =>
      MdCstNode.BulletList(bullet, tight, assembleRegion(source, span, markers, items), span)

    case CstFragment.OrderedList(start, delimiter, tight, span, items) =>
      MdCstNode.OrderedList(start, delimiter, tight, assembleRegion(source, span, markers, items), span)

    case CstFragment.ListItem(span, own, children) =>
      val all = merged(markers, own)
      MdCstNode.ListItem(assembleRegion(source, span, all, children), span)

  /**
   * The enclosing container's markers and this container's, one sorted run. A container's own recorder only sees the
   * lines its cursor read directly, so the outer set fills in lines that fell between — the `>` on a blank line
   * standing between two list items, which the items never read.
   */
  private def merged(outer: Chunk[Marker], own: Chunk[Marker]): Chunk[Marker] =
    if outer.isEmpty then own
    else if own.isEmpty then outer
    else Chunk.from((outer ++ own).sortBy(_.span.offset))

  /**
   * A prose interior: graduated inline constructs at their source spans, everything between them verbatim.
   *
   * The inline nodes come from the resolved parse and carry source spans, so tiling an interior is the same walk a
   * container's region gets one level up. A construct inside an ungraduated wrapper — a code span inside emphasis —
   * still surfaces here at its own span, with the wrapper's delimiters left in the verbatim gaps around it; the
   * emphasis slice will claim them later. An unfilled slot (a parse that never resolved) degrades to verbatim.
   */
  private def prose(
      source: String,
      from: Int,
      until: Int,
      markers: Chunk[Marker],
      inlines: InlineSlot
  ): Chunk[MdCstNode] =
    inlines.content match
      case Absent         => tile(source, from, until, markers)(MdCstNode.Verbatim(_, _))
      case Present(items) => inlineRegion(source, from, until, markers, items, inlines.notes)

  /** One inline region: graduated constructs at their spans, gaps verbatim; link content recurses through here. */
  private def inlineRegion(
      source: String,
      from: Int,
      until: Int,
      markers: Chunk[Marker],
      items: Chunk[MdNode.PhrasingContent],
      notes: Maybe[InlineNotes]
  ): Chunk[MdCstNode] =
    val out                                        = Chunk.newBuilder[MdCstNode]
    var cursor                                     = from
    def emit(inline: MdNode.PhrasingContent): Unit =
      graduate(source, inline, markers, notes) match
        case Present(node) if node.span.offset >= cursor && node.span.end <= until =>
          out.addAll(inlineGap(source, cursor, node.span.offset, markers, notes))
          out.addOne(node)
          cursor = node.span.end
        case _ =>
          inline match
            case MdNode.Emphasis(content, _)   => content.foreach(emit)
            case MdNode.Strong(content, _)     => content.foreach(emit)
            case MdNode.Link(_, _, content, _) => content.foreach(emit)
            case _                             => ()
    items.foreach(emit)
    out.addAll(inlineGap(source, cursor, until, markers, notes))
    out.result()

  /**
   * A gap between inline constructs: verbatim text with escapes and character references punched out as their own
   * nodes. Only inline gaps get this — an escape is an escape because the inline grammar read it as one, so the
   * punching follows what the parse recorded rather than re-deriving it from the characters.
   */
  private def inlineGap(
      source: String,
      from: Int,
      until: Int,
      markers: Chunk[Marker],
      notes: Maybe[InlineNotes]
  ): Chunk[MdCstNode] =
    notes match
      case Absent             => tile(source, from, until, markers)(MdCstNode.Verbatim(_, _))
      case Present(collected) =>
        val punches =
          (collected.escapeOffsets.map(o => (span = Span.fromStartEnd(o, o + 2), escape = true))
            ++ collected.entitySpans.map(s => (span = s, escape = false))).sortBy(_.span.offset)
        val out    = Chunk.newBuilder[MdCstNode]
        var cursor = from
        punches.foreach { punch =>
          val s = punch.span
          if s.offset >= cursor && s.end <= until then
            val valid =
              if punch.escape then s.offset < source.length && source.charAt(s.offset) == '\\'
              else s.offset < source.length && source.charAt(s.offset) == '&'
            if valid then
              out.addAll(tile(source, cursor, s.offset, markers)(MdCstNode.Verbatim(_, _)))
              if punch.escape then
                val children =
                  leaf(source, s.offset, s.offset + 1)(MdCstNode.Token(_, _))
                    ++ leaf(source, s.offset + 1, s.end)(MdCstNode.Text(_, _))
                out.addOne(MdCstNode.Escape(children, s))
              else out.addOne(MdCstNode.Entity(leaf(source, s.offset, s.end)(MdCstNode.Token(_, _)), s))
              cursor = s.end
        }
        out.addAll(tile(source, cursor, until, markers)(MdCstNode.Verbatim(_, _)))
        out.result()

  /**
   * The typed CST node for one inline construct, for the kinds this slice has graduated; [[kyo.Absent]] otherwise.
   *
   * Each case checks the source at its span before claiming it — a code span must start with a backtick, an autolink or
   * raw HTML with `<`, a bracketed link with `[`. A span that fails the check is a mapping the parse and the source
   * disagree on, and the safe answer is to leave the region verbatim rather than mislabel it. A node carrying no span
   * at all is the same case: nothing to check against, so nothing to claim, and it falls through to verbatim.
   */
  private def graduate(
      source: String,
      inline: MdNode.PhrasingContent,
      markers: Chunk[Marker],
      notes: Maybe[InlineNotes]
  ): Maybe[MdCstNode] = inline match
    case MdNode.InlineCode(_, MdMeta(Present(span), _))
        if span.offset < source.length && source.charAt(span.offset) == '`' =>
      var run = span.offset
      while run < span.end && source.charAt(run) == '`' do run += 1
      val ticks    = run - span.offset
      val children =
        leaf(source, span.offset, run)(MdCstNode.Token(_, _))
          ++ tile(source, run, span.end - ticks, markers)(MdCstNode.Text(_, _))
          ++ leaf(source, span.end - ticks, span.end)(MdCstNode.Token(_, _))
      Present(MdCstNode.CodeSpan(children, span))

    case MdNode.InlineHtml(_, MdMeta(Present(span), _))
        if span.offset < source.length && source.charAt(span.offset) == '<' =>
      Present(MdCstNode.RawHtml(tile(source, span.offset, span.end, markers)(MdCstNode.Text(_, _)), span))

    // An autolink is a Link whose source form starts with `<`; a bracketed link or image starts with `[` or `!`.
    case MdNode.Link(_, _, _, MdMeta(Present(span), _))
        if span.offset < source.length && source.charAt(span.offset) == '<'
          && span.end <= source.length && span.length >= 2 && source.charAt(span.end - 1) == '>' =>
      val children =
        leaf(source, span.offset, span.offset + 1)(MdCstNode.Token(_, _))
          ++ tile(source, span.offset + 1, span.end - 1, markers)(MdCstNode.Text(_, _))
          ++ leaf(source, span.end - 1, span.end)(MdCstNode.Token(_, _))
      Present(MdCstNode.Autolink(children, span))

    case MdNode.Link(_, _, content, MdMeta(Present(span), _))
        if span.offset < source.length && source.charAt(span.offset) == '[' =>
      for
        collected <- notes
        note      <- collected.linkAt(span.offset)
        node      <- linkNode(source, span, note, content, markers, notes, image = false)
      yield node

    case MdNode.Image(_, _, _, MdMeta(Present(span), _))
        if span.offset < source.length && source.charAt(span.offset) == '!' =>
      for
        collected <- notes
        note      <- collected.linkAt(span.offset)
        node      <- linkNode(source, span, note, note.alt.getOrElse(Chunk.empty), markers, notes, image = true)
      yield node

    case MdNode.Emphasis(content, MdMeta(Present(span), _)) =>
      emphasisNode(source, span, content, markers, notes, used = 1, strong = false)

    case MdNode.Strong(content, MdMeta(Present(span), _)) =>
      emphasisNode(source, span, content, markers, notes, used = 2, strong = true)

    // A hard break's span runs to the next line's content, so it may cover a container marker; tiling with the
    // markers keeps the break's own characters and the marker as separate tokens.
    case MdNode.Break(MdMeta(Present(span), _))
        if span.offset < source.length
          && (source.charAt(span.offset) == ' ' || source.charAt(span.offset) == '\\') =>
      Present(MdCstNode.HardBreak(tile(source, span.offset, span.end, markers)(MdCstNode.Token(_, _)), span))

    case _ => Absent

  /**
   * An emphasis node, its extent found from the content rather than from the AST span.
   *
   * The AST span covers the whole delimiter runs, but a run consumed a pair at a time shares its bytes: in
   * `__foo_ bar_` the inner emphasis's AST span starts at the run the outer also draws from. What an emphasis actually
   * owns is `used` delimiters each side of its content, where nested emphasis content is measured by the same rule
   * recursively — so the CST node's span is that narrowed claim, leftover delimiters fall outside it into the
   * surrounding gaps, and nesting tiles cleanly. Content that is empty or delimiters that fail the character check
   * degrade to [[kyo.Absent]].
   */
  private def emphasisNode(
      source: String,
      span: Span,
      content: Chunk[MdNode.PhrasingContent],
      markers: Chunk[Marker],
      notes: Maybe[InlineNotes],
      used: Int,
      strong: Boolean
  ): Maybe[MdCstNode] =
    def claimOf(item: MdNode.PhrasingContent): (start: Int, end: Int) = item match
      case MdNode.Emphasis(inner, MdMeta(Present(s), _)) => narrowed(inner, 1, s)
      case MdNode.Strong(inner, MdMeta(Present(s), _))   => narrowed(inner, 2, s)
      case other                                         =>
        other.span match
          case Present(s) => (start = s.offset, end = s.end)
          // A node the parse did not position claims nothing this can measure. A negative start propagates through
          // every enclosing `narrowed` and trips the `openStart < 0` check below, so the emphasis degrades to
          // verbatim rather than tile against a span it does not have.
          case Absent => (start = -1, end = -1)
    def narrowed(inner: Chunk[MdNode.PhrasingContent], innerUsed: Int, s: Span): (start: Int, end: Int) =
      if inner.isEmpty then (start = s.offset, end = s.end)
      else
        val claims = inner.map(claimOf)
        (start = claims.map(_.start).min - innerUsed, end = claims.map(_.end).max + innerUsed)

    if content.isEmpty then Absent
    else
      val claims                                            = content.map(claimOf)
      val contentStart                                      = claims.map(_.start).min
      val contentEnd                                        = claims.map(_.end).max
      val openStart                                         = contentStart - used
      val closeEnd                                          = contentEnd + used
      def isRun(from: Int, until: Int, char: Char): Boolean = (from until until).forall(i =>
        i >= 0 && i < source.length && source.charAt(i) == char
      )
      if openStart < span.offset || closeEnd > span.end || openStart < 0 then Absent
      else
        val delimiter = source.charAt(openStart)
        if (delimiter != '*' && delimiter != '_')
          || !isRun(openStart, contentStart, delimiter) || !isRun(contentEnd, closeEnd, delimiter)
        then Absent
        else
          val children =
            leaf(source, openStart, contentStart)(MdCstNode.Token(_, _))
              ++ inlineRegion(source, contentStart, contentEnd, markers, content, notes)
              ++ leaf(source, contentEnd, closeEnd)(MdCstNode.Token(_, _))
          Present(MdCstNode.Emphasis(delimiter, strong, children, Span.fromStartEnd(openStart, closeEnd)))

  /**
   * A link or image node from its note, or [[kyo.Absent]] when the note's geometry does not fit the span.
   *
   * The children are laid out as ordered pieces — brackets, content, destination, title, reference label — with every
   * gap between them tiled verbatim. A piece that would run backwards or past the span means the note and the source
   * disagree, and the whole construct stays verbatim rather than tile wrongly.
   */
  private def linkNode(
      source: String,
      span: Span,
      note: LinkNote,
      content: Chunk[MdNode.PhrasingContent],
      markers: Chunk[Marker],
      notes: Maybe[InlineNotes],
      image: Boolean
  ): Maybe[MdCstNode] =
    val pieces = List.newBuilder[(from: Int, until: Int, make: (Int, Int) => Chunk[MdCstNode])]

    def tok(from: Int, until: Int): Unit =
      pieces += ((from = from, until = until, make = (a, b) => leaf(source, a, b)(MdCstNode.Token(_, _))))
    def txt(from: Int, until: Int): Unit =
      pieces += ((from = from, until = until, make = (a, b) => tile(source, a, b, markers)(MdCstNode.Text(_, _))))

    tok(span.offset, note.content.offset)
    pieces +=
      ((
        from = note.content.offset,
        until = note.content.end,
        make = (a, b) => inlineRegion(source, a, b, markers, content, notes)
      ))

    note.form match
      case LinkForm.Inline =>
        tok(note.content.end, note.content.end + 2) // "]("
        note.destination.foreach { destination =>
          if destination.angled then
            tok(destination.span.offset - 1, destination.span.offset)
            txt(destination.span.offset, destination.span.end)
            tok(destination.span.end, destination.span.end + 1)
          else txt(destination.span.offset, destination.span.end)
        }
        note.title.foreach { title =>
          tok(title.offset, title.offset + 1)
          txt(title.offset + 1, title.end - 1)
          tok(title.end - 1, title.end)
        }
        tok(span.end - 1, span.end) // ")"
      case LinkForm.ReferenceFull =>
        note.reference.foreach { reference =>
          tok(note.content.end, reference.offset) // "]["
          txt(reference.offset, reference.end)
          tok(reference.end, span.end) // "]"
        }
      case LinkForm.ReferenceCollapsed =>
        tok(note.content.end, span.end) // "][]"
      case LinkForm.ReferenceShortcut =>
        tok(note.content.end, span.end) // "]"

    val out    = Chunk.newBuilder[MdCstNode]
    var cursor = span.offset
    var sound  = true
    pieces.result().foreach { piece =>
      if piece.from < cursor || piece.until < piece.from || piece.until > span.end then sound = false
      else if sound then
        out.addAll(tile(source, cursor, piece.from, markers)(MdCstNode.Verbatim(_, _)))
        out.addAll(piece.make(piece.from, piece.until))
        cursor = piece.until
    }
    if !sound then Absent
    else
      out.addAll(tile(source, cursor, span.end, markers)(MdCstNode.Verbatim(_, _)))
      val children    = out.result()
      val destination = note.destination.map(d => stripped(source, d.span.offset, d.span.end, markers))
      val title       = note.title.map(t => stripped(source, t.offset + 1, t.end - 1, markers))
      // The label this reference resolves through: the second label for the full form, the link text's own spelling
      // for the collapsed and shortcut forms, nothing for the inline form, which names its target in place.
      val reference = note.form match
        case LinkForm.Inline        => Absent
        case LinkForm.ReferenceFull => note.reference.map(r => stripped(source, r.offset, r.end, markers))
        case LinkForm.ReferenceCollapsed | LinkForm.ReferenceShortcut =>
          Present(stripped(source, note.content.offset, note.content.end, markers))
      Present(
        if image then MdCstNode.Image(note.form, destination, title, reference, children, span)
        else MdCstNode.Link(note.form, destination, title, reference, children, span)
      )

  /** The source over `[from, until)` with container marker bytes cut out: a raw spelling, free of the container. */
  private def stripped(source: String, from: Int, until: Int, markers: Chunk[Marker]): String =
    if until <= from then ""
    else
      val out    = new StringBuilder
      var cursor = from
      markers.foreach { marker =>
        val start = math.max(marker.span.offset, cursor)
        val end   = math.min(marker.span.end, until)
        if start < end then
          out.append(source.substring(cursor, start))
          cursor = end
      }
      out.append(source.substring(cursor, until))
      out.toString
