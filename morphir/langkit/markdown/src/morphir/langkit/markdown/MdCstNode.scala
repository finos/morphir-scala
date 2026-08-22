package morphir.langkit.markdown

import kyo.*
import morphir.langkit.core.Span

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
  case OrderedList(start: ListStart, delimiter: Char, tight: Boolean, children: Chunk[MdCstNode], span: Span)

  /**
   * One item. Its first child is the marker [[Token]]; continuation-line indentation appears as tokens too, and a task
   * list item's checkbox is a further [[Token]] inside its first paragraph. `checked` is Present only for a GFM task
   * list item.
   */
  case ListItem(children: Chunk[MdCstNode], checked: Maybe[Boolean], span: Span)

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

  /** A `~~`-delimited run. The delimiters are `Token` children, as they are for [[Emphasis]]. */
  case Strikethrough(children: Chunk[MdCstNode], span: Span)

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
    case ListItem(children, _, _)             => children
    case HtmlBlock(children, _)               => children
    case LinkReferenceDefinition(children, _) => children
    case CodeSpan(children, _)                => children
    case Autolink(children, _)                => children
    case RawHtml(children, _)                 => children
    case Link(_, _, _, _, children, _)        => children
    case Image(_, _, _, _, children, _)       => children
    case Emphasis(_, _, children, _)          => children
    case Strikethrough(children, _)           => children
    case HardBreak(children, _)               => children
    case Escape(children, _)                  => children
    case Entity(children, _)                  => children
    case PhantomIndent(_, _)                  => Chunk.empty
    case _: (Token | Text | Verbatim)         => Chunk.empty
