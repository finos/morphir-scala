package morphir.langkit.markdown.cst

import kyo.*
import morphir.langkit.core.Span
import morphir.langkit.markdown.HeadingLevel
import morphir.langkit.markdown.Parser

/**
 * A concrete syntax tree over a Markdown source, under one invariant: **leaf tiling**.
 *
 * Interior nodes own structure; only leaves own text. The leaves of a document, read in order, partition the source —
 * every byte belongs to exactly one leaf, with no gap and no overlap — so [[Cst.print]] reproduces the source exactly,
 * by construction rather than by effort. [[Cst.tilingErrors]] checks the invariant and the round-trip suite enforces it
 * over the whole vendored CommonMark corpus.
 *
 * Three leaf kinds, by what they claim. [[CstNode.Token]] is syntax the author spent — a marker run, a fence, an
 * underline. [[CstNode.Text]] is literal content in its final form — the body of a code fence. [[CstNode.Verbatim]] is
 * the graduation device: a region no slice has yet modelled, printed as itself so the round-trip invariant holds while
 * constructs move out of it one slice at a time (morphir-lc8.21 through lc8.26). A block whose interior is still a
 * single verbatim leaf — a paragraph before inlines graduate — is typed at the block level and unmodelled within, which
 * is exactly the state the slice plan names.
 */
enum CstNode derives CanEqual:

  /** The root. Its children, in source order, tile the whole input. */
  case Document(children: Chunk[CstNode], span: Span)

  case ThematicBreak(children: Chunk[CstNode], span: Span)
  case AtxHeading(level: HeadingLevel, children: Chunk[CstNode], span: Span)
  case SetextHeading(level: HeadingLevel, children: Chunk[CstNode], span: Span)
  case FencedCode(children: Chunk[CstNode], span: Span)
  case IndentedCode(children: Chunk[CstNode], span: Span)
  case Paragraph(children: Chunk[CstNode], span: Span)

  /** Syntax the author spent: marker runs, fences, setext underlines. */
  case Token(text: String, span: Span)

  /** Literal content in final form, such as the raw body of a code fence. */
  case Text(text: String, span: Span)

  /** A region held as raw text because no slice has yet given it structure. */
  case Verbatim(text: String, span: Span)

  def span: Span

  /** The exact source text of a leaf; `Absent` for interior nodes. */
  def leafText: Maybe[String] = this match
    case Token(text, _)    => Present(text)
    case Text(text, _)     => Present(text)
    case Verbatim(text, _) => Present(text)
    case _                 => Absent

  /** Children of an interior node, in source order; empty for leaves. */
  def childNodes: Chunk[CstNode] = this match
    case Document(children, _)         => children
    case ThematicBreak(children, _)    => children
    case AtxHeading(_, children, _)    => children
    case SetextHeading(_, children, _) => children
    case FencedCode(children, _)       => children
    case IndentedCode(children, _)     => children
    case Paragraph(children, _)        => children
    case _: (Token | Text | Verbatim)  => Chunk.empty

object Cst:

  /** The source, reproduced from the tree's leaves in order. Byte-exact whenever the tree tiles. */
  def print(node: CstNode): String =
    val out                    = new StringBuilder
    def walk(n: CstNode): Unit = n.leafText match
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
  def tilingErrors(node: CstNode, sourceLength: Int): Chunk[String] =
    val errors                 = Chunk.newBuilder[String]
    var cursor                 = 0
    def walk(n: CstNode): Unit = n.leafText match
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
 * The block phase records a [[CstFragment]] at each graduated top-level construction site; this object turns those
 * fragments into typed nodes by slicing the source, and holds every unclaimed region — containers, HTML blocks, blank
 * runs, link reference definitions — as [[CstNode.Verbatim]] until its slice graduates it. A source the parser rejects
 * (budget exhaustion) degrades to one verbatim leaf, so parsing stays total and round-trip exact.
 */
object CstParser:

  def parse(source: String): CstNode.Document =
    Parser.parseFragments(source) match
      case Result.Success(fragments) => assemble(source, fragments)
      case _                         => fallback(source)

  private def fallback(source: String): CstNode.Document =
    val span = Span(0, source.length)
    if source.isEmpty then CstNode.Document(Chunk.empty, span)
    else CstNode.Document(Chunk(CstNode.Verbatim(source, span)), span)

  private def assemble(source: String, fragments: Chunk[CstFragment]): CstNode.Document =
    val children              = Chunk.newBuilder[CstNode]
    var cursor                = 0
    def gap(until: Int): Unit =
      if until > cursor then
        children.addOne(CstNode.Verbatim(source.substring(cursor, until), Span.fromStartEnd(cursor, until)))
        cursor = until
    fragments.foreach { fragment =>
      if fragment.span.offset >= cursor then
        gap(fragment.span.offset)
        children.addOne(materialize(source, fragment))
        cursor = fragment.span.end
    }
    gap(source.length)
    CstNode.Document(children.result(), Span(0, source.length))

  /**
   * A leaf over `[from, until)`, or nothing when the range is empty — empty leaves would break nothing but say nothing.
   */
  private def leaf(source: String, from: Int, until: Int)(make: (String, Span) => CstNode): Chunk[CstNode] =
    if until > from then Chunk(make(source.substring(from, until), Span.fromStartEnd(from, until)))
    else Chunk.empty

  private def materialize(source: String, fragment: CstFragment): CstNode = fragment match
    case CstFragment.ThematicBreak(span) =>
      CstNode.ThematicBreak(leaf(source, span.offset, span.end)(CstNode.Token(_, _)), span)

    case CstFragment.AtxHeading(level, span, content) =>
      val children =
        leaf(source, span.offset, content.offset)(CstNode.Token(_, _))
          ++ leaf(source, content.offset, content.end)(CstNode.Verbatim(_, _))
          ++ leaf(source, content.end, span.end)(CstNode.Token(_, _))
      CstNode.AtxHeading(level, children, span)

    case CstFragment.SetextHeading(level, span, underlineOffset) =>
      val children =
        leaf(source, span.offset, underlineOffset)(CstNode.Verbatim(_, _))
          ++ leaf(source, underlineOffset, span.end)(CstNode.Token(_, _))
      CstNode.SetextHeading(level, children, span)

    case CstFragment.FencedCode(span, openEnd, closeStart) =>
      val contentEnd = closeStart.getOrElse(span.end)
      val children   =
        leaf(source, span.offset, openEnd)(CstNode.Token(_, _))
          ++ leaf(source, openEnd, contentEnd)(CstNode.Text(_, _))
          ++ leaf(source, contentEnd, span.end)(CstNode.Token(_, _))
      CstNode.FencedCode(children, span)

    case CstFragment.IndentedCode(span) =>
      CstNode.IndentedCode(leaf(source, span.offset, span.end)(CstNode.Verbatim(_, _)), span)

    case CstFragment.Paragraph(span) =>
      CstNode.Paragraph(leaf(source, span.offset, span.end)(CstNode.Verbatim(_, _)), span)
