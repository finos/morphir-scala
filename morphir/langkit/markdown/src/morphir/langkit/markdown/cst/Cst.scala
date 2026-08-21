package morphir.langkit.markdown.cst

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
 * The tree starts deliberately small. [[CstNode.Verbatim]] is the graduation device: a region the CST does not yet
 * model structurally is held as one verbatim leaf, which prints as itself, so the round-trip invariant holds from the
 * first slice while constructs move out of verbatim into typed nodes one slice at a time (morphir-lc8.21 through
 * lc8.26). A parent's children need not tile it in the meantime — a container may own marker leaves that interleave
 * with its children's text, which is how a paragraph inside a block quote spans marker bytes it does not own.
 */
enum CstNode derives CanEqual:

  /** The root. Its children, in source order, tile the whole input. */
  case Document(children: Chunk[CstNode], span: Span)

  /**
   * A region held as raw text because no slice has yet given it structure.
   *
   * `text` is the exact source slice at `span`; nothing is normalised. Every construct starts here and graduates to a
   * typed node in a later slice.
   */
  case Verbatim(text: String, span: Span)

  def span: Span

object Cst:

  /** The source, reproduced from the tree's leaves in order. Byte-exact whenever the tree tiles. */
  def print(node: CstNode): String =
    val out                    = new StringBuilder
    def walk(n: CstNode): Unit = n match
      case CstNode.Document(children, _) => children.foreach(walk)
      case CstNode.Verbatim(text, _)     => out.append(text)
    walk(node)
    out.toString

  /**
   * Violations of the leaf-tiling invariant, empty when the tree is sound.
   *
   * Checks that the leaves, in tree order, cover `[0, sourceLength)` exactly: each leaf starts where the previous one
   * ended, the first starts at zero, the last ends at the length. Verbatim leaves are additionally checked for carrying
   * exactly as much text as their span claims.
   */
  def tilingErrors(node: CstNode, sourceLength: Int): Chunk[String] =
    val errors                                         = Chunk.newBuilder[String]
    var cursor                                         = 0
    def leaf(span: Span, textLength: Maybe[Int]): Unit =
      if span.offset != cursor then errors.addOne(s"leaf at ${span.offset} does not start at cursor $cursor")
      textLength match
        case Present(length) if length != span.length =>
          errors.addOne(s"leaf at ${span.offset} carries $length chars but spans ${span.length}")
        case _ => ()
      cursor = span.end
    def walk(n: CstNode): Unit = n match
      case CstNode.Document(children, _) => children.foreach(walk)
      case CstNode.Verbatim(text, span)  => leaf(span, Present(text.length))
    walk(node)
    if cursor != sourceLength then errors.addOne(s"leaves end at $cursor, source ends at $sourceLength")
    errors.result()

/**
 * Parses a source into its CST.
 *
 * This first slice wraps the whole source in a single [[CstNode.Verbatim]] — trivially byte-exact, structurally empty.
 * Each following slice moves one family of constructs out of verbatim; the round-trip suite holds the invariant steady
 * while that happens.
 */
object CstParser:

  def parse(source: String): CstNode.Document =
    val span = Span(0, source.length)
    if source.isEmpty then CstNode.Document(Chunk.empty, span)
    else CstNode.Document(Chunk(CstNode.Verbatim(source, span)), span)
