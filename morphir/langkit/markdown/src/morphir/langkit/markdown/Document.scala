package morphir.langkit.markdown

import kyo.*
import morphir.langkit.core.Span

/** A markdown document as a sequence of blocks. */
final case class Document(blocks: Chunk[Block], span: Span) derives CanEqual

/**
 * A block-level construct.
 *
 * Blocks that hold prose hold [[Inline]] content rather than a `String`, so a later slice can add code spans, links and
 * emphasis without changing this shape again. `FencedCode` is the exception on purpose: its body is literal text, never
 * scanned for inline constructs.
 */
enum Block derives CanEqual:
  case Heading(level: HeadingLevel, content: Chunk[Inline], span: Span)
  case Paragraph(content: Chunk[Inline], span: Span)
  case FencedCode(info: FenceInfo, content: String, span: Span)

  /**
   * A code block written by indenting four spaces rather than by fencing.
   *
   * A distinct case because the source forms differ and a CST should say which was written, even though CommonMark
   * renders both as `pre > code`. The fold maps this to the same algebra method as a fence with no info string, so no
   * output target has to know the difference.
   */
  case IndentedCode(content: String, span: Span)
  case UnorderedList(items: Chunk[ListItem], span: Span)

  /**
   * A numbered list.
   *
   * `start` is the first marker's number, which HTML needs as a `start` attribute whenever it is not 1. A change of
   * delimiter — `.` to `)` — begins a new list, which is why example 302 renders two.
   */
  case OrderedList(start: Int, items: Chunk[ListItem], span: Span)

  /**
   * A run of raw HTML the document wrote itself.
   *
   * `content` is emitted verbatim, never escaped and never scanned for Markdown: that is the whole point of the form.
   * It is the one place a writer legitimately passes source text straight through.
   */
  case HtmlBlock(content: String, span: Span)

  case ThematicBreak(span: Span)

/**
 * One entry of a bullet list.
 *
 * Its own type rather than a bare `Chunk[Inline]`, because CommonMark list items hold blocks once loose lists and
 * nesting arrive. Widening `content` then leaves [[Block.UnorderedList]] untouched.
 */
final case class ListItem(content: Chunk[Inline], span: Span) derives CanEqual

/**
 * An inline construct: the content of a block that holds prose.
 *
 * Only `Text` exists so far, so a parse yields exactly one node per block and inline markers stay unparsed inside its
 * value. Code spans, links, images and emphasis each arrive as a further case.
 */
enum Inline derives CanEqual:
  case Text(value: String, span: Span)

  /**
   * Backtick-delimited literal text.
   *
   * `value` is the content CommonMark says to render: line endings already turned into spaces, and one space removed
   * from each end when both ends had one. `span` covers the whole construct including its backticks.
   */
  case CodeSpan(value: String, span: Span)

  /**
   * A link, from either the `[text](destination)` form or an autolink.
   *
   * `destination` is already normalised as a URI: percent-encoded where a URI cannot carry a character literally, but
   * with `&` left alone for the writer to HTML-escape. `content` is the label's own inline content, which is why a code
   * span inside a label survives. An autolink arrives here too, with its raw URI as the content.
   */
  case Link(destination: String, title: Maybe[String], content: Chunk[Inline], span: Span)

  /**
   * An image.
   *
   * `alt` is flattened plain text, not inline content, because that is what an `alt` attribute can hold: a link or a
   * further image inside the label contributes only its text.
   */
  case Image(destination: String, title: Maybe[String], alt: String, span: Span)

  /** Emphasis, from a single `*` or `_` delimiter run. */
  case Emphasis(content: Chunk[Inline], span: Span)

  /** Strong emphasis, from a double `**` or `__` delimiter run. */
  case StrongEmphasis(content: Chunk[Inline], span: Span)
