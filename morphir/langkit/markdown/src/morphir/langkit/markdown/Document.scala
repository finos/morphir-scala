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
  case UnorderedList(items: Chunk[ListItem], span: Span)
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
