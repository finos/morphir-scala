package morphir.langkit.markdown

import kyo.*
import morphir.langkit.core.Span

/** A markdown document as a sequence of blocks. */
final case class Document(blocks: Chunk[Block], span: Span) derives CanEqual

enum Block derives CanEqual:
  case Heading(level: Int, text: String, span: Span)
  case Paragraph(text: String, span: Span)
  case FencedCode(info: FenceInfo, content: String, span: Span)
  case UnorderedList(items: Chunk[String], span: Span)
  case ThematicBreak(span: Span)
