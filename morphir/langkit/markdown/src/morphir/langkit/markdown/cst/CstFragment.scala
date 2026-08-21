package morphir.langkit.markdown.cst

import kyo.*
import morphir.langkit.core.Span
import morphir.langkit.markdown.HeadingLevel

/**
 * What a Parser construction site knows about a graduated block, as spans into the source.
 *
 * Fragments carry decisions, not text: the parser records which form it read and where, and [[CstParser]] turns that
 * into leaves by slicing the source. That keeps the parser's CST duty to one `record` call per site and keeps all
 * leaf-splitting in this package, where later slices refine it. Only top-level sites record — a construct inside a
 * container spans marker bytes it does not own, which is lc8.22's problem, not this slice's.
 */
private[markdown] enum CstFragment:
  case ThematicBreak(span: Span)

  /** `content` is the heading text between the opening marker run and any closing sequence. */
  case AtxHeading(level: HeadingLevel, span: Span, content: Span)

  /** `underlineOffset` is where the `===` or `---` line starts inside `span`. */
  case SetextHeading(level: HeadingLevel, span: Span, underlineOffset: Int)

  /** `openEnd` ends the opening fence line; `closeStart` begins the closing one, absent when the fence ran out. */
  case FencedCode(span: Span, openEnd: Int, closeStart: Maybe[Int])

  case IndentedCode(span: Span)
  case Paragraph(span: Span)

  def span: Span

/** Collects fragments in source order while the parser runs. Threaded like the `definitions` map. */
private[markdown] final class CstCollector:
  private val buffer                      = List.newBuilder[CstFragment]
  def record(fragment: CstFragment): Unit = buffer += fragment
  def fragments: Chunk[CstFragment]       = Chunk.from(buffer.result())
