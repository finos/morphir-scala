package morphir.langkit.markdown.internal

import kyo.*
import morphir.langkit.core.Span
import morphir.langkit.markdown.HeadingLevel
import morphir.langkit.markdown.ListStart
import morphir.langkit.markdown.MdNode

/**
 * What a Parser construction site knows about a graduated block, as spans into the source.
 *
 * Fragments carry decisions, not text: the parser records which form it read and where, and [[CstParser]] turns that
 * into leaves by slicing the source. That keeps the parser's CST duty to one `record` call per site and keeps all
 * leaf-splitting in this package, where later slices refine it.
 *
 * Containers nest: a [[BlockQuote]] or [[ListItem]] carries the fragments its own run recorded, plus the `markers` its
 * cursor spent taking prefixes off each line. A child's span covers marker bytes the child does not own — `> foo` over
 * `> bar` is one paragraph spanning the middle `> ` — so [[CstParser]] punches the recorded marker spans out of gaps
 * and leaf interiors as [[MdCstNode.Token]] leaves, and the tiling invariant carries the container problem.
 */
/**
 * One marker a container's cursor spent: its bytes, and the columns its final tab reached past what the container
 * claimed. A tab is consumed by column, not by character, so a marker that needs two columns of a four-column tab owns
 * the whole character and owes the content two `phantom` columns that exist as layout rather than as bytes.
 */
private[markdown] type Marker = (span: Span, phantom: Int)

private[markdown] enum CstFragment:

  /**
   * A frontmatter block the profile recognized at the document's head.
   *
   * `openEnd` ends the opening delimiter line, terminator included; `closeStart` begins the closing one. Both are
   * present because the block is only ever recorded when it closed — an unclosed one is not frontmatter at all — which
   * is why neither is a [[kyo.Maybe]] the way a fence's `closeStart` is.
   */
  case Frontmatter(span: Span, openEnd: Int, closeStart: Int)

  case ThematicBreak(span: Span)

  /** `content` is the heading text between the opening marker run and any closing sequence. */
  case AtxHeading(level: HeadingLevel, span: Span, content: Span, inlines: InlineSlot)

  /** `underlineOffset` is where the `===` or `---` line starts inside `span`. */
  case SetextHeading(level: HeadingLevel, span: Span, underlineOffset: Int, inlines: InlineSlot)

  /** `openEnd` ends the opening fence line; `closeStart` begins the closing one, absent when the fence ran out. */
  case FencedCode(span: Span, openEnd: Int, closeStart: Maybe[Int])

  case IndentedCode(span: Span)
  case Paragraph(span: Span, inlines: InlineSlot)

  /** A raw HTML block, whatever start condition opened it. Its interior is HTML, not Markdown. */
  case HtmlBlock(span: Span)

  /**
   * One `[label]: destination "title"` definition. Recorded where it was consumed — off the front of a paragraph run —
   * so the paragraph that follows starts after it. The reference stays unresolved here; resolution is lowering's.
   */
  case LinkReferenceDefinition(span: Span)

  /** A quote's run: `markers` are the per-line `>` prefixes its cursor spent, `children` what those lines held. */
  case BlockQuote(span: Span, markers: Chunk[Marker], children: Chunk[CstFragment])

  /** A run of bullet items sharing one bullet. `tight` mirrors what the AST list records. */
  case BulletList(bullet: Char, tight: Boolean, span: Span, items: Chunk[CstFragment])

  /** A run of numbered items sharing one delimiter, starting at `start`. */
  case OrderedList(start: ListStart, delimiter: Char, tight: Boolean, span: Span, items: Chunk[CstFragment])

  /** One item: `markers` cover its marker line's prefix and each continuation line's indentation. */
  case ListItem(span: Span, markers: Chunk[Marker], children: Chunk[CstFragment])

  def span: Span

/**
 * The inline content of one prose block, filled in when the block's deferred prose resolves.
 *
 * Block structure is read in one pass but prose is parsed only after every link definition in the document is known, so
 * a prose fragment cannot carry its inlines at record time. The slot is created with the fragment, captured by the
 * block's resolve closure, and filled exactly once when that closure runs — which is before [[CstParser]] reads it,
 * because `parseFragments` resolves every block before returning. The inline nodes carry source spans, which is all
 * materialization needs to tile a prose interior.
 */
private[markdown] final class InlineSlot:
  private var slot: Maybe[Chunk[MdNode.PhrasingContent]] = Absent
  private var noted: Maybe[InlineNotes]                  = Absent

  def fill(content: Chunk[MdNode.PhrasingContent], notes: Maybe[InlineNotes]): Unit =
    slot = Present(content)
    noted = notes

  def content: Maybe[Chunk[MdNode.PhrasingContent]] = slot
  def notes: Maybe[InlineNotes]                     = noted

/**
 * Collects fragments in source order while the parser runs. Threaded like the `definitions` map.
 *
 * Each container run gets its own collector, so nesting is recorded by construction. Marker spans arrive from the
 * container's cursor, which may read, rewind and re-read a line any number of times: keying on the offset makes the
 * repeats idempotent, and a peek past the container's end is clamped away at materialization rather than here.
 */
private[markdown] final class CstCollector:
  private val buffer                                          = List.newBuilder[CstFragment]
  private val markerSpans                                     = scala.collection.mutable.TreeMap.empty[Int, (Int, Int)]
  def record(fragment: CstFragment): Unit                     = buffer += fragment
  def recordMarker(offset: Int, end: Int, phantom: Int): Unit = markerSpans(offset) = (end, phantom)
  def fragments: Chunk[CstFragment]                           = Chunk.from(buffer.result())
  def markers: Chunk[Marker]                                  =
    Chunk.from(markerSpans.iterator.map((o, v) => (span = Span.fromStartEnd(o, v._1), phantom = v._2)))
