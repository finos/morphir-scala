package morphir.langkit.markdown.cst

import kyo.*
import morphir.langkit.core.Span
import morphir.langkit.markdown.Inline

/**
 * Which of CommonMark's four link forms the author wrote.
 *
 * The AST does not care — every form resolves to a destination and a title — but a CST must say which was written,
 * because the four spell differently: `[t](/u)`, `[t][label]`, `[t][]` and `[t]`.
 */
enum LinkForm derives CanEqual:
  case Inline, ReferenceFull, ReferenceCollapsed, ReferenceShortcut

/**
 * What the inline parser knew about one link or image at the moment it succeeded, beyond what the AST keeps.
 *
 * All spans are source coordinates. `content` sits between the brackets; `destination` and `title` are present only for
 * the inline form (reference forms spell theirs in the definition); `reference` is the full form's second label,
 * between its own brackets; `alt` is an image's label parsed as inline nodes, which the AST flattens to a string.
 */
private[markdown] final case class LinkNote(
    form: LinkForm,
    content: Span,
    destination: Maybe[(span: Span, angled: Boolean)],
    title: Maybe[Span],
    reference: Maybe[Span],
    alt: Maybe[Chunk[Inline]]
)

/**
 * Construct details recorded while a prose block's inlines parse, keyed by source offset.
 *
 * Inline parsing speculates: an outer link may parse its content, record what it finds, and then fail, leaving the
 * inner constructs to be parsed again on their own. Keying by source offset makes the repeats idempotent — the same
 * construct at the same offset records the same note.
 */
private[markdown] final class InlineNotes:
  private val links    = scala.collection.mutable.Map.empty[Int, LinkNote]
  private val escapes  = scala.collection.mutable.TreeSet.empty[Int]
  private val entities = scala.collection.mutable.TreeMap.empty[Int, Int]

  def recordLink(offset: Int, note: LinkNote): Unit = links(offset) = note

  def linkAt(offset: Int): Maybe[LinkNote] = links.get(offset) match
    case Some(note) => Present(note)
    case None       => Absent

  /** A backslash escape at `offset`: the backslash, then the character it makes literal. */
  def recordEscape(offset: Int): Unit = escapes += offset

  /** A character reference over `[offset, end)`, as written. */
  def recordEntity(offset: Int, end: Int): Unit = entities(offset) = end

  /** Escape offsets in order, for punching out of prose gaps. */
  def escapeOffsets: Chunk[Int] = Chunk.from(escapes.iterator)

  /** Entity spans in order, for punching out of prose gaps. */
  def entitySpans: Chunk[Span] = Chunk.from(entities.iterator.map((o, e) => Span.fromStartEnd(o, e)))
