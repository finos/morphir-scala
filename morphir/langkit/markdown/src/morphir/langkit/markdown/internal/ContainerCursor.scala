package morphir.langkit.markdown.internal

import kyo.*
import scala.annotation.tailrec
import morphir.langkit.core.Span
import morphir.langkit.core.scanner.*

/**
 * One line of source, carrying the span it was read from.
 *
 * The span is what keeps a block's coordinates true after a container has taken its marker off the front: stripping
 * `> ` shortens the text and moves the offset by the same amount, so an inline node found in the remainder still points
 * at the character the author typed.
 */
private[markdown] final case class Line(view: SourceView, text: String, terminatedByLf: Boolean):
  def offset: Int = view.start.toInt
  def end: Int    = view.end.toInt
  def length: Int = view.length.toInt

/**
 * A line read through the open containers, and whether every one of them claimed it.
 *
 * `matchedAll` is false for a lazy continuation: `> foo` followed by a bare `bar` is one paragraph inside the quote,
 * even though the second line drops the marker. Only a paragraph may take such a line, which is why the flag is
 * reported rather than acted on here.
 */
private[markdown] final case class ContinuedLine(line: Line, matchedAll: Boolean)

/**
 * The marker a container puts at the front of each of its lines.
 *
 * A block quote writes the same `>` on every line. A list item writes its marker once and then indents, which is why
 * [[ContainerPrefix.ListItem]] carries where that marker is: reading at that offset consumes the marker, and reading
 * anywhere else consumes the indentation it established. Keying on the offset rather than on "have I read a line yet"
 * is what makes the decision survive a rewind -- the cursor is read and rewound constantly, and a flag would go stale
 * the first time that happened.
 */
private[markdown] enum ContainerPrefix derives CanEqual:
  case BlockQuote

  /**
   * A list item's content, which begins `columns` characters into the line that carries `markerAt`.
   *
   * `columns` counts the indentation, the marker and the spaces after it, all of which the item spends rather than
   * holds. It is the same number on the marker line and on every continuation line, which is why one field does for
   * both.
   */
  case ListItem(markerAt: Int, columns: Int)

  /**
   * Where this prefix ends in `text`, starting at `from`, or [[kyo.Absent]] when the line does not carry it.
   *
   * `absoluteStart` is the source offset of `text.charAt(0)`, so a prefix can tell which line it is looking at.
   */
  def consume(text: String, from: Int, absoluteStart: Int): Maybe[Int] =
    this match
      case BlockQuote =>
        val marker = ContainerPrefix.skipSpaces(text, from, 3)
        if marker < text.length && text.charAt(marker) == '>' then
          // The single space after the marker belongs to it, not to the content: `>  foo` is a paragraph indented by
          // one space, and `>     foo` is indented code inside the quote.
          val afterMarker = marker + 1
          if afterMarker < text.length && text.charAt(afterMarker) == ' ' then Present(afterMarker + 1)
          else Present(afterMarker)
        else Absent

      case ListItem(markerAt, columns) =>
        if absoluteStart + from == markerAt then
          // The item's own line: the marker stands where the indentation will. An empty item has less line than that.
          Present(math.min(from + columns, text.length))
        else
          val indented = ContainerPrefix.skipSpaces(text, from, columns)
          // A blank line carries no indentation and still belongs to the item, which is what lets an item hold two
          // paragraphs. Whether that makes the list loose is decided by what follows, not here.
          if indented - from == columns || text.substring(indented).isBlank then Present(indented)
          else Absent

private[markdown] object ContainerPrefix:
  @tailrec private[internal] def skipSpaces(text: String, from: Int, remaining: Int): Int =
    if remaining > 0 && from < text.length && text.charAt(from) == ' ' then skipSpaces(text, from + 1, remaining - 1)
    else from

/**
 * The source of lines for one block context, bounded by the containers it sits inside.
 *
 * A container's content is not a contiguous range of the source -- `> foo` and `> bar` are two spans with the markers
 * between them -- so a container cannot be parsed by handing an inner parser a sub-range. What it can be handed is a
 * cursor that removes the markers as it reads, which is what this is. Every charge goes to the one
 * [[morphir.langkit.core.scanner.SourceScanner]] underneath, so nesting cannot be used to buy a fresh budget, and depth
 * is declared to it through `withNesting`.
 *
 * The cursor itself is immutable: `nested` returns a new one over the same scanner. The position is the scanner's, so a
 * nested cursor reads on from wherever its parent left off, and `restore` on either rewinds both.
 */
private[markdown] final class ContainerCursor private (
    val scanner: SourceScanner,
    private val prefixes: List[ContainerPrefix],
    private val lines: LineCache
):

  private val phase = ScanPhase("markdown.lines")

  /**
   * A cursor for the content of a container opening here, reading through `prefix` as well as the ones already open.
   */
  def nested(prefix: ContainerPrefix): ContainerCursor =
    new ContainerCursor(scanner, prefixes :+ prefix, lines)

  def checkpoint(): ScanCheckpoint = scanner.checkpoint()

  def restore(checkpoint: ScanCheckpoint): Unit = scanner.restore(checkpoint)

  /**
   * Whether this container has any line left.
   *
   * At the top level, where no prefix is open, this is the scanner's own question and costs nothing. Inside a container
   * there is no answering it without looking, so this reads the next line and rewinds -- which is cheap, because the
   * scan is shared: see [[LineCache]].
   */
  def isAtEnd: Boolean =
    if scanner.isAtEnd then true
    else if prefixes.isEmpty then false
    else
      val saved     = scanner.checkpoint()
      val continued = readContinued()
      scanner.restore(saved)
      !continued.exists(_.matchedAll)

  /**
   * The next line belonging to this container, with the open markers removed.
   *
   * [[kyo.Absent]] means the container is over, either because the input is or because the line dropped a marker. The
   * scanner is rewound in the second case, so the line is still there for whoever owns it.
   */
  def readLine(): Maybe[Line] =
    val saved = scanner.checkpoint()
    readContinued() match
      case Present(continued) if continued.matchedAll => Present(continued.line)
      case Present(_)                                 =>
        scanner.restore(saved)
        Absent
      case Absent => Absent

  /**
   * The next line with as much of the open markers removed as it carries, whatever it carries.
   *
   * For paragraphs, which are the only blocks a line may continue without repeating the markers.
   */
  def readContinued(): Maybe[ContinuedLine] =
    if scanner.isAtEnd then Absent
    else Present(readFresh())

  private def readFresh(): ContinuedLine =
    val raw = lines.lineAt(this)
    if prefixes.isEmpty then ContinuedLine(raw, matchedAll = true)
    else
      @tailrec def consume(remaining: List[ContainerPrefix], cursor: Int): (Int, Boolean) =
        remaining match
          case Nil          => (cursor, true)
          case head :: tail =>
            head.consume(raw.text, cursor, raw.offset) match
              case Present(next) => consume(tail, next)
              case Absent        => (cursor, false)
      val (consumed, matchedAll) = consume(prefixes, 0)
      ContinuedLine(strip(raw, consumed), matchedAll)

  /** Where the scanner has read to, with the line terminator that got it there discounted. */
  def consumedEnd: Int =
    val offset = scanner.offset.toInt
    val source = scanner.source
    if offset > 0 && source.charAt(offset - 1) == '\n' then
      if offset > 1 && source.charAt(offset - 2) == '\r' then offset - 2 else offset - 1
    else offset

  private def strip(line: Line, consumed: Int): Line =
    if consumed == 0 then line
    else
      val view = scanner.view(Span.fromStartEnd(line.offset + consumed, line.end))
      Line(view, view.text, line.terminatedByLf)

  /** One raw line, terminator consumed but not counted as text, and CRLF reported as its content alone. */
  private[internal] def readRaw(): Line =
    scanner.requireProgress(phase) {
      val start                 = scanner.mark
      var previous: Maybe[Char] = Absent
      var terminatedByLf        = false
      var acquisitionRunning    = true
      while acquisitionRunning do
        scanner.peek() match
          case Present(char) =>
            scanner.advance()
            if char == '\n' then
              terminatedByLf = true
              acquisitionRunning = false
            else previous = Present(char)
          case Absent => acquisitionRunning = false

      val rawEnd  = scanner.offset.toInt - (if terminatedByLf then 1 else 0)
      val textEnd =
        if terminatedByLf && previous.contains('\r') then rawEnd - 1
        else rawEnd
      val view = scanner.view(Span.fromStartEnd(start.toInt, textEnd))
      scanner.chargeWork(WorkUnits.from(view.length.toInt.toLong).getOrThrow)
      Line(view, view.text, terminatedByLf)
    }
end ContainerCursor

private[markdown] object ContainerCursor:
  /** A cursor at the top level of a document, where no container is open. */
  def top(scanner: SourceScanner): ContainerCursor = new ContainerCursor(scanner, Nil, LineCache())

/**
 * The last raw line read, so that no line of the source is scanned twice.
 *
 * A line is looked at by more cursors than one. The document's cursor reads it to see what it starts; the item's cursor
 * reads it again to take the marker off; the list reads it a third time to ask whether another item begins there. What
 * differs between them is which prefixes they strip, which is a handful of characters -- the scan that produced the
 * line is identical, so it is shared rather than repeated. A run of list items cost about four scans a line before
 * this, which was most of what parsing them cost at all.
 *
 * One entry is enough because the repeats are of the *same* line: the cursors take turns at one offset, then all move
 * on together. Keyed on the offset rather than on a flag, because the scanner is rewound constantly and only the offset
 * survives that; and replaying is a rewind to `resume` rather than a re-read, so a line is charged for once, when it
 * was genuinely read.
 */
private[internal] final class LineCache:
  private var at: Int                       = -1
  private var line: Maybe[Line]             = Absent
  private var resume: Maybe[ScanCheckpoint] = Absent

  def lineAt(cursor: ContainerCursor): Line =
    val offset = cursor.scanner.offset.toInt
    line match
      case Present(cached) if at == offset =>
        resume.foreach(cursor.scanner.restore)
        cached
      case _ =>
        val read = cursor.readRaw()
        at = offset
        resume = Present(cursor.scanner.checkpoint())
        line = Present(read)
        read
