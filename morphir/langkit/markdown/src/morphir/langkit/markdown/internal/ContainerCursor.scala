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
 * One case so far. List items add an indentation prefix, which is the same idea measured in columns rather than in
 * characters, and is why this is a stack of prefixes rather than a block-quote depth.
 */
private[markdown] enum ContainerPrefix derives CanEqual:
  case BlockQuote

  /** Where this prefix ends in `text`, starting at `from`, or [[kyo.Absent]] when the line does not carry it. */
  def consume(text: String, from: Int): Maybe[Int] =
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
    private val prefixes: List[ContainerPrefix]
):

  private val phase = ScanPhase("markdown.lines")

  /**
   * A cursor for the content of a container opening here, reading through `prefix` as well as the ones already open.
   */
  def nested(prefix: ContainerPrefix): ContainerCursor =
    new ContainerCursor(scanner, prefixes :+ prefix)

  def checkpoint(): ScanCheckpoint = scanner.checkpoint()

  def restore(checkpoint: ScanCheckpoint): Unit = scanner.restore(checkpoint)

  /**
   * Whether this container has any line left.
   *
   * At the top level, where no prefix is open, this is the scanner's own question and costs nothing -- which is the
   * case that matters, since most documents open no container at all.
   *
   * Inside a container there is no answering it without looking, so this reads the next line and rewinds. The work that
   * read charges stays charged, because it was genuinely done, and the line is then read a second time by whoever
   * asked: a quoted line costs about twice what an unquoted one does. Caching the lookahead would remove most of that,
   * and is recorded as a candidate for the performance pass rather than guessed at here.
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
    else
      val raw = readRaw()
      if prefixes.isEmpty then Present(ContinuedLine(raw, matchedAll = true))
      else
        @tailrec def consume(remaining: List[ContainerPrefix], cursor: Int): (Int, Boolean) =
          remaining match
            case Nil          => (cursor, true)
            case head :: tail =>
              head.consume(raw.text, cursor) match
                case Present(next) => consume(tail, next)
                case Absent        => (cursor, false)
        val (consumed, matchedAll) = consume(prefixes, 0)
        Present(ContinuedLine(strip(raw, consumed), matchedAll))

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
  private def readRaw(): Line =
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
  def top(scanner: SourceScanner): ContainerCursor = new ContainerCursor(scanner, Nil)
