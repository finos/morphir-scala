package morphir.langkit.markdown.internal

import kyo.*
import scala.annotation.tailrec
import morphir.langkit.core.Span
import morphir.langkit.core.scanner.*
import morphir.langkit.markdown.*

/**
 * GFM pipe tables: splitting a row into cells, recognizing a delimiter row under a candidate header, and reading the
 * body rows a delimiter row promotes into a table.
 *
 * Split out of [[Parser]], which still owns the paragraph-gathering loop that decides a table has opened at all and
 * calls back into [[readTable]] once it has.
 */
private[internal] object TableRows:
  import Parser.{Classified, Deferred, LineKind, classify}

  /**
   * One table row's cells, as ranges into `line` with the padding around each already trimmed off.
   *
   * A backslash before a pipe means the pipe is content — spec example 200 puts one inside a code span — so an escaped
   * pipe never splits a cell. Splitting only skips it; the backslash itself is left in the range this function returns.
   * [[cellSites]] is what strips it, once it knows whether a CST is being built and needs the consumed escape recorded.
   *
   * Outer pipes are optional and each produces an empty cell when present. The first and last cell is dropped when it
   * is empty and there was a split to produce it, which is what tells `| a |` (one cell) from `a | b` (two) and from
   * `|| a |` (two, the first genuinely empty) — and, unlike testing the line's own first and last character, from
   * `| a \|`, whose trailing pipe is content rather than a closing delimiter.
   */
  private[internal] def tableCellSpans(line: String): Chunk[(start: Int, end: Int)] =
    val bounds = List.newBuilder[(start: Int, end: Int)]

    // `escaped` rides the recursion as a parameter rather than sitting outside it as a flag, which is the whole
    // readability argument for the tail-recursive form: the state a step depends on is visible in its own signature.
    @tailrec
    def loop(index: Int, cellStart: Int, escaped: Boolean): Unit =
      if index >= line.length then bounds += ((start = cellStart, end = index))
      else
        val char = line.charAt(index)
        if escaped then loop(index + 1, cellStart, escaped = false)
        else if char == '\\' then loop(index + 1, cellStart, escaped = true)
        else if char == '|' then
          bounds += ((start = cellStart, end = index))
          loop(index + 1, index + 1, escaped = false)
        else loop(index + 1, cellStart, escaped = false)

    loop(0, 0, escaped = false)
    val all             = Chunk.from(bounds.result()).map(trimmedCell(line, _))
    val withoutLeading  = if all.size > 1 && isEmptyCell(all.head) then all.drop(1) else all
    val withoutTrailing =
      if withoutLeading.size > 1 && isEmptyCell(withoutLeading.last) then withoutLeading.dropRight(1)
      else withoutLeading
    withoutTrailing

  private def isEmptyCell(cell: (start: Int, end: Int)): Boolean = cell.end <= cell.start

  /** `cell` with the spaces and tabs at either end taken off; they are padding the author spent, not content. */
  private def trimmedCell(line: String, cell: (start: Int, end: Int)): (start: Int, end: Int) =
    @tailrec def left(index: Int): Int =
      if index < cell.end && isSpaceOrTab(line.charAt(index)) then left(index + 1) else index
    @tailrec def right(index: Int, floor: Int): Int =
      if index > floor && isSpaceOrTab(line.charAt(index - 1)) then right(index - 1, floor) else index
    val start = left(cell.start)
    (start = start, end = right(cell.end, start))

  private def isSpaceOrTab(char: Char): Boolean = char == ' ' || char == '\t'

  /**
   * A row's cells as text, for the counting and shape checks the delimiter row is judged by.
   *
   * Read off `view.text`, the raw line, rather than off the tab-expanded `text`: a table row's cells are located by
   * character, and expanding a structural tab to the columns it occupies would move every offset after it.
   */
  private def tableCellTexts(line: Line): Chunk[String] =
    val raw = line.view.text
    tableCellSpans(raw).map(cell => raw.substring(cell.start, cell.end))

  /**
   * The alignments a line spells as a delimiter row under `header`, or [[kyo.Absent]] when it is not one.
   *
   * A table opens where a setext heading does — on the line below the one it promotes — and for the same reason:
   * nothing about `| a | b |` says it is a header until the delimiter row underneath says so. cmark-gfm requires the
   * header to be the paragraph's only line, so a table never interrupts a paragraph that already has content, and it
   * requires the two rows to agree on how many cells they hold (spec example 203).
   *
   * `LineKind.Text` is the whole of "opens nothing else": an indented line is code, and anything the base grammar
   * claims — a fence, a quote, a list marker — it keeps, since the extension hook in cmark-gfm runs after all of them.
   * A setext underline is judged before this is even asked, one level up, which is what keeps a bare `---` under a
   * one-cell header a heading rather than a table.
   */
  private[internal] def tableAlignmentsOf(
      header: Line,
      line: Line,
      continued: ContinuedLine,
      classified: Classified,
      segmentCount: Int,
      profile: MdProfile
  ): Maybe[Chunk[Maybe[ColumnAlignment]]] =
    if !profile.supports(MdExtension.Tables) || segmentCount != 1 || !continued.matchedAll
      || classified.kind != LineKind.Text
    then Absent
    else
      val delimiterCells = tableCellTexts(line)
      if delimiterCells.nonEmpty && delimiterCells.size == tableCellTexts(header).size &&
        delimiterCells.forall(ColumnAlignment.isDelimiterCell)
      then Present(delimiterCells.map(ColumnAlignment.of))
      else Absent

  /**
   * `raw[start, end)` with every escaped pipe unescaped, for a table cell's text.
   *
   * Walks with the same escaped-state recursion [[tableCellSpans]] uses to find the cell boundaries in the first place
   * — splitting and stripping are the same question asked of the same characters, "is this backslash consumed by the
   * pipe after it". Only `\|` strips: GFM gives that treatment to pipes alone, so any other backslash is copied through
   * for ordinary inline escaping to handle, the same as outside a table.
   *
   * `text` is `raw[start, end)` with each stripped backslash gone. `offsetAt` maps an index in `text` — `0` through
   * `text.length` inclusive — back to its offset in `raw`, the same contract [[InlineParser.parse]]'s `sourceOffsetAt`
   * asks of every caller; a cell that stripped a pipe hands it a shorter text against a longer source exactly as a
   * joined multi-line paragraph already does.
   *
   * The stripped pipe's own entry in `offsetAt` points at the backslash that used to precede it, not at the pipe itself
   * — the same span an ordinary backslash escape reports for the character it makes literal (`InlineParser`'s own
   * escape handling spans `[backslash, escaped character]`, two source characters for the one text character it
   * produces). Emphasis and strong emphasis are reconstructed in the CST from their content's own mapped span, measured
   * `used` delimiters back from wherever that span starts (`delimitedExtent` in `Cst.scala`) — a convention that only
   * holds when a content span's start sits flush against its delimiters with nothing hidden in between. A `|` whose
   * offset pointed at itself would open a one-character gap no delimiter run actually has, so `**\|**` would fail to
   * reconstruct as strong emphasis around a literal pipe; pointing at the backslash instead keeps the gap at zero, the
   * same as it is for `**\*text**` today.
   *
   * `escapes` is the `raw` offset of each stripped backslash, in order, for [[InlineNotes.recordEscape]] — the offset
   * of the backslash itself, not the pipe it makes literal.
   */
  private def stripEscapedPipes(
      raw: String,
      start: Int,
      end: Int
  ): (text: String, offsetAt: Int => Int, escapes: Chunk[Int]) =
    val text    = StringBuilder()
    val offsets = List.newBuilder[Int]
    val escapes = List.newBuilder[Int]

    @tailrec
    def loop(index: Int, escaped: Boolean): Unit =
      if index >= end then offsets += index
      else
        val char = raw.charAt(index)
        if escaped then
          offsets += index
          text.append(char)
          loop(index + 1, escaped = false)
        else if char == '\\' && index + 1 < end && raw.charAt(index + 1) == '|' then
          // Both the backslash and the pipe are consumed in one step, rather than deferring the pipe to the next
          // step under `escaped`: the pipe's own offset entry has to point at `index` (the backslash), not at
          // `index + 1` (itself), and folding the two characters together here is what lets it.
          escapes += index
          offsets += index
          text.append('|')
          loop(index + 2, escaped = false)
        else if char == '\\' then
          offsets += index
          text.append(char)
          loop(index + 1, escaped = true)
        else
          offsets += index
          text.append(char)
          loop(index + 1, escaped = false)

    loop(start, escaped = false)
    val offsetArray = offsets.result().toArray
    (text = text.toString, offsetAt = offsetArray(_), escapes = Chunk.from(escapes.result()))

  /**
   * One cell of a row being read: where its content sits, the map from its (possibly escape-stripped) text back to the
   * source, and the slot its prose will fill.
   */
  private final case class CellSite(
      span: Span,
      text: String,
      map: Int => Int,
      slot: InlineSlot,
      notes: Maybe[InlineNotes]
  )

  private def cellSites(line: Line, cst: Maybe[CstCollector]): Chunk[CellSite] =
    val source = line.view.text
    tableCellSpans(source).map { cell =>
      val span     = Span.fromStartEnd(line.offset + cell.start, line.offset + cell.end)
      val notes    = cst.map(_ => InlineNotes())
      val stripped = stripEscapedPipes(source, cell.start, cell.end)
      stripped.escapes.foreach(offset => notes.foreach(_.recordEscape(line.offset + offset)))
      // The cheap `span.offset + index` form covers the common case of a cell with no escaped pipe; only a cell that
      // stripped one needs the source-offset map `stripped.offsetAt` computed for it.
      val map: Int => Int =
        if stripped.escapes.isEmpty then index => span.offset + index
        else index => line.offset + stripped.offsetAt(index)
      CellSite(span = span, text = stripped.text, map = map, slot = InlineSlot(), notes = notes)
    }

  /**
   * The body rows under a delimiter row, and the table they and the header make.
   *
   * `header` is the line the delimiter row promoted and `delimiter` the delimiter row itself, both already consumed.
   * Every following line the cursor offers is a row until one of three things happens: the input ends, a blank line
   * arrives, or a line opens another block — spec example 201's `> bar`. A row that does not spell as many cells as the
   * header did is padded and one that spells more is truncated, so the table is rectangular however the source was
   * written.
   *
   * Each cell is a prose block of its own with its own [[InlineSlot]], filled in the deferred phase exactly as a
   * paragraph's is: a cell may hold a link whose definition appears later in the document.
   */
  private[internal] def readTable(
      cursor: ContainerCursor,
      header: Line,
      delimiter: Line,
      align: Chunk[Maybe[ColumnAlignment]],
      cst: Maybe[CstCollector],
      profile: MdProfile
  ): Deferred =
    val scanner = cursor.scanner
    val body    = List.newBuilder[Line]

    @tailrec def rows(): Unit =
      val checkpoint = cursor.checkpoint()
      cursor.readLine() match
        case Absent        => cursor.restore(checkpoint)
        case Present(line) =>
          if classify(scanner, line).kind == LineKind.Text then
            body += line
            scanner.chargeOutputNodes(NodeCount.one)
            rows()
          else cursor.restore(checkpoint)

    rows()
    val bodyLines = Chunk.from(body.result())
    val span      = Span.fromStartEnd(header.offset, cursor.consumedEnd)

    val headerSites                                                = cellSites(header, cst)
    val bodySites                                                  = bodyLines.map(cellSites(_, cst))
    def rowRecord(line: Line, sites: Chunk[CellSite]): CstTableRow =
      CstTableRow(Span.fromStartEnd(line.offset, line.end), sites.map(site => CstTableCell(site.span, site.slot)))

    cst.foreach(_.record(CstFragment.Table(
      span,
      Span.fromStartEnd(delimiter.offset, delimiter.end),
      rowRecord(header, headerSites),
      Chunk.from(bodyLines.zip(bodySites).map((line, sites) => rowRecord(line, sites)))
    )))

    Deferred.prose { defs =>
      def rowNode(line: Line, sites: Chunk[CellSite]): MdNode.TableRow =
        val cells = sites.map { site =>
          val content = InlineParser.parse(site.text, site.map, defs, site.notes)(using
            profile
          )
          site.slot.fill(content, site.notes)
          MdNode.TableCell(content, MdMeta.at(site.span))
        }
        MdNode.TableRow(
          fittedCells(cells, align.size, MdNode.TableCell(Chunk.empty)),
          MdMeta.at(Span.fromStartEnd(line.offset, line.end))
        )
      MdNode.Table(
        align,
        rowNode(header, headerSites),
        Chunk.from(bodyLines.zip(bodySites).map((line, sites) => rowNode(line, sites))),
        MdMeta.at(span)
      )
    }
