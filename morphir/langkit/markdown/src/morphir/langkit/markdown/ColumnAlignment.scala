package morphir.langkit.markdown

import kyo.*

/**
 * How a table column's cells are aligned, as its delimiter row spelled it.
 *
 * Carried as a [[kyo.Maybe]] wherever it appears, because a column may set none: `---` is a column with no alignment,
 * which is a different thing from a column aligned left. HTML renders the first with no attribute at all.
 */
enum ColumnAlignment derives CanEqual, Schema:
  /** `:---` */
  case Left

  /** `---:` */
  case Right

  /** `:---:` */
  case Center

object ColumnAlignment:
  /**
   * The alignment one delimiter-row cell spells, or Absent when it spells none.
   *
   * Returns Absent for a cell that is not a delimiter at all — no dashes, or a character that is neither a dash nor a
   * leading or trailing colon — so a caller distinguishes "no alignment" from "not a delimiter row" by checking the
   * cell separately. [[isDelimiterCell]] is that check.
   */
  def of(cell: String): Maybe[ColumnAlignment] =
    val trimmed = cell.trim
    val left    = trimmed.startsWith(":")
    val right   = trimmed.endsWith(":") && trimmed.length > 1
    if left && right then Present(Center)
    else if left then Present(Left)
    else if right then Present(Right)
    else Absent

  /** Whether a delimiter-row cell is well formed: optional colons around one or more dashes, and nothing else. */
  def isDelimiterCell(cell: String): Boolean =
    val trimmed = cell.trim
    val body    = trimmed.stripPrefix(":").stripSuffix(":")
    body.nonEmpty && body.forall(_ == '-')
