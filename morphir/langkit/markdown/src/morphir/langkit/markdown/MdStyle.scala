package morphir.langkit.markdown

import kyo.*

/**
 * How a heading's depth is spelled.
 *
 * Room to grow: `AtxClosed` (`# Title #`) and width-matched setext underlines are future cases, not gaps in this one.
 */
enum HeadingStyle derives CanEqual, Schema:
  /** `#` through `######`. */
  case Atx

  /** `===` / `---` underlines for depths one and two; ATX for depths three and up, which setext cannot express. */
  case Setext

/** How a hard line break is spelled at the end of a line. */
enum HardBreakStyle derives CanEqual, Schema:
  /** A trailing backslash. */
  case Backslash

  /** Two or more trailing spaces. */
  case Spaces

/**
 * House style for the Markdown writer: the syntax spellings [[MdNode]] itself does not carry.
 *
 * `MdNode` carries meaning, not spelling — a [[MdNode.Strong]] node says nothing about whether the source should use
 * `**` or `__`. `MdStyle` is that spelling, read by the writer as a default; a per-node override rides [[MdMeta.data]]
 * through the published keys in [[MdStyleKeys]], and the writer consults the node's data before falling back to the
 * style in scope.
 */
final case class MdStyle(
    bullet: Char = '-',
    emphasisMarker: Char = '*',
    strongMarker: Char = '*',
    fence: Char = '`',
    orderedDelimiter: Char = '.',
    headingStyle: HeadingStyle = HeadingStyle.Atx,
    hardBreak: HardBreakStyle = HardBreakStyle.Backslash,
    /** Whether table columns are padded to a common width. Off writes the narrowest legal table. */
    padTableColumns: Boolean = true,
    /** How many dashes a delimiter cell uses at minimum, before alignment colons. */
    tableDelimiterWidth: Int = 3
) derives CanEqual

object MdStyle:
  given default: MdStyle = MdStyle()

/** [[MetaKey]]s for per-node style overrides, minted once and shared by the writer and its callers. */
object MdStyleKeys:
  val bullet: MetaKey[Char]               = MetaKey[Char]("md.bullet")
  val emphasisMarker: MetaKey[Char]       = MetaKey[Char]("md.emphasisMarker")
  val strongMarker: MetaKey[Char]         = MetaKey[Char]("md.strongMarker")
  val fence: MetaKey[Char]                = MetaKey[Char]("md.fence")
  val orderedDelimiter: MetaKey[Char]     = MetaKey[Char]("md.orderedDelimiter")
  val headingStyle: MetaKey[HeadingStyle] = MetaKey[HeadingStyle]("md.headingStyle")
  val hardBreak: MetaKey[HardBreakStyle]  = MetaKey[HardBreakStyle]("md.hardBreak")
  val padTableColumns: MetaKey[Boolean]   = MetaKey[Boolean]("md.padTableColumns")
  val tableDelimiterWidth: MetaKey[Int]   = MetaKey[Int]("md.tableDelimiterWidth")
