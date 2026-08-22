package morphir.langkit.markdown

import kyo.*

/**
 * A CommonMark heading level, always between one and six.
 *
 * Replaces a bare `Int` on [[MdNode.Heading]] so a level the spec cannot express is unrepresentable. CommonMark caps
 * both ATX and setext headings at six, and treats a longer run of `#` as a paragraph rather than a deeper heading.
 */
opaque type HeadingLevel = Int

object HeadingLevel:

  /** The only way in from an arbitrary number: [[kyo.Absent]] for a level outside one to six. */
  def fromInt(value: Int): Maybe[HeadingLevel] =
    if value >= 1 && value <= 6 then Present(value) else Absent

  val One: HeadingLevel   = 1
  val Two: HeadingLevel   = 2
  val Three: HeadingLevel = 3
  val Four: HeadingLevel  = 4
  val Five: HeadingLevel  = 5
  val Six: HeadingLevel   = 6

  extension (level: HeadingLevel)
    /** The level as the number CommonMark writes in `h1`…`h6`. */
    def toInt: Int = level

  given CanEqual[HeadingLevel, HeadingLevel] = CanEqual.derived
end HeadingLevel
