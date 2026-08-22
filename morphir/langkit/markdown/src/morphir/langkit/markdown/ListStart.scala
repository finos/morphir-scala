package morphir.langkit.markdown

import kyo.*

/**
 * The number an ordered list counts from, always between zero and 999999999.
 *
 * Replaces a bare `Int` on [[MdNode.List]] so a start the spec cannot express is unrepresentable. CommonMark's ordered
 * marker is one to nine digits followed by `.` or `)`, which admits `0.` and `999999999.` and nothing outside them; a
 * tenth digit is not a deeper number but an ordinary paragraph.
 *
 * The bound is the writer's problem, not only the parser's. `-1` and `1000000000` both write a marker no reader takes
 * as one — `-1. x` and `1000000000. x` reparse as paragraphs — so a list holding either breaks the writer's
 * structural-fidelity contract at values the old `Int` field let any caller construct.
 */
opaque type ListStart = Int

object ListStart:

  /** The largest start CommonMark's nine-digit marker can spell. */
  private inline val MaxValue = 999999999

  /** The only way in from an arbitrary number: [[kyo.Absent]] for a start outside zero to 999999999. */
  def fromInt(value: Int): Maybe[ListStart] =
    if value >= 0 && value <= MaxValue then Present(value) else Absent

  /**
   * A start written as a literal, checked where it is written: `ListStart(3)`.
   *
   * The bound is decided at compile time, so an out-of-range literal is a compile error rather than an [[kyo.Absent]]
   * the call site has to answer for. It takes literals only — reach for [[fromInt]] when the number is computed.
   */
  inline def apply(inline value: Int): ListStart =
    inline if value < 0 then
      compiletime.error("A list start cannot be negative: CommonMark's ordered marker holds digits only.")
    else if value > MaxValue then
      compiletime.error("A list start cannot exceed 999999999: CommonMark's ordered marker holds at most nine digits.")
    else value

  /** `0.`, which CommonMark accepts and numbers the first item zero. */
  val Zero: ListStart = 0

  /** `1.`, what an ordered list counts from unless it says otherwise. */
  val One: ListStart = 1

  /** `999999999.`, the last start a nine-digit marker reaches. */
  val Max: ListStart = MaxValue

  extension (start: ListStart)
    /** The start as the number CommonMark writes in the first item's marker. */
    def toInt: Int = start

  given CanEqual[ListStart, ListStart] = CanEqual.derived
end ListStart
