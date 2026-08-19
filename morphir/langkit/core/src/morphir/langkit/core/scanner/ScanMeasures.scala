package morphir.langkit.core.scanner

import kyo.*
import scala.compiletime.{codeOf, error}

/** UTF-16 code-unit size. Byte-named constructors count code units using SI (1000) or IEC (1024) scales. */
opaque type InputSize = Long

object InputSize:
  private inline val CodeUnitsPerKilobyte = 1000L
  private inline val CodeUnitsPerKibibyte = 1024L
  private inline val CodeUnitsPerMegabyte = 1000L * 1000L
  private inline val CodeUnitsPerMebibyte = 1024L * 1024L
  private inline val CodeUnitsPerGigabyte = 1000L * 1000L * 1000L
  private inline val CodeUnitsPerGibibyte = 1024L * 1024L * 1024L

  given CanEqual[InputSize, InputSize] = CanEqual.derived

  /**
   * Compile-time constructor for a size in UTF-16 code units. The argument must be a constant; a negative literal fails
   * compilation. Dynamic values use [[fromCodeUnits]].
   */
  inline def codeUnits(inline value: Long): InputSize =
    inline if value < 0L then error("input size must be non-negative: " + codeOf(value))
    else unsafe(value)

  def fromCodeUnits(value: Long): Result[ScanBudgetError, InputSize] =
    if value < 0L then Result.fail(ScanBudgetError.NegativeInputSize(value))
    else Result.succeed(value)

  inline def kilobytes(inline value: Long): InputSize = scaled(value, CodeUnitsPerKilobyte, "kilobytes")
  inline def kibibytes(inline value: Long): InputSize = scaled(value, CodeUnitsPerKibibyte, "kibibytes")
  inline def megabytes(inline value: Long): InputSize = scaled(value, CodeUnitsPerMegabyte, "megabytes")
  inline def mebibytes(inline value: Long): InputSize = scaled(value, CodeUnitsPerMebibyte, "mebibytes")
  inline def gigabytes(inline value: Long): InputSize = scaled(value, CodeUnitsPerGigabyte, "gigabytes")
  inline def gibibytes(inline value: Long): InputSize = scaled(value, CodeUnitsPerGibibyte, "gibibytes")

  def fromKilobytes(value: Long): Result[ScanBudgetError, InputSize] =
    fromScaled(value, CodeUnitsPerKilobyte, "kilobytes")
  def fromKibibytes(value: Long): Result[ScanBudgetError, InputSize] =
    fromScaled(value, CodeUnitsPerKibibyte, "kibibytes")
  def fromMegabytes(value: Long): Result[ScanBudgetError, InputSize] =
    fromScaled(value, CodeUnitsPerMegabyte, "megabytes")
  def fromMebibytes(value: Long): Result[ScanBudgetError, InputSize] =
    fromScaled(value, CodeUnitsPerMebibyte, "mebibytes")
  def fromGigabytes(value: Long): Result[ScanBudgetError, InputSize] =
    fromScaled(value, CodeUnitsPerGigabyte, "gigabytes")
  def fromGibibytes(value: Long): Result[ScanBudgetError, InputSize] =
    fromScaled(value, CodeUnitsPerGibibyte, "gibibytes")

  private inline def scaled(inline value: Long, inline unitsPer: Long, inline unit: String): InputSize =
    inline if value < 0L then error("input size must be non-negative: " + codeOf(value))
    else inline if value > Long.MaxValue / unitsPer then
      error("input size overflow: " + codeOf(value) + " " + unit)
    else unsafe(value * unitsPer)

  private def fromScaled(value: Long, unitsPer: Long, unit: String): Result[ScanBudgetError, InputSize] =
    if value < 0L then Result.fail(ScanBudgetError.NegativeInputSize(value))
    else if value > Long.MaxValue / unitsPer then Result.fail(ScanBudgetError.InputSizeOverflow(value, unit))
    else Result.succeed(value * unitsPer)

  private[scanner] def unsafe(value: Long): InputSize = value

  extension (size: InputSize) def toLong: Long = size

opaque type CodeUnitCount = Int

object CodeUnitCount:
  given CanEqual[CodeUnitCount, CodeUnitCount] = CanEqual.derived

  val one: CodeUnitCount = 1

  def apply(value: Int): CodeUnitCount =
    require(value >= 0, "code-unit count must be non-negative")
    value

  extension (count: CodeUnitCount) def toInt: Int = count

opaque type WorkUnits = Long

object WorkUnits:
  given CanEqual[WorkUnits, WorkUnits] = CanEqual.derived

  /**
   * Compile-time constructor. The argument must be a constant; a negative literal fails compilation. Dynamic values use
   * [[from]].
   */
  inline def apply(inline value: Long): WorkUnits =
    inline if value < 0L then error("work units must be non-negative: " + codeOf(value))
    else unsafe(value)

  def from(value: Long): Result[ScanBudgetError, WorkUnits] =
    if value < 0L then Result.fail(ScanBudgetError.NegativeWork(value))
    else Result.succeed(value)

  private[scanner] def unsafe(value: Long): WorkUnits = value

  extension (units: WorkUnits) def toLong: Long = units

opaque type NestingDepth = Int

object NestingDepth:
  given CanEqual[NestingDepth, NestingDepth] = CanEqual.derived

  /**
   * Compile-time constructor. The argument must be a constant; a negative literal fails compilation. Dynamic values use
   * [[from]].
   */
  inline def apply(inline value: Int): NestingDepth =
    inline if value < 0 then error("nesting depth must be non-negative: " + codeOf(value))
    else unsafe(value)

  def from(value: Int): Result[ScanBudgetError, NestingDepth] =
    if value < 0 then Result.fail(ScanBudgetError.NegativeNestingDepth(value))
    else Result.succeed(value)

  private[scanner] def unsafe(value: Int): NestingDepth = value

  extension (depth: NestingDepth) def toInt: Int = depth

opaque type NodeCount = Long

object NodeCount:
  given CanEqual[NodeCount, NodeCount] = CanEqual.derived

  val one: NodeCount = apply(1L)

  /**
   * Compile-time constructor. The argument must be a constant; a negative literal fails compilation. Dynamic values use
   * [[from]].
   */
  inline def apply(inline value: Long): NodeCount =
    inline if value < 0L then error("node count must be non-negative: " + codeOf(value))
    else unsafe(value)

  def from(value: Long): Result[ScanBudgetError, NodeCount] =
    if value < 0L then Result.fail(ScanBudgetError.NegativeOutputNodes(value))
    else Result.succeed(value)

  private[scanner] def unsafe(value: Long): NodeCount = value

  extension (count: NodeCount) def toLong: Long = count

opaque type SourceOffset = Int

object SourceOffset:
  given CanEqual[SourceOffset, SourceOffset] = CanEqual.derived

  val start: SourceOffset = 0

  def apply(value: Int): SourceOffset =
    require(value >= 0, "source offset must be non-negative")
    value

  private[scanner] def unsafe(value: Int): SourceOffset = value

  extension (offset: SourceOffset) def toInt: Int = offset

opaque type ScanPhase = String

object ScanPhase:
  given CanEqual[ScanPhase, ScanPhase] = CanEqual.derived

  def apply(value: String): ScanPhase =
    require(!value.isBlank, "scan phase must be non-empty and non-blank")
    value

  extension (phase: ScanPhase) def value: String = phase
