package morphir.langkit.core.scanner

opaque type InputSize = Long

object InputSize:
  private val CodeUnitsPerMebibyte = 1024L * 1024L

  given CanEqual[InputSize, InputSize] = CanEqual.derived

  def codeUnits(value: Long): InputSize =
    require(value >= 0L, "input size must be non-negative")
    value

  def mebibytes(value: Long): InputSize =
    require(value >= 0L, "input size must be non-negative")
    if value > Long.MaxValue / CodeUnitsPerMebibyte then
      throw new ArithmeticException("input size overflow")
    value * CodeUnitsPerMebibyte

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

  def apply(value: Long): WorkUnits =
    require(value >= 0L, "work units must be non-negative")
    value

  private[scanner] def unsafe(value: Long): WorkUnits = value

  extension (units: WorkUnits) def toLong: Long = units

opaque type NestingDepth = Int

object NestingDepth:
  given CanEqual[NestingDepth, NestingDepth] = CanEqual.derived

  def apply(value: Int): NestingDepth =
    require(value >= 0, "nesting depth must be non-negative")
    value

  private[scanner] def unsafe(value: Int): NestingDepth = value

  extension (depth: NestingDepth) def toInt: Int = depth

opaque type NodeCount = Long

object NodeCount:
  given CanEqual[NodeCount, NodeCount] = CanEqual.derived

  val one: NodeCount = 1L

  def apply(value: Long): NodeCount =
    require(value >= 0L, "node count must be non-negative")
    value

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
