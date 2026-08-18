package morphir.langkit.core.scanner

sealed trait ScanBudget derives CanEqual

object ScanBudget:
  sealed abstract case class Limited private[scanner] (
      maxInputLength: InputSize,
      maxWork: WorkUnits,
      maxNestingDepth: NestingDepth,
      maxOutputNodes: NodeCount
  ) extends ScanBudget
      derives CanEqual

  case object UnsafeUnbounded extends ScanBudget

  val default: Limited = limited(
    maxInputLength = InputSize.mebibytes(16L),
    maxWork = WorkUnits(256L * 1024L * 1024L),
    maxNestingDepth = NestingDepth(1024),
    maxOutputNodes = NodeCount(4L * 1024L * 1024L)
  )

  def limited(
      maxInputLength: InputSize,
      maxWork: WorkUnits,
      maxNestingDepth: NestingDepth,
      maxOutputNodes: NodeCount
  ): Limited =
    require(maxInputLength.toLong > 0L, "maximum input length must be positive")
    require(maxWork.toLong > 0L, "maximum work must be positive")
    require(maxNestingDepth.toInt > 0, "maximum nesting depth must be positive")
    require(maxOutputNodes.toLong > 0L, "maximum output nodes must be positive")
    new Limited(
      maxInputLength = maxInputLength,
      maxWork = maxWork,
      maxNestingDepth = maxNestingDepth,
      maxOutputNodes = maxOutputNodes
    ) {}
