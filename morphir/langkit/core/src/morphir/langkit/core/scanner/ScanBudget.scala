package morphir.langkit.core.scanner

import kyo.*

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

  val default: Limited =
    limited(
      maxInputLength = InputSize.mebibytes(16L),
      maxWork = WorkUnits(256L * 1024L * 1024L),
      maxNestingDepth = NestingDepth(1024),
      maxOutputNodes = NodeCount(4L * 1024L * 1024L)
    ).getOrThrow

  /**
   * Validate caller-supplied ceilings. Dynamic measure factories report invalid representations as the same `Result`
   * error type; compile-time constructors reject invalid literals before this method runs.
   */
  def limited(
      maxInputLength: InputSize,
      maxWork: WorkUnits,
      maxNestingDepth: NestingDepth,
      maxOutputNodes: NodeCount
  ): Result[ScanBudgetError, Limited] =
    if maxInputLength.toLong <= 0L then Result.fail(ScanBudgetError.NonPositiveInputLength(maxInputLength))
    else if maxWork.toLong <= 0L then Result.fail(ScanBudgetError.NonPositiveWork(maxWork))
    else if maxNestingDepth.toInt <= 0 then Result.fail(ScanBudgetError.NonPositiveNestingDepth(maxNestingDepth))
    else if maxOutputNodes.toLong <= 0L then Result.fail(ScanBudgetError.NonPositiveOutputNodes(maxOutputNodes))
    else
      Result.succeed(
        new Limited(
          maxInputLength = maxInputLength,
          maxWork = maxWork,
          maxNestingDepth = maxNestingDepth,
          maxOutputNodes = maxOutputNodes
        ) {}
      )
