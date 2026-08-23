package morphir.langkit.core.scanner

import morphir.MorphirException

enum ScanBudgetError(message: String) extends MorphirException(message) derives CanEqual:
  case NegativeInputSize(value: Long)
      extends ScanBudgetError(s"input size must be non-negative: $value")
  case InputSizeOverflow(value: Long, unit: String)
      extends ScanBudgetError(s"input size overflow: $value $unit")
  case NegativeWork(value: Long)
      extends ScanBudgetError(s"work units must be non-negative: $value")
  case NegativeNestingDepth(value: Int)
      extends ScanBudgetError(s"nesting depth must be non-negative: $value")
  case NegativeOutputNodes(value: Long)
      extends ScanBudgetError(s"node count must be non-negative: $value")
  case NegativeCodeUnitCount(value: Int)
      extends ScanBudgetError(s"code-unit count must be non-negative: $value")
  case NegativeSourceOffset(value: Int)
      extends ScanBudgetError(s"source offset must be non-negative: $value")
  case BlankScanPhase(value: String)
      extends ScanBudgetError(s"scan phase must be non-empty and non-blank: '$value'")
  case NonPositiveInputLength(inputSize: InputSize)
      extends ScanBudgetError(s"maximum input length must be positive: ${inputSize.toLong}")
  case NonPositiveWork(workUnits: WorkUnits)
      extends ScanBudgetError(s"maximum work must be positive: ${workUnits.toLong}")
  case NonPositiveNestingDepth(nestingDepth: NestingDepth)
      extends ScanBudgetError(s"maximum nesting depth must be positive: ${nestingDepth.toInt}")
  case NonPositiveOutputNodes(nodeCount: NodeCount)
      extends ScanBudgetError(s"maximum output nodes must be positive: ${nodeCount.toLong}")
