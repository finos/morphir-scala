package morphir.langkit.core.scanner

import morphir.MorphirException

enum ScanBudgetError(message: String) extends MorphirException(message) derives CanEqual:
  case NonPositiveInputLength(inputSize: InputSize)
      extends ScanBudgetError(s"maximum input length must be positive: ${inputSize.toLong}")
  case NonPositiveWork(workUnits: WorkUnits)
      extends ScanBudgetError(s"maximum work must be positive: ${workUnits.toLong}")
  case NonPositiveNestingDepth(nestingDepth: NestingDepth)
      extends ScanBudgetError(s"maximum nesting depth must be positive: ${nestingDepth.toInt}")
  case NonPositiveOutputNodes(nodeCount: NodeCount)
      extends ScanBudgetError(s"maximum output nodes must be positive: ${nodeCount.toLong}")
