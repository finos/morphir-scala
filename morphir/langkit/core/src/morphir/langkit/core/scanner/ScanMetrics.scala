package morphir.langkit.core.scanner

/** An immutable snapshot of deterministic scanner resource consumption. */
final case class ScanMetrics(
    work: WorkUnits,
    outputNodes: NodeCount,
    maximumNestingDepth: NestingDepth
) derives CanEqual
