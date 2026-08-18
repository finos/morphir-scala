package morphir.langkit.core.scanner

enum ScanLimitExceeded derives CanEqual:
  case InputLength(limit: InputSize, actual: InputSize)
  case Work(limit: WorkUnits, attempted: WorkUnits)
  case Nesting(limit: NestingDepth, attempted: NestingDepth)
  case OutputNodes(limit: NodeCount, attempted: NodeCount)

final case class ScanFailure(
    exceeded: ScanLimitExceeded,
    offset: SourceOffset,
    phase: Option[ScanPhase]
) derives CanEqual

enum ScanResult[+A] derives CanEqual:
  case Success(value: A)
  case Failure(error: ScanFailure)

  def map[B](f: A => B): ScanResult[B] =
    this match
      case Success(value) => Success(f(value))
      case Failure(error) => Failure(error)
