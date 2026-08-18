package morphir.langkit.core.scanner

import morphir.langkit.core.Span
import scala.util.control.ControlThrowable

object SourceScanner:
  def scan[A](
      source: String,
      budget: ScanBudget = ScanBudget.default,
      phase: Option[ScanPhase] = None
  )(use: SourceScanner => A): ScanResult[A] =
    budget match
      case limited: ScanBudget.Limited if source.length.toLong > limited.maxInputLength.toLong =>
        ScanResult.Failure(
          ScanFailure(
            exceeded = ScanLimitExceeded.InputLength(
              limit = limited.maxInputLength,
              actual = InputSize.unsafe(source.length.toLong)
            ),
            offset = SourceOffset.start,
            phase = phase
          )
        )
      case _ =>
        val scanner = budget match
          case limited: ScanBudget.Limited =>
            new SourceScanner(source, isWorkLimited = true, limited.maxWork.toLong, phase)
          case ScanBudget.UnsafeUnbounded =>
            new SourceScanner(source, isWorkLimited = false, Long.MaxValue, phase)

        try
          try ScanResult.Success(use(scanner))
          catch case exhausted: WorkBudgetExhausted => ScanResult.Failure(exhausted.failure)
        finally scanner.close()

  private[scanner] def saturatingAdd(current: Long, increment: Long): Long =
    if increment >= Long.MaxValue - current then Long.MaxValue
    else current + increment

  private final class WorkBudgetExhausted(val failure: ScanFailure) extends ControlThrowable

final class SourceScanner private[scanner] (
    originalSource: String,
    isWorkLimited: Boolean,
    maxWork: Long,
    phase: Option[ScanPhase]
):
  import SourceScanner.WorkBudgetExhausted

  private var currentOffset = 0
  private var consumedWork  = 0L
  private var active        = true

  def source: String =
    requireActive()
    originalSource

  def offset: SourceOffset =
    requireActive()
    SourceOffset.unsafe(currentOffset)

  def isAtEnd: Boolean =
    requireActive()
    currentOffset == originalSource.length

  def mark: SourceOffset =
    requireActive()
    SourceOffset.unsafe(currentOffset)

  def peek(): Option[Char] = peek(CodeUnitCount(0))

  def peek(distance: CodeUnitCount): Option[Char] =
    requireActive()
    val target = currentOffset.toLong + distance.toInt.toLong
    if target >= originalSource.length.toLong then None
    else
      charge(1L)
      Some(originalSource.charAt(target.toInt))

  def advance(): Unit = advance(CodeUnitCount.one)

  def advance(count: CodeUnitCount): Unit =
    requireActive()
    val target = currentOffset.toLong + count.toInt.toLong
    if target > originalSource.length.toLong then
      throw new IndexOutOfBoundsException(
        s"cannot advance ${count.toInt} code units from offset $currentOffset in source of length ${originalSource.length}"
      )
    if count.toInt != 0 then
      charge(count.toInt.toLong)
      currentOffset = target.toInt

  def viewFrom(start: SourceOffset): SourceView =
    requireActive()
    val startOffset = start.toInt
    require(startOffset <= currentOffset, s"view start $startOffset must not exceed current offset $currentOffset")
    SourceView.fromSpan(originalSource, Span(startOffset, currentOffset - startOffset))

  def view(span: Span): SourceView =
    requireActive()
    SourceView.fromSpan(originalSource, span)

  def remaining: SourceView =
    requireActive()
    SourceView.fromSpan(originalSource, Span(currentOffset, originalSource.length - currentOffset))

  private def charge(increment: Long): Unit =
    val attempted = SourceScanner.saturatingAdd(consumedWork, increment)
    consumedWork = attempted
    if isWorkLimited && attempted > maxWork then
      throw new WorkBudgetExhausted(
        ScanFailure(
          exceeded = ScanLimitExceeded.Work(
            limit = WorkUnits.unsafe(maxWork),
            attempted = WorkUnits.unsafe(attempted)
          ),
          offset = SourceOffset.unsafe(currentOffset),
          phase = phase
        )
      )

  private def requireActive(): Unit =
    if !active then throw new IllegalStateException("scanner session is closed")

  private def close(): Unit = active = false
