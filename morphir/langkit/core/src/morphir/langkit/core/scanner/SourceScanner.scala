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
        val ceilings = budget match
          case limited: ScanBudget.Limited =>
            new BudgetCeilings(
              maxWork = limited.maxWork,
              maxNestingDepth = limited.maxNestingDepth,
              maxOutputNodes = limited.maxOutputNodes
            )
          case ScanBudget.UnsafeUnbounded =>
            null
        val scanner = new SourceScanner(source, ceilings, phase)

        try
          try
            val value = use(scanner)
            scanner.exhaustion match
              case null      => ScanResult.Success(value)
              case exhausted => ScanResult.Failure(exhausted.failure)
          catch
            case exhausted: BudgetExhausted if exhausted.owner.eq(scanner) =>
              ScanResult.Failure(exhausted.failure)
        finally scanner.close()

  private[scanner] def saturatingAdd(current: Long, increment: Long): Long =
    if increment >= Long.MaxValue - current then Long.MaxValue
    else current + increment

  private[scanner] final class BudgetCeilings(
      val maxWork: WorkUnits,
      val maxNestingDepth: NestingDepth,
      val maxOutputNodes: NodeCount
  )

  private final class BudgetExhausted(val owner: SourceScanner, val failure: ScanFailure) extends ControlThrowable

final class ScanCheckpoint private (private val owner: SourceScanner, private val savedOffset: Int)

object ScanCheckpoint:
  private[scanner] def create(owner: SourceScanner, offset: Int): ScanCheckpoint =
    new ScanCheckpoint(owner, offset)

  private[scanner] def belongsTo(checkpoint: ScanCheckpoint, scanner: SourceScanner): Boolean =
    checkpoint.owner.eq(scanner)

  private[scanner] def offset(checkpoint: ScanCheckpoint): Int = checkpoint.savedOffset

final class SourceScanner private[scanner] (
    originalSource: String,
    ceilings: SourceScanner.BudgetCeilings | Null,
    phase: Option[ScanPhase]
):
  import SourceScanner.BudgetExhausted

  private var currentOffset                      = 0
  private var consumedWork                       = 0L
  private var currentNestingDepth                = 0
  private var outputNodes                        = 0L
  private var active                             = true
  private var exhaustion: BudgetExhausted | Null = null

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

  def checkpoint(): ScanCheckpoint =
    requireActive()
    ScanCheckpoint.create(this, currentOffset)

  def restore(checkpoint: ScanCheckpoint): Unit =
    requireActive()
    require(ScanCheckpoint.belongsTo(checkpoint, this), "checkpoint belongs to another scanner session")
    currentOffset = ScanCheckpoint.offset(checkpoint)

  def requireProgress[A](phase: ScanPhase)(operation: => A): A =
    requireActive()
    val start = currentOffset
    val value = operation
    requireActive()
    if currentOffset == start then throw new IllegalStateException(s"${phase.value} made no progress")
    value

  def withNesting[A](operation: => A): A =
    requireActive()
    val attempted = if currentNestingDepth == Int.MaxValue then Int.MaxValue else currentNestingDepth + 1
    if ceilings != null && attempted > ceilings.maxNestingDepth.toInt then
      failBudget(
        ScanLimitExceeded.Nesting(
          limit = ceilings.maxNestingDepth,
          attempted = NestingDepth.unsafe(attempted)
        )
      )
    currentNestingDepth = attempted
    try operation
    finally currentNestingDepth -= 1

  def chargeOutputNodes(count: NodeCount): Unit =
    requireActive()
    if count.toLong != 0L then
      val attempted = SourceScanner.saturatingAdd(outputNodes, count.toLong)
      if ceilings != null && attempted > ceilings.maxOutputNodes.toLong then
        failBudget(
          ScanLimitExceeded.OutputNodes(
            limit = ceilings.maxOutputNodes,
            attempted = NodeCount.unsafe(attempted)
          )
        )
      outputNodes = attempted

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
    if ceilings != null && attempted > ceilings.maxWork.toLong then
      failBudget(
        ScanLimitExceeded.Work(
          limit = ceilings.maxWork,
          attempted = WorkUnits.unsafe(attempted)
        )
      )
    consumedWork = attempted

  private def failBudget(exceeded: ScanLimitExceeded): Nothing =
    val exhausted = new BudgetExhausted(
      owner = this,
      failure = ScanFailure(
        exceeded = exceeded,
        offset = SourceOffset.unsafe(currentOffset),
        phase = phase
      )
    )
    exhaustion = exhausted
    throw exhausted

  private def requireActive(): Unit =
    if !active then throw new IllegalStateException("scanner session is closed")
    if exhaustion != null then throw exhaustion

  private def close(): Unit = active = false
