package morphir.langkit.core.scanner

import kyo.test.*
import scala.language.strictEquality

class ScanBudgetTests extends Test[Any]:

  private def rejects(thunk: => Any): Boolean =
    try
      thunk
      false
    catch case _: IllegalArgumentException | _: ArithmeticException => true

  private def throwsArithmetic(thunk: => Any): Boolean =
    try
      thunk
      false
    catch
      case _: ArithmeticException => true
      case _: Throwable           => false

  "ScanBudget" - {
    "has positive typed defaults" in {
      def check(policy: ScanBudget): Unit = policy match
        case budget: ScanBudget.Limited =>
          assert(budget.maxInputLength.toLong > 0L)
          assert(budget.maxWork.toLong > 0L)
          assert(budget.maxNestingDepth.toInt > 0)
          assert(budget.maxOutputNodes.toLong > 0L)
        case ScanBudget.UnsafeUnbounded => assert(false)
      check(ScanBudget.default)
    }

    "rejects a zero input limit while all other limits are positive" in {
      val rejected = rejects {
        ScanBudget.limited(
          maxInputLength = InputSize.codeUnits(0L),
          maxWork = WorkUnits(1L),
          maxNestingDepth = NestingDepth(1),
          maxOutputNodes = NodeCount.one
        )
      }
      assert(rejected)
    }

    "rejects a zero work limit while all other limits are positive" in {
      val rejected = rejects {
        ScanBudget.limited(
          maxInputLength = InputSize.codeUnits(1L),
          maxWork = WorkUnits(0L),
          maxNestingDepth = NestingDepth(1),
          maxOutputNodes = NodeCount.one
        )
      }
      assert(rejected)
    }

    "rejects a zero nesting limit while all other limits are positive" in {
      val rejected = rejects {
        ScanBudget.limited(
          maxInputLength = InputSize.codeUnits(1L),
          maxWork = WorkUnits(1L),
          maxNestingDepth = NestingDepth(0),
          maxOutputNodes = NodeCount.one
        )
      }
      assert(rejected)
    }

    "rejects a zero output-node limit while all other limits are positive" in {
      val rejected = rejects {
        ScanBudget.limited(
          maxInputLength = InputSize.codeUnits(1L),
          maxWork = WorkUnits(1L),
          maxNestingDepth = NestingDepth(1),
          maxOutputNodes = NodeCount(0L)
        )
      }
      assert(rejected)
    }

    "preserves each distinct typed limit" in {
      val budget = ScanBudget.limited(
        maxInputLength = InputSize.codeUnits(11L),
        maxWork = WorkUnits(22L),
        maxNestingDepth = NestingDepth(33),
        maxOutputNodes = NodeCount(44L)
      )

      assert(budget.maxInputLength.toLong == 11L)
      assert(budget.maxWork.toLong == 22L)
      assert(budget.maxNestingDepth.toInt == 33)
      assert(budget.maxOutputNodes.toLong == 44L)
    }

    "names the unsafe policy explicitly" in
      assert(ScanBudget.UnsafeUnbounded.toString == "UnsafeUnbounded")

    "does not allow input and work limits to be swapped" in {
      val errors = scala.compiletime.testing.typeCheckErrors("""
        import morphir.langkit.core.scanner.*
        ScanBudget.limited(
          maxInputLength = WorkUnits(1L),
          maxWork = InputSize.codeUnits(1L),
          maxNestingDepth = NestingDepth(1),
          maxOutputNodes = NodeCount.one
        )
      """)
      assert(errors.nonEmpty)
    }
  }

  "scan measures" - {
    "support same-measure equality under strict equality" in {
      assert(InputSize.codeUnits(1L) == InputSize.codeUnits(1L))
      assert(CodeUnitCount.one == CodeUnitCount(1))
      assert(WorkUnits(1L) == WorkUnits(1L))
      assert(NestingDepth(1) == NestingDepth(1))
      assert(NodeCount.one == NodeCount(1L))
      assert(SourceOffset.start == SourceOffset(0))
      assert(ScanPhase("tokenize") == ScanPhase("tokenize"))
    }

    "reject negative input sizes" in
      assert(rejects(InputSize.codeUnits(-1L)))
    "reject negative mebibyte input sizes" in
      assert(rejects(InputSize.mebibytes(-1L)))
    "reject negative code-unit counts" in
      assert(rejects(CodeUnitCount(-1)))
    "reject negative work units" in
      assert(rejects(WorkUnits(-1L)))
    "reject negative nesting depths" in
      assert(rejects(NestingDepth(-1)))
    "reject negative node counts" in
      assert(rejects(NodeCount(-1L)))
    "reject negative source offsets" in
      assert(rejects(SourceOffset(-1)))
    "reject empty and blank scan phases" in {
      assert(rejects(ScanPhase("")))
      assert(rejects(ScanPhase(" \t\n")))
    }
    "reject mebibyte overflow" in
      assert(rejects(InputSize.mebibytes(Long.MaxValue)))
    "accept the exact mebibyte boundary and reject its successor with arithmetic overflow" in {
      val codeUnitsPerMebibyte = 1024L * 1024L
      val boundary             = Long.MaxValue / codeUnitsPerMebibyte
      assert(InputSize.mebibytes(boundary).toLong == boundary * codeUnitsPerMebibyte)
      assert(throwsArithmetic(InputSize.mebibytes(boundary + 1L)))
    }
  }

  "ScanResult.map" - {
    "transforms a success" in
      assert(ScanResult.Success(2).map(_ * 3) == ScanResult.Success(6))

    "preserves an exact typed failure" in {
      val failure = ScanFailure(
        exceeded = ScanLimitExceeded.Work(limit = WorkUnits(10L), attempted = WorkUnits(11L)),
        offset = SourceOffset(7),
        phase = Some(ScanPhase("tokenize"))
      )
      assert(ScanResult.Failure(failure).map((value: Int) => value + 1) == ScanResult.Failure(failure))
    }
  }
