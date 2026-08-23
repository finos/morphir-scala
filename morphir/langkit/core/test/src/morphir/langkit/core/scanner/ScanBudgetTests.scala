package morphir.langkit.core.scanner

import kyo.*
import kyo.test.*
import morphir.MorphirException
import scala.language.strictEquality

class ScanBudgetTests extends Test[Any]:

  private def input(value: Long): InputSize     = InputSize.fromCodeUnits(value).getOrThrow
  private def work(value: Long): WorkUnits      = WorkUnits.from(value).getOrThrow
  private def nesting(value: Int): NestingDepth = NestingDepth.from(value).getOrThrow
  private def nodes(value: Long): NodeCount     = NodeCount.from(value).getOrThrow

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

    "returns an input-length failure rather than throwing for a zero input limit" in
      assert(
        ScanBudget.limited(
          maxInputLength = input(0L),
          maxWork = work(1L),
          maxNestingDepth = nesting(1),
          maxOutputNodes = NodeCount.one
        ) == Result.fail(ScanBudgetError.NonPositiveInputLength(input(0L)))
      )

    "returns a work failure rather than throwing for a zero work limit" in
      assert(
        ScanBudget.limited(
          maxInputLength = input(1L),
          maxWork = work(0L),
          maxNestingDepth = nesting(1),
          maxOutputNodes = NodeCount.one
        ) == Result.fail(ScanBudgetError.NonPositiveWork(work(0L)))
      )

    "returns a nesting-depth failure rather than throwing for a zero nesting limit" in
      assert(
        ScanBudget.limited(
          maxInputLength = input(1L),
          maxWork = work(1L),
          maxNestingDepth = nesting(0),
          maxOutputNodes = NodeCount.one
        ) == Result.fail(ScanBudgetError.NonPositiveNestingDepth(nesting(0)))
      )

    "returns an output-node failure rather than throwing for a zero output-node limit" in
      assert(
        ScanBudget.limited(
          maxInputLength = input(1L),
          maxWork = work(1L),
          maxNestingDepth = nesting(1),
          maxOutputNodes = nodes(0L)
        ) == Result.fail(ScanBudgetError.NonPositiveOutputNodes(nodes(0L)))
      )

    "returns the first validation failure in constructor-parameter order" in
      assert(
        ScanBudget.limited(
          maxInputLength = input(0L),
          maxWork = work(0L),
          maxNestingDepth = nesting(0),
          maxOutputNodes = nodes(0L)
        ) == Result.fail(ScanBudgetError.NonPositiveInputLength(input(0L)))
      )

    "uses MorphirException errors with stable messages" in {
      val error: ScanBudgetError = ScanBudgetError.NonPositiveWork(work(0L))
      assert(error.isInstanceOf[MorphirException])
      assert(error.getMessage == "maximum work must be positive: 0")
    }

    "preserves each distinct typed limit" in {
      val result = ScanBudget.limited(
        maxInputLength = input(11L),
        maxWork = work(22L),
        maxNestingDepth = nesting(33),
        maxOutputNodes = nodes(44L)
      )

      result match
        case Result.Success(budget) =>
          assert(budget.maxInputLength.toLong == 11L)
          assert(budget.maxWork.toLong == 22L)
          assert(budget.maxNestingDepth.toInt == 33)
          assert(budget.maxOutputNodes.toLong == 44L)
        case Result.Failure(error) =>
          assert(false, s"unexpected validation failure: ${error.getMessage}")
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
      assert(input(1L) == input(1L))
      assert(CodeUnitCount.one == CodeUnitCount(1))
      assert(work(1L) == work(1L))
      assert(nesting(1) == nesting(1))
      assert(NodeCount.one == nodes(1L))
      assert(SourceOffset.start == SourceOffset(0))
      assert(ScanPhase("tokenize") == ScanPhase("tokenize"))
    }

    "returns a typed failure rather than throwing for a negative input size" in {
      val error: MorphirException = ScanBudgetError.NegativeInputSize(-1L)
      assert(InputSize.fromCodeUnits(-1L) == Result.fail(ScanBudgetError.NegativeInputSize(-1L)))
      assert(error.getMessage == "input size must be non-negative: -1")
    }
    "returns a typed failure rather than throwing for a negative mebibyte input size" in
      assert(InputSize.fromMebibytes(-1L) == Result.fail(ScanBudgetError.NegativeInputSize(-1L)))
    "returns a typed failure rather than throwing for a negative megabyte input size" in
      assert(InputSize.fromMegabytes(-1L) == Result.fail(ScanBudgetError.NegativeInputSize(-1L)))
    "returns a typed failure rather than throwing for a negative code-unit count" in
      assert(CodeUnitCount.fromInt(-1) == Result.fail(ScanBudgetError.NegativeCodeUnitCount(-1)))
    "returns a typed failure rather than throwing for negative work units" in
      assert(WorkUnits.from(-1L) == Result.fail(ScanBudgetError.NegativeWork(-1L)))
    "returns a typed failure rather than throwing for a negative nesting depth" in
      assert(NestingDepth.from(-1) == Result.fail(ScanBudgetError.NegativeNestingDepth(-1)))
    "returns a typed failure rather than throwing for a negative node count" in
      assert(NodeCount.from(-1L) == Result.fail(ScanBudgetError.NegativeOutputNodes(-1L)))
    "returns a typed failure rather than throwing for a negative source offset" in
      assert(SourceOffset.fromInt(-1) == Result.fail(ScanBudgetError.NegativeSourceOffset(-1)))
    "returns a typed failure rather than throwing for empty and blank scan phases" in {
      assert(ScanPhase.fromString("") == Result.fail(ScanBudgetError.BlankScanPhase("")))
      assert(ScanPhase.fromString(" \t\n") == Result.fail(ScanBudgetError.BlankScanPhase(" \t\n")))
    }
    "returns a typed failure rather than throwing for mebibyte overflow" in
      assert(
        InputSize.fromMebibytes(Long.MaxValue) ==
          Result.fail(ScanBudgetError.InputSizeOverflow(Long.MaxValue, "mebibytes"))
      )
    "returns a typed failure rather than throwing for megabyte overflow" in
      assert(
        InputSize.fromMegabytes(Long.MaxValue) ==
          Result.fail(ScanBudgetError.InputSizeOverflow(Long.MaxValue, "megabytes"))
      )
    "accepts the exact mebibyte boundary and returns overflow for its successor" in {
      val codeUnitsPerMebibyte = 1024L * 1024L
      val boundary             = Long.MaxValue / codeUnitsPerMebibyte
      assert(InputSize.fromMebibytes(boundary) == Result.succeed(input(boundary * codeUnitsPerMebibyte)))
      assert(
        InputSize.fromMebibytes(boundary + 1L) ==
          Result.fail(ScanBudgetError.InputSizeOverflow(boundary + 1L, "mebibytes"))
      )
    }
    "scales SI and IEC byte units to UTF-16 code units" in {
      assert(InputSize.kilobytes(1L).toLong == 1000L)
      assert(InputSize.kibibytes(1L).toLong == 1024L)
      assert(InputSize.megabytes(1L).toLong == 1000L * 1000L)
      assert(InputSize.mebibytes(1L).toLong == 1024L * 1024L)
      assert(InputSize.gigabytes(1L).toLong == 1000L * 1000L * 1000L)
      assert(InputSize.gibibytes(1L).toLong == 1024L * 1024L * 1024L)
      assert(InputSize.fromKilobytes(2L) == Result.succeed(InputSize.kilobytes(2L)))
      assert(InputSize.fromGibibytes(1L) == Result.succeed(InputSize.gibibytes(1L)))
    }

    "constructs a literal input size at compile time" in {
      assert(InputSize.codeUnits(0L).toLong == 0L)
      assert(InputSize.codeUnits(16L).toLong == 16L)
      assert(InputSize.kilobytes(1L).toLong == 1000L)
      assert(InputSize.megabytes(1L).toLong == 1000L * 1000L)
      assert(InputSize.mebibytes(1L).toLong == 1024L * 1024L)
      assert(WorkUnits(2L * 3L).toLong == 6L)
      assert(NestingDepth(4).toInt == 4)
      assert(NodeCount(8L).toLong == 8L)
      assert(CodeUnitCount(4).toInt == 4)
      assert(SourceOffset(4).toInt == 4)
      assert(ScanPhase("tokenize").value == "tokenize")
    }

    "rejects a negative literal input size at compile time" in {
      val errors = scala.compiletime.testing.typeCheckErrors("""
        import morphir.langkit.core.scanner.*
        InputSize.codeUnits(-1L)
      """)
      assert(errors.exists(_.message.contains("input size must be non-negative")))
    }

    "rejects a negative literal megabyte size at compile time" in {
      val errors = scala.compiletime.testing.typeCheckErrors("""
        import morphir.langkit.core.scanner.*
        InputSize.megabytes(-1L)
      """)
      assert(errors.exists(_.message.contains("input size must be non-negative")))
    }

    "rejects an overflowing literal megabyte size at compile time" in {
      val errors = scala.compiletime.testing.typeCheckErrors("""
        import morphir.langkit.core.scanner.*
        InputSize.megabytes(Long.MaxValue)
      """)
      assert(errors.exists(_.message.contains("input size overflow")))
    }

    "rejects a negative literal work, nesting, and node count at compile time" in {
      val workErrors = scala.compiletime.testing.typeCheckErrors("""
        import morphir.langkit.core.scanner.*
        WorkUnits(-1L)
      """)
      val nestingErrors = scala.compiletime.testing.typeCheckErrors("""
        import morphir.langkit.core.scanner.*
        NestingDepth(-1)
      """)
      val nodeErrors = scala.compiletime.testing.typeCheckErrors("""
        import morphir.langkit.core.scanner.*
        NodeCount(-1L)
      """)
      assert(workErrors.exists(_.message.contains("work units must be non-negative")))
      assert(nestingErrors.exists(_.message.contains("nesting depth must be non-negative")))
      assert(nodeErrors.exists(_.message.contains("node count must be non-negative")))
    }

    "rejects a negative literal code-unit count and source offset at compile time" in {
      val codeUnitErrors = scala.compiletime.testing.typeCheckErrors("""
        import morphir.langkit.core.scanner.*
        CodeUnitCount(-1)
      """)
      val offsetErrors = scala.compiletime.testing.typeCheckErrors("""
        import morphir.langkit.core.scanner.*
        SourceOffset(-1)
      """)
      assert(codeUnitErrors.exists(_.message.contains("code-unit count must be non-negative")))
      assert(offsetErrors.exists(_.message.contains("source offset must be non-negative")))
    }

    "rejects a blank literal scan phase at compile time" in {
      val emptyErrors = scala.compiletime.testing.typeCheckErrors("""
        import morphir.langkit.core.scanner.*
        ScanPhase("")
      """)
      val whitespaceErrors = scala.compiletime.testing.typeCheckErrors("""
        import morphir.langkit.core.scanner.*
        ScanPhase(" \t\n")
      """)
      assert(emptyErrors.exists(_.message.contains("scan phase must be non-empty and non-blank")))
      assert(whitespaceErrors.exists(_.message.contains("scan phase must be non-empty and non-blank")))
    }

    "rejects a dynamic input size at the compile-time constructor" in {
      val errors = scala.compiletime.testing.typeCheckErrors("""
        import morphir.langkit.core.scanner.*
        val n = 1L
        InputSize.codeUnits(n)
      """)
      assert(errors.nonEmpty)
    }

    "rejects a dynamic code-unit count, source offset, and scan phase at the compile-time constructor" in {
      val codeUnitErrors = scala.compiletime.testing.typeCheckErrors("""
        import morphir.langkit.core.scanner.*
        val n = 1
        CodeUnitCount(n)
      """)
      val offsetErrors = scala.compiletime.testing.typeCheckErrors("""
        import morphir.langkit.core.scanner.*
        val n = 1
        SourceOffset(n)
      """)
      val phaseErrors = scala.compiletime.testing.typeCheckErrors("""
        import morphir.langkit.core.scanner.*
        val s = "tokenize"
        ScanPhase(s)
      """)
      assert(codeUnitErrors.nonEmpty)
      assert(offsetErrors.nonEmpty)
      assert(phaseErrors.nonEmpty)
    }
  }

  "ScanResult.map" - {
    "transforms a success" in
      assert(ScanResult.Success(2).map(_ * 3) == ScanResult.Success(6))

    "preserves an exact typed failure" in {
      val failure = ScanFailure(
        exceeded = ScanLimitExceeded.Work(limit = work(10L), attempted = work(11L)),
        offset = SourceOffset(7),
        phase = Present(ScanPhase("tokenize"))
      )
      assert(ScanResult.Failure(failure).map((value: Int) => value + 1) == ScanResult.Failure(failure))
    }
  }
