package morphir.langkit.core.scanner

import kyo.test.*
import morphir.langkit.core.Span
import scala.language.strictEquality

class SourceScannerTests extends Test[Any]:

  private def limited(input: Long, work: Long): ScanBudget.Limited =
    ScanBudget.limited(
      maxInputLength = InputSize.codeUnits(input),
      maxWork = WorkUnits(work),
      maxNestingDepth = NestingDepth(1),
      maxOutputNodes = NodeCount.one
    )

  private def rejectsArgument(thunk: => Any): Boolean =
    try
      thunk
      false
    catch case _: IllegalArgumentException => true

  private def rejectsIndex(thunk: => Any): Boolean =
    try
      thunk
      false
    catch case _: IndexOutOfBoundsException => true

  private def closedMessage(thunk: => Any): Option[String] =
    try
      thunk
      None
    catch case error: IllegalStateException => Some(error.getMessage)

  "SourceScanner" - {
    "navigates and creates source views in typed UTF-16 coordinates" in {
      val result = SourceScanner.scan("abc") { scanner =>
        assert(scanner.source == "abc")
        assert(scanner.offset == SourceOffset.start)
        assert(!scanner.isAtEnd)
        assert(scanner.peek().contains('a'))
        assert(scanner.peek(CodeUnitCount(2)).contains('c'))
        assert(scanner.peek(CodeUnitCount(3)).isEmpty)
        val start = scanner.mark
        scanner.advance(CodeUnitCount(2))
        assert(scanner.viewFrom(start).text == "ab")
        assert(scanner.remaining.span == Span(2, 1))
        scanner.offset
      }

      assert(result == ScanResult.Success(SourceOffset(2)))
    }

    "rejects oversized input before invoking the callback" in {
      var invoked = false
      val budget  = limited(input = 3L, work = 1L)
      val result  = SourceScanner.scan("abcd", budget) { _ =>
        invoked = true
      }

      assert(!invoked)
      assert(
        result == ScanResult.Failure(
          ScanFailure(
            exceeded = ScanLimitExceeded.InputLength(
              limit = InputSize.codeUnits(3L),
              actual = InputSize.codeUnits(4L)
            ),
            offset = SourceOffset.start,
            phase = None
          )
        )
      )
    }

    "fails at the exact work boundary before the rejected operation" in {
      val phase  = ScanPhase("test")
      val result = SourceScanner.scan("abc", limited(input = 3L, work = 2L), Some(phase)) { scanner =>
        scanner.peek()
        scanner.advance()
        scanner.peek()
      }

      assert(
        result == ScanResult.Failure(
          ScanFailure(
            exceeded = ScanLimitExceeded.Work(limit = WorkUnits(2L), attempted = WorkUnits(3L)),
            offset = SourceOffset(1),
            phase = Some(phase)
          )
        )
      )
    }

    "charges no work for EOF lookahead" in {
      val result = SourceScanner.scan("a", limited(input = 1L, work = 1L)) { scanner =>
        scanner.advance()
        assert(scanner.peek().isEmpty)
        assert(scanner.peek(CodeUnitCount(0)).isEmpty)
        assert(scanner.peek(CodeUnitCount(1)).isEmpty)
        assert(scanner.peek(CodeUnitCount(Int.MaxValue)).isEmpty)
        scanner.offset
      }

      assert(result == ScanResult.Success(SourceOffset(1)))
    }

    "rejects movement past EOF without moving or charging" in {
      val result = SourceScanner.scan("a", limited(input = 1L, work = 1L)) { scanner =>
        assert(rejectsIndex(scanner.advance(CodeUnitCount(2))))
        assert(scanner.offset == SourceOffset.start)
        assert(scanner.peek().contains('a'))
        scanner.offset
      }

      assert(result == ScanResult.Success(SourceOffset.start))
    }

    "propagates callback exceptions unchanged" in {
      val expected          = new RuntimeException("callback failed")
      var caught: Throwable = null

      try SourceScanner.scan("a")(_ => throw expected)
      catch case error: Throwable => caught = error

      assert(caught.eq(expected))
    }

    "closes a retained scanner after its callback returns" in {
      var retained: SourceScanner = null
      assert(SourceScanner.scan("a") { scanner => retained = scanner } == ScanResult.Success(()))

      val closed = "scanner session is closed"
      assert(closedMessage(retained.source).contains(closed))
      assert(closedMessage(retained.offset).contains(closed))
      assert(closedMessage(retained.isAtEnd).contains(closed))
      assert(closedMessage(retained.mark).contains(closed))
      assert(closedMessage(retained.peek()).contains(closed))
      assert(closedMessage(retained.peek(CodeUnitCount.one)).contains(closed))
      assert(closedMessage(retained.advance()).contains(closed))
      assert(closedMessage(retained.advance(CodeUnitCount.one)).contains(closed))
      assert(closedMessage(retained.viewFrom(SourceOffset.start)).contains(closed))
      assert(closedMessage(retained.view(Span.zero)).contains(closed))
      assert(closedMessage(retained.remaining).contains(closed))
    }

    "preserves original UTF-16 coordinates including surrogate code units" in {
      val source = "a\r\n\t\ud83d\ude00"
      val result = SourceScanner.scan(source) { scanner =>
        assert(scanner.peek(CodeUnitCount(4)).exists(_.isHighSurrogate))
        assert(scanner.peek(CodeUnitCount(5)).exists(_.isLowSurrogate))
        scanner.advance(CodeUnitCount(source.length))
        scanner.offset
      }

      assert(result == ScanResult.Success(SourceOffset(source.length)))
    }

    "rejects invalid source-view ranges without cursor movement" in {
      val result = SourceScanner.scan("abc") { scanner =>
        scanner.advance()
        assert(rejectsArgument(scanner.viewFrom(SourceOffset(2))))
        assert(rejectsArgument(scanner.viewFrom(SourceOffset(4))))
        assert(rejectsArgument(scanner.view(Span(-1, 1))))
        assert(rejectsArgument(scanner.view(Span(0, -1))))
        assert(rejectsArgument(scanner.view(Span(Int.MaxValue, 1))))
        assert(rejectsArgument(scanner.view(Span(1, Int.MaxValue))))
        scanner.offset
      }

      assert(result == ScanResult.Success(SourceOffset(1)))
    }

    "allows unbounded work while retaining movement invariants" in {
      val bounded = SourceScanner.scan("a", limited(input = 1L, work = 1L)) { scanner =>
        scanner.peek()
        scanner.peek()
      }
      val unbounded = SourceScanner.scan("a", ScanBudget.UnsafeUnbounded) { scanner =>
        scanner.peek()
        scanner.peek()
        assert(rejectsIndex(scanner.advance(CodeUnitCount(2))))
        scanner.offset
      }

      assert(
        bounded == ScanResult.Failure(
          ScanFailure(
            exceeded = ScanLimitExceeded.Work(limit = WorkUnits(1L), attempted = WorkUnits(2L)),
            offset = SourceOffset.start,
            phase = None
          )
        )
      )
      assert(unbounded == ScanResult.Success(SourceOffset.start))
    }

    "saturates work arithmetic at Long.MaxValue" in {
      assert(SourceScanner.saturatingAdd(Long.MaxValue - 1L, 1L) == Long.MaxValue)
      assert(SourceScanner.saturatingAdd(Long.MaxValue - 1L, 2L) == Long.MaxValue)
      assert(SourceScanner.saturatingAdd(Long.MaxValue, 1L) == Long.MaxValue)
    }
  }
