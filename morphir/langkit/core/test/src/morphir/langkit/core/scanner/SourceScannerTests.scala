package morphir.langkit.core.scanner

import kyo.test.*
import morphir.langkit.core.Span
import scala.compiletime.testing.typeCheckErrors
import scala.language.strictEquality

class SourceScannerTests extends Test[Any]:

  private def limited(
      input: Long,
      work: Long,
      nesting: Int = 1,
      output: Long = 1L
  ): ScanBudget.Limited =
    ScanBudget.limited(
      maxInputLength = InputSize.codeUnits(input),
      maxWork = WorkUnits(work),
      maxNestingDepth = NestingDepth(nesting),
      maxOutputNodes = NodeCount(output)
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
      val phase                   = ScanPhase("test")
      var retained: SourceScanner = null
      val result                  = SourceScanner.scan("abc", limited(input = 3L, work = 2L), Some(phase)) { scanner =>
        retained = scanner
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
      assert(closedMessage(retained.offset).contains("scanner session is closed"))
    }

    "charges explicit typed work at the exact boundary" in {
      val phase  = ScanPhase("line-local inspection")
      val result = SourceScanner.scan("", limited(input = 1L, work = 2L), Some(phase)) { scanner =>
        scanner.chargeWork(WorkUnits(2L))
        scanner.chargeWork(WorkUnits(1L))
      }

      assert(
        result == ScanResult.Failure(
          ScanFailure(
            exceeded = ScanLimitExceeded.Work(limit = WorkUnits(2L), attempted = WorkUnits(3L)),
            offset = SourceOffset.start,
            phase = Some(phase)
          )
        )
      )
    }

    "returns the first work failure when the callback swallows exhaustion" in {
      val result = SourceScanner.scan("a", limited(input = 1L, work = 1L)) { scanner =>
        scanner.peek()
        try scanner.peek()
        catch case _: Throwable => ()
        "ignored"
      }

      assert(
        result == ScanResult.Failure(
          ScanFailure(
            exceeded = ScanLimitExceeded.Work(limit = WorkUnits(1L), attempted = WorkUnits(2L)),
            offset = SourceOffset.start,
            phase = None
          )
        )
      )
    }

    "propagates exhaustion replayed into a different scan and closes that scan" in {
      val callbackFailure             = new RuntimeException("leave first callback")
      var captured: Throwable         = null
      var firstScanner: SourceScanner = null

      try
        SourceScanner.scan("a", limited(input = 1L, work = 1L)) { scanner =>
          firstScanner = scanner
          scanner.peek()
          try scanner.peek()
          catch case error: Throwable => captured = error
          throw callbackFailure
        }
      catch case error: Throwable => assert(error.eq(callbackFailure))

      var secondScanner: SourceScanner = null
      var replayed: Throwable          = null
      try
        SourceScanner.scan("b") { scanner =>
          secondScanner = scanner
          throw captured
        }
      catch case error: Throwable => replayed = error

      assert(captured != null)
      assert(replayed.eq(captured))
      assert(closedMessage(firstScanner.offset).contains("scanner session is closed"))
      assert(closedMessage(secondScanner.offset).contains("scanner session is closed"))
    }

    "rethrows the same first exhaustion after repeated caught attempts" in {
      var first: Throwable  = null
      var second: Throwable = null
      val result            = SourceScanner.scan("a", limited(input = 1L, work = 1L)) { scanner =>
        scanner.peek()
        try scanner.peek()
        catch case error: Throwable => first = error
        try scanner.peek()
        catch case error: Throwable => second = error
        ()
      }

      assert(first != null)
      assert(second.eq(first))
      assert(
        result == ScanResult.Failure(
          ScanFailure(
            exceeded = ScanLimitExceeded.Work(limit = WorkUnits(1L), attempted = WorkUnits(2L)),
            offset = SourceOffset.start,
            phase = None
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
      val expected                = new RuntimeException("callback failed")
      var caught: Throwable       = null
      var retained: SourceScanner = null

      try
        SourceScanner.scan("a") { scanner =>
          retained = scanner
          throw expected
        }
      catch case error: Throwable => caught = error

      assert(caught.eq(expected))
      assert(closedMessage(retained.offset).contains("scanner session is closed"))
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
      assert(closedMessage(retained.chargeWork(WorkUnits(1L))).contains(closed))
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

    "navigates an empty source without work" in {
      val result = SourceScanner.scan("", limited(input = 1L, work = 1L)) { scanner =>
        assert(scanner.offset == SourceOffset.start)
        assert(scanner.isAtEnd)
        assert(scanner.mark == SourceOffset.start)
        assert(scanner.peek().isEmpty)
        assert(scanner.peek(CodeUnitCount(Int.MaxValue)).isEmpty)
        assert(scanner.remaining.isEmpty)
        assert(scanner.remaining.span == Span.zero)
        scanner.offset
      }

      assert(result == ScanResult.Success(SourceOffset.start))
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

    "restores checkpoints without refunding speculative work" in {
      val result = SourceScanner.scan("abc", limited(input = 3L, work = 4L)) { scanner =>
        val checkpoint = scanner.checkpoint()
        scanner.advance(CodeUnitCount(2))
        assert(scanner.offset == SourceOffset(2))
        scanner.restore(checkpoint)
        assert(scanner.offset == SourceOffset.start)
        scanner.advance(CodeUnitCount(2))
        assert(scanner.offset == SourceOffset(2))
        scanner.peek()
      }

      assert(
        result == ScanResult.Failure(
          ScanFailure(
            exceeded = ScanLimitExceeded.Work(limit = WorkUnits(4L), attempted = WorkUnits(5L)),
            offset = SourceOffset(2),
            phase = None
          )
        )
      )
    }

    "restores checkpoints captured before and after movement to their exact offsets" in {
      val result = SourceScanner.scan("abc") { scanner =>
        val before = scanner.checkpoint()
        scanner.advance()
        val after = scanner.checkpoint()
        scanner.advance()
        scanner.restore(after)
        assert(scanner.offset == SourceOffset(1))
        scanner.restore(before)
        scanner.offset
      }

      assert(result == ScanResult.Success(SourceOffset.start))
    }

    "does not expose checkpoint construction to scanner-package consumers" in {
      val constructorErrors = typeCheckErrors("new SourceScanner.Checkpoint(null, 0)")
      val factoryErrors     = typeCheckErrors("ScanCheckpoint.create(null, 0)")

      assert(constructorErrors.nonEmpty)
      assert(factoryErrors.nonEmpty)
    }

    "rejects foreign checkpoints without damaging the receiving scanner" in {
      var foreign: ScanCheckpoint = null
      SourceScanner.scan("a") { scanner => foreign = scanner.checkpoint() }

      var retained: SourceScanner = null
      val result                  = SourceScanner.scan("b") { scanner =>
        retained = scanner
        assert(rejectsArgument(scanner.restore(foreign)))
        assert(scanner.peek().contains('b'))
        scanner.offset
      }

      assert(result == ScanResult.Success(SourceOffset.start))
      assert(closedMessage(retained.offset).contains("scanner session is closed"))
    }

    "rejects checkpoint restoration after the owning scanner closes" in {
      var retained: SourceScanner    = null
      var checkpoint: ScanCheckpoint = null
      SourceScanner.scan("a") { scanner =>
        retained = scanner
        checkpoint = scanner.checkpoint()
      }

      assert(closedMessage(retained.restore(checkpoint)).contains("scanner session is closed"))
    }

    "requires successful parser phases to advance" in {
      val phase  = ScanPhase("inline parser")
      val result = SourceScanner.scan("a") { scanner =>
        val value = scanner.requireProgress(phase) {
          scanner.advance()
          42
        }
        assert(value == 42)
        scanner.offset
      }

      assert(result == ScanResult.Success(SourceOffset(1)))
    }

    "rejects parser phases that finish at their starting offset" in {
      val phase                     = ScanPhase("block parser")
      var stationaryMessage: String = null
      var restoredMessage: String   = null

      val result = SourceScanner.scan("a") { scanner =>
        try scanner.requireProgress(phase)(())
        catch case error: IllegalStateException => stationaryMessage = error.getMessage

        val checkpoint = scanner.checkpoint()
        try
          scanner.requireProgress(phase) {
            scanner.advance()
            scanner.restore(checkpoint)
          }
        catch case error: IllegalStateException => restoredMessage = error.getMessage
      }

      assert(result == ScanResult.Success(()))
      assert(stationaryMessage == "block parser made no progress")
      assert(restoredMessage == "block parser made no progress")
    }

    "rejects parser phases that restore from offset one to an earlier offset" in {
      val phase                   = ScanPhase("backtracking parser")
      var backwardMessage: String = null
      val result                  = SourceScanner.scan("ab") { scanner =>
        val earlier = scanner.checkpoint()
        scanner.advance()
        try scanner.requireProgress(phase)(scanner.restore(earlier))
        catch case error: IllegalStateException => backwardMessage = error.getMessage
        scanner.offset
      }

      assert(result == ScanResult.Success(SourceOffset.start))
      assert(backwardMessage == "backtracking parser made no progress")
    }

    "rejects both backward legs when alternating between checkpoints" in {
      val phase                  = ScanPhase("alternating parser")
      var firstBackward: String  = null
      var secondBackward: String = null
      val result                 = SourceScanner.scan("ab") { scanner =>
        val zero = scanner.checkpoint()
        scanner.advance()
        val one = scanner.checkpoint()

        try scanner.requireProgress(phase)(scanner.restore(zero))
        catch case error: IllegalStateException => firstBackward = error.getMessage

        scanner.requireProgress(phase)(scanner.restore(one))

        try scanner.requireProgress(phase)(scanner.restore(zero))
        catch case error: IllegalStateException => secondBackward = error.getMessage
      }

      assert(result == ScanResult.Success(()))
      assert(firstBackward == "alternating parser made no progress")
      assert(secondBackward == "alternating parser made no progress")
    }

    "propagates requireProgress operation exceptions unchanged" in {
      val expected          = new RuntimeException("phase failed")
      var caught: Throwable = null

      try SourceScanner.scan("a")(_.requireProgress(ScanPhase("parser"))(throw expected))
      catch case error: Throwable => caught = error

      assert(caught.eq(expected))
    }

    "enforces nesting at the exact boundary without entering rejected work" in {
      val phase        = ScanPhase("nested parser")
      var innerEntered = false
      val result       = SourceScanner.scan("a", limited(input = 1L, work = 10L), Some(phase)) { scanner =>
        scanner.withNesting {
          scanner.withNesting {
            innerEntered = true
          }
        }
      }

      assert(!innerEntered)
      assert(
        result == ScanResult.Failure(
          ScanFailure(
            exceeded = ScanLimitExceeded.Nesting(limit = NestingDepth(1), attempted = NestingDepth(2)),
            offset = SourceOffset.start,
            phase = Some(phase)
          )
        )
      )
    }

    "unwinds accepted nesting after an ordinary exception" in {
      val expected          = new RuntimeException("nested operation failed")
      var caught: Throwable = null
      val result            = SourceScanner.scan("a", limited(input = 1L, work = 10L)) { scanner =>
        try scanner.withNesting(throw expected)
        catch case error: Throwable => caught = error
        scanner.withNesting(7)
      }

      assert(caught.eq(expected))
      assert(result == ScanResult.Success(7))
    }

    "enforces output nodes at the exact boundary and keeps exhaustion sticky" in {
      val phase             = ScanPhase("output builder")
      var first: Throwable  = null
      var second: Throwable = null
      val result = SourceScanner.scan("a", limited(input = 1L, work = 10L, output = 2L), Some(phase)) { scanner =>
        scanner.chargeOutputNodes(NodeCount(2L))
        scanner.chargeOutputNodes(NodeCount(0L))
        try scanner.chargeOutputNodes(NodeCount.one)
        catch case error: Throwable => first = error
        try scanner.chargeOutputNodes(NodeCount(0L))
        catch case error: Throwable => second = error
      }

      assert(first != null)
      assert(second.eq(first))
      assert(
        result == ScanResult.Failure(
          ScanFailure(
            exceeded = ScanLimitExceeded.OutputNodes(limit = NodeCount(2L), attempted = NodeCount(3L)),
            offset = SourceOffset.start,
            phase = Some(phase)
          )
        )
      )
    }

    "rejects output beyond Long.MaxValue after accepting the exact ceiling" in {
      var caught: Throwable = null
      val result            = SourceScanner.scan(
        "a",
        limited(input = 1L, work = 10L, output = Long.MaxValue)
      ) { scanner =>
        scanner.chargeOutputNodes(NodeCount(Long.MaxValue))
        try scanner.chargeOutputNodes(NodeCount.one)
        catch case error: Throwable => caught = error
      }

      assert(caught != null)
      assert(
        result == ScanResult.Failure(
          ScanFailure(
            exceeded = ScanLimitExceeded.OutputNodes(
              limit = NodeCount(Long.MaxValue),
              attempted = NodeCount(Long.MaxValue)
            ),
            offset = SourceOffset.start,
            phase = None
          )
        )
      )
    }

    "contains swallowed nesting exhaustion and foreign replay" in {
      var captured: Throwable = null
      val first               = SourceScanner.scan("a", limited(input = 1L, work = 10L)) { scanner =>
        scanner.withNesting {
          try scanner.withNesting(())
          catch case error: Throwable => captured = error
        }
        "ignored"
      }

      var secondScanner: SourceScanner = null
      var replayed: Throwable          = null
      try
        SourceScanner.scan("b") { scanner =>
          secondScanner = scanner
          throw captured
        }
      catch case error: Throwable => replayed = error

      assert(
        first == ScanResult.Failure(
          ScanFailure(
            exceeded = ScanLimitExceeded.Nesting(limit = NestingDepth(1), attempted = NestingDepth(2)),
            offset = SourceOffset.start,
            phase = None
          )
        )
      )
      assert(replayed.eq(captured))
      assert(closedMessage(secondScanner.offset).contains("scanner session is closed"))
    }

    "contains swallowed output exhaustion and foreign replay" in {
      var captured: Throwable = null
      val first               = SourceScanner.scan("a", limited(input = 1L, work = 10L)) { scanner =>
        scanner.chargeOutputNodes(NodeCount.one)
        try scanner.chargeOutputNodes(NodeCount.one)
        catch case error: Throwable => captured = error
        "ignored"
      }

      var secondScanner: SourceScanner = null
      var replayed: Throwable          = null
      try
        SourceScanner.scan("b") { scanner =>
          secondScanner = scanner
          throw captured
        }
      catch case error: Throwable => replayed = error

      assert(
        first == ScanResult.Failure(
          ScanFailure(
            exceeded = ScanLimitExceeded.OutputNodes(limit = NodeCount.one, attempted = NodeCount(2L)),
            offset = SourceOffset.start,
            phase = None
          )
        )
      )
      assert(replayed.eq(captured))
      assert(closedMessage(secondScanner.offset).contains("scanner session is closed"))
    }

    "keeps ownership progress and lifecycle invariants when budgets are unbounded" in {
      var foreign: ScanCheckpoint = null
      SourceScanner.scan("a") { scanner => foreign = scanner.checkpoint() }

      var retained: SourceScanner = null
      var own: ScanCheckpoint     = null
      val result                  = SourceScanner.scan("abc", ScanBudget.UnsafeUnbounded) { scanner =>
        retained = scanner
        own = scanner.checkpoint()
        assert(rejectsArgument(scanner.restore(foreign)))

        def nest(remaining: Int): Int =
          if remaining == 0 then 0
          else scanner.withNesting(nest(remaining - 1) + 1)

        assert(nest(32) == 32)
        scanner.chargeOutputNodes(NodeCount(1024L))
        scanner.requireProgress(ScanPhase("unbounded parser")) {
          scanner.advance()
        }
        scanner.offset
      }

      assert(result == ScanResult.Success(SourceOffset(1)))
      assert(closedMessage(retained.restore(own)).contains("scanner session is closed"))
    }

    "sustains repeated work and output charges only when unbounded" in {
      val repetitions = 10000
      val workLimited = SourceScanner.scan(
        "a",
        limited(input = 1L, work = repetitions.toLong - 1L, output = repetitions.toLong)
      ) { scanner =>
        var index = 0
        while index < repetitions do
          scanner.peek()
          index += 1
      }
      val outputLimited = SourceScanner.scan(
        "a",
        limited(input = 1L, work = repetitions.toLong, output = repetitions.toLong - 1L)
      ) { scanner =>
        var index = 0
        while index < repetitions do
          scanner.chargeOutputNodes(NodeCount.one)
          index += 1
      }
      val unbounded = SourceScanner.scan("a", ScanBudget.UnsafeUnbounded) { scanner =>
        var workIndex = 0
        while workIndex < repetitions do
          scanner.peek()
          workIndex += 1

        var outputIndex = 0
        while outputIndex < repetitions do
          scanner.chargeOutputNodes(NodeCount.one)
          outputIndex += 1

        (workIndex, outputIndex)
      }

      assert(
        workLimited == ScanResult.Failure(
          ScanFailure(
            exceeded = ScanLimitExceeded.Work(
              limit = WorkUnits(repetitions.toLong - 1L),
              attempted = WorkUnits(repetitions.toLong)
            ),
            offset = SourceOffset.start,
            phase = None
          )
        )
      )
      assert(
        outputLimited == ScanResult.Failure(
          ScanFailure(
            exceeded = ScanLimitExceeded.OutputNodes(
              limit = NodeCount(repetitions.toLong - 1L),
              attempted = NodeCount(repetitions.toLong)
            ),
            offset = SourceOffset.start,
            phase = None
          )
        )
      )
      assert(unbounded == ScanResult.Success((repetitions, repetitions)))
    }

    "rejects every new operation after the scanner closes" in {
      var retained: SourceScanner    = null
      var checkpoint: ScanCheckpoint = null
      SourceScanner.scan("a") { scanner =>
        retained = scanner
        checkpoint = scanner.checkpoint()
      }

      val closed = "scanner session is closed"
      assert(closedMessage(retained.checkpoint()).contains(closed))
      assert(closedMessage(retained.restore(checkpoint)).contains(closed))
      assert(closedMessage(retained.requireProgress(ScanPhase("parser"))(())).contains(closed))
      assert(closedMessage(retained.withNesting(())).contains(closed))
      assert(closedMessage(retained.chargeOutputNodes(NodeCount.one)).contains(closed))
    }

    "saturates work arithmetic at Long.MaxValue" in {
      assert(SourceScanner.saturatingAdd(Long.MaxValue - 1L, 1L) == Long.MaxValue)
      assert(SourceScanner.saturatingAdd(Long.MaxValue - 1L, 2L) == Long.MaxValue)
      assert(SourceScanner.saturatingAdd(Long.MaxValue, 1L) == Long.MaxValue)
    }

    "detects work beyond Long.MaxValue despite saturated reporting" in {
      assert(!SourceScanner.wouldExceedLimit(0L, Long.MaxValue, Long.MaxValue))
      assert(!SourceScanner.wouldExceedLimit(Long.MaxValue - 1L, 1L, Long.MaxValue))
      assert(SourceScanner.wouldExceedLimit(Long.MaxValue, 1L, Long.MaxValue))
    }

    "detects saturated nesting attempts without incrementing Int.MaxValue" in {
      assert(!SourceScanner.wouldExceedNesting(0, 1))
      assert(SourceScanner.wouldExceedNesting(1, 1))
      assert(SourceScanner.wouldExceedNesting(Int.MaxValue, Int.MaxValue))
    }
  }
