package morphir.buildkit

import kyo.*
import kyo.test.*

class StageTests extends Test[Any]:

  /** Strip a proven-empty `Abort[Nothing]` so an infallible fixture's result can reach `.eval` directly. */
  private def runPure[A, S](v: A < (Abort[Nothing] & S)): A < S =
    Abort.run[Nothing](v).map(_.getOrThrow)

  private val identityStage: Stage[Int, Int, Nothing, Any] =
    Stage.identity[Int]

  private val pureStage: Stage[Int, String, Nothing, Any] =
    Stage.pure((i: Int) => s"value=$i")

  "Stage" - {
    "identity returns the input unchanged" in {
      val program: Int < Abort[Nothing] = identityStage.run(42)
      val out                           = runPure(program).eval
      assert(out == 42)
    }
    "pure applies a pure function" in {
      val program: String < Abort[Nothing] = pureStage.run(7)
      val out                              = runPure(program).eval
      assert(out == "value=7")
    }
    ">>> composes two stages preserving effect rows" in {
      val composed: Stage[Int, String, Nothing, Any] = identityStage >>> pureStage
      val program                                    = composed.run(13)
      val out                                        = runPure(program).eval
      assert(out == "value=13")
    }
    "composition order is left-to-right" in {
      val plusOne: Stage[Int, Int, Nothing, Any]     = Stage.pure((i: Int) => i + 1)
      val toStr: Stage[Int, String, Nothing, Any]    = Stage.pure((i: Int) => i.toString)
      val pipeline: Stage[Int, String, Nothing, Any] = plusOne >>> toStr
      val program                                    = pipeline.run(4)
      val out                                        = runPure(program).eval
      assert(out == "5")
    }
    "apply lifts an effectful function" in {
      val effStage: Stage[Int, Int, Nothing, Any] =
        Stage[Int, Int, Nothing, Any]((i: Int) => (i * 2): Int < Any)
      val program = effStage.run(21)
      val out     = runPure(program).eval
      assert(out == 42)
    }
    "andThen composes like >>>" in {
      val viaOperator = runPure((identityStage >>> pureStage).run(13)).eval
      val viaMethod   = runPure(identityStage.andThen(pureStage).run(13)).eval
      assert(viaOperator == viaMethod)
    }
    "andThen is callable infix" in {
      val composed = identityStage andThen pureStage
      assert(runPure(composed.run(7)).eval == "value=7")
    }
    "named attaches a label" in {
      val labelled = pureStage.named("show")
      assert(labelled.label == Present("show"))
    }
    "a bare stage has no label" in {
      assert(identityStage.label == Absent)
      assert(identityStage.meta == Absent)
    }
    "named carries an optional description" in {
      val labelled = identityStage.named("id", Present("returns its input"))
      assert(labelled.meta == Present(StageMeta("id", Present("returns its input"))))
    }
    "named preserves run semantics" in {
      val bare     = Stage.pure((i: Int) => i * 2)
      val labelled = bare.named("double")
      assert(runPure(bare.run(21)).eval == runPure(labelled.run(21)).eval)
    }
    "labels survive composition on both sides" in {
      val inc  = Stage.pure((i: Int) => i + 1).named("inc")
      val show = Stage.pure((i: Int) => i.toString).named("show")
      assert((inc andThen show).describe == "inc andThen show")
    }
    "describe renders anonymous stages" in {
      assert(identityStage.describe == "<anonymous>")
      assert((identityStage andThen pureStage).describe == "<anonymous> andThen <anonymous>")
    }
    "renaming replaces the label" in {
      val relabelled = pureStage.named("inner").named("outer")
      assert(relabelled.label == Present("outer"))
      val isSingleWrapper = relabelled match
        case Stage.Named(_, Stage.Run(_)) => true
        case _                            => false
      assert(isSingleWrapper)
    }
    "a blank label renders as anonymous" in
      assert(pureStage.named("").describe == "<anonymous>")
    "typed error channel" - {
      "a stage may abort with its declared error and the row records it" in {
        val s: Stage[Int, Int, String, Any] =
          Stage((i: Int) => if i < 0 then Abort.fail("negative") else i * 2)
        Abort.run[String](s.run(-1)).map(r => assert(r == Result.fail("negative")))
      }
      "an infallible stage composes into a fallible pipeline row" in {
        val pure: Stage[Int, Int, Nothing, Any] = Stage((i: Int) => i + 1)
        val fall: Stage[Int, Int, String, Any]  = Stage((i: Int) => if i > 10 then Abort.fail("big") else i)
        val both: Stage[Int, Int, String, Any]  = pure >>> fall
        Abort.run[String](both.run(1)).map(r => assert(r == Result.succeed(2)))
      }
    }
  }
