package morphir.langkit.elm.compiler

import kyo.*
import kyo.test.*

class StageSpec extends Test[Any]:

  private val identityStage: Stage[Int, Int, Any] =
    Stage.identity[Int]

  private val pureStage: Stage[Int, String, Any] =
    Stage.pure((i: Int) => s"value=$i")

  "Stage" - {
    "identity returns the input unchanged" in {
      val program: Int < Any = identityStage.run(42)
      val out                = program.eval
      assert(out == 42)
    }
    "pure applies a pure function" in {
      val program: String < Any = pureStage.run(7)
      val out                   = program.eval
      assert(out == "value=7")
    }
    ">>> composes two stages preserving effect rows" in {
      val composed: Stage[Int, String, Any] = identityStage >>> pureStage
      val program                           = composed.run(13)
      val out                               = program.eval
      assert(out == "value=13")
    }
    "composition order is left-to-right" in {
      val plusOne: Stage[Int, Int, Any]     = Stage.pure((i: Int) => i + 1)
      val toStr: Stage[Int, String, Any]    = Stage.pure((i: Int) => i.toString)
      val pipeline: Stage[Int, String, Any] = plusOne >>> toStr
      val program                           = pipeline.run(4)
      val out                               = program.eval
      assert(out == "5")
    }
    "fromKyo lifts an effectful function" in {
      val effStage: Stage[Int, Int, Any] =
        Stage.fromKyo((i: Int) => (i * 2): Int < Any)
      val program = effStage.run(21)
      val out     = program.eval
      assert(out == 42)
    }
  }
