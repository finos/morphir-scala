package morphir.knowledge.logic

import morphir.knowledge.logic.{microkanren => mk}
import mk._
import kyo.Chunk
import kyo.test.*

class MicrokanrenSpec extends Test[Any]:
  "equal" - {
    "When 2 values are equal we should get back the initial state" in {
      val value1 = BigDecimal(19.99)
      val value2 = BigDecimal(19.99)
      val state  = State.empty
      val sut    = mk.equal(value1, value2)
      val actual = sut(state).runCollect
      assert(actual == Chunk(state))
    }

    "Calling equal, eq, and === should be equivalent" in {
      import mk._
      val value1      = "Test-Value"
      val value2      = "Test-Value"
      val state       = State.empty
      val equalGoal   = mk.equal(value1, value2)
      val eqGoal      = mk.eq(value1, value2)
      val equalOpGoal = value1 === value2

      val resultsFromEqual   = equalGoal(state).runCollect
      val resultsFromEq      = eqGoal(state).runCollect
      val resultsFromEqualOp = equalOpGoal(state).runCollect
      assert(resultsFromEq == resultsFromEqual)
      assert(resultsFromEq == resultsFromEqualOp)
      assert(resultsFromEqual == resultsFromEqualOp)
    }
  }

  "or/disjunction" - {
    "When the first goal succeeds we should get back the state" in {
      import mk._
      val state       = State.empty
      val successGoal = Goal.succeed
      val failingGoal = Goal.fail
      val results     = or(successGoal, failingGoal)(state).runCollect
      assert(results == Chunk(state))
    }

    "When the first goal fails and second goal succeeds we should get back the state" in {
      import mk._
      val state       = State.empty
      val successGoal = Goal.succeed
      val failingGoal = Goal.fail
      val results     = or(failingGoal, successGoal)(state).runCollect
      assert(results == Chunk(state))
    }
  }

  "and/conjunction" - {
    "Both goals must succeed for us to get back the state" in {
      import mk._
      val state   = State.empty
      val g1      = Goal.succeed
      val g2      = Goal.succeed
      val results = and(g1, g2)(state).runCollect
      assert(results == Chunk(state))
    }

    "If the first goal fails the whole goal fails" in {
      import mk._
      val state   = State.empty
      val g1      = Goal.fail
      val g2      = Goal.succeed
      val results = and(g1, g2)(state).runCollect
      assert(results == Chunk.empty)
    }

    "If the second goal fails the whole goal fails" in {
      import mk._
      val state   = State.empty
      val g1      = Goal.succeed
      val g2      = Goal.fail
      val results = and(g1, g2)(state).runCollect
      assert(results == Chunk.empty)
    }
  }
end MicrokanrenSpec
