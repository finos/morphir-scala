package org.finos.morphir.knowledge.logic

import org.finos.morphir.knowledge.logic.{microkanren => mk}
import mk._
import zio.Chunk
import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect.{ignore, tag}

object MicrokanrenSpec extends ZIOSpecDefault {
  def spec = suite("Fact Spec")(
    suite("equal")(
      test("When 2 values are equal we should get back the initial state") {
        val value1 = BigDecimal(19.99)
        val value2 = BigDecimal(19.99)
        val state  = State.empty
        val sut    = mk.equal(value1, value2)
        for {
          actual <- sut(state).runCollect
        } yield assert(actual)((equalTo(Chunk(state))))
      },
      test("Calling equal, eq, and === should be equivalent") {
        import mk._
        val value1      = "Test-Value"
        val value2      = "Test-Value"
        val state       = State.empty
        val equalGoal   = mk.equal(value1, value2)
        val eqGoal      = mk.eq(value1, value2)
        val equalOpGoal = value1 === value2
        for {
          resultsFromEqual   <- equalGoal(state).runCollect
          resultsFromEq      <- eqGoal(state).runCollect
          resultsFromEqualOp <- equalOpGoal(state).runCollect
        } yield assert(resultsFromEq)(equalTo(resultsFromEqual)) &&
          assert(resultsFromEq)((equalTo(resultsFromEqualOp))) &&
          assert(resultsFromEqual)((equalTo(resultsFromEqualOp)))
      }
    ),
    suite("or/disjunction")(
      test("When the first goal succeeds we should get back the state") {
        import mk._
        val state       = State.empty
        val successGoal = Goal.succeed
        val failingGoal = Goal.fail
        for {
          results <- or(successGoal, failingGoal)(state).runCollect
        } yield assert(results)(equalTo(Chunk(state)))
      },
      test("When the first goal fails and second goal succeeds we should get back the state") {
        import mk._
        val state       = State.empty
        val successGoal = Goal.succeed
        val failingGoal = Goal.fail
        for {
          results <- or(failingGoal, successGoal)(state).runCollect
        } yield assert(results)(equalTo(Chunk(state)))
      }
    ),
    suite("and/conjunction")(
      test("Both goals must succeed for us to get back the state") {
        import mk._
        val state = State.empty
        val g1    = Goal.succeed
        val g2    = Goal.succeed
        for {
          results <- and(g1, g2)(state).runCollect
        } yield assert(results)(equalTo(Chunk(state)))
      },
      test("If the first goal fails the whole goal fails") {
        import mk._
        val state = State.empty
        val g1    = Goal.fail
        val g2    = Goal.succeed
        for {
          results <- and(g1, g2)(state).runCollect
        } yield assert(results)(equalTo(Chunk.empty))
      },
      test("If the second goal fails the whole goal fails") {
        import mk._
        val state = State.empty
        val g1    = Goal.succeed
        val g2    = Goal.fail
        for {
          results <- and(g1, g2)(state).runCollect
        } yield assert(results)(equalTo(Chunk.empty))
      }
    )
  )
}
