package org.finos.morphir.knowledge.logic

import org.finos.morphir.knowledge.logic.{microkanren => mk}
import mk._
import zio.Chunk
import zio.test._
import zio.test.Assertion._

object MicrokanrenSpec extends DefaultRunnableSpec {
  def spec = suite("Fact Spec")(
    suite("equal")(
      testM("When 2 values are equal we should get back the initial state") {
        val value1 = BigDecimal(19.99)
        val value2 = BigDecimal(19.99)
        val state  = State.empty
        val sut    = mk.equal(value1, value2)
        for {
          actual <- sut(state).runCollect
        } yield assert(actual)((equalTo(Chunk(Some(state)))))
      },
      testM("Calling equal, eq, and === should be equivalent") {
        import mk._
        val value1      = "Test-Value"
        val value2      = "Test-Value"
        val state       = State.empty
        val equalGoal   = mk.equal(value1, value2)
        val eqGoal      = mk.eq(value1, value2)
        val equalOpGoal = value1 === value2
        for {
          resultsFromEqual <- equalGoal(state).runCollect
          resultsFromEq    <- eqGoal(state).runCollect
        } yield assert(resultsFromEq)(equalTo(resultsFromEqual)) && assert(resultsFromEq)((equalTo(Chunk(Some(state)))))
      }
    )
  )
}
