package org.finos.morphir.knowledge.core

import zio.Chunk
import zio.test._
import zio.test.Assertion._

object FactSpec extends DefaultRunnableSpec {
  def spec = suite("Fact Spec")(
    suite("equal")(
      testM("When 2 values are equal we should get back the initial state") {
        val value1 = BigDecimal(19.99)
        val value2 = BigDecimal(19.99)
        val state  = State.empty
        val sut    = Fact.equal(value1, value2)
        for {
          actual <- sut(state).runCollect
        } yield assert(actual)((equalTo(Chunk(Some(state)))))
      },
      testM("Calling equal and eq should be equivalent") {
        val value1    = "Test-Value"
        val value2    = "Test-Value"
        val state     = State.empty
        val equalFact = Fact.equal(value1, value2)
        val eqFact    = Fact.eq(value1, value2)
        for {
          resultsFromEqual <- equalFact(state).runCollect
          resultsFromEq    <- eqFact(state).runCollect
        } yield assert(resultsFromEq)(equalTo(resultsFromEqual)) && assert(resultsFromEq)((equalTo(Chunk(Some(state)))))
      }
    )
  )
}
