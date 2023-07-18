package org.finos.morphir.foundations.capabilities.free
import org.finos.morphir.testing.munit.*

class FreeSuite extends MorphirTestSuite {
  describe("Free") {
    test("should be able to create a Free dsl") {
      import org.finos.morphir.foundations.capabilities.free.example.counter
      import counter.dsl.*
      // val program = for {
      //   original   <- get()
      //   _          <- increment(1)
      //   _          <- increment(2)
      //   _          <- increment(3)
      //   checkpoint <- get()
      //   _          <- decrement(1)
      //   _          <- decrement(2)
      //   _          <- decrement(3)
      //   finalVal   <- get()
      // } yield (original, checkpoint, finalVal)
      val program = get() // .map(a => (a, a, a))
      pprint.pprintln(program)
      val result = program.unsafeInterpret(counter.interpreters.unsafeInterpreter(0))
      // expectEquals(result, Right((0, 6, 0)))
      expectEquals(result, Right(0))
    }
  }
}
