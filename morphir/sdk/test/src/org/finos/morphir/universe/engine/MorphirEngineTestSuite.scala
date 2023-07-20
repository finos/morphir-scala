package org.finos.morphir.universe.engine

import org.finos.morphir.testing.munit.MorphirTestSuite
import org.finos.morphir.universe.ir.Name

class MorphirEngineTestSuite extends MorphirTestSuite {
  describe("MorphirEngine") {
    test("Can read a variable when bound and in scope") {
      import Dsl.*
      val prog =
        for {
          varA <- readVariable(Name.fromString("a")) // _ <- subscribe(Subscription())
          (tpe, value) = varA
        } yield value

      val actual = prog.unsafeInterpret(MorphirEngine.unsafe())
      expectEquals(actual, Right(42))
    }
  }
}
