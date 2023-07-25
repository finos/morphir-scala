package org.finos.morphir.universe.engine

import org.finos.morphir.testing.munit.MorphirTestSuite
import org.finos.morphir.universe.ir.Name
import org.finos.morphir.universe.sdk.types.Basics.{Integer as MInteger, Float as MFloat}

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

    test("Can do simple integer arithmetic with add and multiply") {
      import Dsl.*
      val prog =
        for {
          term1 <- Morphir.SDK.Basics.add(MInteger(5), MInteger(5))
          term2 <- Morphir.SDK.Basics.add(MInteger(20), MInteger(22))
          res   <- Morphir.SDK.Basics.multiply(term1, term2)
        } yield res

      val actual = prog.unsafeInterpret(MorphirEngine.unsafe())
      expectEquals(actual, Right(MInteger(420)))
    }
  }
}
