package org.finos.morphir.flowz

import zio.test._
import zio.test.Assertion._
import org.finos.morphir.testing.MorphirBaseSpec

object StepSpec extends MorphirBaseSpec {
  def spec = suite("StepSpec")(
    test("Step.fail") {
      val step = Step.fail("Fail!!!!")
      assertZIO(step.run("Input").exit)(fails(equalTo("Fail!!!!")))
    },
    test("Step.succeed") {
      for {
        result <- Step.succeed(42).run(())
      } yield assertTrue(result == 42)
    },
    test("Steps should compose") {
      val step1  = Step.succeed("12345")
      val step2  = Step.fromFunction[String, Int](_.toInt)
      val result = step1 >>> step2
      for {
        actual <- result.run(())
      } yield assertTrue(actual == 12345)

    }
  )
}
