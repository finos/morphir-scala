package org.finos.morphir.ir.pipeline

import io.lemonlabs.uri.Urn
import zio.test._
import zio.test.Assertion._
import org.finos.morphir.ir.Name
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
    test("nameToUrn") {
      for {
        result <- Step.nameToUrn.run(Name("hello", "world"))
      } yield assertTrue(result == Urn("local-name", "hello-world"))
    },
    test("Steps should compose") {
      val step1  = Step.succeed(Name("StepSpec"))
      val step2  = Step.nameToUrn
      val result = step1 >>> step2
      for {
        urn <- result.run(())
      } yield assertTrue(urn == Urn("local-name", "step-spec"))

    }
  )
}
