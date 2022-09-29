package org.finos.morphir.ir.pipeline

import io.lemonlabs.uri.Urn
import zio.test._
import zio.test.Assertion._
import org.finos.morphir.flowz._
import org.finos.morphir.ir.Name
import org.finos.morphir.testing.MorphirBaseSpec

object StepsSpec extends MorphirBaseSpec {
  def spec = suite("NameStepsSpec")(
    test("nameToUrn") {
      for {
        result <- NameSteps.nameToUrn.run(Name("hello", "world"))
      } yield assertTrue(result == Urn("local-name", "hello-world"))
    },
    test("Steps should compose") {
      val step1  = Step.succeed(Name("StepSpec"))
      val step2  = NameSteps.nameToUrn
      val result = step1 >>> step2
      for {
        urn <- result.run(())
      } yield assertTrue(urn == Urn("local-name", "step-spec"))

    }
  )
}
