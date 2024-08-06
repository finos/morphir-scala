package org.finos.morphir.runtime

import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.datamodel.*
import org.finos.morphir.ir.Type
import org.finos.morphir.naming.*
import org.finos.morphir.runtime.environment.MorphirEnv
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test.*
import zio.test.TestAspect.{ignore, tag}
import zio.{Console, ZIO, ZLayer}
import org.finos.morphir.ir.Distribution

object DefaultsTestingSpec extends MorphirBaseSpec {
  val path =
    "examples/morphir-elm-projects/defaults-tests/morphir-ir.json"

  val testLayer: ZLayer[Any, Throwable, TypedMorphirRuntime] = ZLayer(for {
    dist <- EvaluationLibrary.loadDistributionFromFileZIO(path)
  } yield MorphirRuntime.quick(dist))

  def getRT =
    ZIO.serviceWithZIO[TypedMorphirRuntime] { rt => ZIO.succeed(rt) }

  def spec = suite("MDM Defaults Tests")(
    suite("Default Record")(
      test("Record matches Default") {
        ZIO.serviceWithZIO[TypedMorphirRuntime] { rt =>
          for {
            res <- rt.evaluate(qn"Defaults:ExampleModule:expectedDefault")
              .provideEnvironment(MorphirEnv.live)
              .toZIOWith(RTExecutionContext.typeChecked)
            default <- MDMDefaults.default(res.shape)
          } yield assertTrue(res == default)
        }
      },
      test("Record doesn't match non-default") {
        ZIO.serviceWithZIO[TypedMorphirRuntime] { rt =>
          for {
            res <- rt.evaluate(qn"Defaults:ExampleModule:nonDefault")
              .provideEnvironment(MorphirEnv.live)
              .toZIOWith(RTExecutionContext.typeChecked)
            default <- MDMDefaults.default(res.shape)
          } yield assertTrue(res != default)
        }
      },
      test("Record can be filled with defaults") {
        ZIO.serviceWithZIO[TypedMorphirRuntime] { rt =>
          for {
            small <- rt.evaluate(qn"Defaults:ExampleModule:smallExample")
              .provideEnvironment(MorphirEnv.live)
              .toZIOWith(RTExecutionContext.typeChecked)
            larger <- rt.evaluate(qn"Defaults:ExampleModule:largerExample")
              .provideEnvironment(MorphirEnv.live)
              .toZIOWith(RTExecutionContext.typeChecked)
            largerConcept = larger.shape
            filled  <- MDMDefaults.fillWithDefaults(small, largerConcept)
            default <- MDMDefaults.default(largerConcept)
          } yield assertTrue(filled == larger)
        }
      }
    )
  ).provideLayerShared(testLayer)
}
