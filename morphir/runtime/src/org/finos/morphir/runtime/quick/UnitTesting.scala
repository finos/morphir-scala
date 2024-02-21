package org.finos.morphir.runtime.quick

import org.finos.morphir.runtime.{Distributions, RTValue}
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.runtime.environment.MorphirEnv
import org.finos.morphir.runtime.services.sdk.MorphirSdk

case class TestSummary(
  message : String,
  success : Boolean
)

object UnitTesting{
    //What should this method's types be?
    //Could return test failures on the error channel and have a unit return type
    //Or could return a test report, with no error (barring evaluation failures?)
    //Need to return a summary even on success, so not that first one
    //Seems messy to return a summary either way
    //Also, I would say "Testing" didn't fail even if the tests did
    private[runtime] def runTests(
      globals: GlobalDefs,
      dists: Distributions
  ): RTAction[MorphirEnv, Nothing, TestSummary] =
    RTAction.environmentWithPure[MorphirSdk] { env =>
      def emptySummary = TestSummary("No tests run", true)
      RTAction.succeed(emptySummary)
    }


}