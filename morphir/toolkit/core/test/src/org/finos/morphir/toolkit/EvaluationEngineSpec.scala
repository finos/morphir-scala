package org.finos.morphir
package toolkit

import org.finos.morphir.testing.MorphirBaseSpec

import org.finos.morphir.ir.{Type => T}
import org.finos.morphir.ir.{Value => V}

import zio.{test => _, _}
import zio.test._
import zio.test.TestAspect.{ignore, tag}
import org.finos.morphir.testing.MorphirBaseSpec
import EvaluationContext.{Variables, VariableRef}
import V._
import zio.prelude.fx._
import org.finos.morphir.ir.Type

object EvaluationEngineSpec extends MorphirBaseSpec with TypedEvaluationEngineSpec with EvaluationEngineContext {
  def spec = suite("EvaluationEngineSpec")(typedEvaluationEngineSuite)
}


trait EvaluationEngineContext { self:MorphirBaseSpec =>
  def contextSuite = suite("Context Suite")(
    //test("Scopes can be defined ")()
  )    
}
