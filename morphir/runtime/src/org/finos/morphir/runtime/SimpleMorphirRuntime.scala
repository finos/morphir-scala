package org.finos.morphir.runtime

import org.finos.morphir.ir.Type.UType
import org.finos.morphir.ir.Value.Value
import org.finos.morphir.ir.{Value as V}
import org.finos.morphir.datamodel.Data
import org.finos.morphir.ir.distribution.Distribution
import scala.util.{Failure, Success, Try}
import SimpleMorphirRuntime._

case class SimpleMorphirRuntime(distributions:List[Distribution]) extends MorphirRuntime[Result, scala.Unit,UType] {
  private val store:Store[scala.Unit, UType] = Store.empty  //TODO: Populate the

  def applyParams(entrypoint: Value[Unit, UType], params: Value[Unit, UType]): Value[Unit, UType] = {
    // TODO: Implement applyParams  
    V.unitTyped
  }

  def evaluate(entryPoint: Value[Unit, UType]): Result[Data] = {
    // TODO: Implement evaluate
    Result.fail(new Exception("Not implemented"))
  }
}

object SimpleMorphirRuntime {

  def fromDistributions(distributions:Distribution*):SimpleMorphirRuntime =
    SimpleMorphirRuntime(distributions.toList)

  type Result[+A] = Try[A]
  object Result {
    def apply[A](a: => A): Result[A] = Try(a)
    def fail[E <: Throwable](e: E): Result[Nothing] = Failure(e)
  }
}
