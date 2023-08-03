package org.finos.morphir.runtime
import org.finos.morphir.ir.Type.UType
import org.finos.morphir.ir.Value.Value
import org.finos.morphir.ir.Value as V
import org.finos.morphir.datamodel.Data
import org.finos.morphir.ir.FQName
import Utils.*
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.runtime.quick.QuickMorphirRuntime
import zio.prelude.fx.ZPure
trait MorphirRuntime[TA, VA] {
  import MorphirRuntime.*
  def evaluate(entryPoint: Value[TA, VA], params: Value[TA, VA]): RT[MorphirRuntimeError, Data]
  def evaluate(entryPoint: Value[TA, VA], params: Data): RT[MorphirRuntimeError, Data]
  def evaluate(entryPoint: FQName, params: Data): RT[MorphirRuntimeError, Data]
  def evaluate(entryPoint: FQName, params: Value[TA, VA]): RT[MorphirRuntimeError, Data]
  // TODO: applyParams can fail if things are bad, but we can't combine Fs yet
  def applyParams(entryPoint: Value[TA, VA], params: Value[TA, VA]*): RT[TypeError, Value[TA, VA]]

  def evaluate(value: Value[TA, VA]): RT[EvaluationError, Data]

}

object MorphirRuntime extends MorphirRuntimePlatformSpecific {
  type RT[+E, +A] = ZPure[Nothing, Unit, Unit, Any, E, A]
  val RT: zio.prelude.fx.ZPure.type = zio.prelude.fx.ZPure
  
  def quick(distribution: Distribution): MorphirRuntime[scala.Unit, UType] =
    QuickMorphirRuntime.fromDistribution(distribution)
}
