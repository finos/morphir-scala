package org.finos.morphir.runtime
import org.finos.morphir.datamodel.Data
import org.finos.morphir.internal.AllTypeLevelModules
import org.finos.morphir.ir.Type.UType
import org.finos.morphir.ir.Value.Value
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.naming._

import org.finos.morphir.runtime.environment.MorphirEnv
import org.finos.morphir.runtime.exports.RTAction
import org.finos.morphir.runtime.quick.QuickMorphirRuntime
trait MorphirRuntime[TA, VA] {
  def evaluate(entryPoint: Value[TA, VA], params: Value[TA, VA]): RTAction[MorphirEnv, MorphirRuntimeError, Data]
  def evaluate(entryPoint: Value[TA, VA], params: Data): RTAction[MorphirEnv, MorphirRuntimeError, Data]
  def evaluate(entryPoint: FQName, params: Data): RTAction[MorphirEnv, MorphirRuntimeError, Data]
  def evaluate(entryPoint: FQName, params: Value[TA, VA]): RTAction[MorphirEnv, MorphirRuntimeError, Data]
  def applyParams(entryPoint: Value[TA, VA], params: Value[TA, VA]*): RTAction[MorphirEnv, TypeError, Value[TA, VA]]

  def evaluate(value: Value[TA, VA]): RTAction[MorphirEnv, EvaluationError, Data]

}

object MorphirRuntime extends MorphirRuntimePlatformSpecific {

  def quick(distributions: Distribution*): MorphirRuntime[scala.Unit, UType] =
    QuickMorphirRuntime.fromDistributions(distributions: _*)
}
