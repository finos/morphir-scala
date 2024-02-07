package org.finos.morphir.runtime
import org.finos.morphir.datamodel.Data
import org.finos.morphir.internal.AllTypeLevelModules
import org.finos.morphir.ir.Type.UType
import org.finos.morphir.ir.Value.Value
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.naming.*
import org.finos.morphir.runtime.environment.MorphirEnv
import org.finos.morphir.runtime.exports.RTAction
import org.finos.morphir.runtime.quick.QuickMorphirRuntime
import org.finos.morphir.runtime.MorphirRuntimeError.*

trait MorphirRuntime {
  def evaluate(
      entryPoint: Value[scala.Unit, UType],
      param: Value[scala.Unit, UType],
      params: Value[scala.Unit, UType]*
  ): RTAction[MorphirEnv, MorphirRuntimeError, Data]
  def evaluate(
      entryPoint: Value[scala.Unit, UType],
      param: Data,
      params: Data*
  ): RTAction[MorphirEnv, MorphirRuntimeError, Data]
  def evaluate(entryPoint: FQName, param: Data, params: Data*): RTAction[MorphirEnv, MorphirRuntimeError, Data]
  def evaluate(
      entryPoint: FQName,
      param: Value[scala.Unit, UType],
      params: Value[scala.Unit, UType]*
  ): RTAction[MorphirEnv, MorphirRuntimeError, Data]

  def applyParams(
      entryPoint: Value[scala.Unit, UType],
      params: Value[scala.Unit, UType]*
  ): RTAction[MorphirEnv, TypeError, Value[scala.Unit, UType]]
  def evaluate(entryPoint: FQName): RTAction[MorphirEnv, MorphirRuntimeError, Data]
  def evaluate(value: Value[scala.Unit, UType]): RTAction[MorphirEnv, MorphirRuntimeError, Data]

}

object MorphirRuntime extends MorphirRuntimePlatformSpecific {
  def quick(distributions: Distribution*): TypedMorphirRuntime =
    QuickMorphirRuntime.fromDistributions(distributions: _*)
}
