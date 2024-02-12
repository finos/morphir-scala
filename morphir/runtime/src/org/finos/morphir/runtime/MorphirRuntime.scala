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
import org.finos.morphir.ir.Value.TypedValue

trait MorphirRuntime {

  def evaluate(
      entryPoint: TypedValue,
      param: TypedValue,
      params: TypedValue*
  ): RTAction[MorphirEnv, MorphirRuntimeError, Data]
  def evaluate(
      entryPoint: TypedValue,
      param: Data,
      params: Data*
  ): RTAction[MorphirEnv, MorphirRuntimeError, Data]
  def evaluate(entryPoint: FQName, param: Data, params: Data*): RTAction[MorphirEnv, MorphirRuntimeError, Data]
  def evaluate(
      entryPoint: FQName,
      param: TypedValue,
      params: TypedValue*
  ): RTAction[MorphirEnv, MorphirRuntimeError, Data]

  def applyParams(
      entryPoint: TypedValue,
      params: TypedValue*
  ): RTAction[MorphirEnv, TypeError, TypedValue]
  def evaluate(entryPoint: FQName): RTAction[MorphirEnv, MorphirRuntimeError, Data]
  def evaluate(value: TypedValue): RTAction[MorphirEnv, MorphirRuntimeError, Data]

}

object MorphirRuntime extends MorphirRuntimePlatformSpecific {
  def quick(distributions: Distribution*): TypedMorphirRuntime =
    QuickMorphirRuntime.fromDistributions(distributions: _*)
}
