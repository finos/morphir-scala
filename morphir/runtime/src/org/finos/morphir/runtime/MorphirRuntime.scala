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
  type TypeAttribs
  type ValueAttribs

  def runUnitTests(): RTAction[MorphirEnv, MorphirRuntimeError, TestSummary]

  def evaluate(
      entryPoint: Value[TypeAttribs, ValueAttribs],
      param: Value[TypeAttribs, ValueAttribs],
      params: Value[TypeAttribs, ValueAttribs]*
  ): RTAction[MorphirEnv, MorphirRuntimeError, Data]
  def evaluate(
      entryPoint: Value[TypeAttribs, ValueAttribs],
      param: Data,
      params: Data*
  ): RTAction[MorphirEnv, MorphirRuntimeError, Data]
  def evaluate(entryPoint: FQName, param: Data, params: Data*): RTAction[MorphirEnv, MorphirRuntimeError, Data]
  def evaluate(
      entryPoint: FQName,
      param: Value[TypeAttribs, ValueAttribs],
      params: Value[TypeAttribs, ValueAttribs]*
  ): RTAction[MorphirEnv, MorphirRuntimeError, Data]

  def applyParams(
      entryPoint: Value[TypeAttribs, ValueAttribs],
      params: Value[TypeAttribs, ValueAttribs]*
  ): RTAction[MorphirEnv, TypeError, Value[TypeAttribs, ValueAttribs]]
  def evaluate(entryPoint: FQName): RTAction[MorphirEnv, MorphirRuntimeError, Data]
  def evaluate(
      value: Value[TypeAttribs, ValueAttribs],
      location: Option[CodeLocation] = None
  ): RTAction[MorphirEnv, MorphirRuntimeError, Data]

}

object MorphirRuntime extends MorphirRuntimePlatformSpecific {
  def quick(distributions: Distribution*): TypedMorphirRuntime =
    QuickMorphirRuntime.fromDistributions(distributions: _*)
}
