package org.finos.morphir.runtime

import org.finos.morphir.naming._
import org.finos.morphir.ir.Type.UType
import org.finos.morphir.ir.Value.Value
import org.finos.morphir.ir.Value as V
import org.finos.morphir.datamodel.Data
import Utils.*
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.conversion.*
import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.datamodel.*
import org.finos.morphir.runtime.environment.MorphirEnv
import org.finos.morphir.runtime.exports.*

trait TypedMorphirRuntime extends MorphirRuntime {
  type TypeAttribs  = TypedMorphirRuntime.TypeAttribs
  type ValueAttribs = TypedMorphirRuntime.ValueAttribs

  final def evaluate(
      entryPoint: Value[scala.Unit, UType],
      param: Value[scala.Unit, UType],
      params: Value[scala.Unit, UType]*
  ): RTAction[MorphirEnv, MorphirRuntimeError, Data] =
    for {
      applied   <- applyParams(entryPoint, (param +: params): _*)
      evaluated <- evaluate(applied)
    } yield evaluated

  def evaluate(
      entryPoint: Value[scala.Unit, UType],
      param: Data,
      params: Data*
  ): RTAction[MorphirEnv, MorphirRuntimeError, Data] = {
    val toValue  = ToMorphirValue.summon[Data].typed
    val inputIR  = toValue(param)
    val inputIRs = params.map(toValue(_))
    evaluate(entryPoint, inputIR, inputIRs: _*)
  }

  def evaluate(entryPoint: FQName, param: Data, params: Data*): RTAction[MorphirEnv, MorphirRuntimeError, Data] = {
    val toValue  = ToMorphirValue.summon[Data].typed
    val inputIR  = toValue(param)
    val inputIRs = params.map(toValue(_))
    evaluate(entryPoint, inputIR, inputIRs: _*)
  }

  
}

object TypedMorphirRuntime {
  final type TypeAttribs  = scala.Unit
  final type ValueAttribs = UType
  type RuntimeValue       = Value[TypeAttribs, ValueAttribs]
  type RuntimeDefinition  = Value.Definition[TypeAttribs, ValueAttribs]
}
