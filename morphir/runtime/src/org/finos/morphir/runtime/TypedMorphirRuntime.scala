package org.finos.morphir.runtime
import org.finos.morphir.ir.Type.UType
import org.finos.morphir.ir.Value.Value
import org.finos.morphir.ir.Value as V
import org.finos.morphir.datamodel.Data
import org.finos.morphir.ir.FQName
import Utils.*
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.conversion.*
import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.datamodel.*
import org.finos.morphir.runtime.environment.MorphirEnv
import org.finos.morphir.runtime.exports.*

//TODO: Specify "Either" on lower level
trait TypedMorphirRuntime extends MorphirRuntime[scala.Unit, UType] {
  final def evaluate(
      entryPoint: Value[scala.Unit, UType],
      params: Value[scala.Unit, UType]
  ): RTAction[MorphirEnv, MorphirRuntimeError, Data] =
    for {
      applied   <- applyParams(entryPoint, params)
      evaluated <- evaluate(applied)
    } yield evaluated

  def applyParams(
      entryPoint: Value[scala.Unit, UType],
      params: Value[scala.Unit, UType]*
  ): RTAction[Any, TypeError, Value[scala.Unit, UType]] =
    entryPoint match {
      case Value.Reference.Typed(tpe, entryName) =>
        for {
          tpe <- unCurryTypeFunction(tpe, params.toList.map(_.attributes), Map())
        } yield V.apply(tpe, entryPoint, params.head, params.tail: _*)
      case other => RTAction.fail(UnsupportedType(s"Entry point must be a Reference, instead found $other"))
    }

  def evaluate(entryPoint: Value[scala.Unit, UType], params: Data): RTAction[MorphirEnv, MorphirRuntimeError, Data] = {
    val toValue = ToMorphirValue.summon[Data].typed
    val inputIR = toValue(params)
    evaluate(entryPoint, inputIR)
  }

  def evaluate(entryPoint: FQName, params: Data): RTAction[MorphirEnv, MorphirRuntimeError, Data] = {
    val toValue = ToMorphirValue.summon[Data].typed
    val inputIR = toValue(params)
    evaluate(entryPoint, inputIR)
  }
}
