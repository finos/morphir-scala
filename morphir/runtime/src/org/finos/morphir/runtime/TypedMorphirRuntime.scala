package org.finos.morphir.runtime
import org.finos.morphir.ir.Type.UType
import org.finos.morphir.ir.Value.Value
import org.finos.morphir.ir.Value as V
import org.finos.morphir.datamodel.Data
import org.finos.morphir.ir.{FQName}
import Utils.*
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.conversion.*
import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.datamodel.{:: as _, *}

//TODO: Specify "Either" on lower level
trait TypedMorphirRuntime extends MorphirRuntime[Either, scala.Unit, UType] {
  def applyParams(entryPoint: Value[scala.Unit, UType], params: Value[scala.Unit, UType]*): Value[scala.Unit, UType]= {
    val asEither = entryPoint match {
      case Value.Reference.Typed(tpe, entryName) => {
        for {
          tpe <- unCurryTypeFunction(tpe, params.toList.map(_.attributes))
        } yield V.apply(tpe, entryPoint, params.head, params.tail:_*)
      }
      case other => Left(NotImplemented(s"Entry point must be a Reference, instead found $other"))
    }
    asEither match{
      case Right(value) => value
      case Left(error) => throw error //Function should return error, but that requires F[_. _] to be mappable
    }
  }

  def evaluate(entryPoint: Value[scala.Unit, UType], params: Data): Either[MorphirRuntimeError, Data] = {
    val toValue = ToMorphirValue.summon[Data].typed
    val inputIR = toValue(params)
    evaluate(entryPoint, inputIR)
  }

  def evaluate(entryPoint: FQName, params: Data): Either[MorphirRuntimeError, Data] = {
    val toValue = ToMorphirValue.summon[Data].typed
    val inputIR = toValue(params)
    evaluate(entryPoint, inputIR)
  }
}
