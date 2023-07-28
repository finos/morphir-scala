package org.finos.morphir.runtime.quick

import org.finos.morphir.ir.Type.UType
import org.finos.morphir.ir.Value.Value
import org.finos.morphir.ir.Value as V
import org.finos.morphir.datamodel.Data
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.distribution.Distribution.Library
import org.finos.morphir.ir.Type.Type
import org.finos.morphir.runtime.MorphirRuntime
import org.finos.morphir.ir.FQName
import org.finos.morphir.ir.conversion.*
import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.datamodel.{:: as _, *}

import scala.util.{Failure, Success, Try}
import org.finos.morphir.runtime.{EvaluationError, MorphirRuntimeError}

private[runtime] case class QuickMorphirRuntime(library : Library, store : Store[scala.Unit, UType])
  extends MorphirRuntime[Either, scala.Unit, UType] {
  //private val store: Store[scala.Unit, UType] = Store.empty //


  def evaluate(entryPoint: FQName, params: Value[scala.Unit, UType]): Either[MorphirRuntimeError, Data] = {
    val fetchedType: UType = fetchType(entryPoint)
    evaluate(Value.Reference.Typed(fetchedType, entryPoint), params)
  }

  def evaluate(value: Value[scala.Unit, UType]): Either[EvaluationError, Data] = {
    try{
      Right(EvaluatorQuick.eval(value, store, library))
    } catch{
      case e : EvaluationError => Left(e)
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



  def fetchType(ref : FQName) : UType = {
    val tpe_raw: Type[Unit] = store.getDefinition(ref)
      .get
      .asInstanceOf[SDKValue.SDKValueDefinition[Unit, UType]]
      .definition
      .outputType
    val actual = tpe_raw match {
      case Type.Function(_, _, returnType) => returnType
      case _ => tpe_raw
    }
    actual
  }
}

object QuickMorphirRuntime {

  def fromDistribution(distribution: Distribution): QuickMorphirRuntime ={
    val library = distribution
      .asInstanceOf[Library]
    val store = Store.fromLibrary(library)
    QuickMorphirRuntime(library, store)
  }


}
