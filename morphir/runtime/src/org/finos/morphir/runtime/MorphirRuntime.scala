package org.finos.morphir.runtime
import org.finos.morphir.ir.Type.UType
import org.finos.morphir.ir.Value.Value
import org.finos.morphir.ir.Value as V
import org.finos.morphir.datamodel.Data
import org.finos.morphir.ir.{FQName}
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.runtime.quick.QuickMorphirRuntime

trait MorphirRuntime[F[+_, +_], TA, VA] {

  final def evaluate(entrypoint: Value[TA, VA], params: Value[TA, VA]): F[EvaluationError, Data] =
    evaluate(applyParams(entrypoint, params))
  def evaluate(entryPoint: Value[TA, VA], params: Data): F[MorphirRuntimeError, Data]
//  = {
//    val toValue = ToMorphirValue.summon[Data].typed
//    val inputIR = toValue(params)
//    evaluate(entryPoint, inputIR)
//  }
  def evaluate(entryPoint : FQName, params : Data) : F[MorphirRuntimeError, Data]
//  = {
//
//    val toValue = ToMorphirValue.summon[Data].typed
//    val inputIR = toValue(params)
//    evaluate(entryPoint, inputIR)
//  }
  def evaluate(entryPoint : FQName, params : Value[TA, VA]): F[MorphirRuntimeError, Data]
//= {
//    evaluate(V.reference(entryPoint), params)
//  }
  //Doesn't need to be exposed to user, right?
  def applyParams(entryPoint: Value[TA, VA], params: Value[TA, VA]): Value[TA, VA] =
    V.apply(entryPoint.attributes, entryPoint, params) //WRONG TYPE BAD HACK
    //TODO: Can be a MorphirRuntimeError
    //lambda : result = type Function(..., output) -> output
    //definition -> result
  //def distributions : IndexedSeq[Distribution]

  def evaluate(value: Value[TA, VA]): F[EvaluationError, Data]

}

object MorphirRuntime extends MorphirRuntimePlatformSpecific {
//  def simple(distributions: Distribution*): MorphirRuntime[SimpleMorphirRuntime.Result, scala.Unit, UType] =
//    SimpleMorphirRuntime(distributions.toList)
  def quick(distribution : Distribution) : MorphirRuntime[Either, scala.Unit, UType] =
    QuickMorphirRuntime.fromDistribution(distribution)
}

