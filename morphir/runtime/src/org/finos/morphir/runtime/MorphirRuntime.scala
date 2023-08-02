package org.finos.morphir.runtime
import org.finos.morphir.ir.Type.UType
import org.finos.morphir.ir.Value.Value
import org.finos.morphir.ir.Value as V
import org.finos.morphir.datamodel.Data
import org.finos.morphir.ir.FQName
import Utils.*
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.runtime.quick.QuickMorphirRuntime

trait MorphirRuntime[F[+_, +_], TA, VA] {

  def evaluate(entryPoint: Value[TA, VA], params: Value[TA, VA]): F[MorphirRuntimeError, Data]
  def evaluate(entryPoint: Value[TA, VA], params: Data): F[MorphirRuntimeError, Data]
  def evaluate(entryPoint: FQName, params: Data): F[MorphirRuntimeError, Data]
  def evaluate(entryPoint: FQName, params: Value[TA, VA]): F[MorphirRuntimeError, Data]
  // TODO: applyParams can fail if things are bad, but we can't combine Fs yet
  def applyParams(entryPoint: Value[TA, VA], params: Value[TA, VA]*): F[TypeError, Value[TA, VA]]

  def evaluate(value: Value[TA, VA]): F[EvaluationError, Data]

}

object MorphirRuntime extends MorphirRuntimePlatformSpecific {
  def quick(distribution: Distribution): MorphirRuntime[Either, scala.Unit, UType] =
    QuickMorphirRuntime.fromDistribution(distribution)
}
