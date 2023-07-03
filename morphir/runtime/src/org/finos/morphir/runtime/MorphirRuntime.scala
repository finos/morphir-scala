package org.finos.morphir.runtime
import org.finos.morphir.ir.Type.UType
import org.finos.morphir.ir.Value.Value
import org.finos.morphir.datamodel.Data
import org.finos.morphir.ir.distribution.Distribution

trait MorphirRuntime[F[_], TA, VA] {
  final def evaluate(entrypoint: Value[TA, VA], params: Value[TA, VA]): F[Data] =
    evaluate(applyParams(entrypoint, params))

  def evaluate(entryPoint: Value[TA, VA]): F[Data]

  def applyParams(entrypoint: Value[TA, VA], params: Value[TA, VA]): Value[TA, VA]
}

object MorphirRuntime {
  def simple(distributions: Distribution*): MorphirRuntime[SimpleMorphirRuntime.Result, scala.Unit, UType] =
    SimpleMorphirRuntime(distributions.toList)

}
