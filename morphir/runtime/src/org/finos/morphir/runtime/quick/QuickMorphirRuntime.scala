package org.finos.morphir.runtime.quick

import org.finos.morphir.naming.*
import org.finos.morphir.ir.Type.UType
import org.finos.morphir.ir.Value.Value
import org.finos.morphir.ir.Value as V
import org.finos.morphir.datamodel.Data
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.distribution.Distribution.Library
import org.finos.morphir.ir.Type.Type
import org.finos.morphir.runtime.*
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.runtime.services.sdk.MorphirSdk
import org.finos.morphir.runtime.Utils.*
import org.finos.morphir.ir.conversion.*
import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.datamodel.*

import scala.util.{Failure, Success, Try}
import org.finos.morphir.runtime.{EvaluationError, MorphirRuntimeError}
import org.finos.morphir.runtime.environment.MorphirEnv

private[runtime] case class QuickMorphirRuntime(dists: Distributions, store: Store[scala.Unit, UType])
    extends TypedMorphirRuntime {
  // private val store: Store[scala.Unit, UType] = Store.empty //

  def evaluate(entryPoint: FQName, params: Value[scala.Unit, UType]): RTAction[MorphirEnv, MorphirRuntimeError, Data] =
    for {
      tpe <- fetchType(entryPoint)
      res <- evaluate(Value.Reference.Typed(tpe, entryPoint), params)
    } yield res

  def evaluate(value: Value[scala.Unit, UType]): RTAction[MorphirEnv, EvaluationError, Data] =
    EvaluatorQuick.evalAction(value, store, dists)

  def fetchType(ref: FQName): RTAction[MorphirEnv, MorphirRuntimeError, UType] = {
      val (pkg, mod, loc) = (ref.getPackagePath, ref.getModulePath, ref.localName)
      val maybeSpec       = dists.lookupValueSpecification(PackageName(pkg), ModuleName(mod), loc)
      maybeSpec match {
        case Some(spec) => RTAction.succeed(specificationToType(spec))
        case None       => RTAction.fail(new SpecificationNotFound(s"Could not find $ref during initial type building"))
      }
  }

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

}

object QuickMorphirRuntime {

  def fromDistributions(distributions: Distribution*): QuickMorphirRuntime = {
    val store = Store.fromDistributions(distributions: _*)
    QuickMorphirRuntime(Distributions(distributions:_*), store)
  }

  def fromDistributionRTAction(distributions: Distribution*)
      : RTAction[MorphirEnv, MorphirRuntimeError, QuickMorphirRuntime] =
    RTAction.succeed(fromDistributions(distributions:_*))

}
