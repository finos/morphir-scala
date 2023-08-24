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
import zio.prelude.fx.ZPure

private[runtime] case class QuickMorphirRuntime(dists: Distributions, store: Store[scala.Unit, UType])
    extends TypedMorphirRuntime {
  // private val store: Store[scala.Unit, UType] = Store.empty //

  def evaluate(entryPoint: FQName, params: Value[scala.Unit, UType]): RTAction[MorphirEnv, MorphirRuntimeError, Data] =
    for {
      tpe <- fetchType(entryPoint)
      res <- evaluate(Value.Reference.Typed(tpe, entryPoint), params)
    } yield res

  def evaluate(value: Value[scala.Unit, UType]): RTAction[MorphirEnv, EvaluationError, Data] =
    // val errors = new TypeChecker(dists).check(value)
    for {
//      ctx <- ZPure.get[RTExecutionContext]
//
//      _ <- if (errors.length == 0) RTAction.succeed(()) else {
//        RTAction.fail(TypeCheckerErrors(errors))
//      }
      _   <- typeCheck(value)
      res <- EvaluatorQuick.evalAction(value, store, dists)
    } yield res

  def fetchType(fqn: FQName): RTAction[MorphirEnv, MorphirRuntimeError, UType] = {
    val maybeSpec = dists.lookupValueSpecification(fqn)
    maybeSpec match {
      case Right(spec) => RTAction.succeed(specificationToType(spec))
      case Left(err)   => RTAction.fail(new SpecificationNotFound(s"Lookup failure: ${err.getMsg}"))
    }
  }

  def typeCheck(value: Value[scala.Unit, UType]): RTAction[MorphirEnv, EvaluationError, Unit] = for {
    ctx <- ZPure.get[RTExecutionContext]
    result <- ctx.options.enableTyper match {
      case EnableTyper.Disabled => RTAction.succeed[RTExecutionContext, Unit](())
      case EnableTyper.Warn =>
        val errors = new TypeChecker(dists).check(value)
        errors.foreach(error => println(s"TYPE WARNING: $error"))
        RTAction.succeed[RTExecutionContext, Unit](())
      case EnableTyper.Enabled =>
        val errors = new TypeChecker(dists).check(value)
        if (errors.length == 0) RTAction.succeed[RTExecutionContext, Unit](())
        else RTAction.fail(TypeError.ManyTypeErrors(errors))
    }
  } yield result

  def applyParams(
      entryPoint: Value[scala.Unit, UType],
      params: Value[scala.Unit, UType]*
  ): RTAction[Any, TypeError, Value[scala.Unit, UType]] =
    for {
      ctx <- ZPure.get[RTExecutionContext]
      out <- {
        entryPoint match {
          case Value.Reference.Typed(tpe, _) =>
            for {
              tpe <- findTypeBindings(tpe, params.toList, dists, Map())(ctx.options)
            } yield V.apply(tpe, entryPoint, params.head, params.tail: _*)
          case other => RTAction.fail(UnsupportedType(s"Entry point must be a Reference, instead found $other"))
        }
      }
    } yield out

}

object QuickMorphirRuntime {

  def fromDistributions(distributions: Distribution*): QuickMorphirRuntime = {
    val store = Store.fromDistributions(distributions: _*)
    QuickMorphirRuntime(Distributions(distributions: _*), store)
  }

  def fromDistributionRTAction(distributions: Distribution*)
      : RTAction[MorphirEnv, MorphirRuntimeError, QuickMorphirRuntime] =
    RTAction.succeed(fromDistributions(distributions: _*))

}
