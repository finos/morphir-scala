package org.finos.morphir.runtime.quick

import org.finos.morphir.naming.*
import org.finos.morphir.ir.Type.UType
import org.finos.morphir.ir.Value.{Value, Pattern, TypedValue}
import org.finos.morphir.ir.{Value => V}
import org.finos.morphir.datamodel.Data
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.Type.Type
import org.finos.morphir.runtime.*
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.runtime.services.sdk.MorphirSdk
import org.finos.morphir.runtime.Utils.*
import org.finos.morphir.ir.conversion.*
import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.datamodel.*

import scala.util.{Failure, Success, Try}
import org.finos.morphir.runtime.MorphirRuntimeError
import org.finos.morphir.runtime.MorphirRuntimeError.*
import org.finos.morphir.runtime.environment.MorphirEnv
import zio.prelude.fx.ZPure
import org.finos.morphir.ir.printing.{DetailLevel, PrintIR}

private[runtime] case class QuickMorphirRuntime(dists: Distributions, globals: GlobalDefs)
    extends TypedMorphirRuntime {
  // private val store: Store[scala.Unit, UType] = Store.empty //

  def test() : RTAction[MorphirEnv, Nothing, TestSummary] = {
    val res = UnitTesting.runTests(globals, dists)
    res
  }

  def runUnitTests(distributions: Distribution*): RTAction[MorphirEnv, MorphirRuntimeError, TestSummary] = {
    val res = UnitTesting.runTests(globals, dists)
    res
  }

  def evaluate(
      entryPoint: FQName,
      param: TypedValue,
      params: TypedValue*
  ): RTAction[MorphirEnv, MorphirRuntimeError, Data] =
    for {
      tpe <- fetchType(entryPoint)
      res <- evaluate(Value.Reference.Typed(tpe, entryPoint), param, params: _*)
    } yield res

  def evaluate(value: TypedValue): RTAction[MorphirEnv, MorphirRuntimeError, Data] =
    for {
      _   <- typeCheck(value)
      res <- EvaluatorQuick.evalAction(value, globals, dists)
    } yield res

  def evaluate(entryPoint: FQName): RTAction[MorphirEnv, MorphirRuntimeError, Data] = for {
    tpe <- fetchType(entryPoint)
    res <- evaluate(Value.Reference.Typed(tpe, entryPoint))
  } yield res

  def fetchType(fqn: FQName): RTAction[MorphirEnv, MorphirRuntimeError, UType] = {
    val maybeSpec = dists.lookupValueSpecification(fqn)
    maybeSpec match {
      case Right(spec) => RTAction.succeed(specificationToType(spec))
      case Left(err)   => RTAction.fail(err.withContext("Cannot find Entry Point:"))
    }
  }

  def typeCheck(value: TypedValue): RTAction[MorphirEnv, TypeError, Unit] = for {
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
      entryPoint: TypedValue,
      params: TypedValue*
  ): RTAction[Any, TypeError, TypedValue] =
    for {
      ctx <- ZPure.get[RTExecutionContext]
      out <- {
        entryPoint match {
          case Value.Reference.Typed(tpe, fqn) =>
            for {
              tpe <- findTypeBindings(tpe, params.toList, dists, Map())(ctx.options)
            } yield V.applyInferType(tpe, V.reference(fqn), params: _*)
          case other => RTAction.fail(
              new TypeError.OtherTypeError(s"Entry point must be a Reference, instead found ${PrintIR(other)}")
            )
        }
      }
    } yield out

}

object QuickMorphirRuntime {

  def fromDistributions(distributions: Distribution*): QuickMorphirRuntime = {
    val globalDefs = GlobalDefs.fromDistributions(distributions: _*)
    QuickMorphirRuntime(Distributions(distributions: _*), globalDefs)
  }

  def fromDistributionRTAction(distributions: Distribution*)
      : RTAction[MorphirEnv, MorphirRuntimeError, QuickMorphirRuntime] =
    RTAction.succeed(fromDistributions(distributions: _*))

}
