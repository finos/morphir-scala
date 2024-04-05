package org.finos.morphir.runtime.sdk

import org.finos.morphir.ir.Type
import org.finos.morphir.runtime.*
import org.finos.morphir.runtime.internal.{
  DynamicNativeFunction,
  DynamicNativeFunction1,
  DynamicNativeFunction2,
  DynamicNativeFunction3,
  NativeContext
}
import org.finos.morphir.runtime.MorphirRuntimeError.UnexpectedType
import org.finos.morphir.runtime.RTValue
import org.finos.morphir.runtime.{RTValue => RT}
import org.finos.morphir.naming._

/**
 * The "Maybe" SDK functions somewhat differently from ListSDK or others, because Maybe is not handled as a specific
 * RTValue but through the general RTConstructor type. This is because it is treated as such in the IR - Maybe values
 * are initialized with constructors and pattern matched as such.
 *
 * TODO: This distinction should be refactored away. This entails:
 *   - Adding RTValue variants for Maybe (and Result, while we're at it)
 *   - Adding all supporting code for such (coercers + anything else)
 *   - Adding special cases to the Apply handling for Constructors, to create these special RTValues instead of the
 *     general form
 *   - Adding special cases to Constructor pattern matching to recognize these forms
 *   - Changing the RTValue to MDM process to reflect the new form
 *   - In this file:
 *   - Remove toOption and toMaybe
 *   - Change the types from RT.ConstructorResult to RT.Maybe
 *   - Change toOption(arg) calls to arg.value and toMaybe(result) calls to RT.Maybe(result)
 */
object MaybeSDK {
  private[sdk] def eitherToOption(arg: RT.ConstructorResult): Option[RT] =
    arg match {
      case RTValue.ConstructorResult(fqn, List(value)) if fqn == FQName.fromString("Morphir.SDK:Maybe:Just") =>
        Some(value)
      case RTValue.ConstructorResult(fqn, List()) if fqn == FQName.fromString("Morphir.SDK:Maybe:Nothing") => None
      case RTValue.ConstructorResult(_, _) =>
        throw new UnexpectedType(
          s"Morphir.SDK:Maybe:just value or Morphir.SDK:Maybe:nothing",
          arg,
          "Expected due to use in a native function"
        )
    }
  private[sdk] def resultToMaybe(arg: Option[RT]): RT.ConstructorResult =
    arg match {
      case Some(value) => RTValue.ConstructorResult(FQName.fromString("Morphir.SDK:Maybe:Just"), List(value))
      case None        => RTValue.ConstructorResult(FQName.fromString("Morphir.SDK:Maybe:Nothing"), List())
    }

  val map = DynamicNativeFunction2("map") {
    (ctx: NativeContext) => (f: RT.Function, maybeRaw: RT.ConstructorResult) =>
      {
        val out = eitherToOption(maybeRaw).map(elem => ctx.evaluator.handleApplyResult(Type.variable("a"), f, elem))
        resultToMaybe(out)
      }
  }

  val withDefault = DynamicNativeFunction2("withDefault") {
    (ctx: NativeContext) => (default: RT, maybeRaw: RT.ConstructorResult) =>
      eitherToOption(maybeRaw).getOrElse(default)
  }

  val andThen = DynamicNativeFunction2("andThen") {
    (ctx: NativeContext) => (callback: RT.Function, maybeRaw: RT.ConstructorResult) =>
      {
        val out = eitherToOption(maybeRaw).flatMap { elem =>
          val fromCallbackRaw = ctx.evaluator.handleApplyResult(Type.variable("a"), callback, elem)
          val fromCallbackCr  = RT.coerceConstructorResult(fromCallbackRaw)
          eitherToOption(fromCallbackCr)
        }
        resultToMaybe(out)
      }
  }

  val map2 = DynamicNativeFunction3("map2") {
    (ctx: NativeContext) => (f: RT.Function, maybeRaw1: RT.ConstructorResult, maybeRaw2: RT.ConstructorResult) =>
      {
        val out = for {
          elem1 <- eitherToOption(maybeRaw1)
          elem2 <- eitherToOption(maybeRaw2)
        } yield ctx.evaluator.handleApplyResult2(Type.variable("a"), f, elem1, elem2)
        resultToMaybe(out)
      }
  }

  //   val map2 = DynamicNativeFunction3("map2") {
  //   (ctx: NativeContext) => (f: RT.Function, maybe1Raw: RT.ConstructorResult, maybe2Raw: RT.ConstructorResult) =>
  //     {
  //       val maybe1 = eitherToOption(maybe1Raw)
  //       val maybe2 = eitherToOption(maybe2Raw)
  //       (maybe1, maybe2) match {
  //         case (Some(value1), Some(value2)) =>
  //           val out = ctx.evaluator.handleApplyResult2(Type.variable("a"), f, value1, value2)
  //           resultToMaybe(Some(out))
  //         case _ => resultToMaybe(None)
  //       }
  //     }
  // }

  val hasValue = DynamicNativeFunction1("hasValue") {
    (_: NativeContext) => (maybeRaw: RT.ConstructorResult) =>
      RT.Primitive.Boolean(eitherToOption(maybeRaw).isDefined)
  }
}
