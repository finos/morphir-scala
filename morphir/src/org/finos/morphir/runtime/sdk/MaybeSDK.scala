package org.finos.morphir.runtime.sdk

import org.finos.morphir.ir.Type
import org.finos.morphir.runtime.*
import org.finos.morphir.runtime.internal.{
  DynamicNativeFunction,
  DynamicNativeFunction1,
  DynamicNativeFunction2,
  DynamicNativeFunction3,
  DynamicNativeFunction4,
  DynamicNativeFunction5,
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

  /**
   * Converts a "Maybe" - i.e., a ConstructorResult representing a morphir-elm Maybe value - to a Scala Option
   *
   * @param arg
   *   A ConstructorResult representing a morphir-elm Maybe value (Just or Nothing)
   * @return
   *   An Option representing the argument in Scala terms - `Some(x)` if arg was `Just x`, `None` if arg was `Nothing`
   */
  private[sdk] def maybeToOption(arg: RT.ConstructorResult): Option[RT] =
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

  /**
   * Converts a Scala Option to a "Maybe" - i.e., a ConstructorResult representing a morphir-elm Maybe value
   *
   * @param arg
   *   A Scala Option
   * @return
   *   A ConstructorResult representing the argument in Morphir terms - `Just x` if arg was `Some(x)`, `Nothing` if arg
   *   was `None`
   */
  private[sdk] def optionToMaybe(arg: Option[RT]): RT.ConstructorResult =
    arg match {
      case Some(value) => RTValue.ConstructorResult(FQName.fromString("Morphir.SDK:Maybe:Just"), List(value))
      case None        => RTValue.ConstructorResult(FQName.fromString("Morphir.SDK:Maybe:Nothing"), List())
    }

  val map = DynamicNativeFunction2("map") {
    (ctx: NativeContext) => (f: RT.Function, maybeRaw: RT.ConstructorResult) =>
      {
        val out = maybeToOption(maybeRaw).map(elem => ctx.evaluator.handleApplyResult(Type.variable("a"), f, elem))
        optionToMaybe(out)
      }
  }

  val withDefault = DynamicNativeFunction2("withDefault") {
    (ctx: NativeContext) => (default: RT, maybeRaw: RT.ConstructorResult) =>
      maybeToOption(maybeRaw).getOrElse(default)
  }

  val andThen = DynamicNativeFunction2("andThen") {
    (ctx: NativeContext) => (callback: RT.Function, maybeRaw: RT.ConstructorResult) =>
      {
        val out = maybeToOption(maybeRaw).flatMap { elem =>
          val fromCallbackRaw = ctx.evaluator.handleApplyResult(Type.variable("a"), callback, elem)
          val fromCallbackCr  = RT.coerceConstructorResult(fromCallbackRaw)
          maybeToOption(fromCallbackCr)
        }
        optionToMaybe(out)
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

  val map3 = DynamicNativeFunction4("map3") {
    (ctx: NativeContext) => (
        f: RT.Function,
        maybeRaw1: RT.ConstructorResult,
        maybeRaw2: RT.ConstructorResult,
        maybeRaw3: RT.ConstructorResult
    ) =>
      {
        val out = for {
          elem1 <- eitherToOption(maybeRaw1)
          elem2 <- eitherToOption(maybeRaw2)
          elem3 <- eitherToOption(maybeRaw3)
        } yield ctx.evaluator.handleApplyResult3(Type.variable("a"), f, elem1, elem2, elem3)
        resultToMaybe(out)
      }
  }

  val map4 = DynamicNativeFunction5("map4") {
    (ctx: NativeContext) => (
        f: RT.Function,
        maybeRaw1: RT.ConstructorResult,
        maybeRaw2: RT.ConstructorResult,
        maybeRaw3: RT.ConstructorResult,
        maybeRaw4: RT.ConstructorResult
    ) =>
      {
        val out = for {
          elem1 <- eitherToOption(maybeRaw1)
          elem2 <- eitherToOption(maybeRaw2)
          elem3 <- eitherToOption(maybeRaw3)
          elem4 <- eitherToOption(maybeRaw4)
        } yield ctx.evaluator.handleApplyResult4(Type.variable("a"), f, elem1, elem2, elem3, elem4)
        resultToMaybe(out)
      }
  }

  val hasValue = DynamicNativeFunction1("hasValue") {
    (_: NativeContext) => (maybeRaw: RT.ConstructorResult) =>
      RT.Primitive.Boolean(eitherToOption(maybeRaw).isDefined)
  }
}
