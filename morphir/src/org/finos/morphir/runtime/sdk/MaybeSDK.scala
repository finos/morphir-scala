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
import org.finos.morphir.runtime.RTValue
import org.finos.morphir.runtime.{RTValue => RT}
import org.finos.morphir.naming._

/**
 * The "Maybe" SDK functions somewhat differently from ListSDK or others, because Maybe is not handled as a specific
 * RTValue but through the general RTConstructor type. This is because it is treated as such in the IR - Maybe values
 * are initialized with constructors and pattern matched as such.
 */
object MaybeSDK {
  private def toOption(arg: RT.ConstructorResult): Option[RT] =
    arg match {
      case RTValue.ConstructorResult(fqn, List(value)) if fqn == FQName.fromString("Morphir.SDK:Maybe:Just") =>
        Some(value)
      case RTValue.ConstructorResult(fqn, List()) if fqn == FQName.fromString("Morphir.SDK:Maybe:Nothing") => None
      case RTValue.ConstructorResult(fqn, args) =>
        throw new UnexpectedType(s"Expected Just(something) or Nothing, found $fqn(${args}")
    }
  private def toMaybe(arg: Option[RT]): RT.ConstructorResult =
    arg match {
      case Some(value) => RTValue.ConstructorResult(FQName.fromString("Morphir.SDK:Maybe:Just"), List(value))
      case None        => RTValue.ConstructorResult(FQName.fromString("Morphir.SDK:Maybe:Nothing"), List())
    }

  val map = DynamicNativeFunction2("map") {
    (ctx: NativeContext) => (f: RT.Function, maybeRaw: RT.ConstructorResult) =>
      {
        val out = toOption(maybeRaw).map(elem => ctx.evaluator.handleApplyResult(Type.variable("a"), f, elem))
        toMaybe(out)
      }
  }
  val withDefault = DynamicNativeFunction2("withDefault") {
    (ctx: NativeContext) => (default: RT, maybeRaw: RT.ConstructorResult) =>
      toOption(maybeRaw).getOrElse(default)
  }

}
