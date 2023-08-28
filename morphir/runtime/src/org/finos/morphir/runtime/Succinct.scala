package org.finos.morphir.runtime

import org.finos.morphir.naming._
import org.finos.morphir.ir.{Type as T, Value as V}
import org.finos.morphir.ir.Value.{Value, Pattern, TypedValue}
import org.finos.morphir.ir.Type.{Type, UType}
import org.finos.morphir.ir.sdk
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.ir.Field
import org.finos.morphir.runtime.exports.*

//Utlity object (mostly for errors) to give briefer versions of Values/Types by only showing the top few levels
//Additional cases should be filled out as needed (WIP)
object Succinct {
  object Value {
    import V.Value.*
    def apply[TA, VA](value: Value[TA, VA], depth: Int): String =
      value match {
        case Literal(_, lit) => s"Literal($lit)"
        case other           => other.getClass.getSimpleName
      }
    def apply[TA, VA](value: Value[TA, VA]): String = apply(value, 2)
  }
  object Type {
    import T.Type.*
    def apply[TA](tpe: Type[TA], depth: Int): String =
      tpe match {
        case Reference(_, fqName, args) => s"$fqName ${args.map(apply(_, depth - 1)).mkString(" ")}"
        case Tuple(_, elements)         => s"(${elements.map(apply(_, depth - 1)).mkString(", ")})"
        case Function(_, arg, ret)      => s"(${apply(arg, depth - 1)} -> ${apply(ret, depth - 1)}})"
        case other                      => other.getClass.getSimpleName
      }

    def apply[TA](tpe: Type[TA]): String = apply(tpe, 2)

  }
  object Pattern {
    def apply[VA](pattern: Pattern[VA], depth: Int): String =
      pattern match {
        case other => other.getClass.getSimpleName
      }
    def apply[VA](pattern: Pattern[VA]): String = apply(pattern, 2)

  }
  object TypeSpec {
    def apply[TA](spec: T.Specification[TA], depth: Int): String =
      spec match {
        case other => other.getClass.getSimpleName
      }
    def apply[TA](spec: T.Specification[TA]): String = apply(spec, 2)
  }
}
