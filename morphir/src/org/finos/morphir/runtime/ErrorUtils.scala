package org.finos.morphir.runtime

import org.finos.morphir.datamodel.{Concept, Data}
import org.finos.morphir.naming._
import org.finos.morphir.ir.{Type => T, Value => V}
import org.finos.morphir.ir.Value.{Value, Pattern, TypedValue, USpecification => UValueSpec}
import org.finos.morphir.ir.Type.{Type, UType, USpecification => UTypeSpec}
import org.finos.morphir.ir.printing.PrintIR

object ErrorUtils {
  implicit class ErrorInterpolator(sc: StringContext) {
    def err(args: Any*): String = {
      val messageBits = sc.parts
      val processed   = args.map(process(_))
      messageBits.head + processed.zip(messageBits.tail).flatMap { case (message, ir) => List(message, ir) }.mkString(
        ""
      )
      // (messageBits.map(bit => s"from sc.parts: $bit\n") ++ args.map(bit => s"from args: $bit\n")).mkString("")
    }

    def process(astLike: Any): String =
      if (isASTLike(astLike))
        s""" {
           |\t Elm: $astLike
           |\t IR: ${PrintIR(astLike)}
           |}""".stripMargin
      else
        astLike.toString

    def isIR(any: Any): Boolean = any match {
      case _: Type[_]            => true
      case _: Value[_, _]        => true
      case _: Pattern[_]         => true
      case _: V.Specification[_] => true
      case _: V.Definition[_, _] => true
      case _: T.Specification[_] => true
      case _: T.Definition[_]    => true
      case _                     => false
    }

    def isASTLike(any: Any): Boolean = any match {
      case _: RTValue => true
      case _: Data    => true
      case _: Concept => true
      case other      => isIR(other)
    }
  }
}
