package org.finos.morphir.runtime

import fansi.Str
import org.finos.morphir.datamodel.{Concept, Data}
import org.finos.morphir.naming.*
import org.finos.morphir.ir.{Type as T, Value as V}
import org.finos.morphir.ir.Value.{Pattern, TypedValue, Value, USpecification as UValueSpec}
import org.finos.morphir.ir.Type.{Type, UType, USpecification as UTypeSpec}
import org.finos.morphir.ir.printing.PrintIR

object ErrorUtils {
  implicit class ErrorInterpolator(sc: StringContext) {
    def err(args: Any*): String = {
      // Okay so we want to:
      // Turn the AST parts into (1), (2), etc
      val indexed = args.zipWithIndex
      val irArgs = indexed
        .filter { case (arg, _) => isIR(arg) }
        .zipWithIndex
        .map { case ((arg, mainIndex), argIndex) => (mainIndex, (argIndex, arg)) }
        .toMap
      val processed = indexed.map { case (arg, mainIndex) =>
        irArgs.get(mainIndex) match {
          case Some((irIndex, _)) => s"($irIndex)"
          case None               => arg.toString
        }
      }
      val messageBits = sc.parts
      // val processed   = args.map(process(_))
//      messageBits.head + processed.zip(messageBits.tail).flatMap { case (message, ir) => List(message, ir) }.mkString(
//        ""
//      )
      val explanation = sc.s(processed: _*)
      val terms = irArgs.values.map { case (argIndex, arg) =>
        process2(s" $argIndex: ", arg)
      }.mkString
      explanation + "\n" + terms
      // (messageBits.map(bit => s"from sc.parts: $bit\n") ++ args.map(bit => s"from args: $bit\n")).mkString("")
    }

    def process(astLike: Any): String =
      if (isASTLike(astLike)) {
        headerize("ELM:", astLike.toString) +
          headerize("IR:", PrintIR(astLike).toString)
      } else
        astLike.toString

    def process2(title: String, astLike: Any): String = {
      val elmBody = barIndentBlock("Elm", astLike.toString)
      val irBody  = barIndentBlock("IR", PrintIR(astLike).toString)
      barBlock(barIndentBlock(title, elmBody + irBody))
    }
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

    def indentBlock(s: String): String = s.split("\n").map("\t" + _).mkString("\n")
    def indentBlockPreserveBar(s: String): String = s.split("\n").map { line =>
      if (line.startsWith("|")) "|\t" + line.drop(1)
      else "\t" + line
    }.mkString("\n")

    def barIndentBlock(header: String, body: String): String =
      "====" + header + "====\n" +
        body.split("\n").map("|\t" + _).mkString("\n") +
        "\n"

    def barIndentBlockTerminated(header: String, body: String): String =
      barIndentBlock(header, body) +
        ("=" * (8 + header.length)) + "\n"

    def barBlock(s: String): String = s.split("\n").map("|" + _).mkString("\n") + "\n"
    def barBlockNoDup(s: String): String = s.split("\n").map { line =>
      if (line.startsWith("|")) line
      else "|" + line
    }.mkString("\n")
    def headerize(header: String, body: String) =
      s"\n====$header====\n" + indentBlockPreserveBar(
        barBlockNoDup(body)
      ) + "\n" // + ("=" * (header.length + 8) + "\n")
  }
}
