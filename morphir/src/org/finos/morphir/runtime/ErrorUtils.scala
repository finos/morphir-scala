package org.finos.morphir.runtime

import fansi.Str
import org.finos.morphir.datamodel.{Concept, Data}
import org.finos.morphir.naming.*
import org.finos.morphir.ir.{Type as T, Value as V}
import org.finos.morphir.ir.Value.{Pattern, TypedValue, Value, USpecification as UValueSpec}
import org.finos.morphir.ir.Type.{Type, UType, USpecification as UTypeSpec}
import org.finos.morphir.ir.printing.PrintIR
import org.finos.morphir.util.PrintMDM
import org.finos.morphir.util.PrintRTValue

import java.util
import scala.util.control.NonFatal

object ErrorUtils {
  def indentBlock(s: String): String = s.split("\n").map("\t" + _).mkString("\n")

  implicit class ErrorInterpolator(sc: StringContext) {
    //format: off
    /**
     * This interpolator is intended to format errors with potentially bulky arguments which we wish to display as both
     * IR and native elm. As such it takes the IR arguments and replaces them with a numeric ID in the string, and then
     * prints them after the message in formatted blocks. For instance:
     *
     * Value type does not match declared type: (0) vs (1)
     * \========== 0: ==========
     * \| ====Elm====
     * \| | (1, 2)
     * \| ====IR====
     * \| | T.Tuple(T.Ref(int), T.Ref(int))
     * \========== 1: ==========
     * \| ====Elm====
     * \| | ()
     * \| ====IR====
     * \| | T.Unit(())
     *
     * @param args
     *   Either IR or non-IR elements to be displayed; IR elements will be handled specially.
     * @return
     *   A human-readable error with the IR elements formatted for display after the error message.
     */
      //format: on
    def err(args: Any*): String = {
      val indexed = args.zipWithIndex
      val irArgs = indexed
        .filter { case (arg, _) => isASTLike(arg) }
        .zipWithIndex
        .map { case ((arg, mainIndex), argIndex) => (mainIndex, (argIndex, arg)) }
        .toMap
      val processed = indexed.map { case (arg, mainIndex) =>
        irArgs.get(mainIndex) match {
          case Some((irIndex, _)) => s"($irIndex)"
          case None               => arg.toString
        }
      }
      val explanation = sc.s(processed: _*)
      val terms = irArgs.values.map { case (argIndex, arg) =>
        process(s" $argIndex: ", arg)
      }.mkString
      explanation + "\n" + terms
    }

    def process(title: String, astLike: Any): String =
      if (isMDM(astLike)) {
        val mdmBody = barIndentBlock(" the MDM looks like this: ", PrintMDM(astLike).plainText)
        barBlock(barIndentBlock(title, mdmBody, barWidth = 10)) // Console likes to drop leading |?
      } else if (isIR(astLike)) {
        val elmBody = barIndentBlock(" the Elm looks like this: ", astLike.toString)
        val irBody  = barIndentBlock(" the IR looks like this: ", PrintIR(astLike).plainText)
        barBlock(barIndentBlock(title, elmBody + irBody, barWidth = 10)) // Console likes to drop leading |?
      } else {
        // TODO: RTValue probably needs its own Printer
        val astBody = barIndentBlock(" the AST looks like: ", PrintRTValue(astLike).plainText)
        barBlock(barIndentBlock(title, astBody, barWidth = 10)) // Console likes to drop leading |?
      }
    def isIR(any: Any): Boolean = any match {
      case _: Type[_]                                 => true
      case _: Value[_, _]                             => true
      case _: Pattern[_]                              => true
      case _: V.Specification[_]                      => true
      case _: V.Definition[_, _]                      => true
      case _: T.Specification[_]                      => true
      case _: T.Definition[_]                         => true
      case iterable: Iterable[_] if !iterable.isEmpty => isIR(iterable.head)
      case _                                          => false
    }
    def isMDM(any: Any): Boolean = any match {
      case _: Data                                    => true
      case _: Concept                                 => true
      case iterable: Iterable[_] if !iterable.isEmpty => isMDM(iterable.head)
      case _                                          => false
    }

    def isASTLike(any: Any): Boolean = any match {
      case _: RTValue                                 => true
      case iterable: Iterable[_] if !iterable.isEmpty => isASTLike(iterable.head)
      case other                                      => isMDM(other) || isIR(other)
    }

    def indentBlockPreserveBar(s: String): String = s.split("\n").map { line =>
      if (line.startsWith("|")) "|\t" + line.drop(1)
      else "\t" + line
    }.mkString("\n")

    def barIndentBlock(header: String, body: String, barWidth: Int = 4): String = {
      val bodyLines = body.split("\n").map("|\t" + _)
      ("=" * barWidth) + header + ("=" * barWidth) + "\n" +
        bodyLines.mkString("\n") +
        "\n"
    }

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

  def tryOption[A](block: => A): Option[A] =
    try Some(block)
    catch {
      case NonFatal(_) => None
    }
}
