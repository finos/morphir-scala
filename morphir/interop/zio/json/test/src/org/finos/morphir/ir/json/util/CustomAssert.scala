package org.finos.morphir.ir.json.util

import zio.test.*
import zio.test.Assertion.equalTo
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter.Indenter
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter
import com.fasterxml.jackson.core.util.DefaultIndenter
import org.finos.morphir.util.{Compare, PrintDiff}

object CustomAssert {
  def stringEqualTo(thatRaw: String): Assertion[String] =
    Assertion[String] {
      import zio.test.diff.Diff
      import zio.test.{ErrorMessage => M}
      import zio.test.diff.{Diff, DiffResult}
      import zio.internal.ansi._
      import scala.{Console => SConsole}
      object ConsoleUtils {
        def underlined(s: String): String =
          SConsole.UNDERLINED + s + SConsole.RESET
      }
      TestArrow
        .make[String, Boolean] { aRaw =>
          val result = aRaw == thatRaw
          TestTrace.boolean(result) {
            val printer  = new DefaultPrettyPrinter();
            val indenter = new DefaultIndenter("", "")
            printer.indentArraysWith(indenter)
            val mapper = new ObjectMapper()
            val writer = mapper.writer(printer)

            val (that, a) =
              (for {
                thatFormatted <- scala.util.Try(mapper.readTree(thatRaw))
                aFormatted    <- scala.util.Try(mapper.readTree(aRaw))
              } yield (writer.writeValueAsString(thatFormatted), writer.writeValueAsString(aFormatted)))
                .getOrElse((thatRaw, aRaw))

            val diffResult = Diff.stringDiff.diff(that, a)
            diffResult match {
              case DiffResult.Different(_, _, None) =>
                M.pretty(a) + M.equals + M.pretty(that)
              case diffResult =>
                M.choice("There was no difference", "There was a difference") ++
                  M.custom(
                    ConsoleUtils.underlined(
                      "Diff"
                    ) + s" ${scala.Console.RED}-expected ${scala.Console.GREEN}+obtained".faint
                  ) ++
                  M.custom(scala.Console.RESET + diffResult.render)

              // Can also get a pretty printer Expected/Actual output but not doing that for
              // now because it is too verbose
              // ++
              // M.custom(ConsoleUtils.underlined("Expected") + "\n") ++
              // M.text(a) ++
              // M.custom(ConsoleUtils.underlined("Actual") + "\n") ++
              // M.text(that)
            }
          }
        }
    }

  def objectEqualTo[T](that: T): Assertion[T] =
    Assertion[T] {
      import zio.test.diff.Diff
      import zio.test.{ErrorMessage => M}
      import zio.test.diff.{Diff, DiffResult}
      import zio.internal.ansi._
      import scala.{Console => SConsole}
      object ConsoleUtils {
        def underlined(s: String): String =
          SConsole.UNDERLINED + s + SConsole.RESET
      }
      TestArrow
        .make[T, Boolean] { a =>
          val result = a == that
          TestTrace.boolean(result) {
            val compare     = Compare(a, that)
            val printedDiff =
              compare.map(PrintDiff(_).toString()).getOrElse("<No Diff: Contents Identical>")

            M.text(printedDiff)
          }
        }
    }
}
