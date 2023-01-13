package org.finos
package morphir.meta

import scala.quoted._
object PrintMacro {
  inline def apply(inline any: Any): Unit = ${ printImpl('any) }
  def printImpl(expr: Expr[Any])(using Quotes): Expr[Unit] = {
    import quotes.reflect._
    report.info("================== The Short Version  ================")
    report.info(Printer.TreeAnsiCode.show(expr.asTerm.underlyingArgument))
    '{ () }
  }

  inline def detail(inline any: Any): Unit = ${ detailImpl('any) }
  def detailImpl(expr: Expr[Any])(using Quotes): Expr[Unit] = {
    import quotes.reflect._
    val sb = new StringBuilder
    sb.append("================== The Short Version ================").append(System.lineSeparator())
    sb.append(expr.show).appengit d(System.lineSeparator())
    sb.append("================== The Long Version ================").append(System.lineSeparator())
    sb.append(pprint.apply(expr.asTerm.underlyingArgument)).append(System.lineSeparator())
    report.info(sb.toString)
    '{ () }
  }
}
