package org.finos
package morphir.meta

import scala.quoted.*
object PrintMacro {
  transparent inline def apply(inline any: Any): Any = ${ printImpl('any) }
  def printImpl(expr: Expr[Any])(using Quotes): Expr[Any] = {
    import quotes.reflect._
    val sb = new StringBuilder()
    sb.appendln("================== The Short Version  ================")
    sb.appendln(Printer.TreeAnsiCode.show(expr.asTerm.underlyingArgument))
    println(sb.toString)
    expr
  }

  transparent inline def detailed(inline any: Any): Any = ${ detailedImpl('any) }
  def detailedImpl(expr: Expr[Any])(using Quotes): Expr[Any] = {
    import quotes.reflect._
    val sb = new StringBuilder
    sb.appendln("================== The Short Version ================")
    sb.appendln(expr.show)
    sb.appendln("================== The Long Version ================")
    sb.appendln(pprint(expr.asTerm.underlyingArgument).render)
    println(sb.toString)
    expr
  }

  extension (sb: StringBuilder) def appendln(s: String): StringBuilder = sb.append(s).append(System.lineSeparator())
}
