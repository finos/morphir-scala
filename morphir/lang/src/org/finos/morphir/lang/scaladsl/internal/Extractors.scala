package org.finos
package morphir
package lang.scaladsl.internal

import ir.Name.Name
object Extractors:
  import scala.quoted.*

  object FunctionDef:
    object Tree:
      def unapply(using Quotes)(tree: quotes.reflect.Tree): Option[(Name, Int)] =
        import quotes.reflect.*
        tree match
          case defm @ DefDef(name, paramss, _, _) =>
            scribe.info(s"""
                           |===================== Function Structure ==========================
                           |${Printer.TreeStructure.show(defm)}
                           |""".stripMargin)
            Some((Name.fromString(name), paramss.size))
          case _ => None
  object Module:

    object Tree:
      def unapply(using Quotes)(node: quotes.reflect.Tree): Option[Any] =
        import quotes.reflect.*
        node match
          case cd @ ClassDef(name, _, _, _, body) if cd.symbol.flags.is(Flags.Module) =>
            val functions = body.collect { case f @ FunctionDef.Tree(name, arity) => (name, arity) }
            Some((name, body.size))
          case _ =>
            scribe.info(s"""
                           |===================== Not Matched Structure ==========================
                           |${Printer.TreeStructure.show(node)}
                           |""".stripMargin)
            None
end Extractors
