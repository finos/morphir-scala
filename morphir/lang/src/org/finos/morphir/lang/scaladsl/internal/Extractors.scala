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
            println(s"""
                       |===================== Function Structure ==========================
                       |${pprint(defm)}
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
            println(s"""
                       |===================== Not Matched Structure ==========================
                       |${pprint(node)}
                       |""".stripMargin)
            None
end Extractors
