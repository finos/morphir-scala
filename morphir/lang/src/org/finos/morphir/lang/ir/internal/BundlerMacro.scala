package org.finos
package morphir.lang.ir.internal

import morphir.ir.Module.ModuleName
import morphir.meta.{PrintMacro, Trees}
import PrintMacro.appendln

import scala.quoted.*
object BundlerMacro:
  inline def bundle(inline name: String)(inline modules: Any*): (String, List[ModuleName]) =
    ${ bundleImpl('name, 'modules) }
  def bundleImpl(name: Expr[String], modules: Expr[Seq[Any]])(using Quotes): Expr[(String, List[ModuleName])] =
    new BundlerMacro().bundle(name, modules)
end BundlerMacro

class BundlerMacro(using Quotes):
  import quotes.reflect.*

  def bundle(name: Expr[String], modules: Expr[Seq[Any]])(using Quotes): Expr[(String, List[ModuleName])] =
    import quotes.reflect.*
    modules match
      case Varargs(modules) =>
        // Expr.ofList(modules.map(m => Expr(ModuleName.fromString(m.getName))))
        val moduleNames = modules.map { (module: Expr[Any]) =>
          report.info(s"Module: ${pprint(module.asTerm)}", module.asTerm.pos)
          val moduleTerm: Term = module.asTerm

          val moduleType: TypeRepr = moduleTerm.tpe
          val moduleSymbol: Symbol = moduleType.typeSymbol
          val moduleName: String   = moduleSymbol.fullName
          '{ ModuleName.fromString(${ Expr(moduleName) }) }

        }
        val res = Expr.ofList(moduleNames)
        '{ ($name, $res) }
      case _ =>
        report.errorAndAbort(
          "Expected explicit varargs sequence. " +
            "Notation `args*` is not supported.",
          modules.asTerm.pos
        )
  end bundle
end BundlerMacro
