package org.finos
package morphir.lang.scaladsl.internal

import morphir.ir.Module.ModuleName
import morphir.lang.scaladsl.Syntax
import Syntax.{BundleInfo, Bundle}

import scala.quoted.*
object BundlerMacro:
  inline def bundle(inline info: BundleInfo)(inline modules: Any*): Bundle =
    ${ bundleImpl('{ info }, '{ modules }) }
  def bundleImpl(info: Expr[BundleInfo], modules: Expr[Seq[Any]])(using Quotes): Expr[Bundle] =
    new BundlerMacro().bundle(info, modules)
end BundlerMacro

class BundlerMacro(using Quotes):
  import quotes.reflect.*

  def bundle(info: Expr[BundleInfo], modules: Expr[Seq[Any]])(using Quotes): Expr[Bundle] =
    import quotes.reflect.*
    modules match
      case Varargs(modules) =>
        // Expr.ofList(modules.map(m => Expr(ModuleName.fromString(m.getName))))
        val syntaxModules = modules
          .map { (module: Expr[Any]) =>
            report.info(s"Module: ${pprint(module.asTerm)}", module.asTerm.pos)
            val moduleTerm: Term = module.asTerm

            val moduleType: TypeRepr = moduleTerm.tpe
            val moduleSymbol: Symbol = moduleType.typeSymbol
            val moduleName: String   = moduleSymbol.fullName
            '{ ModuleName.fromString(${ Expr(moduleName) }) }

          }
          .map(name => '{ Syntax.Module($name) })
        val sModules = Expr.ofList(syntaxModules)
        '{ Bundle($info, $sModules) }
      case _ =>
        report.errorAndAbort(
          "Expected explicit varargs sequence. " +
            "Notation `args*` is not supported.",
          modules.asTerm.pos
        )
  end bundle
end BundlerMacro
