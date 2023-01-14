package org.finos
package morphir.lang.ir.internal

import morphir.ir.Module.ModuleName

import scala.quoted.*
object BundlerMacro:
  inline def bundle(inline modules: Any*): List[ModuleName] = ${ bundleImpl('modules) }
  def bundleImpl(modules: Expr[Seq[Any]])(using Quotes): Expr[List[ModuleName]] =
    new BundlerMacro().bundle(modules)
end BundlerMacro

class BundlerMacro(using Quotes):
  import quotes.reflect.*

  def bundle(modules: Expr[Seq[Any]])(using Quotes): Expr[List[ModuleName]] =
    import quotes.reflect.*
    modules match
      case Varargs(modules) =>
        // Expr.ofList(modules.map(m => Expr(ModuleName.fromString(m.getName))))
        val moduleNames = modules.map { (module: Expr[Any]) =>
          val moduleTerm: Term     = module.asTerm
          val moduleType: TypeRepr = moduleTerm.tpe
          val moduleSymbol: Symbol = moduleType.typeSymbol
          val moduleName: String   = moduleSymbol.fullName
          '{ ModuleName.fromString(${ Expr(moduleName) }) }

        }
        Expr.ofList(moduleNames)
      case _ =>
        report.errorAndAbort(
          "Expected explicit varargs sequence. " +
            "Notation `args*` is not supported.",
          modules.asTerm.pos
        )
  end bundle
end BundlerMacro
