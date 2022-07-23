package morphir.mscplugin

import dotty.tools.dotc.report
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.transform.{Pickler, PickleQuotes, PostInlining, SetRootTree, Staging, ElimOpaque}
import dotty.tools.dotc.typer.TyperPhase
import dotty.tools.dotc.printing.Printer
import dotty.tools.dotc.core.Flags

class MorphirModuleSelectPhase extends PluginPhase:
  import tpd.*
  private var counter: Int = 0 
  val phaseName = MorphirModuleSelectPhase.name
  override val runsAfter = Set(TyperPhase.name)
  override val runsBefore = Set(ElimOpaque.name, MorphirIRGenPhase.name)

  override def transformTypeDef(tree: TypeDef)(using ctx:Context): Tree =
    tree match
      case TypeDef(name, inner) => 
        println("====================================================")
        println(i"[$counter:TypeDef] for $name; isClassDef = ${tree.isClassDef}")
        println("----------------------------------------------------")
        println(i"${tree.symbol}")
        println("====================================================")
        println()

    // if (tree.mods.is(Flags.Module)) 
    //   println(tree.source.content.slice(tree.span.start, tree.span.end).mkString)
    // else     
    //   report.error("MorphirModuleSelectPhase: morphir only supports top-level modules", tree.source.atSpan(tree.span))
    counter += 1
    tree

  override def transformUnit(tree: Tree)(using Context): Tree = 
    
    println(i"Transforming unit in ${tree.source.file.absolutePath}")
    tree    

object MorphirModuleSelectPhase:
  val name = "morphirModuleSelect"
