package morphir.mscplugin


import dotty.tools.dotc.{CompilationUnit, report}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core
import core.Contexts._
import core.Decorators._
import core.Flags._
import core.Symbols._
import core.Names._
import dotty.tools.FatalError
import dotty.tools.dotc.printing.Printer
import scala.collection.mutable
import scala.language.implicitConversions

class MIRCodeGen(val settings: GenMorphirIR.Settings)(using ctx:Context):
  import tpd._
  def run():Unit =
    try
      genCompilationUnit(ctx.compilationUnit)
    catch
      case e:Throwable =>
        report.error(s"[MIRCodeGen:Error]: ${e.getMessage}")
        report.warning("MIR code generation failed")        
    finally
      {}

  def genCompilationUnit(cunit:CompilationUnit):Unit =
    println(i"MIRCodeGen.genCompilationUnit: ${cunit.source}")
    def collectTypeDefs(tree: Tree): List[TypeDef] =
      tree match
        case EmptyTree => Nil
        case PackageDef(_, stats) => stats.flatMap(collectTypeDefs)
        case cd: TypeDef => cd :: Nil
        case _: ValDef => Nil // module instance

    val allTypeDefs = collectTypeDefs(cunit.tpdTree)
    println(i"MIRCodeGen.genCompilationUnit: allTypeDefs = ${allTypeDefs}")
    for (typeDef <- allTypeDefs)
      if (typeDef.symbol.is(ModuleClass))
        println("====================================================")
        println(i"Module: ${typeDef.symbol}")
        println("----------------------------------------------------")
        println(i"")
        println("====================================================")
        println()
      else
        println(i"Here: $typeDef")

  private def genIRFile(cunit:CompilationUnit):Unit =
    try {}
    finally {}

