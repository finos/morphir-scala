package morphir.mscplugin

import dotty.tools.dotc.{CompilationUnit, report}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.core
import core.Contexts.*
import core.Decorators.*
import core.Flags.*
import core.Symbols.*
import core.Names.*
import dotty.tools.FatalError
import dotty.tools.dotc.printing.Printer
import morphir.mir.FQName
import morphir.mir.Module.Definition as MorphirModuleDef
import morphir.mir.module.ModuleName
import morphir.mir.file.format.MirFile

import io.bullet.borer.{Cbor, Output}
import morphir.mir.MirFileSupport.given
import java.io.OutputStream

class MirCodeGen(val settings: GenMorphirIR.Settings)(using ctx:Context):
  import tpd._

  protected val defnMir = MirDefinitions.get

  def run():Unit =
    try
      genCompilationUnit(ctx.compilationUnit)
    catch
      case e:Throwable =>
        report.error(s"[MIRCodeGen:Error]: ${e.getMessage}\r\n$e")
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
    for (typeDef <- allTypeDefs)
      if (typeDef.symbol.is(ModuleClass))
        val (moduleFQN, moduleDef) = genModuleData(typeDef)
        val mirFile = MirFile(moduleFQN.getModuleName)
        println("====================================================")
        println(i"Module: ${typeDef.symbol}")
        println("----------------------------------------------------")
        println(i"ModuleName: ${moduleFQN.getModuleName}")
        println("====================================================")
        println()      
        genIRFile(cunit, moduleFQN, moduleDef, mirFile)
      else
        println(i"Here: ${typeDef.symbol.name}")
  end genCompilationUnit

  private def genModuleData(td:TypeDef):(FQName, MorphirModuleDef[Any,Any]) =
    val sym = td.symbol.asClass
    //TODO: Capture Source location information
    //implicit val pos:Position = sym.span

    val fqn = MorphirEncoding.encodeModuleName(sym)
    (fqn, MorphirModuleDef.empty)
  end genModuleData


  private def genIRFile(cunit:CompilationUnit, fqn:FQName, moduleDef:MorphirModuleDef[Any,Any], mirFile:MirFile):Unit =
    val outfile = getFileFor(cunit, fqn.getModuleName, ".mir")
    val output:OutputStream = outfile.bufferedOutput
    try {
      Cbor.encode(mirFile).to(output).result
    }
    finally {
      output.close()
    }
  end genIRFile

  private def getFileFor(cunit: CompilationUnit, moduleName:ModuleName, suffix:String): dotty.tools.io.AbstractFile =
    val outputDirectory = ctx.settings.outputDir.value
    val pathParts = moduleName.namespace.segments.map(_.toLowerCase)
    val dir = pathParts.foldLeft(outputDirectory)(_.subdirectoryNamed(_))
    val filename = moduleName.localName.toTitleCase
    dir.fileNamed(filename + suffix)
