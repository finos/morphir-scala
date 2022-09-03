package morphir.mscplugin

import dotty.tools.dotc.{CompilationUnit, report}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.core
import core.Contexts.*
import core.Decorators.*
import core.Flags.*
import core.Symbols.*
import core.SymDenotations.{ClassDenotation, SymDenotation}
import core.Denotations.NonSymSingleDenotation
import core.Names.*
import dotty.tools.FatalError
import dotty.tools.dotc.printing.Printer
import morphir.mir
import morphir.mir.FQName
import morphir.mir.Module.Definition as MorphirModuleDef
import morphir.mir.Type.Definition as MorphirTypeDef
import morphir.mir.module.ModuleName
import morphir.mir.file.format.MirFile

import io.bullet.borer.{Cbor, Output}
import morphir.mir.MirFileSupport.given
import java.io.OutputStream
import zio.Chunk

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
        case _: Import => Nil

    val allTypeDefs = collectTypeDefs(cunit.tpdTree)
    for (typeDef <- allTypeDefs)
      if (typeDef.symbol.is(ModuleClass))
        println("====================================================")
        println(i"Module: ${typeDef.symbol}")
        println("----------------------------------------------------")
        val (moduleFQN, moduleDef) = genModuleData(typeDef)
        val mirFile = MirFile(moduleFQN.getModuleName)        
        println(i"ModuleName: ${moduleFQN.getModuleName}")
        println("====================================================")
        println()      
        genIRFile(cunit, moduleFQN, moduleDef, mirFile)
      else
        println(i"Here: ${typeDef.symbol.name}")
  end genCompilationUnit

  private def genModuleData(td:TypeDef):(FQName, MorphirModuleDef[Any,Any]) =
    def collectTypeDefs(tree:Tree):List[TypeDef] =
      tree match
        case cd:TypeDef => 
          println("td" * 40)
          println(i"tpe: ${cd.tpe}")
          println("td" * 40)

          cd :: Nil
        case _ => 
          println("?" * 60)
          println(i"tpe: ${tree.tpe}")
          println("?" * 60)
          Nil

    val sym = td.symbol.asClass
    //TODO: Capture Source location information
    //implicit val pos:Position = sym.span        

    // td.tpe.typeMembers.foreach { d =>
    //   println(s"symbol: ${d.symbol}, isOpaqueAlias: ${d.symbol.isOpaqueAlias}")
    //   d match
    //     case cd:ClassDenotation => 
    //       println("|++++++++++++++++++++++++++++++++++++++++++++++++++++")
    //       println(i"|ClassDenotation: $cd")
    //       println(i"|owner: ${cd.owner}")
    //       println(i"prefix: ${cd.prefix}")
    //       //
    //       println("|++++++++++++++++++++++++++++++++++++++++++++++++++++")      
    //     case sd:SymDenotation =>
    //       println("|++++++++++++++++++++++++++++++++++++++++++++++++++++")
    //       println(i"|SymDenotation: $sd")
    //       println(i"prefix: ${sd.prefix}")
    //       //
    //       println("|++++++++++++++++++++++++++++++++++++++++++++++++++++")      
    //     case sd:NonSymSingleDenotation if sd.symbol.isOpaqueAlias =>
    //       println("|++++++++++++++++++++++++++++++++++++++++++++++++++++")
    //       println("Processing Opaque type @@@@@@@@@")
    //       println(i"|NonSymSingleDenotation: $sd")
          
    //       println(i"| isTerm: ${sd.isTerm}, isType: ${sd.isType}")
    //       println(i"|prefix: ${sd.prefix}")
    //       println(i"|isTypeAlias: ${sd.info.isTypeAlias}, ${sd.showDcl}")
    //       //
    //       println("|++++++++++++++++++++++++++++++++++++++++++++++++++++")      
    //     case sd:NonSymSingleDenotation =>
    //       println("|++++++++++++++++++++++++++++++++++++++++++++++++++++")
    //       println(i"|NonSymSingleDenotation: $sd")
          
    //       println(i"| isTerm: ${sd.isTerm}, isType: ${sd.isType}")
    //       println(i"|prefix: ${sd.prefix}")
    //       println(i"|isTypeAlias: ${sd.info.isTypeAlias}, ${sd.showDcl}")
    //       //
    //       println("|++++++++++++++++++++++++++++++++++++++++++++++++++++")      
    //     case sd =>
    //       println("|++++++++++++++++++++++++++++++++++++++++++++++++++++")
    //       println(i"|SingleDenotation: $sd")
          
    //       println(i"| isTerm: ${sd.isTerm}, isType: ${sd.isType}")
    //       println(i"|prefix: ${sd.prefix}")
    //       println(i"|isTypeAlias: ${sd.info.isTypeAlias}, ${sd.showDcl}")
    //       //
    //       println("|++++++++++++++++++++++++++++++++++++++++++++++++++++")      
    // }

    val types = td.tpe.typeMembers.collect {
      case sd:NonSymSingleDenotation if sd.symbol.isOpaqueAlias =>                
        println(i"TypeName: ${sd.name}")
        val name = mir.Name.fromString(sd.name.show)
        println(i"Name: ${name}")
        println(i"TypeParams: ${sd.info.typeParams}")
        val typeRef = mir.Type.reference("Placeholder.For.LHS")
        val typeParams = Chunk.empty[mir.Name]
        val typeAlias = mir.Type.Definition.TypeAlias(typeParams, typeRef)
        val documented = mir.Documented("", typeAlias)
        val accessControlled = mir.AccessControlled.publicAccess(documented)
        (name, accessControlled)
    }.toList

    // Debug output
    types.foreach{ case (name, typeAlias) =>
      println(i"NamedType: ${(name, typeAlias)}")
    }

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
