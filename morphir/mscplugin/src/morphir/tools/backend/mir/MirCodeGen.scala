package morphir.tools.backend.mir

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
import morphir.util.ScopedVar
import io.bullet.borer.{Cbor, Output}
import morphir.mir.MirFileSupport.given
import java.io.OutputStream
import zio.Chunk

class MirCodeGen(val settings: GenMorphirIR.Settings)(using ctx: Context)
    extends MirGenName
    with MirGenUtil
    with MirGenSupport
    with MirGenType:
  import tpd._
  import mir._

  protected val defnMir              = MirDefinitions.mirDefn
  protected val positionsConversions = new MirPositions()

  protected val curClassSym   = new ScopedVar[ClassSymbol]
  protected val curClassFresh = new ScopedVar[mir.Fresh]

  protected val curFresh = new ScopedVar[mir.Fresh]

  def run(): Unit =
    try genCompilationUnit(ctx.compilationUnit)
    catch
      case e: Throwable =>
        report.error(
          i"[MIRCodeGen:Error]: While compiling ${ctx.compilationUnit}%n==============> Message: ${e.getMessage}%n$e"
        )
        report.warning("MIR code generation failed")
    finally generatedDefns.clear()

  def genCompilationUnit(cunit: CompilationUnit): Unit =
    def collectTypeDefs(tree: Tree): List[TypeDef] =
      tree match
        case EmptyTree            => Nil
        case PackageDef(_, stats) => stats.flatMap(collectTypeDefs)
        case cd: TypeDef          => cd :: Nil
        case _: ValDef            => Nil // module instance
        case _: Import            => Nil

    collectTypeDefs(cunit.tpdTree)
      .foreach(genClass)

    generatedDefns.toSeq
      .groupBy(defn => getFileFor(cunit, defn.name.top))
      .foreach(genIRFile(_, _))

  end genCompilationUnit

  private def genModuleData(td: TypeDef): (FQName, MorphirModuleDef[Any, Any]) =
    def collectTypeDefs(tree: Tree): List[TypeDef] =
      tree match
        case EmptyTree            => Nil
        case PackageDef(_, stats) => stats.flatMap(collectTypeDefs)
        case cd: TypeDef          => cd :: Nil
        case _: ValDef            => Nil // module instance
    end collectTypeDefs

    val sym = td.symbol.asClass
    val fqn = MorphirEncoding.encodeModuleName(sym)
    (fqn, MorphirModuleDef.empty)
  end genModuleData

  private def genIRFile(
      cunit: CompilationUnit,
      fqn: FQName,
      moduleDef: MorphirModuleDef[Any, Any],
      mirFile: MirFile
  ): Unit =
    val outfile              = getFileFor(cunit, fqn.getModuleName, ".mir")
    val output: OutputStream = outfile.bufferedOutput
    try
      Cbor.encode(mirFile).to(output).result
    finally
      output.close()
  end genIRFile

  private def genIRFile(outfile: dotty.tools.io.AbstractFile, defns: Seq[mir.Defn]): Unit =
    import morphir.mir.serialization.serializeBinary
    val output = outfile.bufferedOutput
    try
      serializeBinary(defns, output)
    finally
      output.close()

  private def getFileFor(cunit: CompilationUnit, moduleName: ModuleName, suffix: String): dotty.tools.io.AbstractFile =
    val outputDirectory = ctx.settings.outputDir.value
    val pathParts       = moduleName.namespace.segments.map(_.toLowerCase)
    val dir             = pathParts.foldLeft(outputDirectory)(_.subdirectoryNamed(_))
    val filename        = moduleName.localName.toTitleCase
    dir.fileNamed(filename + suffix)

  private def getFileFor(cunit: CompilationUnit, ownerName: mir.Global): dotty.tools.io.AbstractFile =
    val mir.Global.Top(className) = ownerName: @unchecked
    val outputDirectory           = ctx.settings.outputDir.value
    val pathParts                 = className.split('.')
    val dir                       = pathParts.init.foldLeft(outputDirectory)(_.subdirectoryNamed(_))
    val filename                  = pathParts.last
    dir.fileNamed(filename + ".mir")
