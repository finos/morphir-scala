package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.Concept
import org.finos.morphir.naming.*
import zio.Chunk
import org.finos.morphir.datamodel.*
import org.finos.morphir.datamodel.PrintSpec.WriteFiles.WriteToPath

import java.nio.file.{FileSystem, OpenOption, StandardOpenOption}
import scala.annotation._

object PrintSpec {

  class QualifiedNameCollector extends ConceptStatefulTransformer[Chunk[FQName]] {
    private def addToState(v: Concept)(value: FQName) =
      Stateful.succeedWithState(v)(chunk => chunk :+ value)

    override def of(c: Concept) =
      c match {
        case v @ Concept.Record(name, _) => addToState(v)(name)
        case v @ Concept.Alias(name, _)  => addToState(v)(name)
        case v @ Concept.Enum(name, _)   => addToState(v)(name)
        case v: Concept.List             => addToState(v)(FQName.fromString("Morphir.SDK:List:List"))
        case v: Concept.Map              => addToState(v)(FQName.fromString("Morphir.SDK:Dict:Dict"))
        case v: Concept.Basic[_] =>
          v match {
            // Assuming that ToMorphirValue maps bytes to ints and this is a "standard" definition
            case Concept.Byte      => throw new RuntimeException("Morphir Byte (Int8) not supported yet")
            case Concept.Decimal   => addToState(v)(FQName.fromString("Morphir.SDK:Decimal:Decimal"))
            case Concept.Integer   => throw new RuntimeException("Morphir Integer (BigInt) not supported yet")
            case Concept.Int16     => addToState(v)(FQName.fromString("Morphir.SDK:Int:Int16"))
            case Concept.Int32     => addToState(v)(FQName.fromString("Morphir.SDK:Int:Int"))
            case Concept.LocalDate => addToState(v)(FQName.fromString("Morphir.SDK:LocalDate:LocalDate"))
            case Concept.Month     => addToState(v)(FQName.fromString("Morphir.SDK:Month:Month"))
            case Concept.LocalTime => addToState(v)(FQName.fromString("Morphir.SDK:LocalTime:LocalTime"))
            case other             => super.of(other)
          }

        case other => super.of(other)
      }
  }
  object QualifiedNameCollector {
    def collectFrom(c: Concept) =
      (new QualifiedNameCollector().of(c)).run(Chunk[FQName]()) match {
        case (chunk, _) => chunk
      }
  }

  @nowarn
  // NOTE: Is this needed, if not remove
  private implicit class ConceptOptsExt(c: Concept) {
    def isMultiArg =
      c match {
        case _: Concept.List     => true
        case _: Concept.Map      => true
        case _: Concept.Optional => true
        case _: Concept.Result   => true
        case _                   => false
      }
  }
  private implicit class StringExt(s: String) {
    def inParensIf(cond: Boolean) =
      if (cond)
        s"(${s})"
      else
        s
  }

  import PathRenderer.TitleCase.*

  trait WriteFiles
  object WriteFiles {
    case class WriteToPath(path: java.nio.file.Path) extends WriteFiles
    object Skip                                      extends WriteFiles
  }

  def of(concept: Concept, writeFiles: WriteFiles = WriteFiles.Skip, printHeadings: Boolean = true): String = {
    val typesList = concept.collectAll

    def printModuleDef(qn: FQName) = {
      val heading =
        if (printHeadings)
          s"{- ============ Declaring ${s"${qn.pack.render}:${qn.modulePath.path.render}:${qn.localName.render}"} ============ -}\n"
        else ""

      heading + s"module ${qn.pack.render}.${qn.modulePath.path.render} exposing (${qn.localName.render})"
    }

    def printImportDef(qn: FQName) = {
      val heading =
        if (printHeadings) {
          s"{- Importing ${s"${qn.pack.render}:${qn.modulePath.path.render}:${qn.localName.render}"} -}\n"
        } else ""

      heading + s"import ${qn.pack.render}.${qn.modulePath.path.render} exposing (${qn.localName.render})"
    }

    def printFields(fields: List[(Label, Concept)]): String = {
      val fieldPrints = fields.map { case (label, field) =>
        val fieldPrint = printDef(field, false)
        s"${label.value}: $fieldPrint"
      }
      val output =
        if (fields.size > 4) {
          fieldPrints.mkString("\n{\n", ",\n", "\n}\n").split("\n").map("  " + _).mkString("\n")
        } else {
          fieldPrints.mkString("{ ", ", ", " }")
        }
      output
    }

    def printRecordDef(r: Concept.Record) = {
      val alias       = s"type alias ${r.namespace.localName.render} ="
      val fieldPrints = printFields(r.fields)
      val allFieldPrints =
        alias + "  " + fieldPrints
      allFieldPrints + "\n"
    }

    def handleEnum(e: Concept.Enum) = {
      val casePrints =
        e.cases.map { case Concept.Enum.Case(label, values) =>
          val valuePrint =
            values.map { case (fieldLabel, concept) =>
              val valueConceptPrint = printDef(concept, true)
              val valueConceptLabelPrint =
                fieldLabel match {
                  case EnumLabel.Empty        => ""
                  case EnumLabel.Named(value) => s"{- ${value}: -} "
                }

              s"${valueConceptLabelPrint}${valueConceptPrint}"
            }
          s"${label.value} ${valuePrint.mkString(" ")}"
        }

      val casePrintString = {
        val first     = "= " + casePrints.take(1).head
        val afterward = casePrints.drop(1).map("| " + _)
        val combined  = first +: afterward
        combined.mkString("\n").split("\n").map("  " + _).mkString("\n")
      }

      val importDefs =
        e.cases.flatMap(_.fields.map(_._2)).flatMap { c =>
          // Collect all qualified names of all top-level Records, Enums, and Aliases used.
          // If there are things that refer to them (e.g. a struct) then recurse into that
          QualifiedNameCollector.collectFrom(c).toList
        }

      val enumDef = s"type ${e.name.localName.render}" + "\n" + casePrintString
      ConceptDef(enumDef, e.name, importDefs)
    }

    def handleRecord(r: Concept.Record): ConceptDef = {
      val usedDefs =
        r.fields.map(_._2).flatMap { c =>
          QualifiedNameCollector.collectFrom(c).toList
        }
      val recordDef = printRecordDef(r)
      ConceptDef(recordDef, r.namespace, usedDefs)
    }

    def handleAlias(alias: Concept.Alias) = {
      val usedDefs  = QualifiedNameCollector.collectFrom(alias)
      val name      = alias.name
      val rhs       = printDef(alias.value)
      val recordDef = s"type alias ${name.localName.render} = $rhs".stripMargin
      ConceptDef(recordDef, alias.name, usedDefs.toList)
    }

    case class ConceptDef(defString: String, name: FQName, imports: List[FQName])

    /*
     * Here we only care about things that introduce new definitions i.e. Concept.Alias, Concept.Record, and
     * Concept.Enum. Everything just needs to have it's internal definitions collected and then printed. It's only for
     * Aliases, Records, and Enums that we need to actually handle their definitions on the top-level.
     */
    def handleDef(concept: Concept, isTopLevel: Boolean = false): Option[ConceptDef] =
      concept match {
        case _: Concept.Basic[_]  => None
        case _: Concept.Any.type  => None
        case v: Concept.Record    => Some(handleRecord(v))
        case _: Concept.Struct    => None
        case v: Concept.Alias     => Some(handleAlias(v))
        case Concept.List(_)      => None
        case Concept.Map(_, _)    => None
        case Concept.Tuple(_)     => None
        case Concept.Optional(_)  => None
        case Concept.Result(_, _) => None
        case e: Concept.Enum      => Some(handleEnum(e))
        case Concept.Union(_)     => None
      }

    /*
     * isInside - Is it inside of a parent constructor (e.g. in a enum typedef constructor)
     */
    def printDef(concept: Concept, isInside: Boolean = true): String =
      concept match {
        case basic: Concept.Basic[_] =>
          basic match {
            case Concept.Boolean => "Bool"
            case Concept.Int32   => "Int"
            case _               => basic.toString
          }

        case Concept.Any            => "Any"
        case r: Concept.Record      => r.namespace.localName.render
        case Concept.Struct(fields) => printFields(fields)
        case Concept.Alias(name, _) => name.localName.render
        case Concept.List(elementType) =>
          s"List ${printDef(elementType)}"
            .inParensIf(isInside)

        case Concept.Map(keyType, valueType) =>
          s"Dict ${printDef(keyType)} ${printDef(valueType)}"
            .inParensIf(isInside)

        case Concept.Tuple(values) =>
          val valuePrints = values.map(printDef(_))
          s"(${valuePrints.mkString(", ")})"

        case Concept.Optional(elementType) =>
          s"Maybe ${printDef(elementType)}"
            .inParensIf(isInside)

        case Concept.Result(errType, okType) =>
          val err = printDef(errType)
          val ok  = printDef(okType)
          s"Result $err $ok".inParensIf(isInside)

        case e: Concept.Enum  => e.name.localName.render
        case Concept.Union(_) => "<UNION NOT SUPPORTED IN ELM>"
      }

    val defs = typesList.map(tpe => handleDef(tpe, true)).collect { case Some(conceptDef) => conceptDef }

    val groupedByModule = defs.groupBy(_.name.getQualifiedModuleName)
    val allModules =
      groupedByModule.toList.map { case (qualifiedModuleName, defs) =>
        val defConceptsToUse =
          defs.groupBy(_.name)
            .map { case (defName, defConcepts) =>
              val uniqueConcepts = defConcepts.distinct
              if (uniqueConcepts.length > 1) {
                println(
                  s"[WARNING] the definition ${defName} was found multiple times (using 1st def):\n" + uniqueConcepts.mkString(
                    "\n\n"
                  )
                )
              }
              defConcepts.head
            }

        val allImportsPrint =
          defs.flatMap(_.imports)
            .toList
            // filter out everything that is in the current module since that's the thing being created in this one
            .filterNot(_.getQualifiedModuleName == qualifiedModuleName)
            .filterNot(_ == FQName.fromString("Morphir.SDK:Int:Int")) // Don't need to import integers
            .distinct
            .sortBy(_.sortKey)
            .map(fqname => printImportDef(fqname))
            .mkString("\n")

        val allDefsPrint = defConceptsToUse.map(_.defString).mkString("\n\n")

        val modNamePrint =
          qualifiedModuleName.packageName.path.render + ":" + qualifiedModuleName.modulePath.path.render
        val moduleText = {
          val heading =
            s"{- ******************************************************* Module ${modNamePrint} ******************************************************* -}\n"

          heading + s"module  ${qualifiedModuleName.packageName.render}.${qualifiedModuleName.modulePath.path.render} exposing (..)" +
            "\n\n" +
            allImportsPrint + "\n\n" + allDefsPrint
        }

        (qualifiedModuleName, moduleText)
      }.sortBy(_._1.sortKey)

    val allContent =
      allModules.map(_._2).mkString("\n\n")

    def writeToFiles(path: java.nio.file.Path) = {
      import java.io.File

      val dumpDir = path.toFile
      if (dumpDir.exists()) {
        val deleted = dumpDir.delete()
        if (!deleted) throw new RuntimeException(s"Could not delete the file: ${dumpDir}")
      }

      for ((qualifiedModule, content) <- allModules) {
        val packagePath         = qualifiedModule.packageName.path.segments.map(_.render)
        val modulePathUntilFile = qualifiedModule.modulePath.path.segments.map(_.render)
        val fullFilePath        = (packagePath ++ modulePathUntilFile).mkString("/")
        val elmFile             = new File(dumpDir, fullFilePath + ".elm")
        println(s"---------------------- File: ${elmFile} ----------------------")

        elmFile.getParentFile.mkdirs()

        import java.nio.file.{Paths, Files}
        import java.nio.charset.StandardCharsets

        Files.write(
          elmFile.toPath,
          content.getBytes(StandardCharsets.UTF_8)
        )
      }
    }

    writeFiles match {
      case WriteToPath(path) => writeToFiles(path)
      case _                 =>
    }

    allContent
  }

  implicit class FQNameExt(name: FQName) {
    def getQualifiedModuleName = QualifiedModuleName(name.packagePath, name.modulePath)
    def sortKey                = (name.packagePath.path.render, name.modulePath.toPath.render, name.localName.render)
  }

  implicit class QualifiedModuleNameExt(name: QualifiedModuleName) {
    def sortKey = (name.packageName.path.render, name.modulePath.toPath.render)
  }
}
