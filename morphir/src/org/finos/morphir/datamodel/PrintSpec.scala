package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.Concept
import org.finos.morphir.naming.*
import zio.Chunk
import org.finos.morphir.datamodel.*

import java.nio.file.Path as JPath
import java.nio.file.{FileSystem, Files, OpenOption, StandardOpenOption}
import java.util.Comparator
import scala.annotation.nowarn
import scala.jdk.CollectionConverters.*

object PrintSpec {

  private implicit class StringExt(s: String) {
    def inParensIf(cond: Boolean) =
      if (cond)
        s"(${s})"
      else
        s
  }

  import PathRenderer.TitleCase.*

  sealed trait HeadingPrint
  case object HeadingPrint {
    case object AllHeadings        extends HeadingPrint
    case object JustFileSeparators extends HeadingPrint
    case object None               extends HeadingPrint
  }

  def writeToFiles(concept: Concept, path: java.nio.file.Path, printHeadings: HeadingPrint = HeadingPrint.None) = {
    import java.io.File
    import zio.*

    val allModules = print(concept, printHeadings)

    for {
      dumpDir <- ZIO.succeed(path.toFile)
      _ <- ZIO.attempt {
        if (dumpDir.exists()) {
          deleteWalk(path)
        }
      }
      _ <- {
        val filesWrites =
          allModules.map { case (qualifiedModule, content) =>
            val packagePath         = qualifiedModule.packageName.path.segments.map(_.render)
            val modulePathUntilFile = qualifiedModule.modulePath.path.segments.map(_.render)
            val fullFilePath        = (packagePath ++ modulePathUntilFile).mkString("/")
            val elmFile             = new File(dumpDir, fullFilePath + ".elm")
            println(s"---------------------- Writing File: ${elmFile} ----------------------")

            elmFile.getParentFile.mkdirs()

            import java.nio.file.{Paths, Files}
            import java.nio.charset.StandardCharsets

            ZIO.attempt {
              Files.write(
                elmFile.toPath,
                content.getBytes(StandardCharsets.UTF_8)
              )
            }
          }
        ZIO.collectAll(filesWrites)
      }
    } yield ()
  }

  def of(concept: Concept, printHeadings: HeadingPrint = HeadingPrint.AllHeadings): String = {
    val allModules = print(concept, printHeadings)
    val allContent = allModules.map(_._2).mkString("\n\n")
    allContent
  }

  private[morphir] def print(concept: Concept, printHeadings: HeadingPrint = HeadingPrint.AllHeadings) = {
    val typesList = concept.collectAll
    val (printFileHeadings, printImportHeadings) =
      printHeadings match {
        case HeadingPrint.AllHeadings        => (true, true)
        case HeadingPrint.JustFileSeparators => (true, false)
        case HeadingPrint.None               => (false, false)
      }

    def printModuleDef(qn: FQName) = {
      val heading =
        if (printFileHeadings)
          s"{- ============ Declaring ${s"${qn.pack.render}:${qn.modulePath.path.render}:${qn.localName.render}"} ============ -}\n"
        else ""

      heading + s"module ${qn.pack.render}.${qn.modulePath.path.render} exposing (${qn.localName.render})"
    }

    def printImportDef(qn: FQName) = {
      val heading =
        if (printImportHeadings) {
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
        case _: Concept.Basic[_] => None
        case _: Concept.Any.type => None
        case v: Concept.Record   => Some(handleRecord(v))
        case _: Concept.Struct   => None
        case v: Concept.Alias    => Some(handleAlias(v))
        case _: Concept.List     => None
        case _: Concept.Map      => None
        case _: Concept.Tuple    => None
        case _: Concept.Optional => None
        case _: Concept.Result   => None
        case e: Concept.Enum     => Some(handleEnum(e))
        case _: Concept.Union    => None
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

    allModules
  }

  implicit class FQNameExt(name: FQName) {
    def getQualifiedModuleName = QualifiedModuleName(name.packagePath, name.modulePath)
    def sortKey                = (name.packagePath.path.render, name.modulePath.toPath.render, name.localName.render)
  }

  implicit class QualifiedModuleNameExt(name: QualifiedModuleName) {
    def sortKey = (name.packageName.path.render, name.modulePath.toPath.render)
  }

  private def deleteWalk(dir: JPath) = {
    import java.util.stream.{Stream => JStream}
    val filesSorted: JStream[JPath] =
      Files
        .walk(dir) // Traverse the file tree in depth-first order
        .sorted(Comparator.reverseOrder())
    val filesToDelete = filesSorted.iterator().asScala.toList
    filesToDelete.foreach(Files.delete(_))
  }

}
