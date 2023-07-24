package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.Concept
import org.finos.morphir.datamodel.namespacing.QualifiedName
import zio.Chunk

object PrintSpec {

  private[datamodel] class QualifiedNameCollector extends ConceptStatefulTransformer[Chunk[QualifiedName]] {
    override def of(c: Concept) =
      c match {
        case v @ Concept.Record(name, fields) => Stateful.succeedWithState(v)(chunk => chunk :+ name)
        case v @ Concept.Alias(name, value)   => Stateful.succeedWithState(v)(chunk => chunk :+ name)
        case v @ Concept.Enum(name, cases)    => Stateful.succeedWithState(v)(chunk => chunk :+ name)
        case other                            => super.of(other)
      }
  }
  private[datamodel] object QualifiedNameCollector {
    def collectFrom(c: Concept) =
      (new QualifiedNameCollector().of(c)).run(Chunk[QualifiedName]()) match {
        case (chunk, _) => chunk
      }
  }

  def of(concept: Concept): String = {
    val typesList = concept.collectAll

    def printModuleDef(qn: QualifiedName) =
      s"{- ============ Declaring ${s"${qn.pack.show}:${qn.namespace.show}:${qn.localName}"} ============ -}\n" +
        s"module ${qn.pack.show}.${qn.namespace.show} exposing (${qn.localName})"

    def printImportDef(qn: QualifiedName) =
      s"{- Importing ${s"${qn.pack.show}:${qn.namespace.show}:${qn.localName}"} -}\n" +
        s"import ${qn.pack.show}.${qn.namespace.show} exposing (${qn.localName})"

    def printFields(fields: List[(Label, Concept)]): String = {
      val fieldPrints = fields.map { case (label, field) =>
        val fieldPrint = printConcept(field, false)
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
      val alias       = s"type alias ${r.namespace.localName} ="
      val fieldPrints = printFields(r.fields)
      val allFieldPrints =
        alias + "\n" + "  " + fieldPrints
      allFieldPrints + "\n"
    }

    def printEnumDef(e: Concept.Enum) = {
      val casePrints =
        e.cases.map { case Concept.Enum.Case(label, values) =>
          val valuePrint =
            values.map { case (fieldLabel, concept) =>
              val valueConceptPrint = printConcept(concept)
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

      val importDefsPrint = {
        val importDefs =
          e.cases.flatMap(_.fields.map(_._2)).flatMap { c =>
            // Collect all qualified names of all top-level Records, Enums, and Aliases used.
            // If there are things that refer to them (e.g. a struct) then recurse into that
            val qualifiedNames = QualifiedNameCollector.collectFrom(c).toList
            qualifiedNames.map(printImportDef(_))
          }
        if (importDefs.isEmpty) "" else importDefs.mkString("\n") + "\n"
      }

      printModuleDef(e.name) + "\n" +
        importDefsPrint +
        s"type ${e.name.localName}" + "\n" + casePrintString
    }

    def printRecordInfo(r: Concept.Record) = {
      val moduleDef = printModuleDef(r.namespace)
      val importDefs =
        r.fields.map(_._2).flatMap { c =>
          val qualifiedNames = QualifiedNameCollector.collectFrom(c).toList
          qualifiedNames.map(printImportDef(_))
        }
      val recordDef       = printRecordDef(r)
      val importDefsPrint = if (importDefs.isEmpty) "" else importDefs.mkString("\n") + "\n"

      moduleDef + "\n" + importDefsPrint + recordDef
    }

    def printConcept(concept: Concept, isTopLevel: Boolean = false): String =
      concept match {
        // only show if this is part of a type e.g. part of a record, not on the top level
        case basic: Concept.Basic[_] =>
          if (isTopLevel) "" else basic.toString

        // only show if this is part of a type e.g. part of a record, not on the top level
        // (Even for that we don't need type defs for 'Any' it doesn't exist in ELM)
        case Concept.Any =>
          if (isTopLevel) "" else "Any"

        case r: Concept.Record =>
          if (isTopLevel) printRecordInfo(r)
          else r.namespace.localName.toString

        case Concept.Struct(fields) =>
          printFields(fields)

        // TODO what if it's an alias of a tuple, we need module imports for the types used in the tuple
        //      we need a folder that collects QNames (what about things inside of record? should record def handle that?)
        case Concept.Alias(name, value) =>
          val rhs = printConcept(value)
          s"${printModuleDef(name)}\ntype alias ${name.localName} = $rhs".stripMargin

        case Concept.List(elementType) =>
          val rhs = printConcept(elementType)
          s"List $rhs"

        case Concept.Map(keyType, valueType) =>
          val k = printConcept(keyType)
          val v = printConcept(valueType)
          s"Dict $k $v"

        case Concept.Tuple(values) =>
          val valuePrints = values.map(printConcept(_))
          s"(${valuePrints.mkString(", ")})"

        case Concept.Optional(elementType) =>
          val rhs = printConcept(elementType)
          s"Maybe $rhs"

        case e: Concept.Enum =>
          if (isTopLevel) printEnumDef(e)
          else e.name.localName.toString

        case Concept.Union(cases) => "<UNION NOT SUPPORTED IN ELM>"
      }

    val out = typesList.map(tpe => printConcept(tpe, true) + "\n")
    out.mkString("\n")
  }
}
