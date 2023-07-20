package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.Concept.Basic
import org.finos.morphir.datamodel.namespacing.QualifiedName
import org.finos.morphir.foundations.Chunk

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

//TODO: Keep this non-GADT version as Concept and make a GADT version `Schema[A]`
sealed trait Concept { self =>
  final def foldContext[C, Z](context: C)(folder: Folder[C, Z]): Z = {
    @tailrec
    def loop(in: List[Concept], out: List[Either[Concept, Z]]): List[Z] =
      import folder._
      in match {
        case (basic: Basic[_]) :: types =>
          loop(types, Left(basic) :: out)
        case (t @ Concept.Any) :: types =>
          loop(types, Left(t) :: out)
        case (t @ Concept.Record(namespace, fields)) :: types =>
          val fieldTypes = fields.map(_._2)
          loop(fieldTypes ++ types, Left(t) :: out)
        case (t @ Concept.Struct(fields)) :: types =>
          val fieldTypes = fields.map(_._2)
          loop(fieldTypes ++ types, Left(t) :: out)
        case (t @ Concept.Alias(name, value)) :: types =>
          loop(value +: types, Left(t) :: out)
        case (t @ Concept.List(elementType)) :: types =>
          loop(elementType +: types, Left(t) :: out)
        case (t @ Concept.Map(keyType, valueType)) :: types =>
          loop(keyType +: valueType +: types, Left(t) :: out)
        case (t @ Concept.Tuple(values)) :: types =>
          loop(values ++ types, Left(t) :: out)
        case (t @ Concept.Optional(elementType)) :: types =>
          loop(elementType +: types, Left(t) :: out)
        case (t @ Concept.Enum(name, cases)) :: types =>
          val fieldTypes = cases.flatMap(_.fields.map(_._2))
          loop(fieldTypes ++ types, Left(t) :: out)
        case (t @ Concept.Union(cases)) :: types =>
          loop(cases ++ types, Left(t) :: out)
        case Nil =>
          out.foldLeft[List[Z]](List.empty) {
            case (acc, Right(results)) =>
              results :: acc
            case (acc, Left(t: Concept.Basic[_])) =>
              basicCase(context, t) :: acc
            case (acc, Left(t @ Concept.Any)) =>
              anyCase(context) :: acc

            case (acc, Left(t @ Concept.Record(qname, fields))) =>
              val size       = t.fields.size
              val fieldTypes = acc.take(size)
              val rest       = acc.drop(size)
              val fields     = t.fields.zip(fieldTypes).map { case ((label, _), fieldType) => (label, fieldType) }
              recordCase(context, qname, fields) :: rest
            case (acc, Left(t @ Concept.Struct(fields))) =>
              val size       = t.fields.size
              val fieldTypes = acc.take(size)
              val rest       = acc.drop(size)
              val fields     = t.fields.zip(fieldTypes).map { case ((label, _), fieldType) => (label, fieldType) }
              structCase(context, fields) :: rest
            case (acc, Left(t @ Concept.Alias(name, value))) =>
              // just take one value from the accumulator which is the alias type
              aliasCase(context, name, acc.head) :: acc.tail

            case (acc, Left(t @ Concept.List(elementType))) =>
              // just take one value from the accumulator which is the element type
              listCase(context, acc.head) :: acc.tail

            case (acc, Left(t @ Concept.Map(keyType, valueType))) =>
              val kv   = acc.take(2)
              val rest = acc.drop(2)
              // TODO check that they have the right order (key comes before value)
              mapCase(context, kv(0), kv(1)) :: rest

            case (acc, Left(t @ Concept.Tuple(values))) =>
              val size   = t.values.size
              val values = acc.take(size)
              val rest   = acc.drop(size)
              tupleCase(context, values) :: rest

            case (acc, Left(t @ Concept.Optional(elementType))) =>
              optionalCase(context, acc.head) :: acc.tail

            case (acc, Left(t @ Concept.Enum(name, cases))) =>
              import scala.collection.mutable.ArrayBuffer
              import scala.collection.mutable.ArrayDeque

              // using local mutability + iterative logic here because it is intricate
              val mutableAcc       = ArrayDeque.from(acc)
              val enumCaseMappings = ArrayBuffer[FoldableCase[Z]]()
              for (enumCase <- cases) {
                // for each enum case, need to pop-off a number of types from the accumulator equivalent to the number
                // of values that the enum-case fields hold
                val enumCaseFields = ArrayBuffer[Z]()
                for (i <- (0 until enumCase.fields.size))
                  enumCaseFields += mutableAcc.removeHead()

                // create a specific foldable case out of every single found mapping
                val foldableCase =
                  FoldableCase(
                    enumCase.label,
                    enumCase.fields.zip(enumCaseFields).map {
                      case ((label, _), z) => (label, z)
                    }
                  )
                enumCaseMappings += foldableCase
              }

              // now since all the types of all the enum-fields of all the enum-cases were removed,
              // the rest of the accumulator has the remaining types
              enumCase(context, name, enumCaseMappings.toList) :: mutableAcc.toList

            case (acc, Left(t @ Concept.Union(cases))) =>
              val size       = t.cases.size
              val fieldTypes = acc.take(size)
              val rest       = acc.drop(size)
              unionCase(context, fieldTypes) :: rest

            // Unreachable, all types covered
            //  case (acc, Left(t)) =>
            //    throw new IllegalStateException(
            //      s"Unexpected type ${t.getClass.getSimpleName()} encountered during transformation. (Type Expr: $t)"
            //    )
          }
      }
    loop(List(self), List.empty).head
  }

  case class FoldableCase[Z](label: Label, fields: scala.List[(EnumLabel, Z)])

  trait Folder[-Context, Z] {
    def basicCase(ctx: Context, value: Concept): Z
    def anyCase(ctx: Context): Z
    def recordCase(ctx: Context, namespace: QualifiedName, fields: List[(Label, Z)]): Z
    def structCase(ctx: Context, fields: List[(Label, Z)]): Z
    def aliasCase(ctx: Context, name: QualifiedName, value: Z): Z
    def listCase(ctx: Context, elementType: Z): Z
    def mapCase(ctx: Context, keyType: Z, valueType: Z): Z
    def tupleCase(ctx: Context, values: List[Z]): Z
    def optionalCase(ctx: Context, elementType: Z): Z
    def enumCase(ctx: Context, name: QualifiedName, cases: List[FoldableCase[Z]]): Z
    def unionCase(ctx: Context, cases: List[Z]): Z
  }

  object TypeCollectingFolder extends Folder[ArrayBuffer[Concept], Concept] {
    override def anyCase(ctx: ArrayBuffer[Concept]): Concept =
      Concept.Any
    override def recordCase(
        ctx: ArrayBuffer[Concept],
        namespace: QualifiedName,
        fields: List[(Label, Concept)]
    ): Concept = {
      val out = Concept.Record(namespace, fields)
      ctx += out
      out
    }

    override def basicCase(ctx: ArrayBuffer[Concept], value: Concept): Concept =
      val out = value
      ctx += out
      out
    override def structCase(ctx: ArrayBuffer[Concept], fields: List[(Label, Concept)]): Concept =
      val out = Concept.Struct(fields)
      ctx += out
      out
    override def aliasCase(ctx: ArrayBuffer[Concept], name: QualifiedName, value: Concept): Concept =
      val out = Concept.Alias(name, value)
      ctx += out
      out
    override def listCase(ctx: ArrayBuffer[Concept], elementType: Concept): Concept =
      val out = Concept.List(elementType)
      ctx += out
      out
    override def mapCase(ctx: ArrayBuffer[Concept], keyType: Concept, valueType: Concept): Concept =
      val out = Concept.Map(keyType, valueType)
      ctx += out
      out
    override def tupleCase(ctx: ArrayBuffer[Concept], values: List[Concept]): Concept =
      val out = Concept.Tuple(values)
      ctx += out
      out
    override def optionalCase(ctx: ArrayBuffer[Concept], elementType: Concept): Concept =
      val out = Concept.Optional(elementType)
      ctx += out
      out
    override def unionCase(ctx: ArrayBuffer[Concept], cases: List[Concept]): Concept =
      val out = Concept.Union(cases)
      ctx += out
      out
    override def enumCase(ctx: ArrayBuffer[Concept], name: QualifiedName, cases: List[FoldableCase[Concept]]): Concept =
      val out = Concept.Enum(name, cases.map(c => Concept.Enum.Case(c.label, c.fields)).toList)
      ctx += out
      out
  }

  def printSpec: String = {
    val buff = scala.collection.mutable.ArrayBuffer[Concept]()
    // use the folder to gather the types
    foldContext(buff)(TypeCollectingFolder)
    val typesList = buff.distinct.toList

    // TODO use showInstances or change Package/Namespace to not be newtypes
    def printModuleDef(qn: QualifiedName) =
      s"{- ============ Declaring ${s"${qn.pack.show}:${qn.namespace.show}:${qn.localName}"} ============ -}\n" +
        s"module ${qn.pack.show}.${qn.namespace.show} exposing (${qn.localName})"

    // TODO use showInstances or change Package/Namespace to not be newtypes
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
          e.cases.flatMap(_.fields.map(_._2)).collect {
            case v: Concept.Record => printImportDef(v.namespace)
            // TODO All sub-types of whatever is inside the alias that needs it e.g. a struct
            case v: Concept.Alias => printImportDef(v.name)
            case v: Concept.Enum  => printImportDef(v.name)
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
        r.fields.map(_._2).collect {
          case v: Concept.Record => printImportDef(v.namespace)
          // TODO All sub-types of whatever is inside the alias that needs it e.g. a struct
          case v: Concept.Alias => printImportDef(v.name)
          case v: Concept.Enum  => printImportDef(v.name)
        }
      val recordDef       = printRecordDef(r)
      val importDefsPrint = if (importDefs.isEmpty) "" else importDefs.mkString("\n") + "\n"

      moduleDef + "\n" + importDefsPrint + recordDef
    }

    def printConcept(concept: Concept, isTopLevel: Boolean = false): String =
      concept match {
        // only show if this is part of a type e.g. part of a record, not on the top level
        case basic: Basic[_] =>
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
          s"List $elementType"

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

    val out = typesList.map(tpe => printConcept(tpe, true))
    out.mkString("\n")
  }
}

object Concept {
  sealed trait Basic[+A] extends Concept

  object Basic {
    type Boolean = Concept.Boolean.type
    val Boolean = Concept.Boolean
    type Byte = Concept.Byte.type
    val Byte = Concept.Byte
    type Decimal = Concept.Decimal.type
    val Decimal = Concept.Decimal
    type Integer = Concept.Integer.type
    val Integer = Concept.Integer
    type Int16 = Concept.Int16.type
    val Int16 = Concept.Int16
    type Int32 = Concept.Int32.type
    val Int32 = Concept.Int32
    type String = Concept.String.type
    val String = Concept.String
    type LocalDate = Concept.LocalDate.type
    val LocalDate = Concept.LocalDate
    type Month = Concept.Month.type
    val Month = Concept.Month
    type LocalTime = Concept.LocalTime.type
    val LocalTime = Concept.LocalTime
    type Char = Concept.Char.type
    val Char = Concept.Char
    type Unit = Concept.Unit.type
    val Unit = Concept.Unit
  }

  /// Represents any concept but also means that you have no reasonable idea of the shape of the associated data
  case object Any extends Concept

  case object Boolean   extends Basic[scala.Boolean]
  case object Byte      extends Basic[Byte]
  case object Decimal   extends Basic[scala.BigDecimal]
  case object Integer   extends Basic[scala.BigInt]
  case object Int16     extends Basic[Short]
  case object Int32     extends Basic[Int]
  case object String    extends Basic[java.lang.String]
  case object LocalDate extends Basic[java.time.LocalDate]
  case object Month     extends Basic[java.time.Month]
  case object LocalTime extends Basic[java.time.LocalTime]
  case object Char      extends Basic[scala.Char]
  case object Unit      extends Basic[scala.Unit]
  case object Nothing   extends Basic[scala.Nothing]

  case class Record(namespace: QualifiedName, fields: scala.List[(Label, Concept)]) extends Concept
  object Record {
    def apply(namespace: QualifiedName, fields: (Label, Concept)*) = new Record(namespace, fields.toList)
  }

  case class Struct(fields: scala.List[(Label, Concept)]) extends Concept
  object Struct {
    def apply(fields: (Label, Concept)*) = new Struct(fields.toList)
  }

  case class Alias(name: QualifiedName, value: Concept) extends Concept

  case class List(elementType: Concept) extends Concept

  case class Map(keyType: Concept, valueType: Concept) extends Concept

  case class Tuple(values: scala.List[Concept]) extends Concept

  /**
   * We can only know if an optional-value is Some or None on the value-level, not the type-level because the
   * parent-derivation stage does not know this information. This is generally understood to be a standard practice. For
   * example, using Scala 3 enums, the specific type of an enum element is not known, only the general coproduct type.
   * For example:
   * {{{
   * enum Customer:
   *   case Person
   *   case Robot
   *
   * // this will be implicitly typed as Customer
   * val c = Customer.Person
   * }}}
   * Coproduct types in other languages (e.g. Haskell) work similarly.
   */
  case class Optional(elementType: Concept) extends Concept

  /**
   * A discrimiated union type such as an ELM union (either with labels or not)
   *
   * Given an Elm Datatype that looks like this:
   * {{{
   * type MyUnion =
   *   = NoValue
   *   | IntValue x:Int
   *   | MultiValue x:Int y:String
   *   | MultiValueAnon Int String // no labels for the types
   * }}}
   *
   * Or a Scala 3 enum that looks like this:
   * {{{
   *   enum MyUnion:
   *     case NoValue
   *     case IntValue(x:Int)
   *     case MultiValue(x:Int, y:String)
   *     // case MultiValueAnon(Int, String) // cannot have un-labeled unions in Scala3
   * }}}
   *
   * The corresponding type-representation should look like this:
   * {{{
   * Enum(
   *   Case("NoValue", List()),
   *   Case("IntValue", List(Case.Field.Named("x", Schema.Int))),
   *   Case("MultiValue", List(Case.Field.Named("x", Schema.Int), Case.Field.Named("y", Schema.String)))
   *   Case("MultiValueAnon", List(Case.Field.Anon(Schema.Int), Case.Field.Anon(Schema.String)))
   * )
   * }}}
   *
   * On the value level this should look as follows
   * {{{
   *   // Given a type definition that looks like this (In Scala)
   *   val x: MyUnion = MyUnion.IntValue(123)
   *
   *   // It's data-level encoding should look like this
   *   Data.Case(
   *     value: Data.Int(123)
   *     case: Case("IntValue", List(Case.Field.Named("x", Schema.Int)))
   *     schema: Schema.Enum
   *   )
   * }}}
   */
  case class Enum(name: QualifiedName, cases: scala.List[Enum.Case]) extends Concept

  object Enum {
    def apply(name: QualifiedName, cases: Enum.Case*) =
      new Enum(name, cases.toList)

    case class Case(label: Label, fields: scala.List[(EnumLabel, Concept)])

    object Case {
      def apply(label: Label, fields: (EnumLabel, Concept)*) =
        new Case(label, fields.toList)
    }
  }

  /**
   * A non-discrimiated union-type such as a Scala 3
   * {{{
   *   type MyUnion = Int | String
   * }}}
   * Would be defined as
   * {{{
   *   Union(Schema.Int, Schema.String)
   * }}}
   */
  case class Union(cases: scala.List[Concept]) extends Concept
}
