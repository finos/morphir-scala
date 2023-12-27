package org.finos.morphir.datamodel

import org.finos.morphir.naming.FQName
import org.finos.morphir.datamodel.Concept.Basic
import org.finos.morphir.util.{DetailLevel, PrintMDM}

import scala.annotation.tailrec
import zio.Chunk
import zio.prelude.fx.ZPure

//TODO: Keep this non-GADT version as Concept and make a GADT version `Schema[A]`
sealed trait Concept { self =>
  def collectAll: Chunk[Concept] =
    (new Concept.Collector[Concept](PartialFunction.fromFunction(x => x)).of(self))
      .run(Chunk[Concept]()) match {
      case (chunk, _) => chunk
    }

  def collect[T](p: PartialFunction[Concept, T]): Chunk[T] =
    (new Concept.Collector[T](p).of(self)).run(Chunk[T]()) match {
      case (chunk, _) => chunk
    }

  def getNameString: Option[String] =
    getName.map(_.localName.toTitleCase)
  def getName: Option[FQName] =
    this match {
      case _: Concept.Basic[_] => None
      case _: Concept.Any.type => None
      case c: Concept.Record   => Some(c.namespace)
      case _: Concept.Struct   => None
      case c: Concept.Alias    => Some(c.name)
      case _: Concept.List     => None
      case _: Concept.Map      => None
      case _: Concept.Set      => None
      case _: Concept.Tuple    => None
      case _: Concept.Optional => None
      case _: Concept.Result   => None
      case c: Concept.Enum     => Some(c.name)
      case _: Concept.Union    => None
    }

  def toStringPretty: String = toStringPretty(true)
  def toStringPretty(color: Boolean, detailLevel: DetailLevel = DetailLevel.BirdsEye): String =
    if (color)
      PrintMDM(this, detailLevel).toString
    else
      PrintMDM(this, detailLevel).plainText

  def toMorphirElm: String                           = PrintSpec.of(this)
  def writeMorphirElmFiles(path: java.nio.file.Path) = PrintSpec.writeToFiles(this, path)
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
    type DayOfWeek = Concept.DayOfWeek.type
    val DayOfWeek = Concept.DayOfWeek
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
  case object Float     extends Basic[scala.Double]
  case object Decimal   extends Basic[scala.BigDecimal]
  case object Integer   extends Basic[scala.BigInt]
  case object Int16     extends Basic[Short]
  case object Int32     extends Basic[Int]
  case object Int64     extends Basic[Long]
  case object String    extends Basic[java.lang.String]
  case object LocalDate extends Basic[java.time.LocalDate]
  case object Month     extends Basic[java.time.Month]
  case object DayOfWeek extends Basic[java.time.DayOfWeek]
  case object LocalTime extends Basic[java.time.LocalTime]
  case object Char      extends Basic[scala.Char]
  case object Unit      extends Basic[scala.Unit]
  case object Nothing   extends Basic[scala.Nothing]

  case class Record(namespace: FQName, fields: scala.List[(Label, Concept)]) extends Concept {
    def toStruct: Concept.Struct = Concept.Struct(fields: _*)
  }
  object Record {
    def apply(namespace: FQName, fields: (Label, Concept)*) = new Record(namespace, fields.toList)
  }

  case class Struct(fields: scala.List[(Label, Concept)]) extends Concept
  object Struct {
    def apply(fields: (Label, Concept)*) = new Struct(fields.toList)
  }

  case class Alias(name: FQName, value: Concept) extends Concept

  case class List(elementType: Concept) extends Concept

  case class Map(keyType: Concept, valueType: Concept) extends Concept

  case class Set(elementType: Concept) extends Concept

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

  case class Result(errType: Concept, okType: Concept) extends Concept

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
  case class Enum(name: FQName, cases: scala.List[Enum.Case]) extends Concept

  object Enum {
    def apply(name: FQName, cases: Enum.Case*) =
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

  /** Collector to help with Traversal */
  class Collector[T](p: PartialFunction[Concept, T])
      extends ConceptStatefulTransformer[Chunk[T]] {

    override def transform[R <: Concept](concept: R): Stateful[R] =
      if (p.isDefinedAt(concept)) {
        ZPure.update[Chunk[T], Chunk[T]](state => state :+ p(concept)) *> ZPure.succeed(concept)
      } else {
        ZPure.succeed(concept)
      }
  }
}
