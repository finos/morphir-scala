package org.finos.morphir.datamodel

import org.finos.morphir.naming.FQName
import org.finos.morphir.datamodel.Concept.Basic
import org.finos.morphir.util.{DetailLevel, PrintMDM}

import scala.annotation.tailrec
import zio.Chunk
import zio.prelude.fx.ZPure

import scala.collection.immutable.List

//TODO: Keep this non-GADT version as Concept and make a GADT version `Schema[A]`
sealed trait Concept { self =>
  def meta: Concept.Meta

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
      case _: Concept.Any      => None
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
  case class Meta(typeParams: scala.List[Concept])
  object Meta {
    def default = Meta(scala.List())
  }

  sealed abstract class Basic[+A] extends Concept

  object Basic {
    type Boolean = Concept.Boolean
    val Boolean = Concept.Boolean
    type Byte = Concept.Byte
    val Byte = Concept.Byte
    type Decimal = Concept.Decimal
    val Decimal = Concept.Decimal
    type Integer = Concept.Integer
    val Integer = Concept.Integer
    type Int16 = Concept.Int16
    val Int16 = Concept.Int16
    type Int32 = Concept.Int32
    val Int32 = Concept.Int32
    type String = Concept.String
    val String = Concept.String
    type LocalDate = Concept.LocalDate
    val LocalDate = Concept.LocalDate
    type Month = Concept.Month
    val Month = Concept.Month
    type LocalTime = Concept.LocalTime
    val LocalTime = Concept.LocalTime
    type Char = Concept.Char
    val Char = Concept.Char
    type Unit = Concept.Unit
    val Unit = Concept.Unit
  }

  /// Represents any concept but also means that you have no reasonable idea of the shape of the associated data
  case class Any() extends Concept {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new Any { override def meta = newMeta }
  }

  case class Boolean() extends Basic[scala.Boolean] {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new Boolean { override def meta = newMeta }
  }

  case class Byte() extends Basic[Byte] {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new Byte { override def meta = newMeta }
  }

  case class Decimal() extends Basic[scala.BigDecimal] {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new Decimal { override def meta = newMeta }
  }

  case class Integer() extends Basic[scala.BigInt] {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new Integer { override def meta = newMeta }
  }

  case class Int16() extends Basic[Short] {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new Int16 { override def meta = newMeta }
  }

  case class Int32() extends Basic[Int] {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new Int32 { override def meta = newMeta }
  }

  case class Int64() extends Basic[Long] {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new Int64 { override def meta = newMeta }
  }

  case class String() extends Basic[java.lang.String] {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new String { override def meta = newMeta }
  }

  case class LocalDate() extends Basic[java.time.LocalDate] {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new LocalDate { override def meta = newMeta }
  }

  case class Month() extends Basic[java.time.Month] {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new Month { override def meta = newMeta }
  }

  case class LocalTime() extends Basic[java.time.LocalTime] {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new LocalTime { override def meta = newMeta }
  }

  case class Char() extends Basic[scala.Char] {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new Char { override def meta = newMeta }
  }

  case class Unit() extends Basic[scala.Unit] {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new Unit { override def meta = newMeta }
  }

  case class Nothing() extends Basic[scala.Nothing] {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new Nothing { override def meta = newMeta }
  }

  case class Record(namespace: FQName, fields: scala.List[(Label, Concept)])
      extends Concept { self =>
    def toStruct                = new Struct(fields) { override def meta = self.meta }
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new Record(namespace, fields) { override def meta = newMeta }
  }
  object Record {
    def apply(namespace: FQName, fields: (Label, Concept)*) = new Record(namespace, fields.toList)
  }

  case class Struct(fields: scala.List[(Label, Concept)]) extends Concept {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new Struct(fields) { override def meta = newMeta }
  }
  object Struct {
    def apply(fields: (Label, Concept)*) = new Struct(fields.toList)
  }

  case class Alias(name: FQName, value: Concept) extends Concept {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new Alias(name, value) { override def meta = newMeta }
  }

  case class List(elementType: Concept) extends Concept {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new List(elementType) { override def meta = newMeta }
  }

  case class Map(keyType: Concept, valueType: Concept) extends Concept {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new Map(keyType, valueType) { override def meta = newMeta }
  }

  case class Set(elementType: Concept) extends Concept {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new Set(elementType) { override def meta = newMeta }
  }

  case class Tuple(values: scala.List[Concept]) extends Concept {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new Tuple(values) { override def meta = newMeta }
  }
  object Tuple {
    def apply(values: Concept*) = new Tuple(values.toList)
  }

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
  case class Optional(elementType: Concept) extends Concept {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new Optional(elementType) { override def meta = newMeta }
  }

  case class Result(errType: Concept, okType: Concept) extends Concept {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new Result(errType, okType) { override def meta = newMeta }
  }

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
  case class Enum(name: FQName, cases: scala.List[Enum.Case]) extends Concept {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new Enum(name, cases) { override def meta = newMeta }
  }

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
  case class Union(cases: scala.List[Concept]) extends Concept {
    def meta                    = Meta.default
    def withMeta(newMeta: Meta) = new Union(cases) { override def meta = newMeta }
  }

  object Union {
    def apply(cases: Concept*) = new Union(cases.toList)
  }

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
