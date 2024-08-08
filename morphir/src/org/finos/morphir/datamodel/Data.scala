package org.finos.morphir.datamodel

import org.finos.morphir.naming.*
import org.finos.morphir.util.PrintMDM
import org.finos.morphir.util.PrintMDM.DetailLevel

import java.io.OutputStream
import scala.collection.mutable

sealed trait Data extends geny.Writable {
  def shape: Concept

  def getNameString: Option[String] =
    getName.map(_.localName.toTitleCase)
  def getName: Option[FQName] =
    this match {
      case _: Data.Basic[_] => None
      case v: Data.Case     => Some(v.shape.name)
      case _: Data.Tuple    => None
      case v: Data.Record   => Some(v.shape.namespace)
      case _: Data.Struct   => None
      case _: Data.Optional => None
      case _: Data.Result   => None
      case _: Data.List     => None
      case _: Data.Map      => None
      case _: Data.Set      => None
      case _: Data.Union    => None
      case v: Data.Aliased  => Some(v.shape.name)
    }

  def toStringPretty: String = toStringPretty(true)
  def toStringPretty(color: Boolean, detailLevel: DetailLevel = DetailLevel.BirdsEye): String =
    if (color)
      PrintMDM(this, detailLevel).toString
    else
      PrintMDM(this, detailLevel).plainText

  def writeBytesTo(out: OutputStream): Unit = {
    // TODO: Implement writing
  }
}

object Data {
  val True: Data  = Boolean(true)
  val False: Data = Boolean(false)

  def Int(value: Int) = Int32(value)

  sealed trait Basic[+A]                   extends Data
  case class Boolean(value: scala.Boolean) extends Basic[scala.Boolean] { val shape: Concept = Concept.Boolean }
  case class Byte(value: scala.Byte)       extends Basic[Byte]          { val shape: Concept = Concept.Byte    }
  // A Morphir/ELM float is a Double
  case class Float(value: scala.Double)       extends Basic[scala.Double]     { val shape: Concept = Concept.Float   }
  case class Decimal(value: scala.BigDecimal) extends Basic[scala.BigDecimal] { val shape: Concept = Concept.Decimal }
  case class Integer(value: scala.BigInt)     extends Basic[scala.BigInt]     { val shape: Concept = Concept.Integer }
  case class Int16(value: scala.Short)        extends Basic[Short]            { val shape: Concept = Concept.Int16   }
  case class Int32(value: scala.Int)          extends Basic[Int]              { val shape: Concept = Concept.Int32   }
  case class Int64(value: scala.Long)         extends Basic[Long]             { val shape: Concept = Concept.Int64   }
  case class String(value: java.lang.String)  extends Basic[java.lang.String] { val shape: Concept = Concept.String  }
  case class LocalDate(value: java.time.LocalDate) extends Basic[java.time.LocalDate] {
    val shape: Concept = Concept.LocalDate
  }
  case class Month(value: java.time.Month) extends Basic[java.time.Month] { val shape: Concept = Concept.Month }
  case class DayOfWeek(value: java.time.DayOfWeek) extends Basic[java.time.DayOfWeek] {
    val shape: Concept = Concept.DayOfWeek
  }
  case class LocalTime(value: java.time.LocalTime) extends Basic[java.time.LocalTime] {
    val shape: Concept = Concept.LocalTime
  }
  case class Char(value: scala.Char) extends Basic[scala.Char] { val shape: Concept = Concept.Char  }
  case class Order(value: Int)       extends Basic[Int]        { val shape: Concept = Concept.Order }
  case object Unit                   extends Basic[scala.Unit] { val shape: Concept = Concept.Unit  }

  // Needed for Scala 3 extension methods to work
  object Boolean   {}
  object Byte      {}
  object Decimal   {}
  object Integer   {}
  object Int16     {}
  object Int32     {}
  object String    {}
  object LocalDate {}
  object Month     {}
  object DayOfWeek {}
  object LocalTime {}
  object Char      {}
  object Order     {}

  /**
   * See notes on Concept.Enum for information on how this type is modelled
   */
  case class Case(
      values: scala.List[(EnumLabel, Data)],
      enumLabel: java.lang.String, // TODO: Why is this a string when the associated Concept is a label?
      shape: Concept.Enum
  ) extends Data

  object Case {
    def apply(values: (EnumLabel, Data)*)(enumLabel: java.lang.String, shape: Concept.Enum) =
      new Case(values.toList, enumLabel, shape)
  }

  case class Tuple(values: scala.List[Data]) extends Data {
    val shape: Concept.Tuple = Concept.Tuple(values.map(_.shape))
  }
  object Tuple {
    def apply(values: Data*): Tuple = Tuple(values.toList)
  }

  case class Record(values: scala.List[(Label, Data)], shape: Concept.Record) extends Data {
    def toStruct = Data.Struct(values)
  }
  object Record {
    def apply(namespace: FQName, fields: (Label, Data)*): Record =
      apply(namespace, fields.toList)

    def apply(namespace: FQName, fields: scala.List[(Label, Data)]): Record = {
      val concept = Concept.Record(namespace, fields.map { case (label, data) => (label, data.shape) })
      Record(fields.toList, concept)
    }

    /** Unapply of a record should contain it's qname and the field values, not the shape */
    def unapply(record: Record) =
      Some((record.shape.namespace, record.values))
  }

  case class Struct(values: scala.List[(Label, Data)]) extends Data {
    val shape: Concept.Struct = Concept.Struct(values.map { case (label, data) => (label, data.shape) })
  }
  object Struct {
    def apply(fields: (Label, Data)*): Struct = new Struct(fields.toList)
  }

  /**
   * Equlvalent to ELM Optional or Scala Option
   */
  sealed trait Optional extends Data
  object Optional {
    // Note that despite the fact that there is only one element, the Data element
    // can potentially have a more specific shape than the shape given by Some. Therefore
    // the option is given to pass in the actual shape
    case class Some private[datamodel] (data: Data, shape: Concept.Optional) extends Optional
    object Some {
      def apply(data: Data)                        = new Some(data, Concept.Optional(data.shape))
      def apply(data: Data, elementShape: Concept) = new Some(data, Concept.Optional(elementShape))
    }
    case class None private[datamodel] (shape: Concept.Optional) extends Optional
    object None {
      def apply(elementShape: Concept) = new None(Concept.Optional(elementShape))
    }
  }
  sealed trait Result extends Data
  object Result {
    case class Ok(data: Data, shape: Concept.Result) extends Result
    object Ok {
      def withErrConcept(data: Data, errConcept: Concept) = Ok(data, Concept.Result(errConcept, data.shape))
    }
    case class Err(data: Data, shape: Concept.Result) extends Result

    object Err {
      def withOkConcept(data: Data, okConcept: Concept) = Err(data, Concept.Result(data.shape, okConcept))
    }
  }

  case class List private[datamodel] (values: scala.List[Data], shape: Concept.List) extends Data
  object List {
    def apply(values: scala.List[Data], elementShape: Concept) =
      new List(values, Concept.List(elementShape))

    def apply(value: Data, rest: Data*) =
      new List(value +: rest.toList, Concept.List(value.shape))

    def empty(elementShape: Concept) =
      new List(scala.List(), Concept.List(elementShape))

    def validated(values: scala.List[Data]): Option[List] =
      // Validate that element-type of everything is the same
      if (values.nonEmpty && values.forall(_.shape == values.head.shape))
        Some(List(values, Concept.List(values.head.shape)))
      else
        None
  }

  /**
   * Since in ELM, record-based datastructures are generally kept in order, we want to preserve element ordering until
   * the last possible second (i.e. within the DDL structure). This should perserve a certain amount of determinism. We
   * have chosen to use LinkedHashMap because Scala's immutable ListMap is only suitable for a small amount of elements
   * and has O(n) lookup time. Despite the fact that this datastructure is mutable
   */
  case class Map private[datamodel] (values: mutable.LinkedHashMap[Data, Data], shape: Concept.Map) extends Data
  object Map {
    def copyFrom(values: mutable.LinkedHashMap[Data, Data], shape: Concept.Map) =
      Map(values.clone(), shape)

    def apply(value: (Data, Data), rest: (Data, Data)*) =
      new Map(mutable.LinkedHashMap.from(value +: rest.toList), Concept.Map(value._1.shape, value._2.shape))

    def empty(keyShape: Concept, valueShape: Concept) =
      new Map(mutable.LinkedHashMap.empty, Concept.Map(keyShape, valueShape))
  }

  case class Set private[datamodel] (values: mutable.LinkedHashSet[Data], shape: Concept.Set) extends Data

  object Set {
    def apply(values: mutable.LinkedHashSet[Data], elementShape: Concept) =
      new Set(values, Concept.Set(elementShape))

    def apply(value: Data, rest: Data*) =
      new Set(mutable.LinkedHashSet.from(value +: rest.toList), Concept.Set(value.shape))

    def empty(elementShape: Concept) =
      new Set(mutable.LinkedHashSet(), Concept.Set(elementShape))

    def validated(values: mutable.LinkedHashSet[Data]): Option[Set] =
      // Validate that element-type of everything is the same
      if (values.nonEmpty && values.forall(_.shape == values.head.shape))
        Some(Set(values, Concept.Set(values.head.shape)))
      else
        None
  }

  /**
   * A instance of a non-discrimiated union-type such as a Scala 3
   * {{{
   *   type MyUnion = Int | String
   * }}}
   * Would be defined as
   * {{{
   *   Union(Schema.Int, Schema.String)
   * }}}
   * A value of it would defined as:
   * {{{
   *   Data.Union(Data.Int(123), Union(Schema.Int, Schema.String))
   * }}}
   */
  case class Union(value: Data, shape: Concept.Union) extends Data

  /**
   * Represents data that lives beind a typedef. For example,
   * {{{
   *   type Label = String
   *   val x: Label = "xyz"
   * }}}
   *
   * Should would be represented as
   * {{{
   *   Aliased(Data.String("xyz"), schema = Schema.Alias("Label", Data.String))
   * }}}
   */
  case class Aliased(data: Data, shape: Concept.Alias) extends Data
}
