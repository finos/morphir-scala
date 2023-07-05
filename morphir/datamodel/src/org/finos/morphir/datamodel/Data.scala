package org.finos.morphir.datamodel
import java.io.OutputStream
import scala.collection.immutable.ListMap
import scala.collection.mutable
sealed trait Data extends geny.Writable {
  def shape: Concept
  def writeBytesTo(out: OutputStream): Unit = {
    // TODO: Implement writing
  }
}

object Data {
  val True: Data  = Boolean(true)
  val False: Data = Boolean(false)

  def Int(value: Int) = Int32(value)

  sealed trait Basic[+A] extends Data

  case class Boolean(value: scala.Boolean)         extends Basic[scala.Boolean]       { val shape = Concept.Boolean   }
  case class Byte(value: scala.Byte)               extends Basic[Byte]                { val shape = Concept.Byte      }
  case class Decimal(value: scala.BigDecimal)      extends Basic[scala.BigDecimal]    { val shape = Concept.Decimal   }
  case class Integer(value: scala.BigInt)          extends Basic[scala.BigInt]        { val shape = Concept.Integer   }
  case class Int16(value: scala.Short)             extends Basic[Short]               { val shape = Concept.Int16     }
  case class Int32(value: scala.Int)               extends Basic[Int]                 { val shape = Concept.Int32     }
  case class String(value: java.lang.String)       extends Basic[java.lang.String]    { val shape = Concept.String    }
  case class LocalDate(value: java.time.LocalDate) extends Basic[java.time.LocalDate] { val shape = Concept.LocalDate }
  case class Month(value: java.time.Month)         extends Basic[java.time.Month]     { val shape = Concept.Month     }
  case class LocalTime(value: java.time.LocalTime) extends Basic[java.time.LocalTime] { val shape = Concept.LocalTime }
  case class Char(value: scala.Char)               extends Basic[scala.Char]          { val shape = Concept.Char      }
  case object Unit                                 extends Basic[scala.Unit]          { val shape = Concept.Unit      }

  /**
   * See notes on Concept.Enum for information on how this type is modelled
   */
  case class Case(
      values: scala.List[(EnumLabel, Data)],
      enumLabel: java.lang.String,
      shape: Concept.Enum
  ) extends Data

  object Case {
    def apply(values: (EnumLabel, Data)*)(enumLabel: java.lang.String, shape: Concept.Enum) =
      new Case(values.toList, enumLabel, shape)
  }

  case class Tuple(values: scala.List[Data]) extends Data {
    val shape: Concept.Tuple = Concept.Tuple(values.map(_.shape))
  }
  case class Record(values: scala.List[(Label, Data)]) extends Data {
    val shape: Concept.Record = Concept.Record(values.map { case (label, data) => (label, data.shape) })
  }
  object Record {
    def apply(entry: (Label, Data)*): Record = Record(entry.toList)
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
      if (values.nonEmpty && values.forall(_ == values.head))
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
  case class Union(value: Data, unionSchema: Concept.Union)

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
