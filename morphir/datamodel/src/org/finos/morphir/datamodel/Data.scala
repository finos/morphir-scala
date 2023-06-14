package org.finos.morphir.datamodel
sealed trait Data {
  def shape: Concept
}

object Data {
  sealed trait Basic[+A] extends Data

  case class String(value: java.lang.String)       extends Basic[java.lang.String]    { val shape = Concept.String    }
  case class LocalDate(value: java.time.LocalDate) extends Basic[java.time.LocalDate] { val shape = Concept.LocalDate }
  case class Month(value: Int)                     extends Basic[Int]                 { val shape = Concept.Month     }
  case class LocalTime(value: java.time.LocalTime) extends Basic[java.time.LocalTime] { val shape = Concept.LocalTime }
  case class Char(value: scala.Char)               extends Basic[scala.Char]          { val shape = Concept.Char      }

  /**
   * See notes on Concept.Enum for information on how this type is modelled
   */
  case class Case(
      value: Data,
      enumCase: Concept.Enum.Case,
      shape: Concept.Enum
  )

  case class Tuple(values: scala.List[Data]) extends Data {
    val shape: Concept.Tuple = Concept.Tuple(values.map(_.shape))
  }
  case class Record private (values: scala.List[(Label, Data)]) extends Data {
    val shape: Concept.Record = Concept.Record(values.map { case (label, data) => (label, data.shape) })
  }

  /**
   * Equlvalent to ELM Optional or Scala Option
   */
  case class Optional(data: Data) extends Data {
    val shape: Concept.Optional = Concept.Optional(data.shape)
  }

  case class List private (values: scala.List[Data], shape: Concept.List)
  object List {
    def apply(value: Data, rest: Data*) =
      new List(value +: rest.toList, Concept.List(value.shape))

    def empty(schema: Concept.List) =
      new List(scala.List(), schema)

    def validated(values: scala.List[Data]): Option[List] =
      // Validate that element-type of everything is the same
      if (values.nonEmpty && values.forall(_ == values.head))
        Some(List(values, Concept.List(values.head.shape)))
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
