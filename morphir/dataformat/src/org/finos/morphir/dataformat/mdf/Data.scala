package org.finos.morphir.dataformat.mdf

sealed trait Data {
  def schema: Schema
}

object Data {
  case class Int(value: scala.Int)                      extends Data { val schema = Schema.Int       }
  case class String(value: java.lang.String)            extends Data { val schema = Schema.String    }
  case class Decimal(value: scala.Double)               extends Data { val schema = Schema.Decimal   }
  case class LocalDate(day: Int, month: Int, year: Int) extends Data { val schema = Schema.LocalDate }
  case class Boolean(value: Boolean)                    extends Data { val schema = Schema.Boolean   }

  /**
   * See notes on Schema.Enum for information on how this type is modelled
   */
  case class Case(
      value: Data,
      enumCase: Schema.Enum.Case,
      schema: Schema.Enum
  )

  case class Tuple(values: scala.List[Data]) extends Data {
    val schema: Schema.Tuple = Schema.Tuple(values.map(_.schema))
  }
  case class Record private (values: scala.List[(Label, Data)]) extends Data {
    val schema: Schema.Record = Schema.Record(values.map { case (label, data) => (label, data.schema) })
  }

  /**
   * Equlvalent to ELM Optional or Scala Option
   */
  case class Optional(data: Data) extends Data {
    val schema: Schema.Optional = Schema.Optional(data.schema)
  }

  case class List private (values: scala.List[Data], schema: Schema.List)
  object List {
    def apply(value: Data, rest: Data*) =
      new List(value +: rest.toList, Schema.List(value.schema))

    def empty(schema: Schema.List) =
      new List(scala.List(), schema)

    def validated(values: scala.List[Data]): Option[List] =
      // Validate that element-type of everything is the same
      if (values.nonEmpty && values.forall(_ == values.head))
        Some(List(values, Schema.List(values.head.schema)))
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
  case class Union(value: Data, unionSchema: Schema.Union)

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
  case class Aliased(data: Data, schema: Schema.Alias) extends Data
}
