package org.finos.morphir
package ddl

case class Label(value: String)

sealed trait Schema
object Schema {
  case object Int       extends Schema
  case object String    extends Schema
  case object Decimal   extends Schema
  case object LocalDate extends Schema
  case object Boolean   extends Schema

  case class Record(fields: scala.List[(Label, Schema)]) extends Schema
  case class Alias(name: String, value: Schema)          extends Schema
  case class List(elementType: Schema)                   extends Schema
  case class Map(keyType: Schema, valueType: Schema)     extends Schema
  case class Tuple(values: scala.List[Schema])           extends Schema
  case class Optional(elementType: Schema)               extends Schema
  
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
  case class Enum(cases: scala.List[Case]) extends Schema

  case class Case(label: Label, fields: scala.List[Case.Field])
  object Case {
    sealed trait Field
    object Field {
      case class Named(label: Label, value: Schema)
      case class Anon(value: Schema)
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
  case class Union(cases: scala.List[Schema]) extends Schema
}

// OutputType.Custom("List", List(OutputType.Int))

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
  case class Aliased(data: Data, alias: Schema.Alias)
}
