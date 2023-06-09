package org.finos.morphir.dataformat.mdf

sealed trait Schema

object Schema {
  case object Int extends Schema

  case object String extends Schema

  case object Decimal extends Schema

  case object LocalDate extends Schema

  case object Boolean extends Schema

  case class Record(fields: scala.List[(Label, Schema)]) extends Schema

  case class Alias(name: String, value: Schema) extends Schema

  case class List(elementType: Schema) extends Schema

  case class Map(keyType: Schema, valueType: Schema) extends Schema

  case class Tuple(values: scala.List[Schema]) extends Schema

  case class Optional(elementType: Schema) extends Schema

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
  case class Enum(cases: scala.List[Enum.Case]) extends Schema

  object Enum {
    case class Case(label: Label, fields: scala.List[Case.Field])

    object Case {
      sealed trait Field

      object Field {
        case class Named(label: Label, value: Schema) extends Field

        case class Anon(value: Schema) extends Field
      }
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