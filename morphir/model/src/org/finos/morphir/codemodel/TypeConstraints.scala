package org.finos.morphir.codemodel

import org.finos.morphir.naming._
import kyo.Chunk
import kyo.Schema

enum NumericConstraint derives Schema {
  case Arbitrary
  case Signed(bits: IntWidth)
  case Unsigned(bits: IntWidth)
  case FloatingPoint(bits: FloatWidth)
  case Bounded(min: Option[BigInt], max: Option[BigInt])
  case Decimal(precision: Int, scale: Int)
}

enum IntWidth derives Schema {
  case I8, I16, I32, I64
}

enum FloatWidth derives Schema {
  case F32, F64
}

enum StringEncoding derives Schema {
  case UTF8, UTF16, ASCII, Latin1
}

final case class StringConstraint(
    encoding: Option[StringEncoding],
    minLength: Option[Int],
    maxLength: Option[Int],
    pattern: Option[String]
) derives Schema

final case class CollectionConstraint(
    minLength: Option[Int],
    maxLength: Option[Int],
    uniqueItems: Boolean
) derives Schema

final case class CustomConstraint(predicate: FQName, arguments: Chunk[Expr]) derives Schema

final case class TypeConstraints(
    numeric: Option[NumericConstraint],
    string: Option[StringConstraint],
    collection: Option[CollectionConstraint],
    custom: Chunk[CustomConstraint]
) derives Schema

object TypeConstraints {
  val empty: TypeConstraints = TypeConstraints(None, None, None, Chunk.empty)
}
