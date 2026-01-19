package org.finos.morphir.ir.v4

import org.finos.morphir.naming._
import zio.Chunk

enum NumericConstraint {
  case Arbitrary
  case Signed(bits: IntWidth)
  case Unsigned(bits: IntWidth)
  case FloatingPoint(bits: FloatWidth)
  case Bounded(min: Option[BigInt], max: Option[BigInt])
  case Decimal(precision: Int, scale: Int)
}

enum IntWidth {
  case I8, I16, I32, I64
}

enum FloatWidth {
  case F32, F64
}

enum StringEncoding {
  case UTF8, UTF16, ASCII, Latin1
}

final case class StringConstraint(
    encoding: Option[StringEncoding],
    minLength: Option[Int],
    maxLength: Option[Int],
    pattern: Option[String]
)

final case class CollectionConstraint(
    minLength: Option[Int],
    maxLength: Option[Int],
    uniqueItems: Boolean
)

final case class CustomConstraint(predicate: FQName, arguments: Chunk[Value])

final case class TypeConstraints(
    numeric: Option[NumericConstraint],
    string: Option[StringConstraint],
    collection: Option[CollectionConstraint],
    custom: Chunk[CustomConstraint]
)

object TypeConstraints {
  val empty: TypeConstraints = TypeConstraints(None, None, None, Chunk.empty)
}
