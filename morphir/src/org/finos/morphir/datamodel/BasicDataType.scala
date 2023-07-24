package org.finos.morphir.datamodel
sealed trait BasicDataType[+A]

object BasicDataType {
  case object Boolean   extends BasicDataType[scala.Boolean]
  case object Byte      extends BasicDataType[Byte]
  case object Decimal   extends BasicDataType[scala.BigDecimal]
  case object Integer   extends BasicDataType[scala.BigInt]
  case object Int16     extends BasicDataType[Short]
  case object Int32     extends BasicDataType[Int]
  case object String    extends BasicDataType[java.lang.String]
  case object LocalDate extends BasicDataType[java.time.LocalDate]

  val all: Set[BasicDataType[Any]] = Set(
    Boolean,
    Byte,
    Decimal,
    Integer,
    Int16,
    Int32,
    String,
    LocalDate
  )
}
