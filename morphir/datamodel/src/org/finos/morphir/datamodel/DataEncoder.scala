package org.finos.morphir.datamodel
trait DataEncoder[A] {
  final def apply(value: A): Data = encode(value)
  def encode(value: A): Data
}

object DataEncoder {
  def apply[A](implicit toData: DataEncoder[A]): DataEncoder[A] = toData

  implicit val boolean: DataEncoder[Boolean] = (value: Boolean) => Data.Boolean(value)

  implicit val byte: DataEncoder[Byte] = (value: Byte) => Data.Byte(value)

  implicit val decimal: DataEncoder[BigDecimal] = (value: BigDecimal) => Data.Decimal(value)

  implicit val string: DataEncoder[String] = (value: String) => Data.String(value)
}
