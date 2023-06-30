package org.finos.morphir.datamodel
trait DataEncoder[A] {
  final def apply(value: A): Data = encode(value)
  def encode(value:A) : Data
}

object DataEncoder {
  def apply[A](implicit toData: DataEncoder[A]): DataEncoder[A] = toData

  implicit val boolean: DataEncoder[Boolean] = new DataEncoder[Boolean] {
    override def encode(value: Boolean): Data = Data.Boolean(value)
  }

  implicit val string: DataEncoder[String] = new DataEncoder[String] {
    override def encode(value: String): Data = Data.String(value)
  }
}
