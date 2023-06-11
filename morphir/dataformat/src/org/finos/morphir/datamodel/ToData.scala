package org.finos.morphir.datamodel

trait ToData[A] {
  def apply(value: A): Data
}

object ToData {
  def apply[A](implicit toData: ToData[A]): ToData[A] = toData

  implicit val boolean: ToData[Boolean] = new ToData[Boolean] {
    override def apply(value: Boolean): Data = Data.Boolean(value)
  }

  implicit val string: ToData[String] = new ToData[String] {
    override def apply(value: String): Data = Data.String(value)
  }
}
