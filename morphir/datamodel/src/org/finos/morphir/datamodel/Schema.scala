package org.finos.morphir.datamodel

sealed trait Schema[A] {
}

object Schema {
  val boolean = Primitive(BasicDataType.Boolean)
  val byte = Primitive(BasicDataType.Byte)
  val decimal = Primitive(BasicDataType.Decimal)
  val integer = Primitive(BasicDataType.Integer)
  val int16 = Primitive(BasicDataType.Int16)
  val int32 = Primitive(BasicDataType.Int32)
  val string = Primitive(BasicDataType.String)
  val localDate = Primitive(BasicDataType.LocalDate)

  case class Primitive[A](basicDataType: BasicDataType[A]) extends Schema[A]
}
