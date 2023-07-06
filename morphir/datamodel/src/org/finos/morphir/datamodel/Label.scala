package org.finos.morphir.datamodel

case class Label(value: String)

sealed trait EnumLabel
object EnumLabel {
  def apply(value: String): EnumLabel = EnumLabel.Named(value)

  case object Empty               extends EnumLabel
  case class Named(value: String) extends EnumLabel
}
