package org.finos.morphir.datamodel.classic

sealed trait EnumTranslation
object EnumTranslation {
  case object MutiFieldConstructor  extends EnumTranslation
  case object SingleFieldWithRecord extends EnumTranslation
}
