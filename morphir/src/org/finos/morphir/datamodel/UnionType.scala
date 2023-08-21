package org.finos.morphir.datamodel

sealed trait UnionType
object UnionType {
  case object Enum extends UnionType
  case object Sum  extends UnionType
}
