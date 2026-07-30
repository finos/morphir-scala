package org.finos.morphir.datamodel.classic

sealed trait UnionType
object UnionType {
  case object Enum extends UnionType
  case object Sum  extends UnionType
}
