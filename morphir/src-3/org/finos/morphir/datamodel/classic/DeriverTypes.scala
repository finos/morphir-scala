package org.finos.morphir.datamodel.classic

object DeriverTypes {
  type IsProduct[P <: scala.Product] = P
  type IsOption[P <: Option[_]]      = P
}
