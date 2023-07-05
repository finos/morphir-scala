package org.finos.morphir.datamodel

object DeriverTypes {
  type IsProduct[P <: scala.Product] = P
  type IsOption[P <: Option[_]]      = P
}
