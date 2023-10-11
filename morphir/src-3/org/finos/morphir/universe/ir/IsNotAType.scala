package org.finos.morphir.universe.ir

import scala.util.NotGiven

sealed abstract class IsNotAType[-A] extends Serializable
object IsNotAType extends IsNotAType[Any] {
  implicit def isNotAType[A](using NotGiven[A <:< Type[_]]): IsNotAType[A] = IsNotAType
}
