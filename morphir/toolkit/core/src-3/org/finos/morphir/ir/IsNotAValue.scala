package org.finos.morphir.ir

import scala.annotation.implicitNotFound
import scala.util.NotGiven

sealed abstract class IsNotAValue[-A] extends Serializable

object IsNotAValue extends IsNotAValue[Any] {
  implicit def isNotAValue[A](using NotGiven[A <:< Value.Value[_, _]]): IsNotAValue[A] = IsNotAValue

}
