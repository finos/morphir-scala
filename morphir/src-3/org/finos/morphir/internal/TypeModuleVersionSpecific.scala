package org.finos.morphir.internal

import scala.util.NotGiven

trait TypeModuleVersionSpecific { self: TypeModule =>

  sealed abstract class IsNotAType[-A] extends Serializable
  object IsNotAType extends IsNotAType[Any] {
    implicit def isNotAType[A](using NotGiven[A <:< Type[_]]): IsNotAType[A] = IsNotAType
  }
}
