package org.finos.morphir.universe.ir

import scala.annotation.implicitAmbiguous

sealed trait IsNotAType[-A] extends Serializable

object IsNotAType extends IsNotAType[Any] with IsNotATypeLowerPriority {

  implicit def isNotAType[A]: IsNotAType[A] = IsNotAType

}

trait IsNotATypeLowerPriority {

  @implicitAmbiguous(
    "This operation assumes that ${A} is not a Morphir IR Type. " +
      "However, ${A} is a Type."
  )
  implicit def isNotATypeAmbiguousA[A](implicit ev: A <:< Type[_]): IsNotAType[A] = IsNotAType

  implicit def isNotATypeAmbiguousB[A](implicit ev: A <:< Type[_]): IsNotAType[A] = IsNotAType
}
