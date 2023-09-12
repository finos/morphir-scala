package org.finos.morphir.ir

import scala.annotation.implicitAmbiguous

sealed trait IsNotAValue[-A]

object IsNotAValue extends IsNotAValue[Any] with IsNotAValueLowerPriority {

  implicit def isNotAValue[A]: IsNotAValue[A] = IsNotAValue

}

trait IsNotAValueLowerPriority {

  @implicitAmbiguous(
    "This operation assumes that ${A} is not a morphir IR Value node. " +
      "However, ${A} is a Value."
  )
  implicit def isNotAValueAmbiguousA[A](implicit ev: A <:< Value.Value[_, _]): IsNotAValue[A] = IsNotAValue

  implicit def isNotAValueAmbiguousB[A](implicit ev: A <:< Value.Value[_, _]): IsNotAValue[A] = IsNotAValue
}
