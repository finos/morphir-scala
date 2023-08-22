package org.finos.morphir

import scala.annotation.implicitNotFound
import scala.util.NotGiven

/// Evidence type `A` is not equal to type `B`.
@implicitNotFound("${A} must not be equal to ${B}")
abstract class =!=[A, B] extends Serializable
object =!= {
  implicit def neq[A, B](implicit ev: NotGiven[A =!= B]): A =!= B = new =!=[A, B] {}
}
