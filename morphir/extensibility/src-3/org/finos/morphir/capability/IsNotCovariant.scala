package org.finos.morphir.capability

import scala.util.NotGiven

sealed abstract class IsNotCovariant[-A] extends Serializable
object IsNotCovariant extends IsNotCovariant[Any] {
  implicit def isNotAType[F[+_], A](using NotGiven[Covariant[F]]): IsNotCovariant[F[A]] = IsNotCovariant
}
