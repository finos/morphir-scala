package org.finos.morphir.functional
import zio.prelude._

trait IsNotCovariant[-A] extends Serializable

object IsNotCovariant extends IsNotCovariant[Any] {
  implicit def isNotCovariant[F[+_]]: IsNotCovariant[F[_]] = IsNotCovariant
}

trait IsNotCovariantLowerPriority {
  implicit def isNotCovariantAmbiguousA[F[+_]](implicit ev: Covariant[F]): IsNotCovariant[F[_]] = IsNotCovariant
  implicit def isNotCovariantAmbiguousB[F[+_]](implicit ev: Covariant[F]): IsNotCovariant[F[_]] = IsNotCovariant
}
