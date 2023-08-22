package org.finos.morphir.functional

trait Covariant[F[+_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Covariant {
  def apply[F[+_]](implicit instance: Covariant[F]): Covariant[F] = instance

  implicit val covariantOption: Covariant[Option] = new Covariant[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit val covariantList: Covariant[List] = new Covariant[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit val covariantVector: Covariant[Vector] = new Covariant[Vector] {
    override def map[A, B](fa: Vector[A])(f: A => B): Vector[B] = fa.map(f)
  }

}
