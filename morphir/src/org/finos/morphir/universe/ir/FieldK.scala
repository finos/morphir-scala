package org.finos.morphir.universe.ir

import org.finos.morphir.naming._
import zio.prelude._

final case class FieldK[F[+_], +A](name: Name, data: F[A]) { self =>
  @inline def tpe[A0 >: A](implicit ev: F[A] <:< Type[A0]): F[A0] = data
  def map[B](f: A => B)(implicit covariant: Covariant[F]): FieldK[F, B] =
    FieldK(name, data.map(f))

  /// Map the type of the field to get a new field.
  def mapFieldType[A0 >: A, B](f: F[A0] => F[B])(implicit ev: F[A0] <:< Type[A0]): FieldK[F, B] =
    self.copy(data = f(self.tpe))

  /// Map the name of the field to get a new field.
  def transformFieldName(f: Name => Name): FieldK[F, A] = FieldK(f(name), data)

}
object FieldK {
  def apply[F[+_], A](name: String, data: F[A]): FieldK[F, A] = FieldK(Name.fromString(name), data)

  implicit def toField[A](field: IField[Type[A]]): Field[A] = Field(field.name, Id.unwrap(field.data))
}
