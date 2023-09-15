package org.finos.morphir.universe.ir

import org.finos.morphir.naming._
import zio.prelude._

final case class FieldK[F[+_], +A](name: Name, data: F[A]) { self =>

  // def forEach[G[+_]: IdentityBoth: Covariant, U](f: A => G[U]): G[Field[U]] =
  //   f(self.data).map(newType => self.copy(data = newType))

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

  implicit def toFieldT[A](field: IField[Type[A]]): FieldT[A] = FieldT(field.name, Id.unwrap(field.data))

  final implicit class FieldTOps[+A](private val self: FieldT[A]) extends AnyVal {

    def fieldType: Type[A] = self.data

    /**
     * Attributes the field with the given `attributes`.
     */
    def attributeTypeAs[Attribs](attributes: => Attribs): FieldT[Attribs] =
      FieldT(self.name, self.data.mapAttributes(_ => attributes))

    /**
     * Attributes the field's type using the given function.
     */
    def attributeTypeWith[B](f: A => B): FieldT[B] =
      FieldT(self.name, self.data.mapAttributes(f))

    def mapAttributes[B](f: A => B): FieldT[B] =
      FieldT(self.name, self.data.mapAttributes(f))
  }
}

final case class Field[+T](name: Name, data: T) { self =>

  def forEach[G[+_]: IdentityBoth: Covariant, U](f: T => G[U]): G[Field[U]] =
    f(self.data).map(newType => self.copy(data = newType))

  def map[U](f: T => U): Field[U] = Field(name, f(data))

}

object Field {

  def apply[T](name: String, data: T): Field[T] = Field(Name.fromString(name), data)

  type Untyped = Field[Unit]
  object Untyped {
    def apply(name: Name): Field[Unit]    = Field(name, ())
    def unapply(field: Field[Unit]): Name = field.name
  }

  final implicit class FieldOfType[A](private val self: Field[Type[A]]) {

    def fieldType: Type[A] = self.data

    /**
     * Attributes the field with the given `attributes`.
     */
    def attributeTypeAs[Attribs](attributes: => Attribs): Field[Type[Attribs]] =
      Field(self.name, self.data.mapAttributes(_ => attributes))

    /**
     * Attributes the field's type using the given function.
     */
    def attributeTypeWith[B](f: A => B): Field[Type[B]] =
      Field(self.name, self.data.mapAttributes(f))

    def mapAttributes[B](f: A => B): Field[Type[B]] =
      Field(self.name, self.data.mapAttributes(f))
  }
}
