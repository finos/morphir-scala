package org.finos.morphir
package ir

import org.finos.morphir.naming._
import zio.prelude._

final case class Field[+T](name: Name, data: T) { self =>

  def forEach[G[+_]: IdentityBoth: Covariant, U](f: T => G[U]): G[Field[U]] =
    f(self.data).map(newType => self.copy(data = newType))

  def map[U](f: T => U): Field[U] = Field(name, f(data))

}

object Field {
  import Type.Type

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
