package org.finos.morphir.universe.ir

import org.finos.morphir.naming._
import org.finos.morphir.util.attribs._
import zio.prelude._

final case class Field[+A](name: Name, data: A) { self =>

  def forEach[G[+_]: IdentityBoth: Covariant, B](f: A => G[B]): G[Field[B]] =
    f(self.data).map(newType => self.copy(data = newType))

  def map[B](f: A => B): Field[B] = Field(name, f(data))

}

object Field {

  def apply[A](name: String, data: A): Field[A] = Field(Name.fromString(name), data)

  @inline def define[A](name: String, fieldType: Type[A]): Field[Type[A]] = Field(name, fieldType)
  @inline def define[A](name: Name, fieldType: Type[A]): Field[Type[A]]   = Field(name, fieldType)

  type Untyped = Field[Unit]
  object Untyped {
    def apply(name: Name): Field[Unit]    = Field(name, ())
    def unapply(field: Field[Unit]): Name = field.name
  }

  type Attributed = Field[Type[Attributes]]
  object Attributed {
    def unapply(field: Field[Type[Attributes]]): Some[(Attributes, Name, Type[Attributes])] =
      Some((field.fieldType.attributes, field.name, field.fieldType))
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
