package org.finos.morphir
package ir
package internal
package types

import org.finos.morphir.ir.Name
import org.finos.morphir.ir.internal.types.Type.{FieldT, UType}

trait FieldSyntax {
  final def defineField(name: Name, fieldType: UType): Field[UType] = Field(name, fieldType)

  final def defineField(name: String, fieldType: UType): Field[UType] = Field(Name.fromString(name), fieldType)

  final def field[A](name: String, tpe: Type[A]): FieldT[A] = Field(Name.fromString(name), tpe)
  final def field[A](name: Name, tpe: Type[A]): FieldT[A]   = Field(name, tpe)

  final def field[A](tuple: (String, Type[A])): FieldT[A] = Field(Name.fromString(tuple._1), tuple._2)

  final implicit class FieldOfType[A](private val self: Field[Type[A]]) {

    def fieldType: Type[A] = self.data

    /**
     * Attributes the field with the given `attributes`.
     */
    def attributeTypeAs[Attributes](attributes: => Attributes): Field[Type[Attributes]] =
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
