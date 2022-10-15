package org.finos.morphir
package ir
package internal
package types

import org.finos.morphir.ir.Name
import org.finos.morphir.ir.types.Field
import org.finos.morphir.ir.internal.types.Type.{FieldT, UType}

trait FieldSyntax {
  final def defineField(name: Name, fieldType: UType): Field[UType] = Field(name, fieldType)

  final def defineField(name: String, fieldType: UType): Field[UType] = Field(Name.fromString(name), fieldType)

  final def field[A](name: String, tpe: Type[A]): FieldT[A] = Field(Name.fromString(name), tpe)
  final def field[A](name: Name, tpe: Type[A]): FieldT[A]   = Field(name, tpe)

  final def field[A](tuple: (String, Type[A])): FieldT[A] = Field(Name.fromString(tuple._1), tuple._2)

}
