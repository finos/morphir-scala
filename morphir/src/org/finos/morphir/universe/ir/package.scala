package org.finos.morphir.universe

import org.finos.morphir.naming._
import org.finos.morphir.mir
import org.finos.morphir.util.attribs._
import zio.=!=
import zio.prelude._

import scala.annotation.nowarn

package object ir {

  type AccessControlled[+A] = mir.AccessControlled[A]
  val AccessControlled: mir.AccessControlled.type = mir.AccessControlled

  // type Field[+A] = FieldK[Type, A]
  // object Field {
  //   def apply[A](name: String, tpe: Type[A]): Field[A] = FieldK(Name.fromString(name), tpe)
  //   def apply[A](name: Name, tpe: Type[A]): Field[A]   = FieldK(name, tpe)

  //   @inline def define[A](name: String, fieldType: Type[A]): Field[A] = Field(name, fieldType)
  //   @inline def define[A](name: Name, fieldType: Type[A]): Field[A]   = Field(name, fieldType)

  //   type Attributed = Field[Attributes]
  //   object Attributed {
  //     def unapply(field: Field[Attributes]): Some[(Attributes, Name, Type[Attributes])] =
  //       Some((field.tpe.attributes, field.name, field.tpe))
  //   }

  //   type Untyped = Field[Unit]

  //   object Untyped {
  //     // def apply(name: Name): Field[Unit] = Field(name, ())
  //     def unapply(field: Field[Unit]): Name = field.name
  //   }
  // }

  // type IField[+A] = FieldK[Id, A]
  // object IField {
  //   def apply[A](name: String, data: A): IField[A] = FieldK(Name.fromString(name), Id[A](data))
  //   def apply[A](name: Name, data: A): IField[A]   = FieldK(name, Id[A](data))
  // }

  type FieldK[+F[+_], +A] = Field[F[A]]
  object FieldK {
    def apply[F[+_], A](name: String, data: F[A]): FieldK[F, A] = Field(Name.fromString(name), data)
    def apply[F[+_], A](name: Name, data: F[A]): FieldK[F, A]   = Field(name, data)
  }

  type FieldT[+A] = FieldK[Type, A]
  object FieldT {
    def apply[A](name: String, tpe: Type[A]): FieldT[A] = FieldK(Name.fromString(name), tpe)
    def apply[A](name: Name, tpe: Type[A]): FieldT[A]   = FieldK(name, tpe)
  }

  type UType = RawType
  val UType: RawType.type = RawType

  type RawType = RawType.Type
  object RawType extends Subtype[Type[scala.Unit]]

  type RawTypeInfo = RawTypeInfo.Type
  object RawTypeInfo extends Subtype[TypeInfo[scala.Unit]] {
    def apply[A](typeInfo: TypeInfo[A])(implicit @nowarn ev: A =!= scala.Unit): RawTypeInfo =
      wrap(typeInfo.map(_ => ()))

  }
}
