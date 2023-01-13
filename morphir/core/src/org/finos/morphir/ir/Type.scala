package org.finos
package morphir
package ir

import FQName.FQName
import Name.Name

object Type {
  sealed trait Type[+A] {
    def map[B](f: A => B): Type[B] = ???
  }
  object Type {

    sealed case class ExtensibleRecord[+A](attributes: A, name: Name, fields: List[Field[A]])   extends Type[A]
    sealed case class Function[+A](attributes: A, argumentType: Type[A], returnType: Type[A])   extends Type[A]
    sealed case class Record[+A](attributes: A, fields: List[Field[Type[A]]])                   extends Type[A]
    sealed case class Reference[+A](attributes: A, typeName: FQName, typeParams: List[Type[A]]) extends Type[A]
    sealed case class Tuple[+A](attributes: A, elements: List[Type[A]])                         extends Type[A]
    sealed case class Unit[+A](attributes: A)                                                   extends Type[A]
    sealed case class Variable[+A](attributes: A, name: Name)                                   extends Type[A]
  }

  final case class Field[+A](name: String, tpe: Type[A]) {
    def map[B](f: A => B): Field[B] = Field(name, tpe.map(f))
  }
}
