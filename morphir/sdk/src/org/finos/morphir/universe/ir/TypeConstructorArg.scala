package org.finos.morphir.universe.ir

import Type.*
import zio.Chunk

final case class TypeConstructorArg[+A](name: Name, tpe: Type[A]) {
  def map[B](f: A => B): TypeConstructorArg[B] = TypeConstructorArg(name, tpe.map(f))
}

object TypeConstructorArg {
  implicit def toTuple[A](arg: TypeConstructorArg[A]): (Name, Type[A]) = (arg.name, arg.tpe)
}
