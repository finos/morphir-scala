package org.finos.morphir.universe.ir

import Type.*
import zio.Chunk

final case class TypeConstructors[+A](byName: Map[Name, TypeConstructor[A]]) {
  def map[B](f: A => B): TypeConstructors[B]       = TypeConstructors(byName.view.mapValues(_.map(f)).toMap)
  @inline def toMap: Map[Name, TypeConstructor[A]] = byName
}

object TypeConstructors {
  implicit def toMap[A](ctors: TypeConstructors[A]): Map[Name, TypeConstructor[A]] = ctors.byName
}
