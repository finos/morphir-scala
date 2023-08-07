package org.finos.morphir.universe.ir
import org.finos.morphir.naming.*

import Type.*
import zio.Chunk

final case class TypeConstructor[+A](name: Name, args: TypeConstructorArgs[A]) {
  def map[B](f: A => B): TypeConstructor[B] = copy(args = args.map(f))
}
