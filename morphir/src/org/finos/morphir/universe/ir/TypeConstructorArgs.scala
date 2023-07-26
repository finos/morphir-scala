package org.finos.morphir.universe.ir

import Type.*
import zio.Chunk

final case class TypeConstructorArgs[+A](args: List[TypeConstructorArg[A]]) extends AnyVal { self =>
  def map[B](f: A => B): TypeConstructorArgs[B] = TypeConstructorArgs(self.args.map(_.map(f)))
  def toList: List[TypeConstructorArg[A]]       = args
}

object TypeConstructorArgs {
  implicit def toList[A](args: TypeConstructorArgs[A]): List[TypeConstructorArg[A]] = args.args
}
