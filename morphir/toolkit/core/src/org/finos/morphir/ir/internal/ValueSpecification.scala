package org.finos.morphir
package ir
package internal

import zio.{Chunk, NonEmptyChunk}
import Type.{tuple => tupleType, unit => unitType, Type, UType}
import Literal.Lit
import scala.annotation.tailrec

private[ir] case class ValueSpecification[+TA](inputs: Chunk[(Name, Type[TA])], output: Type[TA]) { self =>
  def map[B](f: TA => B): ValueSpecification[B] =
    ValueSpecification(
      inputs = inputs.map { case (name, tpe) => (name, Type.mapTypeAttributes(tpe)(f)) },
      output = Type.mapTypeAttributes(output)(f)
    )
}

private[ir] object ValueSpecification {
  def create[Attributes](inputs: (Name, Type[Attributes])*): CreatePartiallyApplied[Attributes] =
    new CreatePartiallyApplied(() => Chunk.fromIterable(inputs))

  type Raw = ValueSpecification[scala.Unit]
  object Raw {
    def apply(inputs: (String, UType)*)(output: UType): Raw =
      ValueSpecification(
        inputs = Chunk.fromIterable(inputs.map { case (n, t) => Name.fromString(n) -> t }),
        output = output
      )
  }

  final class CreatePartiallyApplied[A](private val inputs: () => Chunk[(Name, Type[A])]) extends AnyVal {
    def apply(output: Type[A]): ValueSpecification[A] =
      ValueSpecification(inputs(), output)
  }
}
