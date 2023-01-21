package org.finos
package morphir
package ir

import prelude.*
import FQName.FQName
import Literal.Literal
import Name.Name
import Type.Type

object Value extends ValueVersionSpecific {

  final case class Definition[+TA, +VA](inputTypes: Chunk[Parameter[TA, VA]], outputType: Type[TA], body: Value[TA, VA])
  final case class Specification[+TA](inputs: Chunk[SpecParameter[TA]], output: Type[TA]) { self =>
    def map[TB](f: TA => TB): Specification[TB] =
      Specification(inputs.map(_.map(f)), output.map(f))
  }
  sealed trait Value[+TA, +VA]
  case class Unit[+VA](attributes: VA)                 extends Value[Nothing, VA]
  case class Variable[+VA](attributes: VA, name: Name) extends Value[Nothing, VA]

  sealed trait Pattern[+A] {
    def attributes: A
  }
  object Pattern {
    final case class WildcardPattern[+A](attributes: A)                                  extends Pattern[A]
    final case class AsPattern[+A](attributes: A, pattern: Pattern[A], name: Name)       extends Pattern[A]
    final case class TuplePattern[+A](attributes: A, elementPatterns: Chunk[Pattern[A]]) extends Pattern[A]
    final case class ConstructorPattern[+A](attributes: A, constructorName: FQName, argumentPatterns: Chunk[Pattern[A]])
        extends Pattern[A]
    final case class EmptyListPattern[+A](attributes: A) extends Pattern[A]
    final case class HeadTailPattern[+A](attributes: A, headPattern: Pattern[A], tailPattern: Pattern[A])
        extends Pattern[A]
    final case class LiteralPattern[+A](attributes: A, literal: Literal) extends Pattern[A]
    final case class UnitPattern[+A](attributes: A)                      extends Pattern[A]
  }

}
