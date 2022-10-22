package org.finos.morphir
package ir
package internal
package value

import zio.Chunk

sealed trait Value[+TA, +VA] {
  def attributes: VA
}
object Value {
  final case class Apply[+TA, +VA](attributes: VA, function: Value[TA, VA], argument: Value[TA, VA])
      extends Value[TA, VA]
  final case class Constructor[+TA, +VA](attributes: VA, name: FQName) extends Value[TA, VA]
  final case class Destructure[+TA, +VA](
      attributes: VA,
      pattern: Pattern[VA],
      valueToDestruct: Value[TA, VA],
      inValue: Value[TA, VA]
  ) extends Value[TA, VA]
  final case class Field[+TA, +VA](attributes: VA, target: Value[TA, VA], name: Name) extends Value[TA, VA]
  final case class FieldFunction[+TA, +VA](attributes: VA, name: Name)                extends Value[TA, VA]
  final case class IfThenElse[+TA, +VA](
      attributes: VA,
      condition: Value[TA, VA],
      thenBranch: Value[TA, VA],
      elseBranch: Value[TA, VA]
  ) extends Value[TA, VA]
  final case class Lambda[+TA, +VA](attributes: VA, argumentPattern: Pattern[VA], body: Value[TA, VA])
      extends Value[TA, VA]
  final case class List[+TA, +VA](attributes: VA, elements: Chunk[Value[TA, VA]]) extends Value[TA, VA]
  final case class PatternMatch[+TA, +VA](
      attributes: VA,
      branchOutOn: Value[TA, VA],
      cases: Chunk[(Pattern[VA], Value[TA, VA])]
  ) extends Value[TA, VA]
  final case class Record[+TA, +VA](attributes: VA, fields: Chunk[(Name, Value[TA, VA])]) extends Value[TA, VA]
  final case class Reference[+VA](attributes: VA, name: FQName)                           extends Value[Nothing, VA]
  final case class Tuple[+TA, +VA](attributes: VA, elements: Chunk[Value[TA, VA]])        extends Value[TA, VA]
  final case class Unit[+VA](attributes: VA)                                              extends Value[Nothing, VA]
  final case class Variable[+VA](attributes: VA, name: Name)                              extends Value[Nothing, VA]

  trait Folder[-Context, -TA, -VA, Z] {

    def recordCase(context: Context, attributes: VA, fields: Chunk[(Name, Z)]): Z
    def referenceCase(context: Context, attributes: VA, name: FQName): Z
    def tupleCase(context: Context, attributes: VA, elements: Chunk[Z]): Z
    def unitCase(context: Context, attributes: VA): Z
    def variableCase(context: Context, value: Value[TA, VA], attributes: VA, name: Name): Z
  }
}
