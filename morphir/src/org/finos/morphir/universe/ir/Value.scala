package org.finos.morphir.universe.ir

import org.finos.morphir.universe.ir.Literal as Lit
import zio.Chunk

sealed trait Value[+TA, +VA] {
  def attributes: VA
}

object Value extends ValueVersionSpecific {
  import Pattern.*

  final case class Definition[+TA, +VA](inputTypes: Chunk[Parameter[TA, VA]], outputType: Type[TA], body: Value[TA, VA])
  final case class Specification[+TA](inputs: Chunk[SpecParameter[TA]], outputs: Type[TA]) { self =>
    def map[TB](f: TA => TB): Specification[TB] =
      Specification(inputs.map(_.map(f)), outputs.map(f))
  }

  final case class Apply[+TA, +VA](attributes: VA, function: Value[TA, VA], argument: Value[TA, VA])
      extends Value[TA, VA]
  final case class Constructor[+VA](attributes: VA, fullyQualifiedName: FQName) extends Value[Nothing, VA]
  final case class Destructure[+TA, +VA](
      attributes: VA,
      pattern: Pattern[VA],
      valueToDestruct: Value[TA, VA],
      inValue: Value[TA, VA]
  ) extends Value[TA, VA]

  final case class Field[+TA, +VA](attributes: VA, subjectValue: Value[TA, VA], fieldName: Name) extends Value[TA, VA]
  final case class FieldFunction[+VA](attributes: VA, name: Name) extends Value[Nothing, VA]

  final case class IfThenElse[+TA, +VA](
      attributes: VA,
      condition: Value[TA, VA],
      thenBranch: Value[TA, VA],
      elseBranch: Value[TA, VA]
  ) extends Value[TA, VA]
  final case class Lambda[+TA, +VA](attributes: VA, argumentPattern: Pattern[VA], body: Value[TA, VA])
      extends Value[TA, VA]
  sealed case class LetDefinition[+TA, +VA](
      attributes: VA,
      valueName: Name,
      valueDefinition: Definition[TA, VA],
      inValue: Value[TA, VA]
  ) extends Value[TA, VA]
  sealed case class LetRecursion[+TA, +VA](
      attributes: VA,
      valueDefinitions: Map[Name, Definition[TA, VA]],
      inValue: Value[TA, VA]
  ) extends Value[TA, VA]
  final case class List[+TA, +VA](attributes: VA, items: Chunk[Value[TA, VA]]) extends Value[TA, VA]
  final case class Literal[+VA](attributes: VA, value: Lit)                    extends Value[Nothing, VA]
  sealed case class PatternMatch[+TA, +VA](
      attributes: VA,
      branchOutOn: Value[TA, VA],
      cases: Chunk[(Pattern[VA], Value[TA, VA])]
  ) extends Value[TA, VA]
  final case class Record[+TA, +VA](attributes: VA, fields: Chunk[(Name, Value[TA, VA])]) extends Value[TA, VA]
  final case class Reference[+VA](attributes: VA, fullyQualifiedName: FQName)             extends Value[Nothing, VA]
  final case class Tuple[+TA, +VA](attributes: VA, elements: Chunk[Value[TA, VA]])        extends Value[TA, VA]
  final case class Unit[+VA](attributes: VA)                                              extends Value[Nothing, VA]
  sealed case class UpdateRecord[+TA, +VA](
      attributes: VA,
      valueToUpdate: Value[TA, VA],
      fieldsToUpdate: Map[Name, Value[TA, VA]]
  ) extends Value[TA, VA]

  final case class Variable[+VA](attributes: VA, name: Name) extends Value[Nothing, VA]

  def asPattern[A](attributes: A, pattern: Pattern[A], name: Name): Pattern[A] =
    AsPattern(attributes = attributes, pattern = pattern, name = name)

  def tuplePattern[A](attributes: A, elementPatterns: Pattern[A]*): Pattern[A] =
    Pattern.TuplePattern(attributes = attributes, elementPatterns = Chunk.fromIterable(elementPatterns))
  def wildcardPattern[A](attributes: A): Pattern[A] = Pattern.WildcardPattern(attributes)

}
