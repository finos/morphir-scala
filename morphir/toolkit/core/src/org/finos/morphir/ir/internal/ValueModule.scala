package org.finos.morphir
package ir
package internal

import zio.Chunk

trait ValueModule extends MorphirValueModule { module =>

  sealed trait Value[+TA, +VA] {
    def attributes: VA
    def collectReferences: Set[FQName] = ???
    def toRawValue: RawValue           = ???
  }
  object Value {

    /**
     * Represents a function application. The two arguments are the target function and the argument to apply.
     * Multi-argument invocations are expressed by wrapping multiple `Apply` nodes in each other (currying).
     */
    sealed case class Apply[+TA, +VA](attributes: VA, function: Value[TA, VA], argument: Value[TA, VA])
        extends Value[TA, VA]

    object Apply {
      type Raw = Apply[scala.Unit, scala.Unit]
      object Raw {
        def apply(function: RawValue, argument: RawValue): Raw =
          new Apply[scala.Unit, scala.Unit]((), function, argument)
        def unapply[TA, VA](value: Value[TA, VA]): Option[(RawValue, RawValue)] = value match {
          case apply: Apply[TA, VA] => Some((apply.function.toRawValue, apply.argument.toRawValue))
          case _                    => None
        }
      }
      type Typed = Apply[scala.Unit, UType]
    }

    sealed case class Constructor[+TA, +VA](attributes: VA, name: FQName) extends Value[TA, VA]
    object Constructor {
      type Raw = Constructor[scala.Unit, scala.Unit]
      object Raw {
        def apply(name: FQName): Raw = new Constructor[scala.Unit, scala.Unit]((), name)
        def unapply[TA, VA](value: Value[TA, VA]): Option[FQName] = value match {
          case constructor: Constructor[TA, VA] => Some(constructor.name)
          case _                                => None
        }
      }
      type Typed = Constructor[scala.Unit, UType]
    }
    sealed case class Destructure[+TA, +VA](
        attributes: VA,
        pattern: Pattern[VA],
        valueToDestruct: Value[TA, VA],
        inValue: Value[TA, VA]
    ) extends Value[TA, VA]

    object Destructure {
      type Raw   = Destructure[scala.Unit, scala.Unit]
      type Typed = Destructure[scala.Unit, UType]
    }

    sealed case class Field[+TA, +VA](attributes: VA, subjectValue: Value[TA, VA], fieldName: Name)
        extends Value[TA, VA]
    object Field {
      type Raw   = Field[scala.Unit, scala.Unit]
      type Typed = Field[scala.Unit, UType]
    }

    sealed case class FieldFunction[+TA, +VA](attributes: VA, name: Name) extends Value[TA, VA]
    object FieldFunction {
      type Raw   = FieldFunction[scala.Unit, scala.Unit]
      type Typed = FieldFunction[scala.Unit, UType]
    }
    sealed case class IfThenElse[+TA, +VA](
        attributes: VA,
        condition: Value[TA, VA],
        thenBranch: Value[TA, VA],
        elseBranch: Value[TA, VA]
    ) extends Value[TA, VA]

    object IfThenElse {
      type Raw   = IfThenElse[scala.Unit, scala.Unit]
      type Typed = IfThenElse[scala.Unit, UType]
    }

    sealed case class Lambda[+TA, +VA](attributes: VA, argumentPattern: Pattern[VA], body: Value[TA, VA])
        extends Value[TA, VA]
    object Lambda {
      type Raw   = Lambda[scala.Unit, scala.Unit]
      type Typed = Lambda[scala.Unit, UType]
    }

    sealed case class LetDefinition[+TA, +VA](
        attributes: VA,
        valueName: Name,
        valueDefinition: Definition[TA, VA],
        inValue: Value[TA, VA]
    ) extends Value[TA, VA]

    object LetDefinition {
      type Raw   = LetDefinition[scala.Unit, scala.Unit]
      type Typed = LetDefinition[scala.Unit, UType]
    }

    sealed case class LetRecursion[+TA, +VA](
        attributes: VA,
        valueDefinitions: Map[Name, Definition[TA, VA]],
        inValue: Value[TA, VA]
    ) extends Value[TA, VA]

    object LetRecursion {
      type Raw   = LetRecursion[scala.Unit, scala.Unit]
      type Typed = LetRecursion[scala.Unit, UType]
    }

    sealed case class List[+TA, +VA](attributes: VA, elements: Chunk[Value[TA, VA]]) extends Value[TA, VA]
    object List {
      type Raw   = List[scala.Unit, scala.Unit]
      type Typed = List[scala.Unit, UType]
    }

    sealed case class Literal[+TA, +VA](attributes: VA, literal: Lit) extends Value[TA, VA]
    object Literal {
      type Raw   = Literal[scala.Unit, scala.Unit]
      type Typed = Literal[scala.Unit, UType]
    }

    sealed case class PatternMatch[+TA, +VA](
        attributes: VA,
        branchOutOn: Value[TA, VA],
        cases: Chunk[(Pattern[VA], Value[TA, VA])]
    ) extends Value[TA, VA]
    object PatternMatch {
      type Raw   = PatternMatch[scala.Unit, scala.Unit]
      type Typed = PatternMatch[scala.Unit, UType]
    }

    sealed case class Record[+TA, +VA](attributes: VA, fields: Chunk[(Name, Value[TA, VA])]) extends Value[TA, VA]
    object Record {
      type Raw   = Record[scala.Unit, scala.Unit]
      type Typed = Record[scala.Unit, UType]
    }

    sealed case class Reference[+VA](attributes: VA, name: FQName) extends Value[Nothing, VA]
    object Reference {
      type Raw = Reference[scala.Unit]
      object Raw {
        def apply(name: FQName): Raw = new Reference[scala.Unit]((), name)
        def unapply[VA](value: Value[_, VA]): Option[FQName] = value match {
          case reference: Reference[VA] => Some(reference.name)
          case _                        => None
        }
      }
      type Typed = Reference[UType]
    }

    sealed case class Tuple[+TA, +VA](attributes: VA, elements: Chunk[Value[TA, VA]]) extends Value[TA, VA]
    object Tuple {
      type Raw   = Tuple[scala.Unit, scala.Unit]
      type Typed = Tuple[scala.Unit, UType]
    }

    sealed case class Unit[+VA](attributes: VA) extends Value[Nothing, VA]
    object Unit {
      type Raw   = Unit[scala.Unit]
      type Typed = Unit[UType]
    }

    sealed case class UpdateRecord[+TA, +VA](
        attributes: VA,
        valueToUpdate: Value[TA, VA],
        fieldsToUpdate: Map[Name, Value[TA, VA]]
    ) extends Value[TA, VA]
    object UpdateRecord {
      type Raw   = UpdateRecord[scala.Unit, scala.Unit]
      type Typed = UpdateRecord[scala.Unit, UType]
    }

    sealed case class Variable[+VA](attributes: VA, name: Name) extends Value[Nothing, VA]
    object Variable {
      type Raw   = Variable[scala.Unit]
      type Typed = Variable[UType]
    }

    trait Folder[-Context, -TA, -VA, Z] {

      def applyCase(context: Context, value: Value[TA, VA], attributes: VA, function: Z, argument: Z): Z
      def constructorCase(context: Context, value: Value[TA, VA], attributes: VA, name: FQName): Z
      def destructureCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          pattern: Pattern[VA],
          valueToDestruct: Z,
          inValue: Z
      ): Z
      def fieldCase(context: Context, value: Value[TA, VA], attributes: VA, subjectValue: Z, fieldName: Name): Z
      def fieldFunctionCase(context: Context, value: Value[TA, VA], attributes: VA, fieldName: Name): Z
      def ifThenElseCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          condition: Z,
          thenBranch: Z,
          elseBranch: Z
      ): Z
      def lambdaCase(context: Context, value: Value[TA, VA], attributes: VA, argumentPattern: Pattern[VA], body: Z): Z
      def letDefCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          valueName: Name,
          valueDefinition: Definition[TA, VA],
          inValue: Z
      ): Z
      def letDestruvt(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          valueDefinitions: Map[Name, Definition[TA, VA]],
          inValue: Z
      ): Z
      def letRecCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          valueDefinitions: Map[Name, Definition[TA, VA]],
          inValue: Z
      ): Z
      def listCase(context: Context, value: Value[TA, VA], attributes: VA, elements: Chunk[Z]): Z
      def literalCase(context: Context, value: Value[TA, VA], attributes: VA, literal: Lit): Z
      def patternMatchCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          branchOutOn: Z,
          cases: Chunk[(Pattern[VA], Z)]
      ): Z
      def recordCase(context: Context, value: Value[TA, VA], attributes: VA, fields: Chunk[(Name, Z)]): Z
      def referenceCase(context: Context, value: Value[TA, VA], attributes: VA, name: FQName): Z
      def tupleCase(context: Context, value: Value[TA, VA], attributes: VA, elements: Chunk[Z]): Z
      def unitCase(context: Context, value: Value[TA, VA], attributes: VA): Z
      def updateRecordCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          valueToUpdate: Z,
          fieldsToUpdate: Map[Name, Z]
      ): Z
      def variableCase(context: Context, value: Value[TA, VA], attributes: VA, name: Name): Z
    }
  }

  def reference[VA](attributes: VA, fullyQualifiedName: FQName)(implicit ev: NeedsAttributes[VA]): Value[Nothing, VA] =
    Value.Reference(attributes, fullyQualifiedName)

  def reference(fullyQualifiedName: FQName): Value[Nothing, Unit] = Value.Reference.Raw(fullyQualifiedName)

}
