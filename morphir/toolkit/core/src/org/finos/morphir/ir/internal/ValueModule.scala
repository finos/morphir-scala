package org.finos.morphir
package ir
package internal

import org.finos.morphir.ir.sdk.List.listType
import zio.{Chunk, NonEmptyChunk}
import Type.{Type, UType}

trait ValueModule extends PatternModule with LiteralModule { module =>
  import ValueAdt.{Literal => LiteralValue, _}
  final type RawValue       = Value[scala.Unit, scala.Unit]
  final type TypedValue     = Value[scala.Unit, UType]
  final type USpecification = Specification[scala.Unit]

  sealed trait Value[+TA, +VA] {
    def attributes: VA
    def collectReferences: Set[FQName]                                 = ???
    def collectVariables: Set[Name]                                    = ???
    def mapAttributes[TB, VB](f: TA => TB, g: VA => VB): Value[TB, VB] = ???
    def toRawValue: RawValue                                           = ???
  }

  object Value {

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
          valueDefinition: (Chunk[(Name, VA, Type[TA])], Type[TA], Z),
          inValue: Z
      ): Z
      def letDestruvt(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          valueDefinitions: Map[Name, (Chunk[(Name, VA, Type[TA])], Type[TA], Z)],
          inValue: Z
      ): Z
      def letRecCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          valueDefinitions: Map[Name, (Chunk[(Name, VA, Type[TA])], Type[TA], Z)],
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

  private[ir] object ValueAdt {

    /**
     * Represents a function application. The two arguments are the target function and the argument to apply.
     * Multi-argument invocations are expressed by wrapping multiple `Apply` nodes in each other (currying).
     */
    sealed case class Apply[+TA, +VA](attributes: VA, function: Value[TA, VA], argument: Value[TA, VA])
        extends Value[TA, VA]

    object Apply {
      def apply[TA, VA](
          attributes: VA,
          function: Value[TA, VA],
          argument: Value[TA, VA],
          arguments: Value[TA, VA]*
      ): Apply[TA, VA] =
        arguments.foldLeft(Apply(attributes, function, argument)) { case (acc, arg) => Apply(acc.attributes, acc, arg) }

      type Raw = Apply[scala.Unit, scala.Unit]
      object Raw {

        def apply(function: RawValue, argument: RawValue, arguments: RawValue*): Raw =
          arguments.foldLeft(Apply(function.attributes, function, argument)) { case (acc, arg) =>
            Apply(acc.attributes, acc, arg)
          }

        def unapply(value: RawValue): Option[(RawValue, RawValue)] = value match {
          case Apply(attributes, function, argument) => Some((function, argument))
          case _                                     => None
        }
      }

      type Typed = Apply[scala.Unit, UType]
      object Typed {

        def apply(tpe: UType, function: TypedValue, argument: TypedValue, arguments: TypedValue*): Typed =
          arguments.foldLeft(Apply(tpe, function, argument)) { case (acc, arg) =>
            Apply(acc.attributes, acc, arg)
          }

        def apply(function: TypedValue, argument: TypedValue, arguments: TypedValue*): Typed =
          Typed(function.attributes, function, argument, arguments: _*)

        def unapply(value: TypedValue): Option[(UType, TypedValue, TypedValue)] = value match {
          case Apply(attributes, function, argument) => Some((attributes, function, argument))
          case _                                     => None
        }
      }
    }

    sealed case class Constructor[+VA](attributes: VA, name: FQName) extends Value[Nothing, VA]
    object Constructor {
      def apply[A](attributes: A, name: String): Constructor[A] =
        Constructor(attributes, FQName.fromString(name))

      type Raw = Constructor[scala.Unit]
      object Raw {
        @inline def apply(name: String): Raw = Constructor((), name)
        @inline def apply(name: FQName): Raw = Constructor((), name)
        def unapply(value: Value[Nothing, Any]): Option[FQName] = value match {
          case Constructor(_, name) => Some(name)
          case _                    => None
        }
      }

      type Typed = Constructor[UType]
      object Typed {
        def apply(name: FQName, ascribedType: UType): Typed = Constructor(ascribedType, name)
        def apply(fqName: String, ascribedType: UType): Typed =
          Constructor(ascribedType, FQName.fromString(fqName))
        def apply(ascribedType: UType, name: FQName): Typed = Constructor(ascribedType, name)
        def apply(ascribedType: UType, fqName: String): Typed =
          Constructor(ascribedType, FQName.fromString(fqName))

        def unapply(value: TypedValue): Option[(UType, FQName)] = value match {
          case Constructor(attributes, name) => Some((attributes, name))
          case _                             => None
        }
      }
    }
    sealed case class Destructure[+TA, +VA](
        attributes: VA,
        pattern: Pattern[VA],
        valueToDestruct: Value[TA, VA],
        inValue: Value[TA, VA]
    ) extends Value[TA, VA]

    object Destructure {
      type Raw = Destructure[scala.Unit, scala.Unit]
      object Raw {
        def apply(pattern: UPattern, valueToDestruct: RawValue, inValue: RawValue): Raw =
          Destructure((), pattern, valueToDestruct, inValue)

        def unapply(value: RawValue): Option[(UPattern, RawValue, RawValue)] =
          value match {
            case Destructure(_, pattern, valueToDestruct, inValue) => Some((pattern, valueToDestruct, inValue))
            case _                                                 => None
          }
      }

      type Typed = Destructure[scala.Unit, UType]
      object Typed {
        def apply(tpe: UType, pattern: Pattern[UType], valueToDestruct: TypedValue, inValue: TypedValue): Typed =
          Destructure(tpe, pattern, valueToDestruct, inValue)

        def apply(pattern: Pattern[UType], valueToDestruct: TypedValue, inValue: TypedValue): Typed =
          Destructure(inValue.attributes, pattern, valueToDestruct, inValue)

        def unapply(value: TypedValue): Option[(UType, Pattern[UType], TypedValue, TypedValue)] =
          value match {
            case Destructure(attributes, pattern, valueToDestruct, inValue) =>
              Some((attributes, pattern, valueToDestruct, inValue))
            case _ => None
          }
      }
    }

    sealed case class Field[+TA, +VA](attributes: VA, subjectValue: Value[TA, VA], fieldName: Name)
        extends Value[TA, VA]
    object Field {
      def apply[TA, VA](attributes: VA, subjectValue: Value[TA, VA], fieldName: String): Field[TA, VA] =
        Field(attributes, subjectValue, Name.fromString(fieldName))

      def unapply[TA, VA](value: Value[TA, VA]): Option[(VA, Value[TA, VA], Name)] = value match {
        case Field(attributes, target, name) => Some((attributes, target, name))
        case _                               => None
      }

      type Raw = Field[scala.Unit, scala.Unit]
      object Raw {
        def apply(target: RawValue, name: Name): Raw =
          Field(target.attributes, target, name)

        def apply(target: RawValue, name: String): Raw =
          Field(target.attributes, target, Name.fromString(name))

        def unapply(value: RawValue): Option[(RawValue, Name)] = value match {
          case Field(attributes, target, name) => Some((target, name))
          case _                               => None
        }
      }

      type Typed = Field[scala.Unit, UType]
      object Typed {
        def apply(tpe: UType, subjectValue: TypedValue, name: Name): Typed =
          Field(tpe, subjectValue, name)

        def apply(tpe: UType, subjectValue: TypedValue, name: String): Typed =
          Field(tpe, subjectValue, Name.fromString(name))

        def unapply(value: TypedValue): Option[(UType, TypedValue, Name)] = value match {
          case Field(attributes, target, name) => Some((attributes, target, name))
          case _                               => None
        }
      }
    }

    sealed case class FieldFunction[+VA](attributes: VA, name: Name) extends Value[Nothing, VA]
    object FieldFunction {
      def apply[VA](attributes: VA, name: String): FieldFunction[VA] = FieldFunction(attributes, Name.fromString(name))

      type Raw = FieldFunction[scala.Unit]
      object Raw {
        @inline def apply(name: String): Raw = FieldFunction((), name)
        @inline def apply(name: Name): Raw   = FieldFunction((), name)

        def unapply(value: RawValue): Option[Name] = value match {
          case FieldFunction(_, name) => Some(name)
          case _                      => None
        }
      }

      type Typed = FieldFunction[UType]
      object Typed {
        def apply(tpe: UType, name: String): Typed = FieldFunction(tpe, name)
        def apply(tpe: UType, name: Name): Typed   = FieldFunction(tpe, name)

        def unapply(value: TypedValue): Option[(UType, Name)] = value match {
          case FieldFunction(attributes, name) => Some((attributes, name))
          case _                               => None
        }
      }
    }
    sealed case class IfThenElse[+TA, +VA](
        attributes: VA,
        condition: Value[TA, VA],
        thenBranch: Value[TA, VA],
        elseBranch: Value[TA, VA]
    ) extends Value[TA, VA]

    object IfThenElse {

      type Raw = IfThenElse[scala.Unit, scala.Unit]
      object Raw {
        def apply(condition: RawValue, thenValue: RawValue, elseValue: RawValue): Raw =
          IfThenElse((), condition, thenValue, elseValue)

        def unapply(value: RawValue): Option[(RawValue, RawValue, RawValue)] =
          value match {
            case IfThenElse(_, condition, thenValue, elseValue) => Some((condition, thenValue, elseValue))
            case _                                              => None
          }
      }

      type Typed = IfThenElse[scala.Unit, UType]
      object Typed {
        def apply(
            tpe: UType,
            condition: TypedValue,
            thenBranch: TypedValue,
            elseBranch: TypedValue
        ): Typed =
          IfThenElse(tpe, condition, thenBranch, elseBranch)

        def apply(condition: TypedValue, thenBranch: TypedValue, elseBranch: TypedValue): Typed =
          IfThenElse(thenBranch.attributes, condition, thenBranch, elseBranch)

        def unapply(value: TypedValue): Option[(UType, TypedValue, TypedValue, TypedValue)] =
          value match {
            case IfThenElse(attributes, condition, thenValue, elseValue) =>
              Some((attributes, condition, thenValue, elseValue))
            case _ => None
          }
      }
    }

    sealed case class Lambda[+TA, +VA](attributes: VA, argumentPattern: Pattern[VA], body: Value[TA, VA])
        extends Value[TA, VA]
    object Lambda {

      type Raw = Lambda[scala.Unit, scala.Unit]
      object Raw {
        def apply(argumentPattern: UPattern, body: RawValue): Raw =
          Lambda(body.attributes, argumentPattern, body)

        def unapply(value: RawValue): Option[(UPattern, RawValue)] = value match {
          case Lambda(attributes, argumentPattern, body) => Some((argumentPattern, body))
          case _                                         => None
        }
      }

      type Typed = Lambda[scala.Unit, UType]
      object Typed {
        def apply(
            tpe: UType,
            argumentPattern: Pattern[UType],
            body: TypedValue
        ): Typed =
          Lambda(tpe, argumentPattern, body)

        def unapply(value: TypedValue): Option[(UType, Pattern[Any], TypedValue)] =
          value match {
            case Lambda(attributes, argumentPattern, body) => Some((attributes, argumentPattern, body))
            case _                                         => None
          }
      }
    }

    sealed case class LetDefinition[+TA, +VA](
        attributes: VA,
        valueName: Name,
        valueDefinition: Definition[TA, VA],
        inValue: Value[TA, VA]
    ) extends Value[TA, VA]

    object LetDefinition {
      def apply[TA, VA](
          attributes: VA,
          name: String,
          valueDefinition: Definition[TA, VA],
          inValue: Value[TA, VA]
      ): LetDefinition[TA, VA] =
        LetDefinition(attributes, Name.fromString(name), valueDefinition, inValue)

      type Raw = LetDefinition[scala.Unit, scala.Unit]
      object Raw {
        def apply(name: Name, valueDefinition: Definition.Raw, inValue: RawValue): Raw =
          LetDefinition(inValue.attributes, name, valueDefinition, inValue)

        def apply(name: String, valueDefinition: Definition.Raw, inValue: RawValue): Raw =
          LetDefinition(inValue.attributes, Name.fromString(name), valueDefinition, inValue)
      }
      type Typed = LetDefinition[scala.Unit, UType]
      object Typed {
        def apply(
            tpe: UType,
            name: Name,
            valueDefinition: Definition.Typed,
            inValue: TypedValue
        ): TypedValue =
          LetDefinition(tpe, name, valueDefinition, inValue)

        def apply(tpe: UType, name: String, valueDefinition: Definition.Typed, inValue: TypedValue): Typed =
          LetDefinition(tpe, Name.fromString(name), valueDefinition, inValue)

        def apply(
            name: Name,
            valueDefinition: Definition.Typed,
            inValue: TypedValue
        ): TypedValue =
          LetDefinition(inValue.attributes, name, valueDefinition, inValue)

        def apply(name: String, valueDefinition: Definition.Typed, inValue: TypedValue): Typed =
          LetDefinition(inValue.attributes, Name.fromString(name), valueDefinition, inValue)

      }
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
      def apply[TA, VA](attributes: VA, elements: Value[TA, VA]*): Value[TA, VA] =
        List(attributes, Chunk.fromIterable(elements))

      type Raw = List[scala.Unit, scala.Unit]
      object Raw {
        def apply(elements: Chunk[RawValue]): Raw = List((), elements)
        def apply(elements: RawValue*): Raw       = List((), Chunk.fromIterable(elements))

        def unapply(value: RawValue): Option[Chunk[RawValue]] = value match {
          case List(_, elements) => Some(elements)
          case _                 => None
        }
      }

      type Typed = List[scala.Unit, UType]
      object Typed {
        def apply(tpe: UType, elements: Chunk[TypedValue]): Typed = List(tpe, elements)

        def apply(elements: NonEmptyChunk[TypedValue]): Typed = {
          val tpe = ir.sdk.List.listType(elements.head.attributes)
          List(tpe, elements)
        }

        def apply(tpe: UType, elements: TypedValue*): Typed =
          List(tpe, Chunk.fromIterable(elements))

        def apply(head: TypedValue, tail: TypedValue*): Typed =
          List(head.attributes, Chunk.fromIterable(head +: tail))

        def unapply(value: TypedValue): Option[(UType, Chunk[TypedValue])] = value match {
          case List(tpe, elements) => Some((tpe, elements))
          case _                   => None
        }
      }
    }

    sealed case class Literal[+VA](attributes: VA, literal: Lit) extends Value[Nothing, VA]
    object Literal {
      type Raw = Literal[scala.Unit]
      object Raw {
        def apply[A](literal: Lit): Raw = Literal((), literal)

        def unapply(value: RawValue): Option[Lit] = value match {
          case Literal(_, literal) => Some(literal)
          case _                   => None
        }
      }

      type Typed = Literal[UType]
      object Typed {
        // def apply[A](value: Lit[A]): TypedValue = Literal(value.inferredType, value)
        def apply(tpe: UType, value: Lit): Typed = Literal(tpe, value)

        def unapply(value: TypedValue): Option[Lit] = value match {
          case Literal(_, literal) => Some(literal)
          case _                   => None
        }
      }
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
      def apply[TA, VA](attributes: VA, fields: (String, Value[TA, VA])*): Record[TA, VA] =
        Record(attributes, Chunk.fromIterable(fields.map { case (name, value) => (Name.fromString(name), value) }))

      def apply[TA, VA](
          attributes: VA,
          firstField: (Name, Value[TA, VA]),
          otherFields: (Name, Value[TA, VA])*
      ): Record[TA, VA] =
        Record(attributes, firstField +: Chunk.fromIterable(otherFields))

      def apply[TA, VA](attributes: VA, fields: Map[String, Value[TA, VA]]): Record[TA, VA] = {
        val fieldsChunk = Chunk.fromIterable(fields.map { case (name, value) => (Name.fromString(name), value) })
        Record(attributes, fieldsChunk)
      }

      def fromMap[TA, VA](attributes: VA, fields: Map[Name, Value[TA, VA]]): Record[TA, VA] =
        Record(attributes, Chunk.fromIterable(fields.map { case (name, value) => (name, value) }))
      type Raw = Record[scala.Unit, scala.Unit]
      object Raw {
        def apply(fields: Chunk[(Name, RawValue)]): Raw = Record((), fields)
        def apply(firstField: (Name, RawValue), otherFields: (Name, RawValue)*): Raw =
          Record((), firstField = firstField, otherFields = otherFields: _*)

        def apply(fields: (String, RawValue)*): Raw = Record((), fields: _*)

        def apply(fields: Map[String, RawValue]): Raw = Record((), fields)

        def unapply(value: RawValue): Option[Chunk[(Name, RawValue)]] = value match {
          case Record(_, fields) => Some(fields)
          case _                 => None
        }
      }

      type Typed = Record[scala.Unit, UType]
    }

    sealed case class Reference[+VA](attributes: VA, name: FQName) extends Value[Nothing, VA]
    object Reference {

      def apply[VA](attributes: VA, name: String): Reference[VA] =
        Reference(attributes, FQName.fromString(name))

      def apply[VA](attributes: VA, packageName: String, moduleName: String, localName: String): Reference[VA] =
        Reference(attributes, FQName.fqn(packageName, moduleName, localName))

      type Raw = Reference[scala.Unit]
      object Raw {
        @inline def apply(name: String): Raw = Reference((), name)
        @inline def apply(name: FQName): Raw = new Reference[scala.Unit]((), name)
        @inline def apply(packageName: String, moduleName: String, localName: String): RawValue =
          Reference((), FQName.fqn(packageName, moduleName, localName))
        def unapply[VA](value: Value[_, VA]): Option[FQName] = value match {
          case reference: Reference[VA] => Some(reference.name)
          case _                        => None
        }
      }
      type Typed = Reference[UType]
      object Typed {
        @inline def apply(tpe: UType, name: String): TypedValue = Reference(tpe, name)
        @inline def apply(tpe: UType, name: FQName): TypedValue = Reference(tpe, name)
        @inline def apply(tpe: UType, packageName: String, moduleName: String, localName: String): TypedValue =
          Reference(tpe, FQName.fqn(packageName, moduleName, localName))

        def unapply(value: TypedValue): Option[(UType, FQName)] = value match {
          case Reference(tpe, name) => Some((tpe, name))
          case _                    => None
        }
      }
    }

    sealed case class Tuple[+TA, +VA](attributes: VA, elements: Chunk[Value[TA, VA]]) extends Value[TA, VA]
    object Tuple {
      def apply[VA](attributes: VA): Tuple[Nothing, VA] = Tuple(attributes, Chunk.empty)

      def apply[TA, VA](attributes: VA, element: Value[TA, VA], otherElements: Value[TA, VA]*): Tuple[TA, VA] =
        apply(attributes, element +: Chunk.fromIterable(otherElements))

      type Raw = Tuple[scala.Unit, scala.Unit]
      object Raw {
        def apply(elements: Chunk[RawValue]): Raw = Tuple((), elements)
        def apply(elements: RawValue*): Raw       = Tuple((), Chunk.fromIterable(elements))

        def unapply(value: RawValue): Option[Chunk[RawValue]] = value match {
          case Tuple(_, elements) => Some(elements)
          case _                  => None
        }
      }
      type Typed = Tuple[scala.Unit, UType]
      object Typed {
        def apply(elements: Chunk[TypedValue]): Typed = {
          val tupleType = ir.Type.tuple(elements.map(_.attributes))
          Tuple(tupleType, elements)
        }

        def apply(elements: TypedValue*): Typed = apply(Chunk.fromIterable(elements))

        def unapply(value: TypedValue): Option[(UType, Chunk[TypedValue])] = value match {
          case Tuple(attributes, elements) => Some((attributes, elements))
          case _                           => None
        }
      }
    }

    sealed case class Unit[+VA](attributes: VA) extends Value[Nothing, VA]
    object Unit {
      type Raw = Unit[scala.Unit]
      object Raw {
        def apply(): Raw = Unit(())
        def unapply(value: RawValue): Option[scala.Unit] = value match {
          case Unit(()) => Some(())
          case _        => None
        }
      }

      type Typed = Unit[UType]
      object Typed {
        def apply: Typed             = Unit(ir.Type.unit)
        def apply(tpe: UType): Typed = Unit(tpe)

        def unapply(value: TypedValue): Option[UType] = value match {
          case Unit(attributes) => Some(attributes)
          case _                => None
        }
      }
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
      def apply[VA](attributes: VA, name: String): Variable[VA] =
        Variable(attributes, Name.fromString(name))

      type Raw = Variable[scala.Unit]
      object Raw {
        @inline def apply(name: Name): Raw   = Variable((), name)
        @inline def apply(name: String): Raw = Variable((), name)
      }

      type Typed = Variable[UType]
      object Typed {
        @inline def apply(tpe: UType, name: Name): Typed   = Variable(tpe, name)
        @inline def apply(tpe: UType, name: String): Typed = Variable(tpe, name)
        @inline def apply(name: String, tpe: UType): Typed = Variable(tpe, name)
        @inline def apply(name: Name, tpe: UType): Typed   = Variable(tpe, name)

        def unapply(value: TypedValue): Option[(UType, Name)] = value match {
          case Variable(tpe, name) => Some((tpe, name))
          case _                   => None
        }
      }
    }
  }

  sealed case class Definition[+TA, +VA](
      inputTypes: Chunk[(Name, VA, Type[TA])],
      outputType: Type[TA],
      body: Value[TA, VA]
  ) { self =>
    def mapAttributes[TB, VB](f: TA => TB, g: VA => VB): Definition[TB, VB] = ???
    def toSpecification: Specification[TA] =
      Specification(self.inputTypes.map { case (name, _, tpe) => name -> tpe }, self.outputType)

  }

  object Definition {
    def apply[TA, VA](outputType: Type[TA], body: Value[TA, VA]): Definition[TA, VA] =
      Definition(Chunk.empty, outputType, body)

    def apply[TA, VA](
        inputTypes: (String, VA, Type[TA])*
    )(outputType: Type[TA])(body: Value[TA, VA]): Definition[TA, VA] = {
      val args = Chunk.fromIterable(inputTypes.map { case (n, va, t) => (Name.fromString(n), va, t) })
      Definition(args, outputType, body)
    }

    def fromRawValue(value: (RawValue, UType)): Definition.Raw =
      Definition(
        inputTypes = Chunk.empty,
        outputType = value._2,
        body = value._1
      )

    def fromRawValue(value: RawValue, outputType: UType): Definition.Raw =
      Definition(
        inputTypes = Chunk.empty,
        outputType = outputType,
        body = value
      )

    def fromTypedValue(value: TypedValue): Definition.Typed =
      Definition(
        inputTypes = Chunk.empty,
        outputType = value.attributes,
        body = value
      )

    type Raw = Definition[scala.Unit, scala.Unit]
    object Raw {

      def apply(inputTypes: Chunk[(Name, UType)], outputType: UType, body: RawValue): Raw =
        Definition(inputTypes.map { case (n, t) => (n, (), t) }, outputType, body)

      def apply(outputType: UType, body: RawValue): Raw =
        Definition(
          inputTypes = Chunk.empty,
          outputType = outputType,
          body = body
        )

      def apply(inputTypes: (String, UType)*)(outputType: UType)(body: RawValue): Raw = {
        val args = Chunk.fromIterable(inputTypes.map { case (n, t) => (Name.fromString(n), (), t) })
        Definition(args, outputType, body)
      }
    }
    type Typed = Definition[scala.Unit, UType]
    object Typed {
      def apply(inputTypes: (String, UType)*)(outputType: UType)(body: TypedValue): Typed = {
        val args = Chunk.fromIterable(inputTypes.map { case (n, t) => (Name.fromString(n), t, t) })
        Definition(args, outputType, body)
      }
    }
  }

  sealed case class Specification[+TA](inputs: Chunk[(Name, Type[TA])], output: Type[TA]) { self =>
    def map[B](f: TA => B): Specification[B] =
      Specification(
        inputs = inputs.map { case (name, tpe) => (name, Type.mapTypeAttributes(tpe)(f)) },
        output = Type.mapTypeAttributes(output)(f)
      )
  }

  object Specification {
    def create[Attributes](inputs: (Name, Type[Attributes])*): Inputs[Attributes] =
      new Inputs(() => Chunk.fromIterable(inputs))

    type Raw = Specification[scala.Unit]
    object Raw {
      def apply(inputs: (String, UType)*)(output: UType): Raw =
        Specification(
          inputs = Chunk.fromIterable(inputs.map { case (n, t) => Name.fromString(n) -> t }),
          output = output
        )
    }

    final class Inputs[A](private val inputs: () => Chunk[(Name, Type[A])]) {
      def apply(output: Type[A]): Specification[A] =
        Specification(inputs(), output)
    }
  }

  final def apply[TA, VA](
      attributes: VA,
      function: Value[TA, VA],
      argument: Value[TA, VA],
      arguments: Value[TA, VA]*
  ): Value[TA, VA] =
    Apply(attributes, function, argument, arguments: _*)

  final def apply(function: RawValue, argument: RawValue): RawValue = Apply.Raw(function, argument)

  final def apply(function: TypedValue, argument: TypedValue, arguments: TypedValue*): TypedValue =
    Apply.Typed(function, argument, arguments: _*)

  final def boolean[A](attributes: A, value: Boolean): Value[Nothing, A] = LiteralValue(attributes, boolLiteral(value))
  final def boolean(value: Boolean): RawValue                            = LiteralValue.Raw(boolLiteral(value))

  final def constructor[A](attributes: A, name: String): Value[Nothing, A] = Constructor(attributes, name)
  final def constructor[A](attributes: A, name: FQName): Value[Nothing, A] = Constructor(attributes, name)
  final def constructor(name: String): RawValue                            = Constructor.Raw(name)
  final def constructor(name: FQName): RawValue                            = Constructor.Raw(name)
  final def constructor(name: String, tpe: UType): TypedValue              = Constructor.Typed(name, tpe)
  final def constructor(name: FQName, tpe: UType): TypedValue              = Constructor.Typed(name, tpe)

  final def decimal[A](attributes: A, value: BigDecimal): Value[Nothing, A] =
    LiteralValue(attributes, Lit.decimal(value))
  final def decimal(value: BigDecimal): RawValue = LiteralValue.Raw(Lit.decimal(value))

  final def destructure[TA, VA](
      attributes: VA,
      pattern: Pattern[VA],
      valueToDestruct: Value[TA, VA],
      inValue: Value[TA, VA]
  ): Value[TA, VA] = Destructure(attributes, pattern, valueToDestruct, inValue)

  final def destructure(pattern: UPattern, valueToDestruct: RawValue, inValue: RawValue): RawValue =
    Destructure.Raw(pattern, valueToDestruct, inValue)

  final def field[TA, VA](attributes: VA, target: Value[TA, VA], name: Name): Value[TA, VA] =
    Field(attributes, target, name)

  final def field[TA, VA](attributes: VA, target: Value[TA, VA], name: String): Value[TA, VA] =
    Field(attributes, target, name)

  final def field(target: RawValue, name: Name): RawValue = Field.Raw(target, name)

  final def field(target: RawValue, name: String): RawValue = Field.Raw(target, name)

  final def fieldFunction[A](attributes: A, name: String): Value[Nothing, A] = FieldFunction(attributes, name)
  final def fieldFunction[A](attributes: A, name: Name): Value[Nothing, A]   = FieldFunction(attributes, name)
  final def fieldFunction(name: String, tpe: UType): TypedValue              = FieldFunction.Typed(tpe, name)
  final def fieldFunction(name: Name, tpe: UType): TypedValue                = FieldFunction.Typed(tpe, name)
  final def fieldFunction(name: String): RawValue                            = FieldFunction.Raw(name)
  final def fieldFunction(name: Name): RawValue                              = FieldFunction.Raw(name)

  final def float[A](attributes: A, value: Double): Value[Nothing, A] = LiteralValue(attributes, Lit.float(value))
  final def float(value: Double): RawValue                            = LiteralValue.Raw(Lit.float(value))
  final def float[A](attributes: A, value: Float): Value[Nothing, A]  = LiteralValue(attributes, Lit.float(value))
  final def float(value: Float): RawValue                             = LiteralValue.Raw(Lit.float(value))

  final def ifThenElse[TA, VA](
      attributes: VA,
      condition: Value[TA, VA],
      thenBranch: Value[TA, VA],
      elseBranch: Value[TA, VA]
  ): Value[TA, VA] = IfThenElse(attributes, condition, thenBranch, elseBranch)

  final def ifThenElse(condition: RawValue, thenBranch: RawValue, elseBranch: RawValue): RawValue =
    IfThenElse.Raw(condition, thenBranch, elseBranch)

  final def int[A](attributes: A, value: Int): Value[Nothing, A] = LiteralValue(attributes, Lit.int(value))
  final def int(value: Int): RawValue                            = LiteralValue.Raw(Lit.int(value))

  final def lambda[TA, VA](attributes: VA, argumentPattern: Pattern[VA], body: Value[TA, VA]): Value[TA, VA] =
    Lambda(attributes, argumentPattern, body)

  final def lambda(argumentPattern: UPattern, body: RawValue): RawValue = Lambda.Raw(argumentPattern, body)

  final def list[TA, VA](attributes: VA, values: Chunk[Value[TA, VA]]): Value[TA, VA] =
    List(attributes, values)

  final def list[TA, VA](attributes: VA, values: Value[TA, VA]*)(implicit ev: IsNotAValue[VA]): Value[TA, VA] =
    List(attributes, values: _*)

  final def list(elements: Chunk[RawValue]): RawValue = List.Raw(elements)
  final def list(elements: RawValue*): RawValue       = List.Raw(elements: _*)

  final def listOf[TA](elementType: UType, elements: Value[TA, UType]*): Value[TA, UType] =
    List(listType(elementType), elements: _*)

  final def listOf(elements: RawValue*)(elementType: UType): TypedValue =
    List(listType(elementType), elements.map(e => (e :> elementType)): _*)

  final def literal[VA](attributes: VA, literal: Lit): Value[Nothing, VA] = LiteralValue(attributes, literal)
  final def literal(literal: Lit): RawValue                               = LiteralValue.Raw(literal)

  final def record[TA, VA](attributes: VA, fields: Chunk[(Name, Value[TA, VA])]): Value[TA, VA] =
    Record(attributes, fields)

  final def record[TA, VA](attributes: VA, fields: Map[Name, Value[TA, VA]]): Value[TA, VA] =
    Record.fromMap(attributes, fields)

  final def record[TA, VA](attributes: VA, fields: (String, Value[TA, VA])*)(implicit
      ev: IsNotAValue[VA]
  ): Value[TA, VA] = Record(attributes, fields: _*)

  final def record(fields: Chunk[(Name, RawValue)]): RawValue = Record.Raw(fields)
  final def record(fields: (String, RawValue)*): RawValue     = Record.Raw(fields: _*)
  final def record(firstField: (Name, RawValue), otherFields: (Name, RawValue)*): RawValue =
    Record.Raw(firstField +: Chunk.fromIterable(otherFields))

  def reference[VA](attributes: VA, fullyQualifiedName: FQName)(implicit ev: NeedsAttributes[VA]): Value[Nothing, VA] =
    Reference(attributes, fullyQualifiedName)

  final def reference[A](attributes: A, fullyQualifiedName: String)(implicit
      ev: NeedsAttributes[A]
  ): Value[Nothing, A] =
    Reference(attributes, fullyQualifiedName)
  final def reference(fullyQualifiedName: String, tpe: UType): TypedValue = Reference.Typed(tpe, fullyQualifiedName)
  final def reference(fullyQualifiedName: FQName, tpe: UType): TypedValue = Reference.Typed(tpe, fullyQualifiedName)
  final def reference[A](
      attributes: A,
      packageName: String,
      moduleName: String,
      localName: String
  ): Value[Nothing, A] =
    Reference(attributes, packageName, moduleName, localName)
  final def reference(fullyQualifiedName: String): RawValue = Reference.Raw(fullyQualifiedName)
  final def reference(fullyQualifiedName: FQName): RawValue = Reference.Raw(fullyQualifiedName)
  final def reference(packageName: String, moduleName: String, localName: String): RawValue =
    Reference.Raw(packageName, moduleName, localName)

  final def string[VA](attributes: VA, value: String): Value[Nothing, VA] =
    LiteralValue(attributes, stringLiteral(value))
  final def string(value: String): RawValue = LiteralValue.Raw(stringLiteral(value))

  final def tuple[TA, VA](attributes: VA, elements: Chunk[Value[TA, VA]]): Value[TA, VA] = Tuple(attributes, elements)
  final def tuple[TA, VA](attributes: VA, first: Value[TA, VA], second: Value[TA, VA], otherElements: Value[TA, VA]*)(
      implicit ev: IsNotAValue[VA]
  ): Value[TA, VA] = Tuple(attributes, first +: second +: Chunk.fromIterable(otherElements))

  final def tuple(elements: RawValue*): RawValue       = Tuple.Raw(elements: _*)
  final def tuple(elements: Chunk[RawValue]): RawValue = Tuple.Raw(elements)
  final def tuple(element: (RawValue, UType), elements: (RawValue, UType)*): TypedValue =
    Tuple.Typed(Chunk.fromIterable((element +: elements).map { case (v, t) => v :> t }))

  def unit[VA](attributes: VA)(implicit ev: NeedsAttributes[VA]): Value[Nothing, VA] = Unit(attributes)
  final val unit: RawValue                                                           = unit(())

  def update[TA, VA](attributes: VA, valueToUpdate: Value[TA, VA], fieldsToUpdate: Map[Name, Value[TA, VA]])(implicit
      ev: NeedsAttributes[VA]
  ): Value[TA, VA] = UpdateRecord(attributes, valueToUpdate, fieldsToUpdate)

  final def variable[A](attributes: A, name: Name): Value[Nothing, A]   = Variable(attributes, name)
  final def variable[A](attributes: A, name: String): Value[Nothing, A] = Variable(attributes, name)
  final def variable(name: Name): RawValue                              = Variable.Raw(name)
  final def variable(name: String): RawValue                            = Variable.Raw(name)
  final def variable(name: String, tpe: UType): TypedValue              = Variable.Typed(tpe, name)
  final def variable(name: Name, tpe: UType): TypedValue                = Variable.Typed(tpe, name)

  final def wholeNumber(value: Long): RawValue =
    literal(Lit.wholeNumber(value))

  final def wholeNumber(value: Int): RawValue =
    literal(Lit.wholeNumber(value))
  trait MorphirValueModule { self =>
    final type Value[+TA, +VA] = module.Value[TA, VA]
    final type RawValue        = Value[scala.Unit, scala.Unit]
    final type TypedValue      = Value[scala.Unit, UType]

    final def definitionToSpecification[TA, VA](definition: Definition[TA, VA]): Specification[TA] =
      definition.toSpecification

    def record[TA, VA](attributes: VA, fields: Chunk[(Name, Value[TA, VA])]): Value[TA, VA] =
      module.record(attributes, fields)

    def record[TA, VA](attributes: VA, fields: Map[Name, Value[TA, VA]]): Value[TA, VA] =
      module.record(attributes, fields)

    def record[TA, VA](attributes: VA, fields: (String, Value[TA, VA])*)(implicit
        ev: IsNotAValue[VA]
    ): Value[TA, VA] = module.record(attributes, fields: _*)

    def record(fields: Chunk[(Name, RawValue)]): RawValue = module.record(fields)
    def record(fields: (String, RawValue)*): RawValue     = module.record(fields: _*)
    def record(firstField: (Name, RawValue), otherFields: (Name, RawValue)*): RawValue =
      module.record(firstField, otherFields: _*)

    def unit[VA](attributes: VA)(implicit ev: NeedsAttributes[VA]): Value[Nothing, VA]
    final val unit: RawValue = unit(())

    def update[TA, VA](attributes: VA, valueToUpdate: Value[TA, VA], fieldsToUpdate: Map[Name, Value[TA, VA]])(implicit
        ev: NeedsAttributes[VA]
    ): Value[TA, VA]
  }

  implicit class RawValueExtensions(private val self: RawValue) {

    /**
     * Ascribe the given type to this `RawValue` and all its children.
     * ===NOTE===
     * This is a recursive operation and all children of this `RawValue` will also be ascribed with the given value.
     */
    def :>(ascribedType: UType): TypedValue = self.mapAttributes(identity, _ => ascribedType)

    def toValDef(returnType: UType): Definition[Any, UType] = Definition(returnType, self :> returnType)
  }
}
