package org.finos.morphir
package ir
package internal

import scala.annotation.tailrec
import Literal.Lit
import Pattern._
import Type.{Type, UType}
import Type.{Unit => UnitType}
import zio.{Chunk, NonEmptyChunk}

sealed trait Value[+TA, +VA] { self =>
  import Value.{List => ListValue, _}
  def attributes: VA
  def collectReferences: Set[FQName] = foldContext(())(Folder.CollectReferences)
  def collectVariables: Set[Name]    = foldContext(())(Folder.CollectVariables)

  def fold[Z](fieldFunctionCase: (VA, Name) => Z, referenceCase: (VA, FQName) => Z, unitCase: VA => Z): Z =
    foldContext(())(
      new Folder.DelegatedFolder(
        onFieldFunctionCase = (_, _, attributes, name) => fieldFunctionCase(attributes, name),
        onReferenceCase = (_, _, attributes, fqName) => referenceCase(attributes, fqName),
        onUnitCase = (_, _, attributes) => unitCase(attributes)
      )
    )

  def foldContext[C, TA1 >: TA, VA1 >: VA, Z](context: C)(folder: Folder[C, TA1, VA1, Z]): Z = {
    import folder._
    @tailrec
    def loop(in: List[Value[TA1, VA1]], out: List[Either[Value[TA1, VA1], Z]]): List[Z] =
      in match {
        case (v @ Apply(attributes, function, arguments)) :: values =>
          loop(function :: arguments :: values, Left(v) :: out)
        case (v @ Destructure(attributes, pattern, valueToDestruct, inValue)) :: values =>
          loop(valueToDestruct :: inValue :: values, Left(v) :: out)
        case (v @ Constructor(attributes, name)) :: values =>
          loop(values, Right(constructorCase(context, v, attributes, name)) :: out)
        case (v @ FieldFunction(attributes, name)) :: values =>
          loop(values, Right(fieldFunctionCase(context, v, attributes, name)) :: out)
        case (v @ Literal(attributes, lit)) :: values =>
          loop(values, Right(literalCase(context, v, attributes, lit)) :: out)
        case (v @ Reference(attributes, name)) :: values =>
          loop(values, Right(referenceCase(context, v, attributes, name)) :: out)
        case (v @ Variable(attributes, name)) :: values =>
          loop(values, Right(variableCase(context, v, attributes, name)) :: out)
        case (v @ Unit(attributes)) :: values =>
          loop(values, Right(unitCase(context, v, attributes)) :: out)
        case Nil =>
          out.foldLeft[List[Z]](List.empty) {
            case (acc, Right(results)) => results :: acc
            case _                     => ???
          }
      }
    loop(List(self), List.empty).head
  }

  def foldContextWith[C, Z](
      context: C
  )(
      fieldFunctionCase: (C, Value[TA, VA], VA, Name) => Z,
      referenceCase: (C, Value[TA, VA], VA, FQName) => Z,
      unitCase: (C, Value[TA, VA], VA) => Z
  )(
      fieldCase: (C, Value[TA, VA], VA, Z, Name) => Z
  ): Z = foldContext(context)(
    new Folder.DelegatedFolder[C, TA, VA, Z](
      onFieldFunctionCase = fieldFunctionCase,
      onReferenceCase = referenceCase,
      onUnitCase = unitCase
    )
  )

  def mapAttributes[TB, VB](f: TA => TB, g: VA => VB): Value[TB, VB] = fold(
    fieldFunctionCase = (attributes, name) => FieldFunction(g(attributes), name),
    referenceCase = (attributes, fqName) => Reference(g(attributes), fqName),
    unitCase = attributes => Unit(g(attributes))
  )
  def toRawValue: RawValue = mapAttributes((_ => ()), (_ => ()))

  final override def toString: String = foldContext(())(Folder.ToString)
}

object Value {

  final type RawValue             = Value[scala.Unit, scala.Unit]
  final type TypedValue           = Value[scala.Unit, UType]
  final type Definition[+TA, +VA] = ValueDefinition[TA, VA]
  final val Definition = ValueDefinition

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

    def apply[TA, VA](attributes: VA, function: Value[TA, VA], arguments: scala.List[Value[TA, VA]])(implicit
        ev: NeedsAttributes[VA]
    ): Value[TA, VA] =
      arguments match {
        case Nil => function
        case head :: tail =>
          tail.foldLeft(Apply(attributes, function, head)) { case (acc, arg) => Apply(acc.attributes, acc, arg) }
      }
      // arguments.foldLeft(Apply(attributes, function, arguments.head)) { case (acc, arg) => Apply(acc.attributes, acc, arg) }

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

  sealed case class Field[+TA, +VA](attributes: VA, subjectValue: Value[TA, VA], fieldName: Name) extends Value[TA, VA]
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
    sealed case class Unbound[+TA, +VA](name: Name, valueDefinition: Definition[TA, VA]) {
      def bind[TB >: TA, VB >: VA](value: Value[TB, VB]): Value[TB, VB] =
        LetDefinition(value.attributes, name, valueDefinition, value)

      def in[TB >: TA, VB >: VA](value: Value[TB, VB]): Value[TB, VB] =
        LetDefinition(value.attributes, name, valueDefinition, value)

      override def toString(): String = {
        val args = valueDefinition.inputTypes.map(_._1.toCamelCase).mkString(" ")
        val body = valueDefinition.body.toString()
        s"let ${name.toCamelCase}$args = $body"
      }
    }

  }

  sealed case class LetRecursion[+TA, +VA](
      attributes: VA,
      valueDefinitions: Map[Name, Definition[TA, VA]],
      inValue: Value[TA, VA]
  ) extends Value[TA, VA]

  object LetRecursion {
    def apply[TA, VA](attributes: VA, valueDefinitions: (String, Definition[TA, VA])*)(
        inValue: Value[TA, VA]
    ): LetRecursion[TA, VA] =
      LetRecursion(
        attributes,
        valueDefinitions.map { case (n, d) => (Name.fromString(n), d) }.toMap,
        inValue
      )

    type Raw = LetRecursion[scala.Unit, scala.Unit]
    object Raw {
      def apply(valueDefinitions: Map[Name, Definition.Raw], inValue: RawValue): RawValue =
        LetRecursion(inValue.attributes, valueDefinitions.map { case (n, d) => (n, d) }, inValue)

      def apply(valueDefinitions: (String, Definition.Raw)*)(inValue: RawValue): RawValue =
        LetRecursion(
          inValue.attributes,
          valueDefinitions.map { case (n, d) => (Name.fromString(n), d) }.toMap,
          inValue
        )

    }
    type Typed = LetRecursion[scala.Unit, UType]
    object Typed {
      def apply(
          tpe: UType,
          valueDefinitions: Map[Name, Definition.Typed],
          inValue: TypedValue
      ): Typed =
        LetRecursion(tpe, valueDefinitions.map { case (n, d) => (n, d) }, inValue)

      def apply(tpe: UType, valueDefinitions: (String, Definition[scala.Unit, UType])*)(
          inValue: TypedValue
      ): Typed =
        LetRecursion(tpe, valueDefinitions.map { case (n, d) => (Name.fromString(n), d) }.toMap, inValue)

      def apply(
          valueDefinitions: Map[Name, Definition.Typed],
          inValue: TypedValue
      ): Typed = LetRecursion(inValue.attributes, valueDefinitions.map { case (n, d) => (n, d) }, inValue)

      def apply(valueDefinitions: (String, Definition[scala.Unit, UType])*)(
          inValue: TypedValue
      ): Typed =
        LetRecursion(
          inValue.attributes,
          valueDefinitions.map { case (n, d) => (Name.fromString(n), d) }.toMap,
          inValue
        )
    }
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

      def apply(elements: NonEmptyChunk[TypedValue]): Typed =
        // val tpe = sdk.List.listType(elements.head.attributes)
        List( /*tpe*/ ???, elements)

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

  sealed case class Reference[+VA](attributes: VA, fullyQualifiedName: FQName) extends Value[Nothing, VA]
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
        case reference: Reference[VA] => Some(reference.fullyQualifiedName)
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
        val tpe = Type.Tuple((), elements.map(_.attributes))
        Tuple(tpe, elements)
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
      def apply: Typed             = Unit(UnitType(()))
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
    def letDestructCase(
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

  object Folder {

    object CollectReferences extends Folder[Any, Any, Any, Set[FQName]] {

      override def applyCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          function: Set[FQName],
          argument: Set[FQName]
      ): Set[FQName] = ???

      override def constructorCase(context: Any, value: Value[Any, Any], attributes: Any, name: FQName): Set[FQName] =
        ???

      override def destructureCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          pattern: Pattern[Any],
          valueToDestruct: Set[FQName],
          inValue: Set[FQName]
      ): Set[FQName] = ???

      override def fieldCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          subjectValue: Set[FQName],
          fieldName: Name
      ): Set[FQName] = ???

      override def fieldFunctionCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          fieldName: Name
      ): Set[FQName] = Set.empty

      override def ifThenElseCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          condition: Set[FQName],
          thenBranch: Set[FQName],
          elseBranch: Set[FQName]
      ): Set[FQName] = ???

      override def lambdaCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          argumentPattern: Pattern[Any],
          body: Set[FQName]
      ): Set[FQName] = ???

      override def letDefCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          valueName: Name,
          valueDefinition: (Chunk[(Name, Any, Type[Any])], Type[Any], Set[FQName]),
          inValue: Set[FQName]
      ): Set[FQName] = ???

      override def letDestructCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          valueDefinitions: Map[Name, (Chunk[(Name, Any, Type[Any])], Type[Any], Set[FQName])],
          inValue: Set[FQName]
      ): Set[FQName] = ???

      override def letRecCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          valueDefinitions: Map[Name, (Chunk[(Name, Any, Type[Any])], Type[Any], Set[FQName])],
          inValue: Set[FQName]
      ): Set[FQName] = ???

      override def listCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          elements: Chunk[Set[FQName]]
      ): Set[FQName] = ???

      override def literalCase(context: Any, value: Value[Any, Any], attributes: Any, literal: Lit): Set[FQName] = ???

      override def patternMatchCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          branchOutOn: Set[FQName],
          cases: Chunk[(Pattern[Any], Set[FQName])]
      ): Set[FQName] = ???

      override def recordCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          fields: Chunk[(Name, Set[FQName])]
      ): Set[FQName] = ???

      override def referenceCase(context: Any, value: Value[Any, Any], attributes: Any, name: FQName): Set[FQName] =
        Set(name)

      override def tupleCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          elements: Chunk[Set[FQName]]
      ): Set[FQName] = ???

      override def unitCase(context: Any, value: Value[Any, Any], attributes: Any): Set[FQName] = Set.empty

      override def updateRecordCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          valueToUpdate: Set[FQName],
          fieldsToUpdate: Map[Name, Set[FQName]]
      ): Set[FQName] = ???

      override def variableCase(context: Any, value: Value[Any, Any], attributes: Any, name: Name): Set[FQName] = ???

    }

    object CollectVariables extends Folder[Any, Any, Any, Set[Name]] {

      override def applyCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          function: Set[Name],
          argument: Set[Name]
      ): Set[Name] = ???

      override def constructorCase(context: Any, value: Value[Any, Any], attributes: Any, name: FQName): Set[Name] = ???

      override def destructureCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          pattern: Pattern[Any],
          valueToDestruct: Set[Name],
          inValue: Set[Name]
      ): Set[Name] = ???

      override def fieldCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          subjectValue: Set[Name],
          fieldName: Name
      ): Set[Name] = ???

      override def fieldFunctionCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          fieldName: Name
      ): Set[Name] = Set.empty

      override def ifThenElseCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          condition: Set[Name],
          thenBranch: Set[Name],
          elseBranch: Set[Name]
      ): Set[Name] = ???

      override def lambdaCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          argumentPattern: Pattern[Any],
          body: Set[Name]
      ): Set[Name] = ???

      override def letDefCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          valueName: Name,
          valueDefinition: (Chunk[(Name, Any, Type[Any])], Type[Any], Set[Name]),
          inValue: Set[Name]
      ): Set[Name] = ???

      override def letDestructCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          valueDefinitions: Map[Name, (Chunk[(Name, Any, Type[Any])], Type[Any], Set[Name])],
          inValue: Set[Name]
      ): Set[Name] = ???

      override def letRecCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          valueDefinitions: Map[Name, (Chunk[(Name, Any, Type[Any])], Type[Any], Set[Name])],
          inValue: Set[Name]
      ): Set[Name] = ???

      override def listCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          elements: Chunk[Set[Name]]
      ): Set[Name] = ???

      override def literalCase(context: Any, value: Value[Any, Any], attributes: Any, literal: Lit): Set[Name] = ???

      override def patternMatchCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          branchOutOn: Set[Name],
          cases: Chunk[(Pattern[Any], Set[Name])]
      ): Set[Name] = ???

      override def recordCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          fields: Chunk[(Name, Set[Name])]
      ): Set[Name] = ???

      override def referenceCase(context: Any, value: Value[Any, Any], attributes: Any, name: FQName): Set[Name] =
        Set.empty

      override def tupleCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          elements: Chunk[Set[Name]]
      ): Set[Name] = ???

      override def unitCase(context: Any, value: Value[Any, Any], attributes: Any): Set[Name] = Set.empty

      override def updateRecordCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          valueToUpdate: Set[Name],
          fieldsToUpdate: Map[Name, Set[Name]]
      ): Set[Name] = ???

      override def variableCase(context: Any, value: Value[Any, Any], attributes: Any, name: Name): Set[Name] = ???

    }
    object ToString extends Folder[Any, Any, Any, String] {

      override def applyCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          function: String,
          argument: String
      ): String = ???

      override def constructorCase(context: Any, value: Value[Any, Any], attributes: Any, name: FQName): String = ???

      override def destructureCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          pattern: Pattern[Any],
          valueToDestruct: String,
          inValue: String
      ): String = ???

      override def fieldCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          subjectValue: String,
          fieldName: Name
      ): String = s"$subjectValue.${fieldName.toCamelCase}"

      override def fieldFunctionCase(context: Any, value: Value[Any, Any], attributes: Any, fieldName: Name): String =
        s".${fieldName.toCamelCase}"

      override def ifThenElseCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          condition: String,
          thenBranch: String,
          elseBranch: String
      ): String = ???

      override def lambdaCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          argumentPattern: Pattern[Any],
          body: String
      ): String = ???

      override def letDefCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          valueName: Name,
          valueDefinition: (Chunk[(Name, Any, Type[Any])], Type[Any], String),
          inValue: String
      ): String = ???

      override def letDestructCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          valueDefinitions: Map[Name, (Chunk[(Name, Any, Type[Any])], Type[Any], String)],
          inValue: String
      ): String = ???

      override def letRecCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          valueDefinitions: Map[Name, (Chunk[(Name, Any, Type[Any])], Type[Any], String)],
          inValue: String
      ): String = ???

      override def listCase(context: Any, value: Value[Any, Any], attributes: Any, elements: Chunk[String]): String =
        ???

      override def literalCase(context: Any, value: Value[Any, Any], attributes: Any, literal: Lit): String = ???

      override def patternMatchCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          branchOutOn: String,
          cases: Chunk[(Pattern[Any], String)]
      ): String = ???

      override def recordCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          fields: Chunk[(Name, String)]
      ): String = ???

      override def referenceCase(context: Any, value: Value[Any, Any], attributes: Any, name: FQName): String = Seq(
        Path.toString(Name.toTitleCase, ".", name.packagePath.toPath),
        Path.toString(Name.toTitleCase, ".", name.modulePath.toPath),
        name.localName.toCamelCase
      ).mkString(".")

      override def tupleCase(context: Any, value: Value[Any, Any], attributes: Any, elements: Chunk[String]): String =
        ???

      override def unitCase(context: Any, value: Value[Any, Any], attributes: Any): String = "()"

      override def updateRecordCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          valueToUpdate: String,
          fieldsToUpdate: Map[Name, String]
      ): String = ???

      override def variableCase(context: Any, value: Value[Any, Any], attributes: Any, name: Name): String = ???

    }

    class DelegatedFolder[-Context, -TA, -VA, Z](
        onFieldFunctionCase: (Context, Value[TA, VA], VA, Name) => Z,
        onReferenceCase: (Context, Value[TA, VA], VA, FQName) => Z,
        onUnitCase: (Context, Value[TA, VA], VA) => Z
    ) extends Folder[Context, TA, VA, Z] {

      override def applyCase(context: Context, value: Value[TA, VA], attributes: VA, function: Z, argument: Z): Z = ???

      override def constructorCase(context: Context, value: Value[TA, VA], attributes: VA, name: FQName): Z = ???

      override def destructureCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          pattern: Pattern[VA],
          valueToDestruct: Z,
          inValue: Z
      ): Z = ???

      override def fieldCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          subjectValue: Z,
          fieldName: Name
      ): Z = ???

      override def fieldFunctionCase(context: Context, value: Value[TA, VA], attributes: VA, fieldName: Name): Z =
        onFieldFunctionCase(context, value, attributes, fieldName)

      override def ifThenElseCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          condition: Z,
          thenBranch: Z,
          elseBranch: Z
      ): Z = ???

      override def lambdaCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          argumentPattern: Pattern[VA],
          body: Z
      ): Z = ???

      override def letDefCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          valueName: Name,
          valueDefinition: (Chunk[(Name, VA, Type[TA])], Type[TA], Z),
          inValue: Z
      ): Z = ???

      override def letDestructCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          valueDefinitions: Map[Name, (Chunk[(Name, VA, Type[TA])], Type[TA], Z)],
          inValue: Z
      ): Z = ???

      override def letRecCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          valueDefinitions: Map[Name, (Chunk[(Name, VA, Type[TA])], Type[TA], Z)],
          inValue: Z
      ): Z = ???

      override def listCase(context: Context, value: Value[TA, VA], attributes: VA, elements: Chunk[Z]): Z = ???

      override def literalCase(context: Context, value: Value[TA, VA], attributes: VA, literal: Lit): Z = ???

      override def patternMatchCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          branchOutOn: Z,
          cases: Chunk[(Pattern[VA], Z)]
      ): Z = ???

      override def recordCase(context: Context, value: Value[TA, VA], attributes: VA, fields: Chunk[(Name, Z)]): Z = ???

      override def referenceCase(context: Context, value: Value[TA, VA], attributes: VA, name: FQName): Z =
        onReferenceCase(context, value, attributes, name)

      override def tupleCase(context: Context, value: Value[TA, VA], attributes: VA, elements: Chunk[Z]): Z = ???

      override def unitCase(context: Context, value: Value[TA, VA], attributes: VA): Z =
        onUnitCase(context, value, attributes)

      override def updateRecordCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          valueToUpdate: Z,
          fieldsToUpdate: Map[Name, Z]
      ): Z = ???

      override def variableCase(context: Context, value: Value[TA, VA], attributes: VA, name: Name): Z = ???

    }
  }

}
