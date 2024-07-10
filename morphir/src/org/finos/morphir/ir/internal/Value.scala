package org.finos.morphir
package ir
package internal

import scala.annotation.{tailrec, unused}
import org.finos.morphir.naming._
import Literal.Lit
import Pattern._
import Type.{Type, UType}
import Type.{Unit => UnitType}
import zio.{Chunk, NonEmptyChunk}
import zio.ChunkBuilder

sealed trait Value[+TA, +VA] { self =>
  import Value.{List => ListValue, _}
  def attributes: VA
  def collectReferences: Set[FQName] = foldContext(())(Folder.CollectReferences)
  def collectVariables: Set[Name]    = foldContext(())(Folder.CollectVariables)

  def fold[Z](
      applyCase: (VA, Z, Z) => Z,
      constructorCase: (VA, FQName) => Z,
      destructureCase: (VA, Pattern[VA], Z, Z) => Z,
      fieldCase: (VA, Z, Name) => Z,
      fieldFunctionCase: (VA, Name) => Z,
      ifThenElseCase: (VA, Z, Z, Z) => Z,
      lambdaCase: (VA, Pattern[VA], Z) => Z,
      letDefinitionCase: (VA, Name, (Chunk[(Name, VA, Type[TA])], Type[TA], Z), Z) => Z,
      letRecursionCase: (VA, Map[Name, (Chunk[(Name, VA, Type[TA])], Type[TA], Z)], Z) => Z,
      listCase: (VA, Chunk[Z]) => Z,
      literalCase: (VA, Lit) => Z,
      patternMatchCase: (VA, Z, Chunk[(Pattern[VA], Z)]) => Z,
      recordCase: (VA, Chunk[(Name, Z)]) => Z,
      referenceCase: (VA, FQName) => Z,
      tupleCase: (VA, Chunk[Z]) => Z,
      unitCase: VA => Z,
      updateRecordCase: (VA, Z, Map[Name, Z]) => Z,
      variableCase: (VA, Name) => Z
  ): Z =
    foldContext(())(
      new Folder.DelegatedFolder(
        onApplyCase = (_, _, attributes, function, argument) => applyCase(attributes, function, argument),
        onConstructorCase = (_, _, attributes, name) => constructorCase(attributes, name),
        onDestructureCase = (_, _, attributes, pattern, valueToDestruct, inValue) =>
          destructureCase(attributes, pattern, valueToDestruct, inValue),
        onFieldCase = (_, _, attributes, value, name) => fieldCase(attributes, value, name),
        onFieldFunctionCase = (_, _, attributes, name) => fieldFunctionCase(attributes, name),
        onIfThenElseCase = (_, _, attributes, condition, thenValue, elseValue) =>
          ifThenElseCase(attributes, condition, thenValue, elseValue),
        onLambdaCase = (_, _, attributes, pattern, body) => lambdaCase(attributes, pattern, body),
        onLetDefinitionCase =
          (_, _, attributes, name, definition, inValue) => letDefinitionCase(attributes, name, definition, inValue),
        onLetRecursionCase =
          (_, _, attributes, definitions, inValue) => letRecursionCase(attributes, definitions, inValue),
        onListCase = (_, _, attributes, values) => listCase(attributes, values),
        onLiteralCase = (_, _, attributes, lit) => literalCase(attributes, lit),
        onPatternMatchCase = (_, _, attributes, value, cases) => patternMatchCase(attributes, value, cases),
        onRecordCase = (_, _, attributes, fields) => recordCase(attributes, fields),
        onReferenceCase = (_, _, attributes, fqName) => referenceCase(attributes, fqName),
        onTupleCase = (_, _, attributes, values) => tupleCase(attributes, values),
        onUnitCase = (_, _, attributes) => unitCase(attributes),
        onUpdateRecordCase =
          (_, _, attributes, valueToUpdate, fields) => updateRecordCase(attributes, valueToUpdate, fields),
        onVariableCase = (_, _, attributes, name) => variableCase(attributes, name)
      )
    )

  def foldContext[C, TA1 >: TA, VA1 >: VA, Z](context: C)(folder: Folder[C, TA1, VA1, Z]): Z = {
    import folder._
    @tailrec
    def loop(in: List[Value[TA1, VA1]], out: List[Either[Value[TA1, VA1], Z]]): List[Z] =
      in match {
        case (v @ Apply(_, function, argument)) :: values =>
          loop(function :: argument :: values, Left(v) :: out)
        case (v @ Destructure(_, _, valueToDestruct, inValue)) :: values =>
          loop(valueToDestruct :: inValue :: values, Left(v) :: out)
        case (v @ Constructor(attributes, name)) :: values =>
          loop(values, Right(constructorCase(context, v, attributes, name)) :: out)
        case (v @ Field(_, subjectValue, _)) :: values =>
          loop(subjectValue :: values, Left(v) :: out)
        case (v @ FieldFunction(attributes, name)) :: values =>
          loop(values, Right(fieldFunctionCase(context, v, attributes, name)) :: out)
        case (v @ IfThenElse(_, condition, thenValue, elseValue)) :: values =>
          loop(condition :: thenValue :: elseValue :: values, Left(v) :: out)
        case (v @ Lambda(_, _, body)) :: values =>
          loop(body :: values, Left(v) :: out)
        case (v @ LetDefinition(_, _, definition, inValue)) :: values =>
          loop(definition.body :: inValue :: values, Left(v) :: out)
        case (v @ LetRecursion(_, definitions, inValue)) :: values =>
          loop(definitions.values.map(_.body).toList ::: inValue :: values, Left(v) :: out)
        case (v @ ListValue(_, elements)) :: values =>
          loop(elements.toList ++ values, Left(v) :: out)
        case (v @ Literal(attributes, lit)) :: values =>
          loop(values, Right(literalCase(context, v, attributes, lit)) :: out)
        case (v @ PatternMatch(_, value, cases)) :: values =>
          loop(value :: cases.map(_._2).toList ++ values, Left(v) :: out)
        case (v @ Record(_, fields)) :: values =>
          val fieldValues = fields.map(_._2).toList
          loop(fieldValues ++ values, Left(v) :: out)
        case (v @ Reference(attributes, name)) :: values =>
          loop(values, Right(referenceCase(context, v, attributes, name)) :: out)
        case (v @ Tuple(_, elements)) :: values =>
          loop(elements.toList ++ values, Left(v) :: out)
        case (v @ Unit(attributes)) :: values =>
          loop(values, Right(unitCase(context, v, attributes)) :: out)
        case (v @ UpdateRecord(_, valueToUpdate, fields)) :: values =>
          val fieldValues = fields.values.toList
          loop(valueToUpdate :: fieldValues ++ values, Left(v) :: out)
        case (v @ Variable(attributes, name)) :: values =>
          loop(values, Right(variableCase(context, v, attributes, name)) :: out)
        case Nil =>
          out.foldLeft[List[Z]](List.empty) {
            case (acc, Right(results)) => results :: acc
            case (acc, Left(v @ Apply(attributes, _, _))) =>
              val function :: argument :: rest = (acc: @unchecked)
              applyCase(context, v, attributes, function, argument) :: rest
            case (acc, Left(v @ Destructure(attributes, pattern, _, _))) =>
              val valueToDestruct :: inValue :: rest = (acc: @unchecked)
              destructureCase(context, v, attributes, pattern, valueToDestruct, inValue) :: rest
            case (acc, Left(v @ Field(attributes, _, fieldName))) =>
              val subjectValue = acc.head
              val rest         = acc.tail
              fieldCase(context, v, attributes, subjectValue, fieldName) :: rest
            case (acc, Left(v @ IfThenElse(attributes, _, _, _))) =>
              val condition :: thenValue :: elseValue :: rest = (acc: @unchecked)
              ifThenElseCase(context, v, attributes, condition, thenValue, elseValue) :: rest
            case (acc, Left(v @ Lambda(attributes, pattern, _))) =>
              val body :: rest = (acc: @unchecked)
              lambdaCase(context, v, attributes, pattern, body) :: rest
            case (acc, Left(v @ LetDefinition(attributes, name, defn, _))) =>
              val body :: inValue :: rest = (acc: @unchecked)
              letDefinitionCase(context, v, attributes, name, (defn.inputTypes, defn.outputType, body), inValue) :: rest
            case (acc, Left(v @ LetRecursion(attributes, definitions, _))) =>
              val definitionValues = acc.take(definitions.size)
              val acc2             = acc.drop(definitions.size)
              val inValue :: rest  = (acc2: @unchecked)
              val definitionsFinal = definitions
                .zip(definitionValues)
                .map { case ((name, defn), value) =>
                  (name, (defn.inputTypes, defn.outputType, value))
                }
                .toMap
              letRecursionCase(context, v, attributes, definitionsFinal, inValue) :: rest

            case (acc, Left(v @ ListValue(attributes, _))) =>
              val elements = Chunk.fromIterable(acc.take(v.elements.size))
              val rest     = acc.drop(v.elements.length)
              listCase(context, v, attributes, elements) :: rest
            case (acc, Left(v @ PatternMatch(attributes, _, _))) =>
              val value :: acc2 = (acc: @unchecked)
              val caseValues    = acc2.take(v.cases.size)
              val cases         = v.cases.zip(caseValues).map { case ((pattern, _), caseValue) => (pattern, caseValue) }
              val rest          = acc2.drop(v.cases.size)
              patternMatchCase(context, v, attributes, value, cases) :: rest
            case (acc, Left(v @ Record(attributes, _))) =>
              val fieldValues = Chunk.fromIterable(acc.take(v.fields.length))
              val rest        = acc.drop(v.fields.length)
              val fields      = v.fields.map(_._1).zip(fieldValues)
              recordCase(context, v, attributes, fields) :: rest
            case (acc, Left(v @ Tuple(attributes, _))) =>
              val arity    = v.elements.length
              val elements = Chunk.fromIterable(acc.take(arity))
              val rest     = acc.drop(arity)
              tupleCase(context, v, attributes, elements) :: rest
            case (acc, Left(v @ UpdateRecord(attributes, _, _))) =>
              val arity                 = v.fieldsToUpdate.size
              val valueToUpdate :: rest = (acc: @unchecked)
              val fieldValues           = Chunk.fromIterable(rest.take(arity))
              val fields                = v.fieldsToUpdate.keys.zip(fieldValues).toMap
              updateRecordCase(context, v, attributes, valueToUpdate, fields) :: rest.drop(arity)
            case (_, Left(value)) =>
              val msg =
                s"Reached an unexpected state while folding a Value. The Value node is a ${value.getClass().getSimpleName()} This is a bug in the traversal logic for values."
              throw new IllegalStateException(msg)
          }
        case lst =>
          val items = lst.map(_.getClass().getSimpleName()).mkString(", ")
          throw new IllegalStateException(
            s"Reached an unexpected state while traversing values. The error occured while matching a list contianing the following Value node types: $items"
          )
      }
    loop(List(self), List.empty).head
  }

  def foldContextWith[C, Z](
      context: C
  )(
      constructorCase: (C, Value[TA, VA], VA, FQName) => Z,
      fieldFunctionCase: (C, Value[TA, VA], VA, Name) => Z,
      literalCase: (C, Value[TA, VA], VA, Lit) => Z,
      referenceCase: (C, Value[TA, VA], VA, FQName) => Z,
      unitCase: (C, Value[TA, VA], VA) => Z,
      variableCase: (C, Value[TA, VA], VA, Name) => Z
  )(
      applyCase: (C, Value[TA, VA], VA, Z, Z) => Z,
      destructureCase: (C, Value[TA, VA], VA, Pattern[VA], Z, Z) => Z,
      fieldCase: (C, Value[TA, VA], VA, Z, Name) => Z,
      ifThenElseCase: (C, Value[TA, VA], VA, Z, Z, Z) => Z,
      lambdaCase: (C, Value[TA, VA], VA, Pattern[VA], Z) => Z,
      letDefinitionCase: (C, Value[TA, VA], VA, Name, (Chunk[(Name, VA, Type[TA])], Type[TA], Z), Z) => Z,
      letRecursionCase: (C, Value[TA, VA], VA, Map[Name, (Chunk[(Name, VA, Type[TA])], Type[TA], Z)], Z) => Z,
      listCase: (C, Value[TA, VA], VA, Chunk[Z]) => Z,
      patternMatchCase: (C, Value[TA, VA], VA, Z, Chunk[(Pattern[VA], Z)]) => Z,
      recordCase: (C, Value[TA, VA], VA, Chunk[(Name, Z)]) => Z,
      tupleCase: (C, Value[TA, VA], VA, Chunk[Z]) => Z,
      updateRecordCase: (C, Value[TA, VA], VA, Z, Map[Name, Z]) => Z
  ): Z = foldContext(context)(
    new Folder.DelegatedFolder[C, TA, VA, Z](
      onApplyCase = applyCase,
      onConstructorCase = constructorCase,
      onDestructureCase = destructureCase,
      onFieldCase = fieldCase,
      onFieldFunctionCase = fieldFunctionCase,
      onIfThenElseCase = ifThenElseCase,
      onLambdaCase = lambdaCase,
      onLetDefinitionCase = letDefinitionCase,
      onLetRecursionCase = letRecursionCase,
      onListCase = listCase,
      onLiteralCase = literalCase,
      onPatternMatchCase = patternMatchCase,
      onRecordCase = recordCase,
      onReferenceCase = referenceCase,
      onTupleCase = tupleCase,
      onUnitCase = unitCase,
      onUpdateRecordCase = updateRecordCase,
      onVariableCase = variableCase
    )
  )

  final def foldLeft[Z](z: Z)(f: PartialFunction[(Z, Value[TA, VA]), Z]): Z = {
    @tailrec
    def loop(z: Z, value: Value[TA, VA], stack: List[Value[TA, VA]]): Z =
      (f.applyOrElse[(Z, Value[TA, VA]), Z](z -> value, _ => z), value) match {
        case (z, v @ Apply(_, _, _)) =>
          loop(z, v.function, v.argument :: stack)
        case (z, v @ Destructure(_, _, _, _)) =>
          loop(z, v.valueToDestruct, v.inValue :: stack)
        case (z, Field(_, value, _)) =>
          loop(z, value, stack)
        case (z, IfThenElse(_, condition, thenBranch, elseBranch)) =>
          loop(z, condition, thenBranch :: elseBranch :: stack)
        case (z, Lambda(_, _, body)) =>
          loop(z, body, stack)
        case (z, LetDefinition(_, _, Value.Definition(_, _, body), inValue)) =>
          loop(z, body, inValue :: stack)
        case (z, LetRecursion(_, valueDefs, inValue)) =>
          val values = valueDefs.values.map(_.body).toList ++ scala.List(inValue)
          loop(z, values.head, values.tail ++ stack)
        case (z, ListValue(_, Chunk(head, tail @ _*))) =>
          loop(z, head, tail.toList ++ stack)
        case (z, PatternMatch(_, branchOutOn, cases)) =>
          val values = cases.map(_._2).toList
          loop(z, branchOutOn, values ++ stack)
        case (z, Record(_, Chunk((_, value), tail @ _*))) =>
          loop(z, value, tail.map(_._2).toList ++ stack)
        case (z, Tuple(_, Chunk(head, tail @ _*))) =>
          loop(z, head, tail.toList ++ stack)
        case (z, v @ UpdateRecord(_, _, _)) =>
          val values = v.fieldsToUpdate.values.toList
          loop(z, v.valueToUpdate, values ++ stack)
        case (z, _) =>
          stack match {
            case Nil          => z
            case head :: tail => loop(z, head, tail)
          }
      }
    loop(z, self, List.empty)
  }

  def isData: Boolean = fold[Boolean](
    applyCase = (_, fun, arg) => fun && arg,
    constructorCase = (_, _) => true,
    destructureCase = (_, _, _, _) => false,
    fieldCase = (_, _, _) => false,
    fieldFunctionCase = (_, _) => false,
    ifThenElseCase = (_, _, _, _) => false,
    lambdaCase = (_, _, _) => false,
    letDefinitionCase = (_, _, _, _) => false,
    letRecursionCase = (_, _, _) => false,
    listCase = (_, items) => items.forall(identity),
    literalCase = (_, _) => true,
    patternMatchCase = (_, _, _) => false,
    recordCase = (_, fields) => fields.forall(_._2),
    referenceCase = (_, _) => false,
    tupleCase = (_, items) => items.forall(identity),
    unitCase = (_) => true,
    updateRecordCase = (_, _, _) => false,
    variableCase = (_, _) => false
  )

  def mapAttributes[TB, VB](f: TA => TB, g: VA => VB): Value[TB, VB] = fold[Value[TB, VB]](
    applyCase = (attributes, function, argument) => Apply(g(attributes), function, argument),
    constructorCase = (attributes, name) => Constructor(g(attributes), name),
    destructureCase = (attributes, pattern, valueToDestruct, inValue) =>
      Destructure(g(attributes), pattern.map(g), valueToDestruct, inValue),
    fieldCase = (attributes, value, name) => Field(g(attributes), value, name),
    fieldFunctionCase = (attributes, name) => FieldFunction(g(attributes), name),
    ifThenElseCase = (attributes, condition, ifTrue, ifFalse) => IfThenElse(g(attributes), condition, ifTrue, ifFalse),
    lambdaCase = (attributes, pattern, body) => Lambda(g(attributes), pattern.map(g), body),
    letDefinitionCase = { (attributes, name, definition, inValue) =>
      val (parameters, returnType, value) = definition
      val parametersFinal                 = parameters.map { case (name, value, tpe) => (name, g(value), tpe.map(f)) }
      val returnTypeFinal                 = returnType.map(f)
      LetDefinition(
        g(attributes),
        name,
        Value.Definition(parametersFinal, returnTypeFinal, value),
        inValue
      )
    },
    letRecursionCase = (attributes, bindings, inValue) => ???,
    listCase = (attributes, values) => ListValue(g(attributes), values),
    literalCase = (attributes, lit) => Literal(g(attributes), lit),
    patternMatchCase = { (attributes, valueToMatch, cases) =>
      val finalCases = cases.map { case (pattern, value) => (pattern.map(g), value) }
      PatternMatch(g(attributes), valueToMatch, finalCases)
    },
    recordCase = (attributes, fields) => Record(g(attributes), fields),
    referenceCase = (attributes, fqName) => Reference(g(attributes), fqName),
    tupleCase = (attributes, elements) => Tuple(g(attributes), elements),
    unitCase = attributes => Unit(g(attributes)),
    updateRecordCase =
      (attributes, valueToUpdate, fieldsToUpdate) => UpdateRecord(g(attributes), valueToUpdate, fieldsToUpdate),
    variableCase = (attributes, name) => Variable(g(attributes), name)
  )
  def toRawValue: RawValue = mapAttributes((_ => ()), (_ => ()))

  final override def toString: String = foldContext(())(Folder.ToString)

  /**
   * Extract the argument list from a curried apply tree. It takes the two arguments of an apply and returns a tuple of
   * the function and a list of arguments.
   *
   * {{{
   *  assert(Apply((), f,a).uncurryApply(b) == (f, List(a, b)))
   * }}}
   */
  def uncurryApply[TB >: TA, VB >: VA](lastArg: Value[TB, VB]): (Value[TB, VB], scala.List[Value[TB, VB]]) =
    self match {
      case Apply(_, nestedFun, nestedArg) =>
        val (f, initArgs) = nestedFun.uncurryApply(nestedArg)
        (f, initArgs :+ lastArg)
      case _ => (self, scala.List(lastArg))
    }
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
        @unused ev: NeedsAttributes[VA]
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

      def apply(function: RawValue, arguments: ::[RawValue]): Raw =
        arguments.tail.foldLeft(Apply(function.attributes, function, arguments.head)) { case (acc, arg) =>
          Apply(acc.attributes, acc, arg)
        }

      def unapply(value: RawValue): Option[(RawValue, RawValue)] = value match {
        case Apply(_, function, argument) => Some((function, argument))
        case _                            => None
      }
    }

    // foo : T1
    // foo(arg) : T2
    // T2 == T1

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

    type Raw = Field[scala.Unit, scala.Unit]
    object Raw {
      def apply(target: RawValue, name: Name): Raw =
        Field(target.attributes, target, name)

      def apply(target: RawValue, name: String): Raw =
        Field(target.attributes, target, Name.fromString(name))

      def unapply(value: RawValue): Option[(RawValue, Name)] = value match {
        case Field(_, target, name) => Some((target, name))
        case _                      => None
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
        case Lambda(_, argumentPattern, body) => Some((argumentPattern, body))
        case _                                => None
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

      // def apply(elements: NonEmptyChunk[TypedValue]): Typed =
      //   // val tpe = sdk.List.listType(elements.head.attributes)
      //   List( /*tpe*/ ???, elements)

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
    def apply[TA, VA](attributes: VA, target: Value[TA, VA], cases: (Pattern[VA], Value[TA, VA])*): Value[TA, VA] =
      PatternMatch(attributes, target, Chunk.fromIterable(cases))

    type Raw = PatternMatch[scala.Unit, scala.Unit]
    object Raw {
      def apply(branchOutOn: RawValue, cases: Chunk[(UPattern, RawValue)]): Raw =
        PatternMatch((), branchOutOn, cases)
      def apply(target: RawValue, cases: (UPattern, RawValue)*): Raw =
        PatternMatch((), target, Chunk.fromIterable(cases))

      def unapply(value: RawValue): Option[(RawValue, Chunk[(UPattern, RawValue)])] =
        value match {
          case PatternMatch(_, target, cases) => Some((target, cases))
          case _                              => None
        }
    }
    type Typed = PatternMatch[scala.Unit, UType]
    object Typed {
      def apply(tpe: UType, target: TypedValue, cases: Chunk[(Pattern[UType], TypedValue)]): Typed =
        PatternMatch(tpe, target, cases)

      def unapply[TA](
          value: Value[TA, Type[TA]]
      ): Option[(Type[TA], Value[TA, Type[TA]], Chunk[(Pattern[Type[TA]], Value[TA, Type[TA]])])] =
        value match {
          case PatternMatch(tpe, target, cases) => Some((tpe, target, cases))
          case _                                => None
        }
    }
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

    final class Builder[TA, VA] private[ir] (private val fields: ChunkBuilder[(Name, Value[TA, VA])]) { self =>
      def +=(field: (String, Value[TA, VA])): Builder[TA, VA] = {
        fields += (Name.fromString(field._1) -> field._2)
        self
      }
      def ++=(fields: Chunk[(Name, Value[TA, VA])]): Builder[TA, VA] = {
        self.fields ++= fields
        self
      }

      def ++=(fields: Map[String, Value[TA, VA]]): Builder[TA, VA] = {
        self.fields ++= Chunk.fromIterable(fields.map { case (name, value) => (Name.fromString(name), value) })
        self
      }
      def addField(name: String, value: Value[TA, VA]): Builder[TA, VA] = {
        fields += (Name.fromString(name) -> value)
        self
      }
      def addField(name: Name, value: Value[TA, VA]): Builder[TA, VA] = {
        fields += (name -> value)
        self
      }
      def result(attributes: VA): Record[TA, VA] = Record(attributes, fields.result())
    }
    object Builder {
      def apply[TA, VA](): Builder[TA, VA] = new Builder(ChunkBuilder.make[(Name, Value[TA, VA])]())
    }
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
        val tpe = Type.Tuple((), elements.map(_.attributes).toList)
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
    def apply[TA, VA](
        attributes: VA,
        valueToUpdate: Value[TA, VA],
        fieldsToUpdate: (String, Value[TA, VA])*
    ): UpdateRecord[TA, VA] =
      UpdateRecord(
        attributes,
        valueToUpdate,
        fieldsToUpdate.map { case (name, value) => (Name.fromString(name), value) }.toMap
      )
    type Raw = UpdateRecord[scala.Unit, scala.Unit]
    object Raw {
      def apply(valueToUpdate: RawValue, fieldsToUpdate: Map[String, RawValue]): Raw =
        UpdateRecord((), valueToUpdate, fieldsToUpdate.map { case (name, value) => (Name.fromString(name), value) })

      def apply(valueToUpdate: RawValue, fieldsToUpdate: (String, RawValue)*): Raw =
        UpdateRecord(
          (),
          valueToUpdate,
          fieldsToUpdate.map { case (name, value) => (Name.fromString(name), value) }.toMap
        )

      def unapply(value: RawValue): Option[(RawValue, Map[String, RawValue])] = value match {
        case UpdateRecord(_, valueToUpdate, fieldsToUpdate) =>
          Some((valueToUpdate, fieldsToUpdate.map { case (name, value) => (name.toString, value) }))
        case _ => None
      }
    }
    type Typed = UpdateRecord[scala.Unit, UType]
    object Typed {
      def apply(valueToUpdate: TypedValue, fieldsToUpdate: Map[Name, TypedValue]): Typed =
        UpdateRecord(valueToUpdate.attributes, valueToUpdate, fieldsToUpdate)

      def apply(valueToUpdate: TypedValue, fieldsToUpdate: (Name, TypedValue)*): Typed =
        UpdateRecord(valueToUpdate.attributes, valueToUpdate, fieldsToUpdate.toMap)

      def unapply(value: TypedValue): Option[(TypedValue, Map[Name, TypedValue])] = value match {
        case UpdateRecord(_, valueToUpdate, fieldsToUpdate) => Some((valueToUpdate, fieldsToUpdate))
        case _                                              => None
      }
    }
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
    def letDefinitionCase(
        context: Context,
        value: Value[TA, VA],
        attributes: VA,
        valueName: Name,
        valueDefinition: (Chunk[(Name, VA, Type[TA])], Type[TA], Z),
        inValue: Z
    ): Z
    def letRecursionCase(
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
      ): Set[FQName] = function union argument

      override def constructorCase(context: Any, value: Value[Any, Any], attributes: Any, name: FQName): Set[FQName] =
        Set.empty

      override def destructureCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          pattern: Pattern[Any],
          valueToDestruct: Set[FQName],
          inValue: Set[FQName]
      ): Set[FQName] = valueToDestruct union inValue

      override def fieldCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          subjectValue: Set[FQName],
          fieldName: Name
      ): Set[FQName] = subjectValue

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
      ): Set[FQName] = condition union thenBranch union elseBranch

      override def lambdaCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          argumentPattern: Pattern[Any],
          body: Set[FQName]
      ): Set[FQName] = body

      override def letDefinitionCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          valueName: Name,
          valueDefinition: (Chunk[(Name, Any, Type[Any])], Type[Any], Set[FQName]),
          inValue: Set[FQName]
      ): Set[FQName] = valueDefinition._3 union inValue

      override def letRecursionCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          valueDefinitions: Map[Name, (Chunk[(Name, Any, Type[Any])], Type[Any], Set[FQName])],
          inValue: Set[FQName]
      ): Set[FQName] = valueDefinitions.values.foldLeft(inValue) { case (acc, (_, _, inBody)) => acc ++ inBody }

      override def listCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          elements: Chunk[Set[FQName]]
      ): Set[FQName] = elements.fold(Set.empty)(_ ++ _)

      override def literalCase(context: Any, value: Value[Any, Any], attributes: Any, literal: Lit): Set[FQName] =
        Set.empty

      override def patternMatchCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          branchOutOn: Set[FQName],
          cases: Chunk[(Pattern[Any], Set[FQName])]
      ): Set[FQName] = cases.foldLeft(branchOutOn) { case (acc, (_, value)) => acc ++ value }

      override def recordCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          fields: Chunk[(Name, Set[FQName])]
      ): Set[FQName] = fields.foldLeft[Set[FQName]](Set.empty) { case (acc, (_, value)) => acc ++ value }

      override def referenceCase(context: Any, value: Value[Any, Any], attributes: Any, name: FQName): Set[FQName] =
        Set(name)

      override def tupleCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          elements: Chunk[Set[FQName]]
      ): Set[FQName] = elements.fold(Set.empty)(_ ++ _)

      override def unitCase(context: Any, value: Value[Any, Any], attributes: Any): Set[FQName] = Set.empty

      override def updateRecordCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          valueToUpdate: Set[FQName],
          fieldsToUpdate: Map[Name, Set[FQName]]
      ): Set[FQName] = fieldsToUpdate.values.foldLeft(valueToUpdate)(_ ++ _)

      override def variableCase(context: Any, value: Value[Any, Any], attributes: Any, name: Name): Set[FQName] =
        Set.empty

    }

    object CollectVariables extends Folder[Any, Any, Any, Set[Name]] {

      override def applyCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          function: Set[Name],
          argument: Set[Name]
      ): Set[Name] = function union argument

      override def constructorCase(context: Any, value: Value[Any, Any], attributes: Any, name: FQName): Set[Name] =
        Set.empty

      override def destructureCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          pattern: Pattern[Any],
          valueToDestruct: Set[Name],
          inValue: Set[Name]
      ): Set[Name] = valueToDestruct union inValue

      override def fieldCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          subjectValue: Set[Name],
          fieldName: Name
      ): Set[Name] = subjectValue

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
      ): Set[Name] = condition union thenBranch union elseBranch

      override def lambdaCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          argumentPattern: Pattern[Any],
          body: Set[Name]
      ): Set[Name] = body

      override def letDefinitionCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          valueName: Name,
          valueDefinition: (Chunk[(Name, Any, Type[Any])], Type[Any], Set[Name]),
          inValue: Set[Name]
      ): Set[Name] = {
        val (_, _, inBody) = valueDefinition
        inBody union inValue
      }

      override def letRecursionCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          valueDefinitions: Map[Name, (Chunk[(Name, Any, Type[Any])], Type[Any], Set[Name])],
          inValue: Set[Name]
      ): Set[Name] =
        valueDefinitions.foldLeft(inValue) { case (acc, (name, (_, _, inBody))) => acc union inBody union Set(name) }

      override def listCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          elements: Chunk[Set[Name]]
      ): Set[Name] = elements.fold(Set.empty)(_ ++ _)

      override def literalCase(context: Any, value: Value[Any, Any], attributes: Any, literal: Lit): Set[Name] =
        Set.empty

      override def patternMatchCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          branchOutOn: Set[Name],
          cases: Chunk[(Pattern[Any], Set[Name])]
      ): Set[Name] = cases.foldLeft(branchOutOn) { case (acc, (_, value)) => acc ++ value }

      override def recordCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          fields: Chunk[(Name, Set[Name])]
      ): Set[Name] = fields.map(_._2).foldLeft(Set.empty[Name])(_ ++ _)

      override def referenceCase(context: Any, value: Value[Any, Any], attributes: Any, name: FQName): Set[Name] =
        Set.empty

      override def tupleCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          elements: Chunk[Set[Name]]
      ): Set[Name] = elements.fold(Set.empty)(_ ++ _)

      override def unitCase(context: Any, value: Value[Any, Any], attributes: Any): Set[Name] = Set.empty

      override def updateRecordCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          valueToUpdate: Set[Name],
          fieldsToUpdate: Map[Name, Set[Name]]
      ): Set[Name] = fieldsToUpdate.values.foldLeft(valueToUpdate)(_ ++ _)

      override def variableCase(context: Any, value: Value[Any, Any], attributes: Any, name: Name): Set[Name] = Set(
        name
      )

    }

    object ToString extends Folder[Any, Any, Any, String] {

      override def applyCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          function: String,
          argument: String
      ): String = s"$function $argument"

      override def constructorCase(context: Any, value: Value[Any, Any], attributes: Any, name: FQName): String =
        name.toReferenceName

      override def destructureCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          pattern: Pattern[Any],
          valueToDestruct: String,
          inValue: String
      ): String = s"let $pattern = $valueToDestruct in $inValue"

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
      ): String = s"if $condition then $thenBranch else $elseBranch"

      override def lambdaCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          argumentPattern: Pattern[Any],
          body: String
      ): String = s"(\\$argumentPattern -> $body)"

      override def letDefinitionCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          valueName: Name,
          valueDefinition: (Chunk[(Name, Any, Type[Any])], Type[Any], String),
          inValue: String
      ): String = {
        val (inputTypes, _, body) = valueDefinition
        val args                  = inputTypes.map(_._1.toCamelCase).mkString(" ")
        s"let ${valueName.toCamelCase}$args = $body in $inValue"
      }

      override def letRecursionCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          valueDefinitions: Map[Name, (Chunk[(Name, Any, Type[Any])], Type[Any], String)],
          inValue: String
      ): String = {
        val defs = valueDefinitions
          .map { case (name, (inputTypes, _, body)) =>
            val args = inputTypes.map(_._1.toCamelCase).mkString(" ")
            s"${name.toCamelCase}$args = $body"
          }
          .mkString("; ")
        s"let $defs in $inValue"
      }

      override def listCase(context: Any, value: Value[Any, Any], attributes: Any, elements: Chunk[String]): String =
        elements.mkString("[", ", ", "]")

      override def literalCase(context: Any, value: Value[Any, Any], attributes: Any, literal: Lit): String =
        literal.toString

      override def patternMatchCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          branchOutOn: String,
          cases: Chunk[(Pattern[Any], String)]
      ): String = {
        val casesStr = cases.map { case (pattern, value) => s"$pattern -> $value" }.mkString("; ")
        s"case $branchOutOn of $casesStr"
      }

      override def recordCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          fields: Chunk[(Name, String)]
      ): String = fields
        .map { case (fieldName, fieldValue) => s"${fieldName.toCamelCase} = $fieldValue" }
        .mkString("{", ", ", "}")

      override def referenceCase(context: Any, value: Value[Any, Any], attributes: Any, name: FQName): String = Seq(
        Path.toString(Name.toTitleCase, ".", name.packagePath.toPath),
        Path.toString(Name.toTitleCase, ".", name.modulePath.toPath),
        name.localName.toCamelCase
      ).mkString(".")

      override def tupleCase(context: Any, value: Value[Any, Any], attributes: Any, elements: Chunk[String]): String =
        elements.mkString("(", ", ", ")")

      override def unitCase(context: Any, value: Value[Any, Any], attributes: Any): String = "()"

      override def updateRecordCase(
          context: Any,
          value: Value[Any, Any],
          attributes: Any,
          valueToUpdate: String,
          fieldsToUpdate: Map[Name, String]
      ): String = {
        val fieldsString = fieldsToUpdate
          .map { case (fieldName, fieldValue) => s"${fieldName.toCamelCase} = $fieldValue" }
          .mkString(", ")
        s"{ $valueToUpdate | $fieldsString }"
      }

      override def variableCase(context: Any, value: Value[Any, Any], attributes: Any, name: Name): String =
        name.toCamelCase

    }

    class DelegatedFolder[-Context, -TA, -VA, Z](
        onApplyCase: (Context, Value[TA, VA], VA, Z, Z) => Z,
        onConstructorCase: (Context, Value[TA, VA], VA, FQName) => Z,
        onDestructureCase: (Context, Value[TA, VA], VA, Pattern[VA], Z, Z) => Z,
        onFieldCase: (Context, Value[TA, VA], VA, Z, Name) => Z,
        onFieldFunctionCase: (Context, Value[TA, VA], VA, Name) => Z,
        onIfThenElseCase: (Context, Value[TA, VA], VA, Z, Z, Z) => Z,
        onLambdaCase: (Context, Value[TA, VA], VA, Pattern[VA], Z) => Z,
        onLetDefinitionCase: (Context, Value[TA, VA], VA, Name, (Chunk[(Name, VA, Type[TA])], Type[TA], Z), Z) => Z,
        onLetRecursionCase: (Context, Value[TA, VA], VA, Map[Name, (Chunk[(Name, VA, Type[TA])], Type[TA], Z)], Z) => Z,
        onListCase: (Context, Value[TA, VA], VA, Chunk[Z]) => Z,
        onLiteralCase: (Context, Value[TA, VA], VA, Lit) => Z,
        onPatternMatchCase: (Context, Value[TA, VA], VA, Z, Chunk[(Pattern[VA], Z)]) => Z,
        onRecordCase: (Context, Value[TA, VA], VA, Chunk[(Name, Z)]) => Z,
        onReferenceCase: (Context, Value[TA, VA], VA, FQName) => Z,
        onTupleCase: (Context, Value[TA, VA], VA, Chunk[Z]) => Z,
        onUnitCase: (Context, Value[TA, VA], VA) => Z,
        onUpdateRecordCase: (Context, Value[TA, VA], VA, Z, Map[Name, Z]) => Z,
        onVariableCase: (Context, Value[TA, VA], VA, Name) => Z
    ) extends Folder[Context, TA, VA, Z] {

      override def applyCase(context: Context, value: Value[TA, VA], attributes: VA, function: Z, argument: Z): Z =
        onApplyCase(
          context,
          value,
          attributes,
          function,
          argument
        )

      override def constructorCase(context: Context, value: Value[TA, VA], attributes: VA, name: FQName): Z =
        onConstructorCase(context, value, attributes, name)

      override def destructureCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          pattern: Pattern[VA],
          valueToDestruct: Z,
          inValue: Z
      ): Z = onDestructureCase(context, value, attributes, pattern, valueToDestruct, inValue)

      override def fieldCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          subjectValue: Z,
          fieldName: Name
      ): Z = onFieldCase(context, value, attributes, subjectValue, fieldName)

      override def fieldFunctionCase(context: Context, value: Value[TA, VA], attributes: VA, fieldName: Name): Z =
        onFieldFunctionCase(context, value, attributes, fieldName)

      override def ifThenElseCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          condition: Z,
          thenBranch: Z,
          elseBranch: Z
      ): Z = onIfThenElseCase(context, value, attributes, condition, thenBranch, elseBranch)

      override def lambdaCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          argumentPattern: Pattern[VA],
          body: Z
      ): Z = onLambdaCase(context, value, attributes, argumentPattern, body)

      override def letDefinitionCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          valueName: Name,
          valueDefinition: (Chunk[(Name, VA, Type[TA])], Type[TA], Z),
          inValue: Z
      ): Z = onLetDefinitionCase(context, value, attributes, valueName, valueDefinition, inValue)

      override def letRecursionCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          valueDefinitions: Map[Name, (Chunk[(Name, VA, Type[TA])], Type[TA], Z)],
          inValue: Z
      ): Z = onLetRecursionCase(context, value, attributes, valueDefinitions, inValue)

      override def listCase(context: Context, value: Value[TA, VA], attributes: VA, elements: Chunk[Z]): Z = onListCase(
        context,
        value,
        attributes,
        elements
      )

      override def literalCase(context: Context, value: Value[TA, VA], attributes: VA, literal: Lit): Z = onLiteralCase(
        context,
        value,
        attributes,
        literal
      )

      override def patternMatchCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          branchOutOn: Z,
          cases: Chunk[(Pattern[VA], Z)]
      ): Z = onPatternMatchCase(context, value, attributes, branchOutOn, cases)

      override def recordCase(context: Context, value: Value[TA, VA], attributes: VA, fields: Chunk[(Name, Z)]): Z =
        onRecordCase(
          context,
          value,
          attributes,
          fields
        )

      override def referenceCase(context: Context, value: Value[TA, VA], attributes: VA, name: FQName): Z =
        onReferenceCase(context, value, attributes, name)

      override def tupleCase(context: Context, value: Value[TA, VA], attributes: VA, elements: Chunk[Z]): Z =
        onTupleCase(
          context,
          value,
          attributes,
          elements
        )

      override def unitCase(context: Context, value: Value[TA, VA], attributes: VA): Z =
        onUnitCase(context, value, attributes)

      override def updateRecordCase(
          context: Context,
          value: Value[TA, VA],
          attributes: VA,
          valueToUpdate: Z,
          fieldsToUpdate: Map[Name, Z]
      ): Z = onUpdateRecordCase(
        context,
        value,
        attributes,
        valueToUpdate,
        fieldsToUpdate
      )

      override def variableCase(context: Context, value: Value[TA, VA], attributes: VA, name: Name): Z = onVariableCase(
        context,
        value,
        attributes,
        name
      )

    }
  }

  implicit final class StringExtensions(private val self: String) extends AnyVal {
    def as(tpe: UType): TypedValue = Variable.Typed(self, tpe)
    def :=(value: TypedValue): LetDefinition.Unbound[Any, UType] =
      LetDefinition.Unbound(Name.fromString(self), Definition.fromTypedValue(value))

    def :=(value: Int): LetDefinition.Unbound[Any, UType] =
      LetDefinition.Unbound(Name.fromString(self), Definition.fromLiteral(Lit.int(value)))

    def :=(value: Long): LetDefinition.Unbound[Any, UType] =
      LetDefinition.Unbound(Name.fromString(self), Definition.fromLiteral(Lit.long(value)))

    def :=(value: Float): LetDefinition.Unbound[Any, UType] =
      LetDefinition.Unbound(Name.fromString(self), Definition.fromLiteral(Lit.float(value)))

    def :=(value: Double): LetDefinition.Unbound[Any, UType] =
      LetDefinition.Unbound(Name.fromString(self), Definition.fromLiteral(Lit.double(value)))

    def :=(value: Boolean): LetDefinition.Unbound[Any, UType] =
      LetDefinition.Unbound(Name.fromString(self), Definition.fromLiteral(Lit.boolean(value)))

    def :=(value: String): LetDefinition.Unbound[Any, UType] =
      LetDefinition.Unbound(Name.fromString(self), Definition.fromLiteral(Lit.string(value)))

  }

}
