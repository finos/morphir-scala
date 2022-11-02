package org.finos.morphir
package ir

import Literal._
import zio.Chunk
import org.finos.morphir.ir.internal.PatternModule
import org.finos.morphir.ir.sdk.List.listType
import internal.{ValueDefinition, ValueSpecification}

/**
 * In functional programming data and logic are treated the same way and we refer to both as values. This module
 * provides the building blocks for those values (data and logic) in the Morphir IR.
 *
 * If you use Elm as your frontend language for Morphir then you should think about all the logic and constant values
 * that you can put in the body of a function. Here are a few examples:
 *
 * {{{
 * myThreshold =
 *     1000
 *
 * min a b =
 *   if a < b then
 *     a
 *   else b
 *
 * addTwo a = a + 2
 * }}}
 *
 * All the above are values: the first one is just data, the second one is logic and the last one has both logic and
 * data. In either case each value is represented by a [`Value`](#Value) expression. This is a recursive data structure
 * with various node types representing each possible language construct. You can check out the documentation for values
 * below to find more details. Here are the Morphir IR snippets for the above values as a quick reference:
 *
 * {{{
 * myThreshold = Literal () (WholeNumberLiteral 1000)
 *
 * min a b = IfThenElse () (Apply () (Apply () (Reference () (fqn "Morphir.SDK" "Basics" "lessThan")) (Variable () [ "a"
 * ]) ) (Variable () [ "b" ]) ) (Variable () [ "a" ]) (Variable () [ "b" ])
 *
 * addTwo a = Apply () (Apply () (Reference () (fqn "Morphir.SDK" "Basics" "add")) (Variable () [ "a" ]) ) (Literal ()
 * (WholeNumberLiteral 2))
 * }}}
 */
object Value extends internal.PatternModule {
  final type Value[+TA, +VA] = internal.Value[TA, VA]
  final val Value: internal.Value.type = internal.Value

  final type Definition[+TA, +VA] = ValueDefinition[TA, VA]
  final val Definition: ValueDefinition.type = ValueDefinition

  final type Specification[+TA] = ValueSpecification[TA]
  final val Specification: ValueSpecification.type = ValueSpecification

  final type RawValue       = Value.RawValue
  final type TypedValue     = Value.TypedValue
  final type USpecification = ValueSpecification[scala.Unit]

  import Type.{Type, UType}
  import Value.{Literal => LiteralValue, _}

  final def apply[TA, VA](
      attributes: VA,
      function: Value[TA, VA],
      argument: Value[TA, VA],
      arguments: Value[TA, VA]*
  )(implicit ev: NeedsAttributes[VA]): Value[TA, VA] =
    Apply(attributes, function, argument, arguments: _*)

  final def apply(function: RawValue, argument: RawValue): RawValue = Apply.Raw(function, argument)

  // final def apply(function: TypedValue, argument: TypedValue, arguments: TypedValue*): TypedValue =
  //   Apply.Typed(function, argument, arguments: _*)

  final def boolean[A](attributes: A, value: Boolean): Value[Nothing, A] = LiteralValue(attributes, boolLiteral(value))
  final def boolean(value: Boolean): RawValue                            = LiteralValue.Raw(boolLiteral(value))

  final def call[TA, VA](function: Value[TA, VA], attributes: VA)(arguments: Value[TA, VA]*)(implicit
      ev: NeedsAttributes[VA]
  ): Value[TA, VA] = Apply(attributes, function, arguments.toList)

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
  final def literal(value: String): TypedValue = {
    val literal = Lit.string(value)
    LiteralValue(literal.inferredType, literal)
  }
  final def literal(value: Int): TypedValue = {
    val literal = Lit.int(value)
    LiteralValue(literal.inferredType, literal)
  }

  final def literal(value: Long): TypedValue = {
    val literal = Lit.long(value)
    LiteralValue(literal.inferredType, literal)
  }

  final def literal(value: Float): TypedValue = {
    val literal = Lit.float(value)
    LiteralValue(literal.inferredType, literal)
  }

  final def literal(value: Double): TypedValue = {
    val literal = Lit.double(value)
    LiteralValue(literal.inferredType, literal)
  }

  final def literal(value: Boolean): TypedValue = {
    val literal = Lit.boolean(value)
    LiteralValue(literal.inferredType, literal)
  }

  final def literal(value: scala.BigDecimal): TypedValue = {
    val literal = Lit.decimal(value)
    LiteralValue(literal.inferredType, literal)
  }

  final def literal(value: java.math.BigDecimal): TypedValue = {
    val literal = Lit.decimal(value)
    LiteralValue(literal.inferredType, literal)
  }

  // final def literal(value: java.math.BigInteger): TypedValue = {
  //   val literal = Lit.wholeNumber(value)
  //   LiteralValue(literal.inferredType, literal)
  // }

  // final def literal(value: scala.BigInt): TypedValue = {
  //   val literal = Lit.wholeNumber(value)
  //   LiteralValue(literal.inferredType, literal)
  // }

  final def literalTyped[A](literal: Lit): TypedValue = literal.toTypedValue

  final def record[VA](attributes: VA)(implicit ev: NeedsAttributes[VA]): RecordPartiallyApplied[VA] =
    new RecordPartiallyApplied(
      attributes
    )

  final def record: RecordWithoutAttributesPartiallyApplied = new RecordWithoutAttributesPartiallyApplied

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

  final def toRawValue(literal: Lit): RawValue = LiteralValue.Raw(literal)
  final def toTypedValue(literal: Lit): TypedValue = {
    val tpe = literal.inferredType
    LiteralValue.Typed(tpe, literal)
  }

  final def tuple[TA, VA](attributes: VA, elements: Chunk[Value[TA, VA]]): Value[TA, VA] = Tuple(attributes, elements)
  final def tuple[TA, VA](attributes: VA, first: Value[TA, VA], second: Value[TA, VA], otherElements: Value[TA, VA]*)(
      implicit ev: IsNotAValue[VA]
  ): Value[TA, VA] = Tuple(attributes, first +: second +: Chunk.fromIterable(otherElements))

  final def tuple(elements: RawValue*): RawValue       = Tuple.Raw(elements: _*)
  final def tuple(elements: Chunk[RawValue]): RawValue = Tuple.Raw(elements)
  final def tuple(element: (RawValue, UType), elements: (RawValue, UType)*): TypedValue =
    Tuple.Typed(Chunk.fromIterable((element +: elements).map { case (v, t) => v :> t }))

  def unit[VA](attributes: VA)(implicit ev: NeedsAttributes[VA]): Value[Nothing, VA] = Unit(attributes)
  final val unit: RawValue                                                           = Unit(())

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

  implicit class RawValueExtensions(private val self: RawValue) extends AnyVal {

    /**
     * Ascribe the given type to this `RawValue` and all its children.
     * ===NOTE===
     * This is a recursive operation and all children of this `RawValue` will also be ascribed with the given value.
     */
    def :>(ascribedType: UType): TypedValue = self.mapAttributes(identity, _ => ascribedType)

    def toValDef(returnType: UType): Definition[Any, UType] = Definition(returnType, self :> returnType)
  }

  class RecordPartiallyApplied[VA](val attributes: VA) extends AnyVal {
    def apply[TA](fields: (String, Value[TA, VA])*): Value[TA, VA] = Value.Record(attributes, fields: _*)
    def apply[TA](fields: Map[Name, Value[TA, VA]]): Value[TA, VA] = Value.Record.fromMap(attributes, fields)
    def withFields[TA](fields: Seq[(Name, Value[TA, VA])]): Value[TA, VA] =
      Value.Record(attributes, Chunk.fromIterable(fields))
  }

  class RecordWithoutAttributesPartiallyApplied(val dummy: Boolean = false) extends AnyVal {
    def apply[TA](fields: (String, Value[TA, scala.Unit])*): Value[TA, scala.Unit] = Value.Record((), fields: _*)
    def apply[TA](fields: Map[Name, Value[TA, scala.Unit]]): Value[TA, scala.Unit] = Value.Record.fromMap((), fields)
    def withFields[TA](fields: Seq[(Name, Value[TA, scala.Unit])]): Value[TA, scala.Unit] =
      Value.Record(Chunk.fromIterable(fields))
  }
}
