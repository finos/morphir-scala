package org.finos.morphir
package ir

import org.finos.morphir.ir.sdk.List.listType
import zio.{Chunk, NonEmptyChunk}

trait ValueModule extends ValueAdtModule with PatternModule with LiteralModule { module =>
  import Type.{Type, UType}
  import Value.{Literal => LiteralValue, _}

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
    List( /*listType(elementType)*/ ???, elements: _*)

  final def listOf(elements: RawValue*)(elementType: UType): TypedValue =
    List( /*listType(elementType)*/ ???, elements.map(e => (e :> elementType)): _*)

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
