package org.finos.morphir
package ir

import Literal._
import zio.Chunk
import org.finos.morphir.naming._
import org.finos.morphir.ir.internal.PatternModule
import org.finos.morphir.ir.sdk.List.listType
import internal.{ValueDefinition, ValueSpecification}
import org.finos.morphir.ir.{Type => T, Value => V}
import scala.annotation.unused

/*
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

  final type RawValue        = Value.RawValue
  final type TypedValue      = Value.TypedValuex
  final type USpecification  = ValueSpecification[scala.Unit]
  final type TypedDefinition = ValueDefinition[scala.Unit, Type.UType]

  import Type.{Type, UType}
  import Value.{Literal => LiteralValue, _}

  final def apply[TA, VA](
      attributes: VA,
      function: Value[TA, VA],
      argument: Value[TA, VA],
      arguments: Value[TA, VA]*
  )(implicit @unused ev: IsNotAValue[VA]): Value[TA, VA] =
    Apply(attributes, function, argument, arguments: _*)

  final def apply(function: RawValue, argument: RawValue): Apply.Raw = Apply.Raw(function, argument)
  final def apply(function: RawValue, argument: RawValue, arguments: RawValue*) =
    Apply.Raw(function, ::(argument, arguments.toList))

  final def applyInferType(finalType: UType, core: RawValue, args: TypedValue*): TypedValue = {
    def loop(outerType: UType, core: RawValue, reversedArgs: TypedValue*): TypedValue =
      reversedArgs match {
        case Nil => core :> outerType
        case head :: tail =>
          val f = loop(T.Type.Function((), head.attributes, outerType), core, tail: _*)
          Apply(outerType, f, head)
      }
    loop(finalType, core, args.toList.reverse: _*)
  }

  // final def apply(function: TypedValue, argument: TypedValue, arguments: TypedValue*): TypedValue =
  //   Apply.Typed(function, argument, arguments: _*)

  final def boolean[A](attributes: A, value: Boolean): Value[Nothing, A] = LiteralValue(attributes, boolLiteral(value))
  final def boolean(value: Boolean): RawValue                            = LiteralValue.Raw(boolLiteral(value))

  final def call[TA, VA](function: Value[TA, VA], attributes: VA)(arguments: Value[TA, VA]*)(implicit
      ev: NeedsAttributes[VA]
  ): Value[TA, VA] = Apply(attributes, function, arguments.toList)

  final def caseOf[TA, VA](
      value: Value[TA, VA]
  )(firstCase: (Pattern[VA], Value[TA, VA]), otherCases: (Pattern[VA], Value[TA, VA])*): Value[TA, VA] =
    PatternMatch(value.attributes, value, firstCase +: Chunk.fromIterable(otherCases))

  final def constructor[A](attributes: A, name: String): Value[Nothing, A] = Constructor(attributes, name)
  final def constructor[A](attributes: A, name: FQName): Value[Nothing, A] = Constructor(attributes, name)
  final def constructor(name: String): RawValue                            = Constructor.Raw(name)
  final def constructor(name: FQName): RawValue                            = Constructor.Raw(name)
  final def constructor(name: String, tpe: UType): TypedValue              = Constructor.Typed(name, tpe)
  final def constructor(name: FQName, tpe: UType): TypedValue              = Constructor.Typed(name, tpe)

  final def decimal[A](attributes: A, value: BigDecimal)(implicit @unused ev: NeedsAttributes[A]): Value[Nothing, A] =
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

  final def emptyTuple[VA](attributes: VA): Value[Nothing, VA] = Tuple(attributes)

  final def field[TA, VA](attributes: VA, target: Value[TA, VA], name: Name): Value[TA, VA] =
    Field(attributes, target, name)

  final def field[TA, VA](attributes: VA, target: Value[TA, VA], name: String): Value[TA, VA] =
    Field(attributes, target, name)

  final def field(target: RawValue, name: Name): RawValue = Field.Raw(target, name)

  final def field(target: RawValue, name: String): RawValue = Field.Raw(target, name)

  final def fields[TA, VA](all: (String, Value[TA, VA])*): Chunk[(Name, Value[TA, VA])] =
    Chunk.fromIterable(all.map { case (name, value) => (Name.fromString(name), value) })

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

  def functionDef(
      firstArg: (String, UType),
      otherArgs: (String, UType)*
  ): FunctionDefInputsClause[scala.Unit, UType] = {
    val args = (firstArg +: Chunk.fromIterable(otherArgs)).map { case (name, tpe) =>
      (Name.fromString(name), tpe, tpe)
    }
    new FunctionDefInputsClause(args)
  }

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
  final def intTyped(value: Int): TypedValue =
    LiteralValue.Typed(sdk.Basics.intType, Lit.int(value))

  final def long[A](attributes: A, value: Long): Value[Nothing, A] = LiteralValue(attributes, Lit.long(value))

  final def lambda[TA, VA](attributes: VA, argumentPattern: Pattern[VA], body: Value[TA, VA]): Value[TA, VA] =
    Lambda(attributes, argumentPattern, body)

  final def lambda(argumentPattern: UPattern, body: RawValue): RawValue = Lambda.Raw(argumentPattern, body)

  final def let[TA, VA](
      attributes: VA,
      name: String,
      valueDefinition: Definition[TA, VA],
      body: Value[TA, VA]
  ): Value[TA, VA] =
    LetDefinition(attributes, name, valueDefinition, body)

  final def let[TA, VA](
      attributes: VA,
      name: Name,
      valueDefinition: Definition[TA, VA],
      body: Value[TA, VA]
  ): Value[TA, VA] =
    LetDefinition(attributes, name, valueDefinition, body)

  final def let(name: Name, valueDefinition: Definition.Raw, body: RawValue): RawValue =
    LetDefinition.Raw(name, valueDefinition, body)

  final def let(name: String, valueDefinition: Definition.Raw, body: RawValue): RawValue =
    LetDefinition.Raw(name, valueDefinition, body)

  final def let(varName: String, value: Int, block: TypedValue): TypedValue = {
    val literalValue = literal(value)
    val vDef         = valueDef(literalValue.attributes).withBody(literalValue)
    LetDefinition.Typed(varName, vDef, block)
  }

  final def let(varName: String, value: String, block: TypedValue): TypedValue = {
    val literalValue = literal(value)
    val vDef         = valueDef(literalValue.attributes).withBody(literalValue)
    LetDefinition.Typed(varName, vDef, block)
  }

  final def let(varName: String, value: Boolean, block: TypedValue): TypedValue = {
    val literalValue = literal(value)
    val vDef         = valueDef(literalValue.attributes).withBody(literalValue)
    LetDefinition.Typed(varName, vDef, block)
  }

  final def let(varName: String, value: Float, block: TypedValue): TypedValue = {
    val literalValue = literal(value)
    val vDef         = valueDef(literalValue.attributes).withBody(literalValue)
    LetDefinition.Typed(varName, vDef, block)
  }

  final def let(varName: String, value: Double, block: TypedValue): TypedValue = {
    val literalValue = literal(value)
    val vDef         = valueDef(literalValue.attributes).withBody(literalValue)
    LetDefinition.Typed(varName, vDef, block)
  }

  final def let(varName: String, value: scala.BigDecimal, block: TypedValue): TypedValue = {
    val literalValue = literal(value)
    val vDef         = valueDef(literalValue.attributes).withBody(literalValue)
    LetDefinition.Typed(varName, vDef, block)
  }

  final def let(varName: String, value: java.math.BigDecimal, block: TypedValue): TypedValue = {
    val literalValue = literal(value)
    val vDef         = valueDef(literalValue.attributes).withBody(literalValue)
    LetDefinition.Typed(varName, vDef, block)
  }

  // final def let(varName: String, value: scala.BigInt, block: TypedValue): TypedValue = {
  //   val literalValue = literal(value)
  //   val vDef         = valueDef(literalValue.attributes).withBody(literalValue)
  //   LetDefinition.Typed(varName, vDef, block)
  // }

  // final def let(varName: String, value: java.math.BigInteger, block: TypedValue): TypedValue = {
  //   val literalValue = literal(value)
  //   val vDef         = valueDef(literalValue.attributes).withBody(literalValue)
  //   LetDefinition.Typed(varName, vDef, block)
  // }

  final def letDef[TA, VA](
      attributes: VA,
      name: Name,
      valueDefinition: Definition[TA, VA],
      body: Value[TA, VA]
  ): Value[TA, VA] =
    LetDefinition(attributes, name, valueDefinition, body)

  final def letDef[TA, VA](
      attributes: VA,
      name: String,
      valueDefinition: Definition[TA, VA],
      body: Value[TA, VA]
  ): Value[TA, VA] =
    LetDefinition(attributes, name, valueDefinition, body)

  final def letDef(name: Name, valueDefinition: Definition.Raw, body: RawValue): RawValue =
    LetDefinition.Raw(name, valueDefinition, body)

  final def letDef(name: String, valueDefinition: Definition.Raw, body: RawValue): RawValue =
    LetDefinition.Raw(name, valueDefinition, body)

  final def letDestruct[TA, VA](
      attributes: VA,
      pattern: Pattern[VA],
      valueToDestruct: Value[TA, VA],
      inValue: Value[TA, VA]
  ): Value[TA, VA] = Destructure(attributes, pattern, valueToDestruct, inValue)

  final def letDestruct(pattern: UPattern, valueToDestruct: RawValue, inValue: RawValue): RawValue =
    Destructure.Raw(pattern, valueToDestruct, inValue)

  final def letRec[TA, VA](
      attributes: VA,
      valueDefinitions: Map[Name, Definition[TA, VA]],
      inValue: Value[TA, VA]
  ): Value[TA, VA] =
    LetRecursion(attributes, valueDefinitions, inValue)

  final def letRec[TA, VA](attributes: VA, valueDefinitions: (String, Definition[TA, VA])*)(
      inValue: Value[TA, VA]
  ): Value[TA, VA] =
    LetRecursion(attributes, valueDefinitions: _*)(inValue)

  final def letRec(valueDefinitions: Map[Name, Definition.Raw], inValue: RawValue): RawValue =
    LetRecursion.Raw(valueDefinitions, inValue)

  final def letRec(valueDefinitions: (String, Definition.Raw)*)(inValue: RawValue): RawValue =
    LetRecursion.Raw(valueDefinitions: _*)(inValue)

  final def list[TA, VA](attributes: VA, values: Chunk[Value[TA, VA]]): Value[TA, VA] =
    List(attributes, values)

  final def list[TA, VA](attributes: VA, values: Value[TA, VA]*)(implicit @unused ev: IsNotAValue[VA]): Value[TA, VA] =
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

  final def patternMatch[TA, VA](
      attributes: VA,
      branchOutOn: Value[TA, VA],
      cases: Chunk[(Pattern[VA], Value[TA, VA])]
  ): Value[TA, VA] =
    PatternMatch(attributes, branchOutOn, cases)

  final def patternMatch[TA, VA](
      attributes: VA,
      branchOutOn: Value[TA, VA],
      cases: (Pattern[VA], Value[TA, VA])*
  ): Value[TA, VA] =
    PatternMatch(attributes, branchOutOn, cases: _*)

  final def patternMatch(branchOutOn: RawValue, cases: Chunk[(UPattern, RawValue)]): RawValue =
    PatternMatch.Raw(branchOutOn, cases)

  final def patternMatch(branchOutOn: RawValue, cases: (UPattern, RawValue)*): RawValue =
    PatternMatch.Raw(branchOutOn, cases: _*)

  final def record[TA, VA](attributes: VA, firstField: (String, Value[TA, VA]), fields: (String, Value[TA, VA])*)(
      implicit
      @unused ev: Not[VA =:= (String, Value[TA, VA])],
      @unused ev1: NeedsAttributes[VA]
  ): Value[TA, VA] =
    Record(attributes, (firstField +: fields): _*)

  final def record[TA, VA](attributes: VA, f: Record.Builder[TA, VA] => Any)(implicit
      @unused ev: IsNotAValue[VA]
  ): Value[TA, VA] = {
    val builder = Record.Builder[TA, VA]()
    f(builder)
    builder.result(attributes)
  }

  // final def record(fields: Chunk[(Name, RawValue)]): RawValue = Record.Raw(fields)
  final def recordRaw(fields: (String, RawValue)*): RawValue = Record.Raw(fields: _*)
  final def recordRaw(f: Record.Builder[scala.Unit, scala.Unit] => Any): RawValue = {
    val builder = Record.Builder[scala.Unit, scala.Unit]()
    f(builder)
    builder.result(())
  }
  // final def record(firstField: (Name, RawValue), otherFields: (Name, RawValue)*): RawValue =
  //   Record.Raw(firstField +: Chunk.fromIterable(otherFields))

  def reference[VA](attributes: VA, fullyQualifiedName: FQName)(implicit
      @unused ev: NeedsAttributes[VA]
  ): Value[Nothing, VA] =
    Reference(attributes, fullyQualifiedName)

  final def reference[A](attributes: A, fullyQualifiedName: String)(implicit
      @unused ev: NeedsAttributes[A]
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
      implicit @unused ev: IsNotAValue[VA]
  ): Value[TA, VA] = Tuple(attributes, first +: second +: Chunk.fromIterable(otherElements))

  final def tuple(elements: RawValue*): RawValue       = Tuple.Raw(elements: _*)
  final def tuple(elements: Chunk[RawValue]): RawValue = Tuple.Raw(elements)
  final def tuple(element: (RawValue, UType), elements: (RawValue, UType)*): TypedValue =
    Tuple.Typed(Chunk.fromIterable((element +: elements).map { case (v, t) => v :> t }))

  def unit[VA](attributes: VA)(implicit @unused ev: NeedsAttributes[VA]): Value[Nothing, VA] = Unit(attributes)
  final val unit: RawValue                                                                   = Unit(())
  lazy val unitTyped: TypedValue                                                             = Unit.Typed(T.unit)

  def update[TA, VA](attributes: VA, valueToUpdate: Value[TA, VA], fieldsToUpdate: Map[Name, Value[TA, VA]])(implicit
      @unused ev: NeedsAttributes[VA]
  ): Value[TA, VA] = UpdateRecord(attributes, valueToUpdate, fieldsToUpdate)

  final def update[TA, VA](
      attributes: VA,
      valueToUpdate: Value[TA, VA],
      fields: Chunk[(Name, Value[TA, VA])]
  ): Value[TA, VA] =
    UpdateRecord(attributes, valueToUpdate, fields.toMap)

  final def update[TA, VA](
      attributes: VA,
      valueToUpdate: Value[TA, VA],
      fields: (String, Value[TA, VA])*
  ): Value[TA, VA] =
    UpdateRecord(attributes, valueToUpdate, fields: _*)

  final def update(valueToUpdate: RawValue, fields: Chunk[(Name, RawValue)]): RawValue =
    UpdateRecord((), valueToUpdate, fields.toMap)

  final def update(valueToUpdate: RawValue, fields: (String, RawValue)*): RawValue =
    UpdateRecord.Raw(valueToUpdate, fields: _*)

  final def valueDef[TA](returnType: Type[TA]): ValueDefClause[TA] = new ValueDefClause(returnType)

  final def variable[A](attributes: A, name: Name): Value[Nothing, A]   = Variable(attributes, name)
  final def variable[A](attributes: A, name: String): Value[Nothing, A] = Variable(attributes, name)
  final def variable(name: Name): RawValue                              = Variable.Raw(name)
  final def variable(name: String): RawValue                            = Variable.Raw(name)
  final def variable(name: String, tpe: UType): TypedValue              = Variable.Typed(tpe, name)
  final def variable(name: Name, tpe: UType): TypedValue                = Variable.Typed(tpe, name)

  final def wholeNumber(value: Long): RawValue =
    literal(Lit.wholeNumber(value))

  final def wholeNumber(value: Int): RawValue =
    literal(Lit.wholeNumber(value.toLong))

  implicit class RawValueExtensions(private val self: RawValue) extends AnyVal {

    /*
     * Ascribe the given type to this `RawValue` and all its children.
     * ===NOTE===
     * This is a recursive operation and all children of this `RawValue` will also be ascribed with the given value.
     */
    def :>(ascribedType: UType): TypedValue = self.mapAttributes(identity, _ => ascribedType)

    def toValDef(returnType: UType): Definition[scala.Unit, UType] = Definition(returnType, self :> returnType)
  }

  class FieldsPartiallyApplied(val dummy: Boolean = false) extends AnyVal {
    def apply[TA, VA](fields: (String, Value[TA, VA])*): Vector[(Name, Value[TA, VA])] = fields.map {
      case (name, value) => Name(name) -> value
    }.toVector
  }

  class RecordPartiallyApplied[VA](val attributes: VA) extends AnyVal {
    def apply[TA](fields: (String, Value[TA, VA])*): Value[TA, VA] = Value.Record(attributes, fields: _*)
    def apply[TA](fields: Map[Name, Value[TA, VA]]): Value[TA, VA] = Value.Record.fromMap(attributes, fields)
    def withFields[TA](fields: Seq[(Name, Value[TA, VA])]): Value[TA, VA] =
      Value.Record(attributes, Chunk.fromIterable(fields))
  }

  // class RecordWithoutAttributesPartiallyApplied(val dummy: Boolean = false) extends AnyVal {
  //   def apply[TA](fields: (String, Value[TA, scala.Unit])*): Value[TA, scala.Unit]      = Value.Record((), fields: _*)
  //   def withFields[TA](fields: (String, Value[TA, scala.Unit])*): Value[TA, scala.Unit] = Value.Record((), fields: _*)
  //   def withFields[TA](fields: Iterable[(String, Value[TA, scala.Unit])]): Value[TA, scala.Unit] =
  //     Value.Record(Chunk.fromIterable(fields.map((Name.fromString(_), _))))
  //   def withNamedFields[TA](fields: Seq[(Name, Value[TA, scala.Unit])]): Value[TA, scala.Unit] =
  //     Value.Record((), Chunk.fromIterable(fields))
  // }

  final class FunctionDefInputsClause[TA, VA](val args: Chunk[(Name, VA, Type[TA])]) extends AnyVal {

    def apply(returnType: Type[TA]): FunctionSignature[TA, VA] = returning(returnType)

    def apply(returnType: Type[TA], body: Value[TA, VA]): Definition[TA, VA] =
      Definition(inputTypes = args, outputType = returnType, body = body)

    def returning(returnType: Type[TA]): FunctionSignature[TA, VA] = new FunctionSignature(() => (args, returnType))
  }

  final class FunctionSignature[TA, VA](val input: () => (Chunk[(Name, VA, Type[TA])], Type[TA])) extends AnyVal {
    def apply(body: => Value[TA, VA]): Definition[TA, VA] = {
      val (args, returnType) = input()
      Definition(inputTypes = args, outputType = returnType, body = body)
    }

    def withBody(body: => Value[TA, VA]): Definition[TA, VA] = {
      val (args, returnType) = input()
      Definition(inputTypes = args, outputType = returnType, body = body)
    }
  }

  final class ValueDefClause[TA](val returnType: Type[TA]) extends AnyVal {
    def apply[VA](body: => Value[TA, VA]): Definition[TA, VA] =
      Definition(inputTypes = Chunk.empty, outputType = returnType, body = body)

    def withBody[VA](body: => Value[TA, VA]): Definition[TA, VA] =
      Definition(inputTypes = Chunk.empty, outputType = returnType, body = body)
  }
  object TypedValue {
    def transform(partial: PartialFunction[TypedValue])(value: TypedValue): TypedValue = {
      def recurse = transform(partial)
      if (partial.isDefinedAt(value)) partial(value)
      else
        value match {
          case Apply(va, function, argument) => Apply(va, recurse(function), recurse(argument))
          case Destructure(va, pattern, valueToDestruct, inValue) =>
            Destructure(va, pattern, recurse(valueToDestruct), recurse(inValue))
          case Field(va, recordValue, name) => Field(va, recurse(recordValue), name)
          case IfThenElse(va, condition, thenValue, elseValue) =>
            IfThenElse(va, recurse(condition), recurse(thenValue), recurse(elseValue))
          case Lambda(va, pattern, body) => Lambda(va, pattern, recurse(body))
          case LetDefinition(va, name, definition, inValue) =>
            LetDefinition(va, name, definition.copy(body = recurse(definition.body)), recurse(inValue))
          case LetRecursion(va, definitions, inValue) => LetRecursion(
              va,
              definitions.map((name, dfn) => (name, dfn.copy(body = recurse(dfn.body)))),
              recurse(inValue)
            )
          case List(va, elements) => List(va, elements.map(recurse))
          case PatternMatch(va, value, cases) =>
            PatternMatch(va, recurse(value), cases.map((casePattern, caseValue) => (casePattern, recurse(caseValue))))
          case Record(va, fields) => Record(
              va,
              fields.map((fieldName, fieldValue) => (fieldName, recurse(fieldValue)))
            )
          case Tuple(va, elements) => Tuple(
              va,
              elements.map(recurse)
            )
          case UpdateRecord(va, valueToUpdate, fields) => UpdateRecord(
              va,
              recurse(valueToUpdate),
              fields.map((fieldName, fieldValue) => (fieldName, recurse(fieldValue)))
            )
          case noNestedIR => noNestedIR

        }
    }

  }
}
