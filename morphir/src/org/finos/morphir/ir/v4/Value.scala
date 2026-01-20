package org.finos.morphir
package ir
package v4

import org.finos.morphir.naming._
import zio.Chunk

enum Literal {
  case BoolLiteral(value: Boolean)
  case CharLiteral(value: String) // Stored as String to support potential unicode chars or gleam approach? Spec says checked "String" for CharLiteral.
  case StringLiteral(value: String)
  case IntegerLiteral(
      value: BigInt
  ) // Spec says "Integer (arbitrary precision)". Int in Scala is 32-bit. BigInt is better.
  case FloatLiteral(value: Double)
  case DecimalLiteral(value: BigDecimal)
}

enum Pattern {
  case WildcardPattern(attributes: ValueAttributes)
  case AsPattern(attributes: ValueAttributes, pattern: Pattern, name: Name)
  case TuplePattern(attributes: ValueAttributes, elements: Chunk[Pattern])
  case ConstructorPattern(attributes: ValueAttributes, constructor: FQName, args: Chunk[Pattern])
  case EmptyListPattern(attributes: ValueAttributes)
  case HeadTailPattern(attributes: ValueAttributes, head: Pattern, tail: Pattern)
  case LiteralPattern(attributes: ValueAttributes, literal: Literal)
  case UnitPattern(attributes: ValueAttributes)
}

enum Value {
  case Literal(attributes: ValueAttributes, literal: v4.Literal)
  case Constructor(attributes: ValueAttributes, fqName: FQName)
  case Tuple(attributes: ValueAttributes, elements: Chunk[Value])
  case List(attributes: ValueAttributes, items: Chunk[Value])
  case Record(attributes: ValueAttributes, fields: Chunk[(Name, Value)])
  case Unit(attributes: ValueAttributes)
  case Variable(attributes: ValueAttributes, name: Name)
  case Reference(attributes: ValueAttributes, fqName: FQName)
  case Field(attributes: ValueAttributes, record: Value, fieldName: Name)
  case FieldFunction(attributes: ValueAttributes, fieldName: Name)
  case Apply(attributes: ValueAttributes, function: Value, argument: Value)
  case Lambda(attributes: ValueAttributes, argumentPattern: Pattern, body: Value)
  case LetDefinition(attributes: ValueAttributes, name: Name, definition: ValueDefinitionBody, inValue: Value)
  case LetRecursion(attributes: ValueAttributes, bindings: Chunk[(Name, ValueDefinitionBody)], inValue: Value)
  case Destructure(attributes: ValueAttributes, pattern: Pattern, valueToDestructure: Value, inValue: Value)
  case IfThenElse(attributes: ValueAttributes, condition: Value, thenBranch: Value, elseBranch: Value)
  case PatternMatch(attributes: ValueAttributes, subject: Value, cases: Chunk[(Pattern, Value)])
  case UpdateRecord(attributes: ValueAttributes, record: Value, updates: Chunk[(Name, Value)])
  case Hole(attributes: ValueAttributes, reason: HoleReason, expectedType: Option[Type])
  case Native(attributes: ValueAttributes, fqName: FQName, nativeInfo: NativeInfo)
  case External(attributes: ValueAttributes, externalName: String, targetPlatform: String)
}

enum NativeHint {
  case Arithmetic
  case Comparison
  case StringOp
  case CollectionOp
  case PlatformSpecific(platform: String)
}

final case class NativeInfo(hint: NativeHint, description: Option[String])

enum ValueDefinitionBody {
  case ExpressionBody(inputTypes: Chunk[(Name, Type)], outputType: Type, body: Value)
  case NativeBody(inputTypes: Chunk[(Name, Type)], outputType: Type, nativeInfo: NativeInfo)
  case ExternalBody(inputTypes: Chunk[(Name, Type)], outputType: Type, externalName: String, targetPlatform: String)
  case IncompleteBody(
      inputTypes: Chunk[(Name, Type)],
      outputType: Option[Type],
      incompleteness: Incompleteness,
      partialBody: Option[Value]
  )
}

final case class ValueDefinition(body: AccessControlled[ValueDefinitionBody])

final case class ValueSpecification(inputs: Chunk[(Name, Type)], output: Type)
