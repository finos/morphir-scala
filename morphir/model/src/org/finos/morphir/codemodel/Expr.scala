package org.finos.morphir.codemodel

import org.finos.morphir.naming._
import kyo.Chunk

/* SPEC SYNC DEBT — `Expr` was named `Value` here and still is upstream.
 *
 * Morphir inherits from Elm the convention that "value" means *expression*, so the v4 draft
 * spec, the upstream `finos/morphir` sources it derives from, and the v3 IR all call this
 * type `Value`. We renamed it to `Expr` because the runtime needs the word "value" for what
 * an expression evaluates to (`org.finos.morphir.datamodel.Val`), and having two things
 * called "value" in every evaluator signature is a permanent readability tax.
 *
 * This is therefore a deliberate divergence from the spec vocabulary, not a typo. Still to do:
 *
 *   - Update the v4 spec bundle under `kb/bundles/morphir/morphir-ir-v4-draft/` — chiefly
 *     `value-expressions.md` and `value-specifications-and-definitions.md`, which describe
 *     this type by name.
 *   - Record the divergence in that bundle's `design/divergences.md`, since the bundle's
 *     front matter cites upstream `docs/spec/draft/values.md` as its source and a reader
 *     comparing the two needs to know the rename is intentional.
 *
 * Note the surrounding names are NOT affected: `ValueDefinition`, `ValueSpecification` and
 * `ValueAttributes` genuinely concern value bindings and keep their spec names. Only the
 * expression tree was renamed.
 */
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

enum Expr {
  case Literal(attributes: ValueAttributes, literal: org.finos.morphir.codemodel.Literal)
  case Constructor(attributes: ValueAttributes, fqName: FQName)
  case Tuple(attributes: ValueAttributes, elements: Chunk[Expr])
  case List(attributes: ValueAttributes, items: Chunk[Expr])
  case Record(attributes: ValueAttributes, fields: Chunk[(Name, Expr)])
  case Unit(attributes: ValueAttributes)
  case Variable(attributes: ValueAttributes, name: Name)
  case Reference(attributes: ValueAttributes, fqName: FQName)
  case Field(attributes: ValueAttributes, record: Expr, fieldName: Name)
  case FieldFunction(attributes: ValueAttributes, fieldName: Name)
  case Apply(attributes: ValueAttributes, function: Expr, argument: Expr)
  case Lambda(attributes: ValueAttributes, argumentPattern: Pattern, body: Expr)
  case LetDefinition(attributes: ValueAttributes, name: Name, definition: ValueDefinitionBody, inValue: Expr)
  case LetRecursion(attributes: ValueAttributes, bindings: Chunk[(Name, ValueDefinitionBody)], inValue: Expr)
  case Destructure(attributes: ValueAttributes, pattern: Pattern, valueToDestructure: Expr, inValue: Expr)
  case IfThenElse(attributes: ValueAttributes, condition: Expr, thenBranch: Expr, elseBranch: Expr)
  case PatternMatch(attributes: ValueAttributes, subject: Expr, cases: Chunk[(Pattern, Expr)])
  case UpdateRecord(attributes: ValueAttributes, record: Expr, updates: Chunk[(Name, Expr)])
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
  case ExpressionBody(inputTypes: Chunk[(Name, Type)], outputType: Type, body: Expr)
  case NativeBody(inputTypes: Chunk[(Name, Type)], outputType: Type, nativeInfo: NativeInfo)
  case ExternalBody(inputTypes: Chunk[(Name, Type)], outputType: Type, externalName: String, targetPlatform: String)
  case IncompleteBody(
      inputTypes: Chunk[(Name, Type)],
      outputType: Option[Type],
      incompleteness: Incompleteness,
      partialBody: Option[Expr]
  )
}

final case class ValueDefinition(body: AccessControlled[ValueDefinitionBody])

final case class ValueSpecification(inputs: Chunk[(Name, Type)], output: Type)
