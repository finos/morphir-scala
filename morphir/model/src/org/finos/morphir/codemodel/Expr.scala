package org.finos.morphir.codemodel

import org.finos.morphir.naming._
import kyo.Chunk
import kyo.Schema

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
enum Literal derives Schema {
  case BoolLiteral(value: Boolean)
  case CharLiteral(value: String) // Stored as String to support potential unicode chars or gleam approach? Spec says checked "String" for CharLiteral.
  case StringLiteral(value: String)
  case IntegerLiteral(
      value: BigInt
  ) // Spec says "Integer (arbitrary precision)". Int in Scala is 32-bit. BigInt is better.
  case FloatLiteral(value: Double)
  case DecimalLiteral(value: BigDecimal)
}

enum Pattern derives Schema {
  case WildcardPattern(attributes: ValueAttributes)
  case AsPattern(attributes: ValueAttributes, pattern: Pattern, name: Name)
  case TuplePattern(attributes: ValueAttributes, elements: Chunk[Pattern])
  case ConstructorPattern(attributes: ValueAttributes, constructor: FQName, args: Chunk[Pattern])
  case EmptyListPattern(attributes: ValueAttributes)
  case HeadTailPattern(attributes: ValueAttributes, head: Pattern, tail: Pattern)
  case LiteralPattern(attributes: ValueAttributes, literal: Literal)
  case UnitPattern(attributes: ValueAttributes)
}

enum Expr derives Schema {
  case Literal(attributes: ValueAttributes, literal: org.finos.morphir.codemodel.Literal)
  case Constructor(attributes: ValueAttributes, fqName: FQName)
  case Tuple(attributes: ValueAttributes, elements: Chunk[Expr])
  case List(attributes: ValueAttributes, items: Chunk[Expr])
  case Record(attributes: ValueAttributes, fields: Chunk[RecordField])
  case Unit(attributes: ValueAttributes)
  case Variable(attributes: ValueAttributes, name: Name)
  case Reference(attributes: ValueAttributes, fqName: FQName)
  case Field(attributes: ValueAttributes, record: Expr, fieldName: Name)
  case FieldFunction(attributes: ValueAttributes, fieldName: Name)
  case Apply(attributes: ValueAttributes, function: Expr, argument: Expr)
  case Lambda(attributes: ValueAttributes, argumentPattern: Pattern, body: Expr)
  case LetDefinition(attributes: ValueAttributes, name: Name, definition: ValueDefinitionBody, inValue: Expr)
  case LetRecursion(attributes: ValueAttributes, bindings: Chunk[Binding], inValue: Expr)
  case Destructure(attributes: ValueAttributes, pattern: Pattern, valueToDestructure: Expr, inValue: Expr)
  case IfThenElse(attributes: ValueAttributes, condition: Expr, thenBranch: Expr, elseBranch: Expr)
  case PatternMatch(attributes: ValueAttributes, subject: Expr, cases: Chunk[MatchCase])
  case UpdateRecord(attributes: ValueAttributes, record: Expr, updates: Chunk[RecordField])
  case Hole(attributes: ValueAttributes, reason: HoleReason, expectedType: Option[Type])
  case Native(attributes: ValueAttributes, fqName: FQName, nativeInfo: NativeInfo)
  case External(attributes: ValueAttributes, externalName: String, targetPlatform: String)
}

/**
 * A single `name: value` entry of a record literal or record update. Replaces the raw `(Name, Expr)` tuple
 * `Expr.Record.fields` and `Expr.UpdateRecord.updates` used to carry. Named `RecordField` rather than `Field` because
 * `Field` (in `Type.scala`) already names the type-level analogue — a record *type*'s `name: Type` slot — and the two
 * are not interchangeable (this one holds a value expression, not a type).
 */
final case class RecordField(name: Name, value: Expr) derives Schema

/**
 * One arm of a `PatternMatch`: the pattern to test against and the expression to evaluate if it matches. Replaces the
 * raw `(Pattern, Expr)` tuple `Expr.PatternMatch.cases` used to carry.
 */
final case class MatchCase(pattern: Pattern, body: Expr) derives Schema

/**
 * One binding of a mutually recursive `let`: the bound name and its definition. Replaces the raw
 * `(Name, ValueDefinitionBody)` tuple `Expr.LetRecursion.bindings` used to carry.
 */
final case class Binding(name: Name, definition: ValueDefinitionBody) derives Schema

enum NativeHint derives Schema {
  case Arithmetic
  case Comparison
  case StringOp
  case CollectionOp
  case PlatformSpecific(platform: String)
}

final case class NativeInfo(hint: NativeHint, description: Option[String]) derives Schema

enum ValueDefinitionBody derives Schema {
  case ExpressionBody(inputTypes: Chunk[Parameter], outputType: Type, body: Expr)
  case NativeBody(inputTypes: Chunk[Parameter], outputType: Type, nativeInfo: NativeInfo)
  case ExternalBody(inputTypes: Chunk[Parameter], outputType: Type, externalName: String, targetPlatform: String)
  case IncompleteBody(
      inputTypes: Chunk[Parameter],
      outputType: Option[Type],
      incompleteness: Incompleteness,
      partialBody: Option[Expr]
  )
}

final case class ValueDefinition(body: AccessControlled[ValueDefinitionBody]) derives Schema

final case class ValueSpecification(inputs: Chunk[Parameter], output: Type) derives Schema
