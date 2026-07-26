package morphir.langkit.elm.ast

import morphir.langkit.elm.Span

// ── Base ─────────────────────────────────────────────────────────────────────

sealed trait AstNode:
  def span: Span

object AstNode:
  given CanEqual[AstNode, AstNode] = CanEqual.derived

// ── Module ───────────────────────────────────────────────────────────────────

case class Module(
    name: QualifiedName,
    exposing: ExposingList,
    imports: IndexedSeq[Import],
    declarations: IndexedSeq[Declaration],
    docComment: Option[String] = None
)(val span: Span)
    extends AstNode derives CanEqual

case class QualifiedName(parts: List[String])(val span: Span) extends AstNode derives CanEqual:
  def fullName: String = parts.mkString(".")

case class Import(
    moduleName: QualifiedName,
    alias: Option[String],
    exposing: Option[ExposingList]
)(val span: Span)
    extends AstNode derives CanEqual

// ── Exposing ─────────────────────────────────────────────────────────────────

sealed trait ExposingList extends AstNode

case class ExposingAll()(val span: Span) extends ExposingList derives CanEqual

case class ExposingExplicit(items: List[ExposedItem])(val span: Span) extends ExposingList derives CanEqual

sealed trait ExposedItem extends AstNode

case class ExposedValue(name: String)(val span: Span)    extends ExposedItem derives CanEqual
case class ExposedOperator(name: String)(val span: Span) extends ExposedItem derives CanEqual

case class ExposedType(name: String, exposeConstructors: Boolean)(val span: Span) extends ExposedItem derives CanEqual

// ── Declarations ─────────────────────────────────────────────────────────────

sealed trait Declaration extends AstNode

case class ValueDeclaration(
    name: String,
    typeAnnotation: Option[TypeExpression],
    parameters: IndexedSeq[Pattern],
    body: Expression,
    docComment: Option[String] = None
)(val span: Span)
    extends Declaration derives CanEqual

case class TypeAliasDeclaration(
    name: String,
    typeVariables: IndexedSeq[String],
    body: TypeExpression,
    docComment: Option[String] = None
)(val span: Span)
    extends Declaration derives CanEqual

case class CustomTypeDeclaration(
    name: String,
    typeVariables: IndexedSeq[String],
    constructors: IndexedSeq[Constructor],
    docComment: Option[String] = None
)(val span: Span)
    extends Declaration derives CanEqual

case class Constructor(
    name: String,
    parameters: IndexedSeq[TypeExpression]
)(val span: Span)
    extends AstNode derives CanEqual

case class PortDeclaration(
    name: String,
    typeExpr: TypeExpression,
    docComment: Option[String] = None
)(val span: Span)
    extends Declaration derives CanEqual

case class InfixDeclaration(
    associativity: Associativity,
    precedence: Int,
    operator: String,
    function: String,
    docComment: Option[String] = None
)(val span: Span)
    extends Declaration derives CanEqual

enum Associativity derives CanEqual:
  case Left, Right, Non

// ── Type Expressions ─────────────────────────────────────────────────────────

sealed trait TypeExpression extends AstNode

case class TypeReference(name: QualifiedName)(val span: Span) extends TypeExpression derives CanEqual

case class TypeVariable(name: String)(val span: Span) extends TypeExpression derives CanEqual

case class TypeApplication(
    constructor: TypeExpression,
    arguments: List[TypeExpression]
)(val span: Span)
    extends TypeExpression derives CanEqual

case class FunctionType(from: TypeExpression, to: TypeExpression)(val span: Span) extends TypeExpression
    derives CanEqual

case class TupleType(elements: List[TypeExpression])(val span: Span) extends TypeExpression derives CanEqual

case class UnitType()(val span: Span) extends TypeExpression derives CanEqual

case class RecordType(
    fields: List[RecordFieldType],
    extensionVariable: Option[String]
)(val span: Span)
    extends TypeExpression derives CanEqual

case class RecordFieldType(name: String, typeExpr: TypeExpression)(val span: Span) extends AstNode derives CanEqual

// ── Expressions ──────────────────────────────────────────────────────────────

sealed trait Expression extends AstNode

case class IntLiteral(value: Long)(val span: Span)      extends Expression derives CanEqual
case class FloatLiteral(value: Double)(val span: Span)  extends Expression derives CanEqual
case class StringLiteral(value: String)(val span: Span) extends Expression derives CanEqual
case class CharLiteral(value: Char)(val span: Span)     extends Expression derives CanEqual

case class VariableRef(name: QualifiedName)(val span: Span) extends Expression derives CanEqual

case class ConstructorRef(name: QualifiedName)(val span: Span) extends Expression derives CanEqual

case class OperatorRef(name: String)(val span: Span) extends Expression derives CanEqual

case class FunctionApplication(
    function: Expression,
    arguments: List[Expression]
)(val span: Span)
    extends Expression derives CanEqual

case class BinaryOp(
    left: Expression,
    operator: String,
    right: Expression
)(val span: Span)
    extends Expression derives CanEqual

case class Negate(expr: Expression)(val span: Span) extends Expression derives CanEqual

case class IfThenElse(
    condition: Expression,
    thenBranch: Expression,
    elseBranch: Expression
)(val span: Span)
    extends Expression derives CanEqual

case class LetIn(
    bindings: List[LetBinding],
    body: Expression
)(val span: Span)
    extends Expression derives CanEqual

case class LetBinding(
    name: String,
    typeAnnotation: Option[TypeExpression],
    parameters: List[Pattern],
    body: Expression
)(val span: Span)
    extends AstNode derives CanEqual

case class CaseOf(
    expr: Expression,
    branches: List[CaseBranch]
)(val span: Span)
    extends Expression derives CanEqual

case class CaseBranch(
    pattern: Pattern,
    body: Expression
)(val span: Span)
    extends AstNode derives CanEqual

case class Lambda(
    parameters: List[Pattern],
    body: Expression
)(val span: Span)
    extends Expression derives CanEqual

case class TupleLiteral(elements: List[Expression])(val span: Span) extends Expression derives CanEqual

case class UnitLiteral()(val span: Span) extends Expression derives CanEqual

case class ListLiteral(elements: List[Expression])(val span: Span) extends Expression derives CanEqual

case class RecordLiteral(fields: List[RecordField])(val span: Span) extends Expression derives CanEqual

case class RecordField(name: String, value: Expression)(val span: Span) extends AstNode derives CanEqual

case class RecordUpdate(
    record: String,
    fields: List[RecordField]
)(val span: Span)
    extends Expression derives CanEqual

case class FieldAccess(record: Expression, field: String)(val span: Span) extends Expression derives CanEqual

case class FieldAccessFunction(field: String)(val span: Span) extends Expression derives CanEqual

case class Parenthesized(expr: Expression)(val span: Span) extends Expression derives CanEqual

case class Glsl(code: String)(val span: Span) extends Expression derives CanEqual

// ── Patterns ─────────────────────────────────────────────────────────────────

sealed trait Pattern extends AstNode

case class AnythingPattern()(val span: Span)             extends Pattern derives CanEqual
case class IntPattern(value: Long)(val span: Span)       extends Pattern derives CanEqual
case class FloatPattern(value: Double)(val span: Span)   extends Pattern derives CanEqual
case class StringPattern(value: String)(val span: Span)  extends Pattern derives CanEqual
case class CharPattern(value: Char)(val span: Span)      extends Pattern derives CanEqual
case class VariablePattern(name: String)(val span: Span) extends Pattern derives CanEqual
case class UnitPattern()(val span: Span)                 extends Pattern derives CanEqual

case class ConstructorPattern(
    name: QualifiedName,
    arguments: List[Pattern]
)(val span: Span)
    extends Pattern derives CanEqual

case class TuplePattern(elements: List[Pattern])(val span: Span) extends Pattern derives CanEqual

case class ListPattern(elements: List[Pattern])(val span: Span) extends Pattern derives CanEqual

case class ConsPattern(head: Pattern, tail: Pattern)(val span: Span) extends Pattern derives CanEqual

case class RecordPattern(fields: List[String])(val span: Span) extends Pattern derives CanEqual

case class AsPattern(pattern: Pattern, alias: String)(val span: Span) extends Pattern derives CanEqual
