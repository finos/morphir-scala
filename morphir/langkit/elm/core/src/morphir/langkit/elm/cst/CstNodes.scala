package morphir.langkit.elm.cst

import morphir.langkit.core.Span

// ── Base ─────────────────────────────────────────────────────────────────────

sealed trait CstNode:
  def span: Span

object CstNode:
  given CanEqual[CstNode, CstNode] = CanEqual.derived

// ── Trivia ──────────────────────────────────────────────────────────────────

sealed trait CstTriviaItem extends CstNode

case class CstTrivia(items: IndexedSeq[CstTriviaItem] = IndexedSeq.empty) derives CanEqual:

  def docComment: Option[CstComment] =
    items.collectFirst { case c: CstComment if c.kind == CommentKind.Doc => c }

  def comments: IndexedSeq[CstComment] =
    items.collect { case c: CstComment => c }
  def isEmpty: Boolean  = items.isEmpty
  def nonEmpty: Boolean = items.nonEmpty

object CstTrivia:
  val empty: CstTrivia = CstTrivia()

// ── Module ───────────────────────────────────────────────────────────────────

case class CstModule(
    moduleDecl: CstModuleDeclaration,
    imports: IndexedSeq[CstImport],
    declarations: IndexedSeq[CstDeclaration],
    trivia: CstTrivia = CstTrivia.empty
)(val span: Span)
    extends CstNode derives CanEqual

case class CstModuleDeclaration(
    moduleType: ModuleType,
    name: CstQualifiedName,
    exposing: CstExposingList,
    manager: Option[CstEffectManager] = None
)(val span: Span)
    extends CstNode derives CanEqual

/**
 * The `where { command = …, subscription = … }` clause of an `effect module`.
 *
 * Elm requires at least one of the two, permits either order, and names an uppercase type for each. The clause exists
 * only on an effect module, which is why it hangs off the module declaration rather than standing alone.
 */
case class CstEffectManager(
    command: Option[CstName],
    subscription: Option[CstName]
)(val span: Span)
    extends CstNode derives CanEqual

enum ModuleType derives CanEqual:
  case Plain, Port, Effect

case class CstQualifiedName(
    parts: List[CstName]
)(val span: Span)
    extends CstNode derives CanEqual

case class CstName(
    value: String
)(val span: Span)
    extends CstNode derives CanEqual

// ── Comments ─────────────────────────────────────────────────────────────────

enum CommentKind derives CanEqual:
  case Line, Block, Doc

case class CstComment(
    kind: CommentKind,
    text: String
)(val span: Span)
    extends CstTriviaItem derives CanEqual

// ── Exposing ─────────────────────────────────────────────────────────────────

sealed trait CstExposingList extends CstNode

case class CstExposingAll()(val span: Span) extends CstExposingList derives CanEqual

case class CstExposingExplicit(
    items: List[CstExposedItem]
)(val span: Span)
    extends CstExposingList derives CanEqual

sealed trait CstExposedItem extends CstNode

case class CstExposedValue(name: CstName)(val span: Span)    extends CstExposedItem derives CanEqual
case class CstExposedOperator(name: CstName)(val span: Span) extends CstExposedItem derives CanEqual

case class CstExposedType(
    name: CstName,
    constructors: Option[CstExposedConstructors]
)(val span: Span)
    extends CstExposedItem derives CanEqual

sealed trait CstExposedConstructors extends CstNode

case class CstExposedConstructorsAll()(val span: Span)  extends CstExposedConstructors derives CanEqual
case class CstExposedConstructorsNone()(val span: Span) extends CstExposedConstructors derives CanEqual

// ── Imports ──────────────────────────────────────────────────────────────────

case class CstImport(
    moduleName: CstQualifiedName,
    alias: Option[CstName],
    exposing: Option[CstExposingList]
)(val span: Span)
    extends CstNode derives CanEqual

// ── Declarations ─────────────────────────────────────────────────────────────

sealed trait CstDeclaration extends CstNode:
  def trivia: CstTrivia

case class CstValueDeclaration(
    annotation: Option[CstTypeAnnotation],
    name: CstName,
    patterns: IndexedSeq[CstPattern],
    body: CstExpression,
    trivia: CstTrivia = CstTrivia.empty
)(val span: Span)
    extends CstDeclaration derives CanEqual

case class CstTypeAnnotation(
    name: CstName,
    typeExpr: CstTypeExpression
)(val span: Span)
    extends CstNode derives CanEqual

case class CstTypeAliasDeclaration(
    name: CstName,
    typeVariables: IndexedSeq[CstName],
    body: CstTypeExpression,
    trivia: CstTrivia = CstTrivia.empty
)(val span: Span)
    extends CstDeclaration derives CanEqual

case class CstCustomTypeDeclaration(
    name: CstName,
    typeVariables: IndexedSeq[CstName],
    constructors: IndexedSeq[CstConstructor],
    trivia: CstTrivia = CstTrivia.empty
)(val span: Span)
    extends CstDeclaration derives CanEqual

case class CstConstructor(
    name: CstName,
    parameters: IndexedSeq[CstTypeExpression]
)(val span: Span)
    extends CstNode derives CanEqual

case class CstPortDeclaration(
    name: CstName,
    typeExpr: CstTypeExpression,
    trivia: CstTrivia = CstTrivia.empty
)(val span: Span)
    extends CstDeclaration derives CanEqual

case class CstInfixDeclaration(
    associativity: Associativity,
    precedence: Int,
    operator: CstName,
    function: CstName,
    trivia: CstTrivia = CstTrivia.empty
)(val span: Span)
    extends CstDeclaration derives CanEqual

enum Associativity derives CanEqual:
  case Left, Right, Non

// ── Type Expressions ─────────────────────────────────────────────────────────

sealed trait CstTypeExpression extends CstNode

case class CstTypeReference(name: CstQualifiedName)(val span: Span) extends CstTypeExpression derives CanEqual

case class CstTypeVariable(name: CstName)(val span: Span) extends CstTypeExpression derives CanEqual

case class CstTypeApplication(
    constructor: CstTypeExpression,
    arguments: List[CstTypeExpression]
)(val span: Span)
    extends CstTypeExpression derives CanEqual

case class CstFunctionType(
    from: CstTypeExpression,
    to: CstTypeExpression
)(val span: Span)
    extends CstTypeExpression derives CanEqual

case class CstTupleType(
    elements: List[CstTypeExpression]
)(val span: Span)
    extends CstTypeExpression derives CanEqual

case class CstUnitType()(val span: Span) extends CstTypeExpression derives CanEqual

case class CstRecordType(
    fields: List[CstRecordFieldType],
    extensionVariable: Option[CstName]
)(val span: Span)
    extends CstTypeExpression derives CanEqual

case class CstRecordFieldType(
    name: CstName,
    typeExpr: CstTypeExpression
)(val span: Span)
    extends CstNode derives CanEqual

// ── Expressions ──────────────────────────────────────────────────────────────

sealed trait CstExpression extends CstNode

case class CstIntLiteral(value: Long)(val span: Span)      extends CstExpression derives CanEqual
case class CstFloatLiteral(value: Double)(val span: Span)  extends CstExpression derives CanEqual
case class CstStringLiteral(value: String)(val span: Span) extends CstExpression derives CanEqual

/**
 * A character literal, carried as a code point.
 *
 * Elm's `Char` is a code point rather than a UTF-16 unit, so `'😀'` is one character there and cannot be held in a JVM
 * `Char`. [[text]] renders it back.
 */
case class CstCharLiteral(codePoint: Int)(val span: Span) extends CstExpression derives CanEqual:
  def text: String = new java.lang.StringBuilder().appendCodePoint(codePoint).toString

case class CstVariableRef(name: CstQualifiedName)(val span: Span) extends CstExpression derives CanEqual

case class CstConstructorRef(name: CstQualifiedName)(val span: Span) extends CstExpression derives CanEqual

case class CstOperatorRef(name: CstName)(val span: Span) extends CstExpression derives CanEqual

case class CstFunctionApplication(
    function: CstExpression,
    arguments: List[CstExpression]
)(val span: Span)
    extends CstExpression derives CanEqual

case class CstBinaryOp(
    left: CstExpression,
    operator: CstName,
    right: CstExpression
)(val span: Span)
    extends CstExpression derives CanEqual

case class CstNegate(expr: CstExpression)(val span: Span) extends CstExpression derives CanEqual

case class CstIfThenElse(
    condition: CstExpression,
    thenBranch: CstExpression,
    elseBranch: CstExpression
)(val span: Span)
    extends CstExpression derives CanEqual

case class CstLetIn(
    bindings: List[CstLetBinding],
    body: CstExpression
)(val span: Span)
    extends CstExpression derives CanEqual

case class CstLetBinding(
    annotation: Option[CstTypeAnnotation],
    pattern: CstPattern,
    parameters: List[CstPattern],
    body: CstExpression
)(val span: Span)
    extends CstNode derives CanEqual

case class CstCaseOf(
    expr: CstExpression,
    branches: List[CstCaseBranch]
)(val span: Span)
    extends CstExpression derives CanEqual

case class CstCaseBranch(
    pattern: CstPattern,
    body: CstExpression
)(val span: Span)
    extends CstNode derives CanEqual

case class CstLambda(
    parameters: List[CstPattern],
    body: CstExpression
)(val span: Span)
    extends CstExpression derives CanEqual

case class CstTupleLiteral(
    elements: List[CstExpression]
)(val span: Span)
    extends CstExpression derives CanEqual

case class CstUnitLiteral()(val span: Span) extends CstExpression derives CanEqual

case class CstListLiteral(
    elements: List[CstExpression]
)(val span: Span)
    extends CstExpression derives CanEqual

case class CstRecordLiteral(
    fields: List[CstRecordField]
)(val span: Span)
    extends CstExpression derives CanEqual

case class CstRecordField(
    name: CstName,
    value: CstExpression
)(val span: Span)
    extends CstNode derives CanEqual

case class CstRecordUpdate(
    record: CstName,
    fields: List[CstRecordField]
)(val span: Span)
    extends CstExpression derives CanEqual

case class CstFieldAccess(
    record: CstExpression,
    field: CstName
)(val span: Span)
    extends CstExpression derives CanEqual

case class CstFieldAccessFunction(
    field: CstName
)(val span: Span)
    extends CstExpression derives CanEqual

case class CstParenthesized(
    expr: CstExpression
)(val span: Span)
    extends CstExpression derives CanEqual

case class CstGlsl(code: String)(val span: Span) extends CstExpression derives CanEqual

// ── Patterns ─────────────────────────────────────────────────────────────────

sealed trait CstPattern extends CstNode

case class CstAnythingPattern()(val span: Span)            extends CstPattern derives CanEqual
case class CstIntPattern(value: Long)(val span: Span)      extends CstPattern derives CanEqual
case class CstFloatPattern(value: Double)(val span: Span)  extends CstPattern derives CanEqual
case class CstStringPattern(value: String)(val span: Span) extends CstPattern derives CanEqual
case class CstCharPattern(codePoint: Int)(val span: Span)  extends CstPattern derives CanEqual:
  def text: String = new java.lang.StringBuilder().appendCodePoint(codePoint).toString
case class CstVariablePattern(name: CstName)(val span: Span) extends CstPattern derives CanEqual
case class CstUnitPattern()(val span: Span)                  extends CstPattern derives CanEqual

case class CstConstructorPattern(
    name: CstQualifiedName,
    arguments: List[CstPattern]
)(val span: Span)
    extends CstPattern derives CanEqual

case class CstTuplePattern(
    elements: List[CstPattern]
)(val span: Span)
    extends CstPattern derives CanEqual

case class CstListPattern(
    elements: List[CstPattern]
)(val span: Span)
    extends CstPattern derives CanEqual

case class CstConsPattern(
    head: CstPattern,
    tail: CstPattern
)(val span: Span)
    extends CstPattern derives CanEqual

case class CstRecordPattern(
    fields: List[CstName]
)(val span: Span)
    extends CstPattern derives CanEqual

case class CstAsPattern(
    pattern: CstPattern,
    alias: CstName
)(val span: Span)
    extends CstPattern derives CanEqual

case class CstParenthesizedPattern(
    pattern: CstPattern
)(val span: Span)
    extends CstPattern derives CanEqual
