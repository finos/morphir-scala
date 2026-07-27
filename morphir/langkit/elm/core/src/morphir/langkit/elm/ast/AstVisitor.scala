package morphir.langkit.elm.ast

import scala.annotation.targetName
import scala.collection.mutable.ArrayBuffer
import scala.util.control.TailCalls.*

/**
 * Visitor trait for AST nodes. Each method has a default that delegates up the type hierarchy, so you only need to
 * override the methods you care about.
 *
 * Hierarchy: AstNode → Declaration, Expression, Pattern, TypeExpression, ExposingList, ExposedItem. Override
 * `visitNode` to handle all nodes, `visitDeclaration` for all declarations, etc.
 */
trait AstVisitor[A]:

  // --- Top-level ---
  def visitNode(node: AstNode): A

  // --- Module structure ---
  def visitModule(node: Module): A               = visitNode(node)
  def visitQualifiedName(node: QualifiedName): A = visitNode(node)
  def visitImport(node: Import): A               = visitNode(node)

  // --- Exposing ---
  def visitExposingList(node: ExposingList): A         = visitNode(node)
  def visitExposingAll(node: ExposingAll): A           = visitExposingList(node)
  def visitExposingExplicit(node: ExposingExplicit): A = visitExposingList(node)

  def visitExposedItem(node: ExposedItem): A         = visitNode(node)
  def visitExposedValue(node: ExposedValue): A       = visitExposedItem(node)
  def visitExposedOperator(node: ExposedOperator): A = visitExposedItem(node)
  def visitExposedType(node: ExposedType): A         = visitExposedItem(node)

  // --- Declarations ---
  def visitDeclaration(node: Declaration): A                     = visitNode(node)
  def visitValueDeclaration(node: ValueDeclaration): A           = visitDeclaration(node)
  def visitTypeAliasDeclaration(node: TypeAliasDeclaration): A   = visitDeclaration(node)
  def visitCustomTypeDeclaration(node: CustomTypeDeclaration): A = visitDeclaration(node)
  def visitPortDeclaration(node: PortDeclaration): A             = visitDeclaration(node)
  def visitInfixDeclaration(node: InfixDeclaration): A           = visitDeclaration(node)

  def visitConstructor(node: Constructor): A = visitNode(node)

  // --- Type Expressions ---
  def visitTypeExpression(node: TypeExpression): A   = visitNode(node)
  def visitTypeReference(node: TypeReference): A     = visitTypeExpression(node)
  def visitTypeVariable(node: TypeVariable): A       = visitTypeExpression(node)
  def visitTypeApplication(node: TypeApplication): A = visitTypeExpression(node)
  def visitFunctionType(node: FunctionType): A       = visitTypeExpression(node)
  def visitTupleType(node: TupleType): A             = visitTypeExpression(node)
  def visitUnitType(node: UnitType): A               = visitTypeExpression(node)
  def visitRecordType(node: RecordType): A           = visitTypeExpression(node)
  def visitRecordFieldType(node: RecordFieldType): A = visitNode(node)

  // --- Expressions ---
  def visitExpression(node: Expression): A                   = visitNode(node)
  def visitIntLiteral(node: IntLiteral): A                   = visitExpression(node)
  def visitFloatLiteral(node: FloatLiteral): A               = visitExpression(node)
  def visitStringLiteral(node: StringLiteral): A             = visitExpression(node)
  def visitCharLiteral(node: CharLiteral): A                 = visitExpression(node)
  def visitVariableRef(node: VariableRef): A                 = visitExpression(node)
  def visitConstructorRef(node: ConstructorRef): A           = visitExpression(node)
  def visitOperatorRef(node: OperatorRef): A                 = visitExpression(node)
  def visitFunctionApplication(node: FunctionApplication): A = visitExpression(node)
  def visitBinaryOp(node: BinaryOp): A                       = visitExpression(node)
  def visitNegate(node: Negate): A                           = visitExpression(node)
  def visitIfThenElse(node: IfThenElse): A                   = visitExpression(node)
  def visitLetIn(node: LetIn): A                             = visitExpression(node)
  def visitCaseOf(node: CaseOf): A                           = visitExpression(node)
  def visitLambda(node: Lambda): A                           = visitExpression(node)
  def visitTupleLiteral(node: TupleLiteral): A               = visitExpression(node)
  def visitUnitLiteral(node: UnitLiteral): A                 = visitExpression(node)
  def visitListLiteral(node: ListLiteral): A                 = visitExpression(node)
  def visitRecordLiteral(node: RecordLiteral): A             = visitExpression(node)
  def visitRecordUpdate(node: RecordUpdate): A               = visitExpression(node)
  def visitFieldAccess(node: FieldAccess): A                 = visitExpression(node)
  def visitFieldAccessFunction(node: FieldAccessFunction): A = visitExpression(node)
  def visitParenthesized(node: Parenthesized): A             = visitExpression(node)
  def visitGlsl(node: Glsl): A                               = visitExpression(node)

  def visitLetBinding(node: LetBinding): A   = visitNode(node)
  def visitCaseBranch(node: CaseBranch): A   = visitNode(node)
  def visitRecordField(node: RecordField): A = visitNode(node)

  // --- Patterns ---
  def visitPattern(node: Pattern): A                       = visitNode(node)
  def visitAnythingPattern(node: AnythingPattern): A       = visitPattern(node)
  def visitIntPattern(node: IntPattern): A                 = visitPattern(node)
  def visitFloatPattern(node: FloatPattern): A             = visitPattern(node)
  def visitStringPattern(node: StringPattern): A           = visitPattern(node)
  def visitCharPattern(node: CharPattern): A               = visitPattern(node)
  def visitVariablePattern(node: VariablePattern): A       = visitPattern(node)
  def visitUnitPattern(node: UnitPattern): A               = visitPattern(node)
  def visitConstructorPattern(node: ConstructorPattern): A = visitPattern(node)
  def visitTuplePattern(node: TuplePattern): A             = visitPattern(node)
  def visitListPattern(node: ListPattern): A               = visitPattern(node)
  def visitConsPattern(node: ConsPattern): A               = visitPattern(node)
  def visitRecordPattern(node: RecordPattern): A           = visitPattern(node)
  def visitAsPattern(node: AsPattern): A                   = visitPattern(node)

/**
 * Utilities for visiting and folding over AST trees.
 *
 * Traversal uses trampolining (`scala.util.control.TailCalls`) for stack safety, so arbitrarily deep AST trees can be
 * processed without risk of stack overflow.
 */
object AstVisitor:

  /** Dispatch a node to the appropriate visitor method. */
  def visit[A](node: AstNode, visitor: AstVisitor[A]): A = node match
    case n: Module                => visitor.visitModule(n)
    case n: QualifiedName         => visitor.visitQualifiedName(n)
    case n: Import                => visitor.visitImport(n)
    case n: ExposingAll           => visitor.visitExposingAll(n)
    case n: ExposingExplicit      => visitor.visitExposingExplicit(n)
    case n: ExposedValue          => visitor.visitExposedValue(n)
    case n: ExposedOperator       => visitor.visitExposedOperator(n)
    case n: ExposedType           => visitor.visitExposedType(n)
    case n: ValueDeclaration      => visitor.visitValueDeclaration(n)
    case n: TypeAliasDeclaration  => visitor.visitTypeAliasDeclaration(n)
    case n: CustomTypeDeclaration => visitor.visitCustomTypeDeclaration(n)
    case n: PortDeclaration       => visitor.visitPortDeclaration(n)
    case n: InfixDeclaration      => visitor.visitInfixDeclaration(n)
    case n: Constructor           => visitor.visitConstructor(n)
    case n: TypeReference         => visitor.visitTypeReference(n)
    case n: TypeVariable          => visitor.visitTypeVariable(n)
    case n: TypeApplication       => visitor.visitTypeApplication(n)
    case n: FunctionType          => visitor.visitFunctionType(n)
    case n: TupleType             => visitor.visitTupleType(n)
    case n: UnitType              => visitor.visitUnitType(n)
    case n: RecordType            => visitor.visitRecordType(n)
    case n: RecordFieldType       => visitor.visitRecordFieldType(n)
    case n: IntLiteral            => visitor.visitIntLiteral(n)
    case n: FloatLiteral          => visitor.visitFloatLiteral(n)
    case n: StringLiteral         => visitor.visitStringLiteral(n)
    case n: CharLiteral           => visitor.visitCharLiteral(n)
    case n: VariableRef           => visitor.visitVariableRef(n)
    case n: ConstructorRef        => visitor.visitConstructorRef(n)
    case n: OperatorRef           => visitor.visitOperatorRef(n)
    case n: FunctionApplication   => visitor.visitFunctionApplication(n)
    case n: BinaryOp              => visitor.visitBinaryOp(n)
    case n: Negate                => visitor.visitNegate(n)
    case n: IfThenElse            => visitor.visitIfThenElse(n)
    case n: LetIn                 => visitor.visitLetIn(n)
    case n: LetBinding            => visitor.visitLetBinding(n)
    case n: CaseOf                => visitor.visitCaseOf(n)
    case n: CaseBranch            => visitor.visitCaseBranch(n)
    case n: Lambda                => visitor.visitLambda(n)
    case n: TupleLiteral          => visitor.visitTupleLiteral(n)
    case n: UnitLiteral           => visitor.visitUnitLiteral(n)
    case n: ListLiteral           => visitor.visitListLiteral(n)
    case n: RecordLiteral         => visitor.visitRecordLiteral(n)
    case n: RecordField           => visitor.visitRecordField(n)
    case n: RecordUpdate          => visitor.visitRecordUpdate(n)
    case n: FieldAccess           => visitor.visitFieldAccess(n)
    case n: FieldAccessFunction   => visitor.visitFieldAccessFunction(n)
    case n: Parenthesized         => visitor.visitParenthesized(n)
    case n: Glsl                  => visitor.visitGlsl(n)
    case n: AnythingPattern       => visitor.visitAnythingPattern(n)
    case n: IntPattern            => visitor.visitIntPattern(n)
    case n: FloatPattern          => visitor.visitFloatPattern(n)
    case n: StringPattern         => visitor.visitStringPattern(n)
    case n: CharPattern           => visitor.visitCharPattern(n)
    case n: VariablePattern       => visitor.visitVariablePattern(n)
    case n: UnitPattern           => visitor.visitUnitPattern(n)
    case n: ConstructorPattern    => visitor.visitConstructorPattern(n)
    case n: TuplePattern          => visitor.visitTuplePattern(n)
    case n: ListPattern           => visitor.visitListPattern(n)
    case n: ConsPattern           => visitor.visitConsPattern(n)
    case n: RecordPattern         => visitor.visitRecordPattern(n)
    case n: AsPattern             => visitor.visitAsPattern(n)

  /** Return the direct children of a node. */
  def children(node: AstNode): List[AstNode] = node match
    case n: Module                => n.exposing :: n.imports.toList ::: n.declarations.toList
    case n: QualifiedName         => Nil
    case n: Import                => n.exposing.toList
    case n: ExposingAll           => Nil
    case n: ExposingExplicit      => n.items
    case n: ExposedValue          => Nil
    case n: ExposedOperator       => Nil
    case n: ExposedType           => Nil
    case n: ValueDeclaration      => n.typeAnnotation.toList ::: n.parameters.toList ::: List(n.body)
    case n: TypeAliasDeclaration  => List(n.body)
    case n: CustomTypeDeclaration => n.constructors.toList
    case n: PortDeclaration       => List(n.typeExpr)
    case n: InfixDeclaration      => Nil
    case n: Constructor           => n.parameters.toList
    case n: TypeReference         => List(n.name)
    case n: TypeVariable          => Nil
    case n: TypeApplication       => n.constructor :: n.arguments
    case n: FunctionType          => List(n.from, n.to)
    case n: TupleType             => n.elements
    case n: UnitType              => Nil
    case n: RecordType            => n.fields
    case n: RecordFieldType       => List(n.typeExpr)
    case n: IntLiteral            => Nil
    case n: FloatLiteral          => Nil
    case n: StringLiteral         => Nil
    case n: CharLiteral           => Nil
    case n: VariableRef           => List(n.name)
    case n: ConstructorRef        => List(n.name)
    case n: OperatorRef           => Nil
    case n: FunctionApplication   => n.function :: n.arguments
    case n: BinaryOp              => List(n.left, n.right)
    case n: Negate                => List(n.expr)
    case n: IfThenElse            => List(n.condition, n.thenBranch, n.elseBranch)
    case n: LetIn                 => n.bindings ::: List(n.body)
    case n: LetBinding            => n.typeAnnotation.toList ::: n.parameters ::: List(n.body)
    case n: CaseOf                => n.expr :: n.branches
    case n: CaseBranch            => List(n.pattern, n.body)
    case n: Lambda                => n.parameters ::: List(n.body)
    case n: TupleLiteral          => n.elements
    case n: UnitLiteral           => Nil
    case n: ListLiteral           => n.elements
    case n: RecordLiteral         => n.fields
    case n: RecordField           => List(n.value)
    case n: RecordUpdate          => n.fields
    case n: FieldAccess           => List(n.record)
    case n: FieldAccessFunction   => Nil
    case n: Parenthesized         => List(n.expr)
    case n: Glsl                  => Nil
    case n: AnythingPattern       => Nil
    case n: IntPattern            => Nil
    case n: FloatPattern          => Nil
    case n: StringPattern         => Nil
    case n: CharPattern           => Nil
    case n: VariablePattern       => Nil
    case n: UnitPattern           => Nil
    case n: ConstructorPattern    => n.arguments
    case n: TuplePattern          => n.elements
    case n: ListPattern           => n.elements
    case n: ConsPattern           => List(n.head, n.tail)
    case n: RecordPattern         => Nil
    case n: AsPattern             => List(n.pattern)

  /** Count all nodes in the tree. */
  def count(node: AstNode): Int =
    def go(n: AstNode): TailRec[Int] =
      val kids = children(n)
      if kids.isEmpty then done(1)
      else
        kids.foldLeft(done(1)) { (acc, child) =>
          for
            a <- acc
            c <- tailcall(go(child))
          yield a + c
        }
    go(node).result

  /** Pre-order left fold over all nodes. */
  def foldLeft[A](node: AstNode, z: A)(f: (A, AstNode) => A): A =
    def go(n: AstNode, acc: A): TailRec[A] =
      val updated = f(acc, n)
      val kids    = children(n)
      if kids.isEmpty then done(updated)
      else
        kids.foldLeft(done(updated)) { (tacc, child) =>
          for
            a <- tacc
            r <- tailcall(go(child, a))
          yield r
        }
    go(node, z).result

  /** Collect nodes matching a partial function, pre-order. */
  def collect[B](node: AstNode)(pf: PartialFunction[AstNode, B]): List[B] =
    val buf = ArrayBuffer.empty[B]
    foldLeft(node, ()) { (_, n) =>
      if pf.isDefinedAt(n) then buf += pf(n)
    }
    buf.toList

  /** Collect nodes matching a partial function, post-order. */
  def collectPostOrder[B](node: AstNode)(pf: PartialFunction[AstNode, B]): List[B] =
    val buf                           = ArrayBuffer.empty[B]
    def go(n: AstNode): TailRec[Unit] =
      val kids     = children(n)
      val kidsWork =
        if kids.isEmpty then done(())
        else
          kids.foldLeft(done(())) { (acc, child) =>
            for
              _ <- acc
              _ <- tailcall(go(child))
            yield ()
          }
      for _ <- kidsWork
      yield if pf.isDefinedAt(n) then buf += pf(n)
    go(node).result
    buf.toList

  extension (node: AstNode)

    @targetName("extVisit")
    def visit[A](visitor: AstVisitor[A]): A = AstVisitor.visit(node, visitor)

    @targetName("extChildren")
    def children: List[AstNode] = AstVisitor.children(node)

    @targetName("extCount")
    def count: Int = AstVisitor.count(node)

    @targetName("extFold")
    def fold[A](z: A)(f: (A, AstNode) => A): A = AstVisitor.foldLeft(node, z)(f)

    @targetName("extCollect")
    def collect[B](pf: PartialFunction[AstNode, B]): List[B] = AstVisitor.collect(node)(pf)
