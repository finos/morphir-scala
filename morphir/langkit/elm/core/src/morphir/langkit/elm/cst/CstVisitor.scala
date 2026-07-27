package morphir.langkit.elm.cst

import scala.annotation.targetName
import scala.collection.mutable.ArrayBuffer
import scala.util.control.TailCalls.*

/**
 * Visitor trait for CST nodes. Each method has a default that delegates up the type hierarchy, so you only need to
 * override the methods you care about.
 *
 * Hierarchy: CstNode → CstDeclaration, CstExpression, CstPattern, CstTypeExpression, CstExposingList, CstExposedItem.
 * Override `visitNode` to handle all nodes, `visitDeclaration` for all declarations, etc.
 */
trait CstVisitor[A]:

  // --- Top-level ---
  def visitNode(node: CstNode): A

  // --- Module structure ---
  def visitModule(node: CstModule): A                       = visitNode(node)
  def visitModuleDeclaration(node: CstModuleDeclaration): A = visitNode(node)
  def visitEffectManager(node: CstEffectManager): A         = visitNode(node)
  def visitQualifiedName(node: CstQualifiedName): A         = visitNode(node)
  def visitName(node: CstName): A                           = visitNode(node)
  def visitTriviaItem(node: CstTriviaItem): A               = visitNode(node)
  def visitComment(node: CstComment): A                     = visitTriviaItem(node)
  def visitImport(node: CstImport): A                       = visitNode(node)

  // --- Exposing ---
  def visitExposingList(node: CstExposingList): A         = visitNode(node)
  def visitExposingAll(node: CstExposingAll): A           = visitExposingList(node)
  def visitExposingExplicit(node: CstExposingExplicit): A = visitExposingList(node)

  def visitExposedItem(node: CstExposedItem): A         = visitNode(node)
  def visitExposedValue(node: CstExposedValue): A       = visitExposedItem(node)
  def visitExposedOperator(node: CstExposedOperator): A = visitExposedItem(node)
  def visitExposedType(node: CstExposedType): A         = visitExposedItem(node)

  // --- Declarations ---
  def visitDeclaration(node: CstDeclaration): A                     = visitNode(node)
  def visitValueDeclaration(node: CstValueDeclaration): A           = visitDeclaration(node)
  def visitTypeAliasDeclaration(node: CstTypeAliasDeclaration): A   = visitDeclaration(node)
  def visitCustomTypeDeclaration(node: CstCustomTypeDeclaration): A = visitDeclaration(node)
  def visitPortDeclaration(node: CstPortDeclaration): A             = visitDeclaration(node)
  def visitInfixDeclaration(node: CstInfixDeclaration): A           = visitDeclaration(node)

  def visitTypeAnnotation(node: CstTypeAnnotation): A = visitNode(node)
  def visitConstructor(node: CstConstructor): A       = visitNode(node)

  // --- Type Expressions ---
  def visitTypeExpression(node: CstTypeExpression): A   = visitNode(node)
  def visitTypeReference(node: CstTypeReference): A     = visitTypeExpression(node)
  def visitTypeVariable(node: CstTypeVariable): A       = visitTypeExpression(node)
  def visitTypeApplication(node: CstTypeApplication): A = visitTypeExpression(node)
  def visitFunctionType(node: CstFunctionType): A       = visitTypeExpression(node)
  def visitTupleType(node: CstTupleType): A             = visitTypeExpression(node)
  def visitUnitType(node: CstUnitType): A               = visitTypeExpression(node)
  def visitRecordType(node: CstRecordType): A           = visitTypeExpression(node)
  def visitRecordFieldType(node: CstRecordFieldType): A = visitNode(node)

  // --- Expressions ---
  def visitExpression(node: CstExpression): A                   = visitNode(node)
  def visitIntLiteral(node: CstIntLiteral): A                   = visitExpression(node)
  def visitFloatLiteral(node: CstFloatLiteral): A               = visitExpression(node)
  def visitStringLiteral(node: CstStringLiteral): A             = visitExpression(node)
  def visitCharLiteral(node: CstCharLiteral): A                 = visitExpression(node)
  def visitVariableRef(node: CstVariableRef): A                 = visitExpression(node)
  def visitConstructorRef(node: CstConstructorRef): A           = visitExpression(node)
  def visitOperatorRef(node: CstOperatorRef): A                 = visitExpression(node)
  def visitFunctionApplication(node: CstFunctionApplication): A = visitExpression(node)
  def visitBinaryOp(node: CstBinaryOp): A                       = visitExpression(node)
  def visitNegate(node: CstNegate): A                           = visitExpression(node)
  def visitIfThenElse(node: CstIfThenElse): A                   = visitExpression(node)
  def visitLetIn(node: CstLetIn): A                             = visitExpression(node)
  def visitCaseOf(node: CstCaseOf): A                           = visitExpression(node)
  def visitLambda(node: CstLambda): A                           = visitExpression(node)
  def visitTupleLiteral(node: CstTupleLiteral): A               = visitExpression(node)
  def visitUnitLiteral(node: CstUnitLiteral): A                 = visitExpression(node)
  def visitListLiteral(node: CstListLiteral): A                 = visitExpression(node)
  def visitRecordLiteral(node: CstRecordLiteral): A             = visitExpression(node)
  def visitRecordUpdate(node: CstRecordUpdate): A               = visitExpression(node)
  def visitFieldAccess(node: CstFieldAccess): A                 = visitExpression(node)
  def visitFieldAccessFunction(node: CstFieldAccessFunction): A = visitExpression(node)
  def visitParenthesized(node: CstParenthesized): A             = visitExpression(node)
  def visitGlsl(node: CstGlsl): A                               = visitExpression(node)

  def visitLetBinding(node: CstLetBinding): A   = visitNode(node)
  def visitCaseBranch(node: CstCaseBranch): A   = visitNode(node)
  def visitRecordField(node: CstRecordField): A = visitNode(node)

  // --- Patterns ---
  def visitPattern(node: CstPattern): A                           = visitNode(node)
  def visitAnythingPattern(node: CstAnythingPattern): A           = visitPattern(node)
  def visitIntPattern(node: CstIntPattern): A                     = visitPattern(node)
  def visitFloatPattern(node: CstFloatPattern): A                 = visitPattern(node)
  def visitStringPattern(node: CstStringPattern): A               = visitPattern(node)
  def visitCharPattern(node: CstCharPattern): A                   = visitPattern(node)
  def visitVariablePattern(node: CstVariablePattern): A           = visitPattern(node)
  def visitUnitPattern(node: CstUnitPattern): A                   = visitPattern(node)
  def visitConstructorPattern(node: CstConstructorPattern): A     = visitPattern(node)
  def visitTuplePattern(node: CstTuplePattern): A                 = visitPattern(node)
  def visitListPattern(node: CstListPattern): A                   = visitPattern(node)
  def visitConsPattern(node: CstConsPattern): A                   = visitPattern(node)
  def visitRecordPattern(node: CstRecordPattern): A               = visitPattern(node)
  def visitAsPattern(node: CstAsPattern): A                       = visitPattern(node)
  def visitParenthesizedPattern(node: CstParenthesizedPattern): A = visitPattern(node)

/**
 * Utilities for visiting and folding over CST trees.
 *
 * Traversal uses trampolining (`scala.util.control.TailCalls`) for stack safety, so arbitrarily deep CST trees can be
 * processed without risk of stack overflow.
 */
object CstVisitor:

  /** Dispatch a node to the appropriate visitor method. */
  def visit[A](node: CstNode, visitor: CstVisitor[A]): A = node match
    case n: CstModule                  => visitor.visitModule(n)
    case n: CstModuleDeclaration       => visitor.visitModuleDeclaration(n)
    case n: CstEffectManager           => visitor.visitEffectManager(n)
    case n: CstQualifiedName           => visitor.visitQualifiedName(n)
    case n: CstName                    => visitor.visitName(n)
    case n: CstComment                 => visitor.visitComment(n)
    case n: CstImport                  => visitor.visitImport(n)
    case n: CstExposingAll             => visitor.visitExposingAll(n)
    case n: CstExposingExplicit        => visitor.visitExposingExplicit(n)
    case n: CstExposedValue            => visitor.visitExposedValue(n)
    case n: CstExposedOperator         => visitor.visitExposedOperator(n)
    case n: CstExposedType             => visitor.visitExposedType(n)
    case n: CstExposedConstructorsAll  => visitor.visitNode(n)
    case n: CstExposedConstructorsNone => visitor.visitNode(n)
    case n: CstValueDeclaration        => visitor.visitValueDeclaration(n)
    case n: CstTypeAnnotation          => visitor.visitTypeAnnotation(n)
    case n: CstTypeAliasDeclaration    => visitor.visitTypeAliasDeclaration(n)
    case n: CstCustomTypeDeclaration   => visitor.visitCustomTypeDeclaration(n)
    case n: CstConstructor             => visitor.visitConstructor(n)
    case n: CstPortDeclaration         => visitor.visitPortDeclaration(n)
    case n: CstInfixDeclaration        => visitor.visitInfixDeclaration(n)
    case n: CstTypeReference           => visitor.visitTypeReference(n)
    case n: CstTypeVariable            => visitor.visitTypeVariable(n)
    case n: CstTypeApplication         => visitor.visitTypeApplication(n)
    case n: CstFunctionType            => visitor.visitFunctionType(n)
    case n: CstTupleType               => visitor.visitTupleType(n)
    case n: CstUnitType                => visitor.visitUnitType(n)
    case n: CstRecordType              => visitor.visitRecordType(n)
    case n: CstRecordFieldType         => visitor.visitRecordFieldType(n)
    case n: CstIntLiteral              => visitor.visitIntLiteral(n)
    case n: CstFloatLiteral            => visitor.visitFloatLiteral(n)
    case n: CstStringLiteral           => visitor.visitStringLiteral(n)
    case n: CstCharLiteral             => visitor.visitCharLiteral(n)
    case n: CstVariableRef             => visitor.visitVariableRef(n)
    case n: CstConstructorRef          => visitor.visitConstructorRef(n)
    case n: CstOperatorRef             => visitor.visitOperatorRef(n)
    case n: CstFunctionApplication     => visitor.visitFunctionApplication(n)
    case n: CstBinaryOp                => visitor.visitBinaryOp(n)
    case n: CstNegate                  => visitor.visitNegate(n)
    case n: CstIfThenElse              => visitor.visitIfThenElse(n)
    case n: CstLetIn                   => visitor.visitLetIn(n)
    case n: CstLetBinding              => visitor.visitLetBinding(n)
    case n: CstCaseOf                  => visitor.visitCaseOf(n)
    case n: CstCaseBranch              => visitor.visitCaseBranch(n)
    case n: CstLambda                  => visitor.visitLambda(n)
    case n: CstTupleLiteral            => visitor.visitTupleLiteral(n)
    case n: CstUnitLiteral             => visitor.visitUnitLiteral(n)
    case n: CstListLiteral             => visitor.visitListLiteral(n)
    case n: CstRecordLiteral           => visitor.visitRecordLiteral(n)
    case n: CstRecordField             => visitor.visitRecordField(n)
    case n: CstRecordUpdate            => visitor.visitRecordUpdate(n)
    case n: CstFieldAccess             => visitor.visitFieldAccess(n)
    case n: CstFieldAccessFunction     => visitor.visitFieldAccessFunction(n)
    case n: CstParenthesized           => visitor.visitParenthesized(n)
    case n: CstGlsl                    => visitor.visitGlsl(n)
    case n: CstAnythingPattern         => visitor.visitAnythingPattern(n)
    case n: CstIntPattern              => visitor.visitIntPattern(n)
    case n: CstFloatPattern            => visitor.visitFloatPattern(n)
    case n: CstStringPattern           => visitor.visitStringPattern(n)
    case n: CstCharPattern             => visitor.visitCharPattern(n)
    case n: CstVariablePattern         => visitor.visitVariablePattern(n)
    case n: CstUnitPattern             => visitor.visitUnitPattern(n)
    case n: CstConstructorPattern      => visitor.visitConstructorPattern(n)
    case n: CstTuplePattern            => visitor.visitTuplePattern(n)
    case n: CstListPattern             => visitor.visitListPattern(n)
    case n: CstConsPattern             => visitor.visitConsPattern(n)
    case n: CstRecordPattern           => visitor.visitRecordPattern(n)
    case n: CstAsPattern               => visitor.visitAsPattern(n)
    case n: CstParenthesizedPattern    => visitor.visitParenthesizedPattern(n)

  /** Return the direct children of a node. */
  def children(node: CstNode): List[CstNode] = node match
    case n: CstModule            => n.moduleDecl :: n.imports.toList ::: n.declarations.toList ::: n.trivia.items.toList
    case n: CstModuleDeclaration => List(n.name, n.exposing) ::: n.manager.toList
    case n: CstEffectManager     => n.command.toList ::: n.subscription.toList
    case n: CstQualifiedName     => n.parts
    case n: CstName              => Nil
    case n: CstComment           => Nil
    case n: CstImport            => n.moduleName :: n.alias.toList ::: n.exposing.toList
    case n: CstExposingAll       => Nil
    case n: CstExposingExplicit  => n.items
    case n: CstExposedValue      => List(n.name)
    case n: CstExposedOperator   => List(n.name)
    case n: CstExposedType       => n.name :: n.constructors.toList
    case _: CstExposedConstructorsAll  => Nil
    case _: CstExposedConstructorsNone => Nil
    case n: CstValueDeclaration        =>
      n.trivia.items.toList ::: n.annotation.toList ::: List(n.name) ::: n.patterns.toList ::: List(n.body)
    case n: CstTypeAnnotation       => List(n.name, n.typeExpr)
    case n: CstTypeAliasDeclaration =>
      n.trivia.items.toList ::: List(n.name) ::: n.typeVariables.toList ::: List(n.body)
    case n: CstCustomTypeDeclaration =>
      n.trivia.items.toList ::: List(n.name) ::: n.typeVariables.toList ::: n.constructors.toList
    case n: CstConstructor          => n.name :: n.parameters.toList
    case n: CstPortDeclaration      => n.trivia.items.toList ::: List(n.name, n.typeExpr)
    case n: CstInfixDeclaration     => n.trivia.items.toList ::: List(n.operator, n.function)
    case n: CstTypeReference        => List(n.name)
    case n: CstTypeVariable         => List(n.name)
    case n: CstTypeApplication      => n.constructor :: n.arguments
    case n: CstFunctionType         => List(n.from, n.to)
    case n: CstTupleType            => n.elements
    case n: CstUnitType             => Nil
    case n: CstRecordType           => n.extensionVariable.toList ::: n.fields
    case n: CstRecordFieldType      => List(n.name, n.typeExpr)
    case n: CstIntLiteral           => Nil
    case n: CstFloatLiteral         => Nil
    case n: CstStringLiteral        => Nil
    case n: CstCharLiteral          => Nil
    case n: CstVariableRef          => List(n.name)
    case n: CstConstructorRef       => List(n.name)
    case n: CstOperatorRef          => List(n.name)
    case n: CstFunctionApplication  => n.function :: n.arguments
    case n: CstBinaryOp             => List(n.left, n.operator, n.right)
    case n: CstNegate               => List(n.expr)
    case n: CstIfThenElse           => List(n.condition, n.thenBranch, n.elseBranch)
    case n: CstLetIn                => n.bindings ::: List(n.body)
    case n: CstLetBinding           => n.annotation.toList ::: List(n.pattern) ::: n.parameters ::: List(n.body)
    case n: CstCaseOf               => n.expr :: n.branches
    case n: CstCaseBranch           => List(n.pattern, n.body)
    case n: CstLambda               => n.parameters ::: List(n.body)
    case n: CstTupleLiteral         => n.elements
    case n: CstUnitLiteral          => Nil
    case n: CstListLiteral          => n.elements
    case n: CstRecordLiteral        => n.fields
    case n: CstRecordField          => List(n.name, n.value)
    case n: CstRecordUpdate         => n.record :: n.fields
    case n: CstFieldAccess          => List(n.record, n.field)
    case n: CstFieldAccessFunction  => List(n.field)
    case n: CstParenthesized        => List(n.expr)
    case n: CstGlsl                 => Nil
    case n: CstAnythingPattern      => Nil
    case n: CstIntPattern           => Nil
    case n: CstFloatPattern         => Nil
    case n: CstStringPattern        => Nil
    case n: CstCharPattern          => Nil
    case n: CstVariablePattern      => List(n.name)
    case n: CstUnitPattern          => Nil
    case n: CstConstructorPattern   => n.name :: n.arguments
    case n: CstTuplePattern         => n.elements
    case n: CstListPattern          => n.elements
    case n: CstConsPattern          => List(n.head, n.tail)
    case n: CstRecordPattern        => n.fields
    case n: CstAsPattern            => List(n.pattern, n.alias)
    case n: CstParenthesizedPattern => List(n.pattern)

  /** Count all nodes in the tree. */
  def count(node: CstNode): Int =
    def go(n: CstNode): TailRec[Int] =
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
  def foldLeft[A](node: CstNode, z: A)(f: (A, CstNode) => A): A =
    def go(n: CstNode, acc: A): TailRec[A] =
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
  def collect[B](node: CstNode)(pf: PartialFunction[CstNode, B]): List[B] =
    val buf = ArrayBuffer.empty[B]
    foldLeft(node, ()) { (_, n) =>
      if pf.isDefinedAt(n) then buf += pf(n)
    }
    buf.toList

  /** Collect nodes matching a partial function, post-order. */
  def collectPostOrder[B](node: CstNode)(pf: PartialFunction[CstNode, B]): List[B] =
    val buf                           = ArrayBuffer.empty[B]
    def go(n: CstNode): TailRec[Unit] =
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

  extension (node: CstNode)

    @targetName("extVisit")
    def visit[A](visitor: CstVisitor[A]): A = CstVisitor.visit(node, visitor)

    @targetName("extChildren")
    def children: List[CstNode] = CstVisitor.children(node)

    @targetName("extCount")
    def count: Int = CstVisitor.count(node)

    @targetName("extFold")
    def fold[A](z: A)(f: (A, CstNode) => A): A = CstVisitor.foldLeft(node, z)(f)

    @targetName("extCollect")
    def collect[B](pf: PartialFunction[CstNode, B]): List[B] = CstVisitor.collect(node)(pf)
