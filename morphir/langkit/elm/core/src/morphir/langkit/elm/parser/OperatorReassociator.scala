package morphir.langkit.elm.parser

import morphir.langkit.core.Span
import morphir.langkit.elm.cst.*

/**
 * Post-processing pass that re-shapes binary operator chains according to operator precedence and associativity.
 *
 * `ExpressionParser` cannot do this while parsing: an operator's fixity comes from an `infix` declaration that may
 * appear anywhere in the module, including after the expression using it. So the parser emits a deliberately flat,
 * left-leaning chain — `a + b * c` as `((a + b) * c)` — and this pass flattens each chain back into its operands and
 * rebuilds it with [[OperatorTable]] deciding the grouping.
 *
 * Two limitations follow from the parser having only one module in hand:
 *
 *   - Operators imported from another module keep [[OperatorTable.unknownFixity]], since resolving them would need the
 *     dependency's source.
 *   - Chaining a non-associative operator (`a == b == c`) is an error in Elm but is accepted here, grouped to the left.
 *     Rejecting it belongs to a later semantic pass, which has the name resolution to do it accurately.
 */
object OperatorReassociator:

  /** Re-associate every operator chain in `module`. */
  def reassociate(module: CstModule): CstModule =
    val table = OperatorTable.forModule(module)
    CstModule(
      module.moduleDecl,
      module.imports,
      module.declarations.map(rewriteDeclaration(_, table)),
      module.trivia
    )(module.span)

  /** Re-associate every operator chain in `expression`, using the fixities in `table`. */
  def reassociateExpression(expression: CstExpression, table: OperatorTable): CstExpression =
    rewrite(expression, table)

  private def rewriteDeclaration(declaration: CstDeclaration, table: OperatorTable): CstDeclaration =
    declaration match
      case d: CstValueDeclaration =>
        CstValueDeclaration(d.annotation, d.name, d.patterns, rewrite(d.body, table), d.trivia)(d.span)
      case d @ (_: CstTypeAliasDeclaration | _: CstCustomTypeDeclaration | _: CstPortDeclaration |
          _: CstInfixDeclaration) =>
        d

  private def rewrite(expression: CstExpression, table: OperatorTable): CstExpression = expression match
    case n: CstBinaryOp =>
      val (first, rest) = flatten(n)
      val leading       = rewrite(first, table)
      val following     = rest.map((operator, operand) => (operator, rewrite(operand, table)))
      climb(leading, following, Int.MinValue, table)._1

    case n: CstFunctionApplication =>
      CstFunctionApplication(rewrite(n.function, table), n.arguments.map(rewrite(_, table)))(n.span)
    case n: CstNegate     => CstNegate(rewrite(n.expr, table))(n.span)
    case n: CstIfThenElse =>
      CstIfThenElse(
        rewrite(n.condition, table),
        rewrite(n.thenBranch, table),
        rewrite(n.elseBranch, table)
      )(n.span)
    case n: CstLetIn =>
      CstLetIn(n.bindings.map(rewriteLetBinding(_, table)), rewrite(n.body, table))(n.span)
    case n: CstCaseOf =>
      CstCaseOf(rewrite(n.expr, table), n.branches.map(rewriteCaseBranch(_, table)))(n.span)
    case n: CstLambda        => CstLambda(n.parameters, rewrite(n.body, table))(n.span)
    case n: CstTupleLiteral  => CstTupleLiteral(n.elements.map(rewrite(_, table)))(n.span)
    case n: CstListLiteral   => CstListLiteral(n.elements.map(rewrite(_, table)))(n.span)
    case n: CstRecordLiteral =>
      CstRecordLiteral(n.fields.map(rewriteRecordField(_, table)))(n.span)
    case n: CstRecordUpdate =>
      CstRecordUpdate(n.record, n.fields.map(rewriteRecordField(_, table)))(n.span)
    case n: CstFieldAccess   => CstFieldAccess(rewrite(n.record, table), n.field)(n.span)
    case n: CstParenthesized => CstParenthesized(rewrite(n.expr, table))(n.span)

    case n @ (_: CstIntLiteral | _: CstFloatLiteral | _: CstStringLiteral | _: CstCharLiteral | _: CstVariableRef |
        _: CstConstructorRef | _: CstOperatorRef | _: CstUnitLiteral | _: CstFieldAccessFunction | _: CstGlsl) =>
      n

  private def rewriteLetBinding(binding: CstLetBinding, table: OperatorTable): CstLetBinding =
    CstLetBinding(binding.annotation, binding.pattern, binding.parameters, rewrite(binding.body, table))(binding.span)

  private def rewriteCaseBranch(branch: CstCaseBranch, table: OperatorTable): CstCaseBranch =
    CstCaseBranch(branch.pattern, rewrite(branch.body, table))(branch.span)

  private def rewriteRecordField(field: CstRecordField, table: OperatorTable): CstRecordField =
    CstRecordField(field.name, rewrite(field.value, table))(field.span)

  /**
   * Split the parser's left-leaning chain back into its leading operand and the `(operator, operand)` pairs that
   * follow. The parser only ever puts a non-operator expression on the right of a `CstBinaryOp`, so walking the left
   * spine recovers the source order exactly.
   */
  private def flatten(expression: CstExpression): (CstExpression, List[(CstName, CstExpression)]) =
    expression match
      case n: CstBinaryOp =>
        val (first, rest) = flatten(n.left)
        (first, rest :+ (n.operator, n.right))
      case other => (other, Nil)

  /**
   * Precedence climbing: fold operands into `left` while the next operator binds at least as tightly as
   * `minPrecedence`, recursing on the right-hand side with the bound the operator's own associativity implies.
   *
   * Returns the tree built so far and the pairs left for an outer, looser-bound caller to consume.
   */
  private def climb(
      left: CstExpression,
      rest: List[(CstName, CstExpression)],
      minPrecedence: Int,
      table: OperatorTable
  ): (CstExpression, List[(CstName, CstExpression)]) =
    rest match
      case (operator, right) :: tail =>
        val fixity = table.fixityOf(operator.value)
        if fixity.precedence < minPrecedence then (left, rest)
        else
          // A right-associative operator lets an equal-precedence operator group into its right-hand side; a left- or
          // non-associative one does not, so the chain closes here and continues on this operator's left.
          val rightBound =
            if fixity.associativity == Associativity.Right then fixity.precedence else fixity.precedence + 1
          val (rightOperand, remaining) = climb(right, tail, rightBound, table)
          val combined                  =
            CstBinaryOp(left, operator, rightOperand)(Span.between(left.span, rightOperand.span))
          climb(combined, remaining, minPrecedence, table)
      case Nil => (left, Nil)
