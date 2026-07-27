package morphir.langkit.elm.parser

import morphir.langkit.core.Span
import morphir.langkit.elm.{ElmParseOptions, Leniency, Reported, Severity}
import morphir.langkit.elm.compiler.ParseDiagnostic
import morphir.langkit.elm.cst.*

/**
 * Post-processing pass that re-shapes binary operator chains according to operator precedence and associativity.
 *
 * `elm/compiler` parses operator chains flat (`Src.Binops`) and resolves precedence during canonicalisation, for the
 * same reason `ExpressionParser` cannot do it while parsing: an operator's fixity comes from an `infix` declaration
 * that may appear anywhere in the module, or in a dependency. So the parser emits a flat, left-leaning chain — `a + b
 * \* c` as `((a + b) * c)` — and this pass flattens each chain back into its operands and rebuilds it from
 * [[OperatorTable]].
 *
 * Two chains Elm refuses to group are refused here too, unless `ElmParseOptions` says otherwise:
 *
 *   - operators of equal precedence that cannot be grouped — a non-associative operator chained (`a == b == c`), or a
 *     left- and a right-associative operator mixed (`a |> f <| g`);
 *   - an operator whose fixity nothing in scope declares.
 */
object OperatorReassociator:

  /** Re-associate every operator chain in `module`, reporting every chain that cannot be grouped. */
  def reassociate(
      module: CstModule,
      source: String,
      options: ElmParseOptions = ElmParseOptions.elm
  ): (CstModule, List[Reported]) =
    val context      = Context(OperatorTable.forModule(module, options.operators), source, options)
    val declarations = module.declarations.map(rewriteDeclaration(_, context))
    val rewritten    = CstModule(module.moduleDecl, module.imports, declarations, module.trivia)(module.span)
    (rewritten, context.reported)

  /** Re-associate every operator chain in `expression` against a table the caller has already assembled. */
  def reassociateExpression(
      expression: CstExpression,
      table: OperatorTable,
      source: String,
      options: ElmParseOptions = ElmParseOptions.elm
  ): (CstExpression, List[Reported]) =
    val context = Context(table, source, options)
    (rewrite(expression, context), context.reported)

  /**
   * The fixities in scope, what the caller wants done about the chains Elm rejects, and what has been found so far.
   *
   * The buffer is local to one `reassociate` call and never escapes it — `reported` hands back an immutable list —
   * which keeps the tree-rebuilding code below free of accumulator plumbing.
   */
  private final class Context(val table: OperatorTable, val source: String, val options: ElmParseOptions):
    private val found = scala.collection.mutable.ListBuffer.empty[Reported]

    def report(diagnostic: ParseDiagnostic, leniency: Leniency): Unit =
      val severity = if leniency == Leniency.Reject then Severity.Error else Severity.Advisory
      found += Reported(diagnostic, severity)

    def reported: List[Reported] = found.toList

  private def rewriteDeclaration(declaration: CstDeclaration, context: Context): CstDeclaration =
    declaration match
      case d: CstValueDeclaration =>
        CstValueDeclaration(d.annotation, d.name, d.patterns, rewrite(d.body, context), d.trivia)(d.span)
      case d @ (_: CstTypeAliasDeclaration | _: CstCustomTypeDeclaration | _: CstPortDeclaration |
          _: CstInfixDeclaration) =>
        d

  private def rewrite(expression: CstExpression, context: Context): CstExpression =
    expression match
      case n: CstBinaryOp =>
        val (first, rest) = flatten(n)
        val leading       = rewrite(first, context)
        val following     = rest.map((operator, operand) => (operator, rewrite(operand, context)))
        climb(leading, following, Int.MinValue, None, context)._1

      case n: CstFunctionApplication =>
        CstFunctionApplication(rewrite(n.function, context), n.arguments.map(rewrite(_, context)))(n.span)
      case n: CstNegate     => CstNegate(rewrite(n.expr, context))(n.span)
      case n: CstIfThenElse =>
        CstIfThenElse(
          rewrite(n.condition, context),
          rewrite(n.thenBranch, context),
          rewrite(n.elseBranch, context)
        )(n.span)
      case n: CstLetIn =>
        CstLetIn(n.bindings.map(rewriteLetBinding(_, context)), rewrite(n.body, context))(n.span)
      case n: CstCaseOf =>
        CstCaseOf(rewrite(n.expr, context), n.branches.map(rewriteCaseBranch(_, context)))(n.span)
      case n: CstLambda        => CstLambda(n.parameters, rewrite(n.body, context))(n.span)
      case n: CstTupleLiteral  => CstTupleLiteral(n.elements.map(rewrite(_, context)))(n.span)
      case n: CstListLiteral   => CstListLiteral(n.elements.map(rewrite(_, context)))(n.span)
      case n: CstRecordLiteral => CstRecordLiteral(n.fields.map(rewriteRecordField(_, context)))(n.span)
      case n: CstRecordUpdate  =>
        CstRecordUpdate(n.record, n.fields.map(rewriteRecordField(_, context)))(n.span)
      case n: CstFieldAccess   => CstFieldAccess(rewrite(n.record, context), n.field)(n.span)
      case n: CstParenthesized => CstParenthesized(rewrite(n.expr, context))(n.span)

      case n @ (_: CstIntLiteral | _: CstFloatLiteral | _: CstStringLiteral | _: CstCharLiteral | _: CstVariableRef |
          _: CstConstructorRef | _: CstOperatorRef | _: CstUnitLiteral | _: CstFieldAccessFunction | _: CstGlsl) =>
        n

  private def rewriteLetBinding(binding: CstLetBinding, context: Context): CstLetBinding =
    CstLetBinding(binding.annotation, binding.pattern, binding.parameters, rewrite(binding.body, context))(binding.span)

  private def rewriteCaseBranch(branch: CstCaseBranch, context: Context): CstCaseBranch =
    CstCaseBranch(branch.pattern, rewrite(branch.body, context))(branch.span)

  private def rewriteRecordField(field: CstRecordField, context: Context): CstRecordField =
    CstRecordField(field.name, rewrite(field.value, context))(field.span)

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
   * `previous` is the operator this one would sit beside in the same precedence group, if any — the pair Elm compares
   * when deciding whether a chain can be grouped at all. Returns the tree built so far and the pairs left for an outer,
   * looser-bound caller to consume.
   */
  private def climb(
      left: CstExpression,
      rest: List[(CstName, CstExpression)],
      minPrecedence: Int,
      previous: Option[(CstName, Fixity)],
      context: Context
  ): (CstExpression, List[(CstName, CstExpression)]) =
    rest match
      case (operator, right) :: tail =>
        val fixity = fixityOf(operator, context)
        if fixity.precedence < minPrecedence then (left, rest)
        else
          checkGrouping(previous, operator, fixity, context)
          // A right-associative operator lets an equal-precedence operator group into its right-hand side, and so
          // stays the neighbour that operator is compared against; a left- or non-associative one closes the group
          // here and continues on its own left.
          val groupsRight               = fixity.associativity == Associativity.Right
          val rightBound                = if groupsRight then fixity.precedence else fixity.precedence + 1
          val neighbour                 = if groupsRight then Some(operator -> fixity) else None
          val (rightOperand, remaining) = climb(right, tail, rightBound, neighbour, context)
          val combined                  =
            CstBinaryOp(left, operator, rightOperand)(Span.between(left.span, rightOperand.span))
          climb(combined, remaining, minPrecedence, Some(operator -> fixity), context)
      case Nil => (left, Nil)

  /**
   * The fixity of `operator`, reporting one that nothing in scope declares.
   *
   * A reported operator still gets [[OperatorTable.unknownFixity]] so the rest of the module can be checked; the report
   * is what decides whether the parse ultimately fails.
   */
  private def fixityOf(operator: CstName, context: Context): Fixity =
    context.table.lookup(operator.value) match
      case Some(fixity) => fixity
      case None         =>
        context.report(
          ParseDiagnostic.unknownOperator(context.source, operator.span, operator.value),
          context.options.unknownOperator
        )
        OperatorTable.unknownFixity

  /**
   * Elm groups two operators of equal precedence only when they agree on which way to lean, and never when either is
   * non-associative. A chain that fails this is reported and then grouped to the left, so the pass can carry on and
   * find the next one.
   */
  private def checkGrouping(
      previous: Option[(CstName, Fixity)],
      operator: CstName,
      fixity: Fixity,
      context: Context
  ): Unit =
    previous match
      case Some((previousOperator, previousFixity))
          if previousFixity.precedence == fixity.precedence &&
            (previousFixity.associativity != fixity.associativity ||
              fixity.associativity == Associativity.Non) =>
        context.report(
          ParseDiagnostic.conflictingOperators(
            context.source,
            Span.between(previousOperator.span, operator.span),
            previousOperator.value,
            operator.value
          ),
          context.options.operatorChainConflict
        )
      case _ => ()
