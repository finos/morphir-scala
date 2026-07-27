package morphir.langkit.elm.parser

import morphir.langkit.core.Span
import morphir.langkit.elm.{ElmParseOptions, Leniency}
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

  /** Re-associate every operator chain in `module`, or report the first chain that cannot be grouped. */
  def reassociate(
      module: CstModule,
      source: String,
      options: ElmParseOptions = ElmParseOptions.elm
  ): Either[ParseDiagnostic, CstModule] =
    val context = Context(OperatorTable.forModule(module, options.operators), source, options)
    traverse(module.declarations.toList)(rewriteDeclaration(_, context)).map { declarations =>
      CstModule(module.moduleDecl, module.imports, declarations.toIndexedSeq, module.trivia)(module.span)
    }

  /** Re-associate every operator chain in `expression` against a table the caller has already assembled. */
  def reassociateExpression(
      expression: CstExpression,
      table: OperatorTable,
      source: String,
      options: ElmParseOptions = ElmParseOptions.elm
  ): Either[ParseDiagnostic, CstExpression] =
    rewrite(expression, Context(table, source, options))

  /** The fixities in scope, plus what the caller wants done about the chains Elm rejects. */
  private final case class Context(table: OperatorTable, source: String, options: ElmParseOptions)

  private def traverse[A, B](items: List[A])(f: A => Either[ParseDiagnostic, B]): Either[ParseDiagnostic, List[B]] =
    items.foldLeft[Either[ParseDiagnostic, List[B]]](Right(Nil)) { (acc, item) =>
      for
        done <- acc
        next <- f(item)
      yield done :+ next
    }

  private def rewriteDeclaration(
      declaration: CstDeclaration,
      context: Context
  ): Either[ParseDiagnostic, CstDeclaration] =
    declaration match
      case d: CstValueDeclaration =>
        rewrite(d.body, context).map { body =>
          CstValueDeclaration(d.annotation, d.name, d.patterns, body, d.trivia)(d.span)
        }
      case d @ (_: CstTypeAliasDeclaration | _: CstCustomTypeDeclaration | _: CstPortDeclaration |
          _: CstInfixDeclaration) =>
        Right(d)

  private def rewrite(expression: CstExpression, context: Context): Either[ParseDiagnostic, CstExpression] =
    expression match
      case n: CstBinaryOp =>
        val (first, rest) = flatten(n)
        for
          leading   <- rewrite(first, context)
          following <- traverse(rest) { (operator, operand) =>
            rewrite(operand, context).map(operator -> _)
          }
          built <- climb(leading, following, Int.MinValue, None, context)
        yield built._1

      case n: CstFunctionApplication =>
        for
          function  <- rewrite(n.function, context)
          arguments <- traverse(n.arguments)(rewrite(_, context))
        yield CstFunctionApplication(function, arguments)(n.span)
      case n: CstNegate     => rewrite(n.expr, context).map(CstNegate(_)(n.span))
      case n: CstIfThenElse =>
        for
          condition  <- rewrite(n.condition, context)
          thenBranch <- rewrite(n.thenBranch, context)
          elseBranch <- rewrite(n.elseBranch, context)
        yield CstIfThenElse(condition, thenBranch, elseBranch)(n.span)
      case n: CstLetIn =>
        for
          bindings <- traverse(n.bindings)(rewriteLetBinding(_, context))
          body     <- rewrite(n.body, context)
        yield CstLetIn(bindings, body)(n.span)
      case n: CstCaseOf =>
        for
          scrutinee <- rewrite(n.expr, context)
          branches  <- traverse(n.branches)(rewriteCaseBranch(_, context))
        yield CstCaseOf(scrutinee, branches)(n.span)
      case n: CstLambda        => rewrite(n.body, context).map(CstLambda(n.parameters, _)(n.span))
      case n: CstTupleLiteral  => traverse(n.elements)(rewrite(_, context)).map(CstTupleLiteral(_)(n.span))
      case n: CstListLiteral   => traverse(n.elements)(rewrite(_, context)).map(CstListLiteral(_)(n.span))
      case n: CstRecordLiteral =>
        traverse(n.fields)(rewriteRecordField(_, context)).map(CstRecordLiteral(_)(n.span))
      case n: CstRecordUpdate =>
        traverse(n.fields)(rewriteRecordField(_, context)).map(CstRecordUpdate(n.record, _)(n.span))
      case n: CstFieldAccess   => rewrite(n.record, context).map(CstFieldAccess(_, n.field)(n.span))
      case n: CstParenthesized => rewrite(n.expr, context).map(CstParenthesized(_)(n.span))

      case n @ (_: CstIntLiteral | _: CstFloatLiteral | _: CstStringLiteral | _: CstCharLiteral | _: CstVariableRef |
          _: CstConstructorRef | _: CstOperatorRef | _: CstUnitLiteral | _: CstFieldAccessFunction | _: CstGlsl) =>
        Right(n)

  private def rewriteLetBinding(binding: CstLetBinding, context: Context): Either[ParseDiagnostic, CstLetBinding] =
    rewrite(binding.body, context).map { body =>
      CstLetBinding(binding.annotation, binding.pattern, binding.parameters, body)(binding.span)
    }

  private def rewriteCaseBranch(branch: CstCaseBranch, context: Context): Either[ParseDiagnostic, CstCaseBranch] =
    rewrite(branch.body, context).map(CstCaseBranch(branch.pattern, _)(branch.span))

  private def rewriteRecordField(field: CstRecordField, context: Context): Either[ParseDiagnostic, CstRecordField] =
    rewrite(field.value, context).map(CstRecordField(field.name, _)(field.span))

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
  ): Either[ParseDiagnostic, (CstExpression, List[(CstName, CstExpression)])] =
    rest match
      case (operator, right) :: tail =>
        fixityOf(operator, context).flatMap { fixity =>
          if fixity.precedence < minPrecedence then Right((left, rest))
          else
            checkGrouping(previous, operator, fixity, context).flatMap { _ =>
              // A right-associative operator lets an equal-precedence operator group into its right-hand side, and so
              // stays the neighbour that operator is compared against; a left- or non-associative one closes the
              // group here and continues on its own left.
              val groupsRight = fixity.associativity == Associativity.Right
              val rightBound  = if groupsRight then fixity.precedence else fixity.precedence + 1
              val neighbour   = if groupsRight then Some(operator -> fixity) else None
              climb(right, tail, rightBound, neighbour, context).flatMap { (rightOperand, remaining) =>
                val combined =
                  CstBinaryOp(left, operator, rightOperand)(Span.between(left.span, rightOperand.span))
                climb(combined, remaining, minPrecedence, Some(operator -> fixity), context)
              }
            }
        }
      case Nil => Right((left, Nil))

  private def fixityOf(operator: CstName, context: Context): Either[ParseDiagnostic, Fixity] =
    context.table.lookup(operator.value) match
      case Some(fixity)                                               => Right(fixity)
      case None if context.options.unknownOperator == Leniency.Accept => Right(OperatorTable.unknownFixity)
      case None                                                       =>
        Left(ParseDiagnostic.unknownOperator(context.source, operator.span, operator.value))

  /**
   * Elm groups two operators of equal precedence only when they agree on which way to lean, and never when either is
   * non-associative.
   */
  private def checkGrouping(
      previous: Option[(CstName, Fixity)],
      operator: CstName,
      fixity: Fixity,
      context: Context
  ): Either[ParseDiagnostic, Unit] =
    previous match
      case Some((previousOperator, previousFixity))
          if previousFixity.precedence == fixity.precedence &&
            (previousFixity.associativity != fixity.associativity ||
              fixity.associativity == Associativity.Non) =>
        if context.options.operatorChainConflict == Leniency.Accept then Right(())
        else
          Left(
            ParseDiagnostic.conflictingOperators(
              context.source,
              Span.between(previousOperator.span, operator.span),
              previousOperator.value,
              operator.value
            )
          )
      case _ => Right(())
