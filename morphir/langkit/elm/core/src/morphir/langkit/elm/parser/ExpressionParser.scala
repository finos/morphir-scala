package morphir.langkit.elm.parser

import parsley.Parsley
import parsley.Parsley.{atomic, lookAhead, many, some}
import parsley.combinator.option
import parsley.position.{offset, pos}

import morphir.langkit.core.Span
import morphir.langkit.elm.cst.*
import morphir.langkit.elm.lexer.ElmLexer.*

/**
 * Parser for Elm expressions.
 *
 * Handles literals, variables, constructors, function application, binary operators, if/then/else, let/in, case/of,
 * lambdas, tuples, lists, records, and field access.
 */
object ExpressionParser:

  /**
   * Guard a parser so it only fires when the next token is on the same line as `start` or indented past `start`'s
   * column. Used to prevent expression continuation from swallowing the first token of a later top-level declaration.
   */
  private def sameLineOrIndentedPast[A](start: (Int, Int))(p: Parsley[A]): Parsley[A] =
    lookAhead(pos.filter { case (line, col) => line == start._1 || col > start._2 }) *> p

  // -----------------------------------------------------------------------
  // Atoms
  // -----------------------------------------------------------------------

  private val intLit: Parsley[CstExpression] = (offset <~> intLiteral <~> offset).map { case ((s, v), e) =>
    CstIntLiteral(v)(Span.fromStartEnd(s, e))
  }

  private val floatLit: Parsley[CstExpression] = (offset <~> floatLiteral <~> offset).map { case ((s, v), e) =>
    CstFloatLiteral(v)(Span.fromStartEnd(s, e))
  }

  private val stringLit: Parsley[CstExpression] = (offset <~> stringLiteral <~> offset).map { case ((s, v), e) =>
    CstStringLiteral(v)(Span.fromStartEnd(s, e))
  }

  private val charLit: Parsley[CstExpression] = (offset <~> charLiteral <~> offset).map { case ((s, v), e) =>
    CstCharLiteral(v)(Span.fromStartEnd(s, e))
  }

  private val variableRef: Parsley[CstExpression] = (offset <~> ModuleParser.qualifiedValueName <~> offset).map {
    case ((s, qn), e) =>
      CstVariableRef(qn)(Span.fromStartEnd(s, e))
  }

  private val constructorRef: Parsley[CstExpression] = (offset <~> ModuleParser.qualifiedName <~> offset).map {
    case ((s, qn), e) =>
      CstConstructorRef(qn)(Span.fromStartEnd(s, e))
  }

  private val unitLit: Parsley[CstExpression] =
    atomic((offset <~> parens(Parsley.pure(())) <~> offset).map { case ((s, _), e) =>
      CstUnitLiteral()(Span.fromStartEnd(s, e))
    })

  private val parenthesized: Parsley[CstExpression] = (offset <~> parens(expression) <~> offset).map {
    case ((s, expr), e) =>
      CstParenthesized(expr)(Span.fromStartEnd(s, e))
  }

  private val tupleLit: Parsley[CstExpression] =
    (offset <~> parens(expression <~> some(symbol(",") *> expression)) <~> offset).map {
      case ((s, (first, rest)), e) =>
        CstTupleLiteral(first :: rest)(Span.fromStartEnd(s, e))
    }

  private val listLit: Parsley[CstExpression] = (offset <~> brackets(commaSep(expression)) <~> offset).map {
    case ((s, elems), e) =>
      CstListLiteral(elems)(Span.fromStartEnd(s, e))
  }

  private val recordField: Parsley[CstRecordField] =
    (offset <~> ModuleParser.lowerName <~> (symbol("=") *> expression) <~> offset).map { case (((s, n), v), e) =>
      CstRecordField(n, v)(Span.fromStartEnd(s, e))
    }

  private val recordLit: Parsley[CstExpression] = (offset <~> braces(commaSep1(recordField)) <~> offset).map {
    case ((s, fields), e) =>
      CstRecordLiteral(fields)(Span.fromStartEnd(s, e))
  }

  private val recordUpdate: Parsley[CstExpression] =
    (offset <~> braces(ModuleParser.lowerName <~> (symbol("|") *> commaSep1(recordField))) <~> offset).map {
      case ((s, (rec, fields)), e) =>
        CstRecordUpdate(rec, fields)(Span.fromStartEnd(s, e))
    }

  private val fieldAccessFn: Parsley[CstExpression] =
    (offset <~> (symbol(".") *> ModuleParser.lowerName) <~> offset).map { case ((s, n), e) =>
      CstFieldAccessFunction(n)(Span.fromStartEnd(s, e))
    }

  /** An atomic expression (no application or binary ops). */
  val atom: Parsley[CstExpression] =
    atomic(floatLit)
      | intLit
      | stringLit
      | charLit
      | unitLit
      | atomic(tupleLit)
      | parenthesized
      | listLit
      | atomic(recordUpdate)
      | recordLit
      | fieldAccessFn
      | variableRef
      | constructorRef

  // -----------------------------------------------------------------------
  // Compound expressions
  // -----------------------------------------------------------------------

  private val ifThenElse: Parsley[CstExpression] =
    (offset <~> (keyword("if") *> expression) <~>
      (keyword("then") *> expression) <~>
      (keyword("else") *> expression) <~> offset).map { case ((((s, cond), thenE), elseE), e) =>
      CstIfThenElse(cond, thenE, elseE)(Span.fromStartEnd(s, e))
    }

  private val letBinding: Parsley[CstLetBinding] =
    (offset <~>
      option(atomic(DeclarationParser.typeAnnotation)) <~>
      PatternParser.pattern <~>
      many(PatternParser.pattern) <~>
      (symbol("=") *> expression) <~> offset).map { case (((((s, ann), pat), params), body), e) =>
      CstLetBinding(ann, pat, params, body)(Span.fromStartEnd(s, e))
    }

  private val letIn: Parsley[CstExpression] =
    (offset <~> (keyword("let") *> some(letBinding)) <~>
      (keyword("in") *> expression) <~> offset).map { case (((s, bindings), body), e) =>
      CstLetIn(bindings, body)(Span.fromStartEnd(s, e))
    }

  private val caseBranch: Parsley[CstCaseBranch] =
    (offset <~> PatternParser.pattern <~> (symbol("->") *> expression) <~> offset).map {
      case (((s, pat), body), e) =>
        CstCaseBranch(pat, body)(Span.fromStartEnd(s, e))
    }

  private val caseOf: Parsley[CstExpression] =
    (offset <~> (keyword("case") *> expression) <~>
      (keyword("of") *> some(caseBranch)) <~> offset).map { case (((s, expr), branches), e) =>
      CstCaseOf(expr, branches)(Span.fromStartEnd(s, e))
    }

  private val lambda: Parsley[CstExpression] =
    (offset <~> (symbol("\\") *> some(PatternParser.pattern)) <~>
      (symbol("->") *> expression) <~> offset).map { case (((s, params), body), e) =>
      CstLambda(params, body)(Span.fromStartEnd(s, e))
    }

  private val negate: Parsley[CstExpression] = (offset <~> (symbol("-") *> atom) <~> offset).map {
    case ((s, expr), e) =>
      CstNegate(expr)(Span.fromStartEnd(s, e))
  }

  private val fieldSuffix: Parsley[CstName] =
    symbol(".") *> ModuleParser.lowerName

  private val postfixAtom: Parsley[CstExpression] = (offset <~> atom <~> many(fieldSuffix) <~> offset).map {
    case (((s, base), fields), e) =>
      fields.foldLeft(base) { (record, field) =>
        CstFieldAccess(record, field)(Span.between(record.span, field.span))
      }
  }

  /** A non-operator expression: atom with optional function application and field access. */
  private val appExpr: Parsley[CstExpression] =
    val base = ifThenElse | letIn | caseOf | lambda | negate | postfixAtom
    ((offset <~> pos) <~> base).flatMap { case ((so, sp), fn) =>
      (many(sameLineOrIndentedPast(sp)(postfixAtom)) <~> offset).map { case (args, eo) =>
        if args.isEmpty then fn
        else CstFunctionApplication(fn, args)(Span.fromStartEnd(so, eo))
      }
    }

  /** Parse a binary operator name. */
  private val binOp: Parsley[CstName] = (offset <~> operator <~> offset).map { case ((s, op), e) =>
    CstName(op)(Span.fromStartEnd(s, e))
  }

  /**
   * A full expression including binary operators.
   *
   * The chain is built flat and left-leaning on purpose: an operator's precedence and associativity may come from an
   * `infix` declaration further down the module, so the shape is only decidable once the whole module is parsed.
   * `OperatorReassociator`, run by `Elm.parseCst`, rebuilds it — including the spans of its nodes.
   */
  lazy val expression: Parsley[CstExpression] = ((offset <~> pos) <~> appExpr).flatMap { case ((so, sp), first) =>
    (many(sameLineOrIndentedPast(sp)(binOp <~> appExpr)) <~> offset).map { case (ops, eo) =>
      ops.foldLeft(first) { case (left, (op, right)) =>
        CstBinaryOp(left, op, right)(Span.fromStartEnd(so, eo))
      }
    }
  }
