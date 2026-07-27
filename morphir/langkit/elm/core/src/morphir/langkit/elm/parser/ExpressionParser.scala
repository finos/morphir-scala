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

  // Atoms are built in a `raw` form that stops at the token's last character, so `postfixAtom` can tell whether a
  // following `.` was adjacent. `atom` below adds the single whitespace boundary the rest of the grammar expects.

  private val intLit: Parsley[CstExpression] = (offset <~> raw.intLiteral <~> offset).map { case ((s, v), e) =>
    CstIntLiteral(v)(Span.fromStartEnd(s, e))
  }

  private val floatLit: Parsley[CstExpression] = (offset <~> raw.floatLiteral <~> offset).map { case ((s, v), e) =>
    CstFloatLiteral(v)(Span.fromStartEnd(s, e))
  }

  private val stringLit: Parsley[CstExpression] = (offset <~> raw.stringLiteral <~> offset).map { case ((s, v), e) =>
    CstStringLiteral(v)(Span.fromStartEnd(s, e))
  }

  private val charLit: Parsley[CstExpression] = (offset <~> raw.charLiteral <~> offset).map { case ((s, v), e) =>
    CstCharLiteral(v)(Span.fromStartEnd(s, e))
  }

  /** A GLSL block, whose contents Elm hands to a shader compiler rather than reading. */
  private val glsl: Parsley[CstExpression] = (offset <~> raw.glslLiteral <~> offset).map { case ((s, code), e) =>
    CstGlsl(code)(Span.fromStartEnd(s, e))
  }

  private val variableRef: Parsley[CstExpression] = (offset <~> ModuleParser.rawQualifiedValueName <~> offset).map {
    case ((s, qn), e) =>
      CstVariableRef(qn)(Span.fromStartEnd(s, e))
  }

  private val constructorRef: Parsley[CstExpression] = (offset <~> ModuleParser.rawQualifiedName <~> offset).map {
    case ((s, qn), e) =>
      CstConstructorRef(qn)(Span.fromStartEnd(s, e))
  }

  /** `p` between brackets, where the closing bracket consumes no trailing whitespace. */
  private def rawEnclosed[A](open: String, close: Char)(p: Parsley[A]): Parsley[A] =
    symbol(open) *> p <* raw.sym(close)

  private val unitLit: Parsley[CstExpression] =
    atomic((offset <~> rawEnclosed("(", ')')(Parsley.pure(())) <~> offset).map { case ((s, _), e) =>
      CstUnitLiteral()(Span.fromStartEnd(s, e))
    })

  private val parenthesized: Parsley[CstExpression] = (offset <~> rawEnclosed("(", ')')(expression) <~> offset).map {
    case ((s, expr), e) =>
      CstParenthesized(expr)(Span.fromStartEnd(s, e))
  }

  private val tupleLit: Parsley[CstExpression] =
    (offset <~> rawEnclosed("(", ')')(expression <~> some(symbol(",") *> expression)) <~> offset).map {
      case ((s, (first, rest)), e) =>
        CstTupleLiteral(first :: rest)(Span.fromStartEnd(s, e))
    }

  private val listLit: Parsley[CstExpression] = (offset <~> rawEnclosed("[", ']')(commaSep(expression)) <~> offset)
    .map { case ((s, elems), e) =>
      CstListLiteral(elems)(Span.fromStartEnd(s, e))
    }

  private val recordField: Parsley[CstRecordField] =
    (offset <~> ModuleParser.lowerName <~> (symbol("=") *> expression) <~> offset).map { case (((s, n), v), e) =>
      CstRecordField(n, v)(Span.fromStartEnd(s, e))
    }

  /** A record literal, including the empty one: `{}` is as valid a value as `{}` is a type. */
  private val recordLit: Parsley[CstExpression] =
    (offset <~> rawEnclosed("{", '}')(commaSep(recordField)) <~> offset).map { case ((s, fields), e) =>
      CstRecordLiteral(fields)(Span.fromStartEnd(s, e))
    }

  private val recordUpdate: Parsley[CstExpression] =
    (offset <~> rawEnclosed("{", '}')(
      ModuleParser.lowerName <~> (symbol("|") *> commaSep1(recordField))
    ) <~> offset).map { case ((s, (rec, fields)), e) =>
      CstRecordUpdate(rec, fields)(Span.fromStartEnd(s, e))
    }

  private val fieldAccessFn: Parsley[CstExpression] =
    (offset <~> (raw.sym('.') *> ModuleParser.rawLowerName) <~> offset).map { case ((s, n), e) =>
      CstFieldAccessFunction(n)(Span.fromStartEnd(s, e))
    }

  /**
   * An operator used as a value: `(+)` in `List.foldr (+) 0`.
   *
   * Elm lets any binary operator be named this way, which is the only reason `(::)` and friends can be passed to a
   * higher-order function.
   */
  private val operatorRef: Parsley[CstExpression] =
    val name = (offset <~> operator <~> offset).map { case ((s, op), e) =>
      CstName(op)(Span.fromStartEnd(s, e))
    }
    atomic((offset <~> (symbol("(") *> name <* raw.sym(')')) <~> offset).map { case ((s, op), e) =>
      CstOperatorRef(op)(Span.fromStartEnd(s, e))
    })

  /** An atomic expression that stops at its last character, consuming no trailing whitespace. */
  private val rawAtom: Parsley[CstExpression] =
    atomic(floatLit)
      | intLit
      | stringLit
      | charLit
      | unitLit
      | glsl
      | operatorRef
      | atomic(tupleLit)
      | parenthesized
      | listLit
      | atomic(recordUpdate)
      | recordLit
      | fieldAccessFn
      | variableRef
      | constructorRef

  /** An atomic expression (no application or binary ops). */
  val atom: Parsley[CstExpression] = rawAtom <* whiteSpace

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

  /**
   * A `let` block, whose bindings all begin in the same column.
   *
   * The alignment is what ends the block: the first token that does not line up belongs to whatever encloses the `let`,
   * which is usually the `in` that follows it.
   */
  private val letIn: Parsley[CstExpression] =
    (offset <~> (keyword("let") *> aligned(letBinding)) <~>
      (keyword("in") *> expression) <~> offset).map { case (((s, bindings), body), e) =>
      CstLetIn(bindings, body)(Span.fromStartEnd(s, e))
    }

  private val caseBranch: Parsley[CstCaseBranch] =
    (offset <~> PatternParser.pattern <~> (symbol("->") *> expression) <~> offset).map {
      case (((s, pat), body), e) =>
        CstCaseBranch(pat, body)(Span.fromStartEnd(s, e))
    }

  /** A `case` expression, whose branches all begin in the same column, and end where that alignment does. */
  private val caseOf: Parsley[CstExpression] =
    (offset <~> (keyword("case") *> expression) <~>
      (keyword("of") *> aligned(caseBranch)) <~> offset).map { case (((s, expr), branches), e) =>
      CstCaseOf(expr, branches)(Span.fromStartEnd(s, e))
    }

  private val lambda: Parsley[CstExpression] =
    (offset <~> (symbol("\\") *> some(PatternParser.pattern)) <~>
      (symbol("->") *> expression) <~> offset).map { case (((s, params), body), e) =>
      CstLambda(params, body)(Span.fromStartEnd(s, e))
    }

  /**
   * A `.field` suffix, which must be adjacent to the expression it accesses: `rec.field`, never `rec . field`.
   *
   * Elm reserves a lone `.` (`BadDot`), so the spaced form is an error rather than a field access.
   */
  private val fieldSuffix: Parsley[CstName] =
    atomic(raw.sym('.') *> ModuleParser.rawLowerName)

  /** An atom with its field accesses, stopping at the last character rather than at the next token. */
  private val rawPostfixAtom: Parsley[CstExpression] = (rawAtom <~> many(fieldSuffix)).map { case (base, fields) =>
    fields.foldLeft(base) { (record, field) =>
      CstFieldAccess(record, field)(Span.between(record.span, field.span))
    }
  }

  private val postfixAtom: Parsley[CstExpression] = rawPostfixAtom <* whiteSpace

  /**
   * Negation: a `-` with the term it negates directly attached.
   *
   * Elm permits no space here — `-x` negates, `- x` is an error — because the spaced form is how subtraction is
   * written.
   */
  private val negativeTerm: Parsley[CstExpression] =
    atomic((offset <~> (raw.sym('-') *> rawPostfixAtom) <~> offset)).map { case ((s, expr), e) =>
      CstNegate(expr)(Span.fromStartEnd(s, e))
    }

  /** Whitespace that must actually be there: the tokens of an application have to be separated. */
  private val requiredSpace: Parsley[Unit] = (offset <~> (whiteSpace *> offset)).filter { case (before, after) =>
    after > before
  }.void

  /**
   * An argument in a function application, which is preceded by whitespace.
   *
   * That whitespace is what distinguishes `f -1` from `f - 1`: with a space before the `-` and none after it, Elm reads
   * a negative term being applied, and only otherwise a subtraction. Being able to ask the question at all is why the
   * atoms above stop at their last character.
   */
  private def argument(start: (Int, Int)): Parsley[CstExpression] =
    atomic(requiredSpace *> sameLineOrIndentedPast(start)(negativeTerm | rawPostfixAtom))

  /** A non-operator expression: atom with optional function application and field access. */
  private val appExpr: Parsley[CstExpression] =
    val compound    = ifThenElse | letIn | caseOf | lambda
    val application = ((offset <~> pos) <~> (negativeTerm | rawPostfixAtom)).flatMap { case ((so, sp), fn) =>
      ((many(argument(sp)) <~> offset) <* whiteSpace).map { case (args, eo) =>
        if args.isEmpty then fn
        else CstFunctionApplication(fn, args)(Span.fromStartEnd(so, eo))
      }
    }
    compound | application

  /**
   * Parse a binary operator name.
   *
   * Elm reserves `.`, `|`, `->`, `=` and `:`: they are structural tokens, never binary operators. Accepting them here
   * would let a malformed expression swallow the `=` of the next declaration or the `->` of the next case branch, and
   * report the failure somewhere far from its cause.
   */
  private val binOp: Parsley[CstName] =
    atomic((offset <~> operator.filterNot(reservedOperators.contains) <~> offset)).map { case ((s, op), e) =>
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
