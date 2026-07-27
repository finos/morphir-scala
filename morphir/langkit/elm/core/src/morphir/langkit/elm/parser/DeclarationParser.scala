package morphir.langkit.elm.parser

import parsley.Parsley
import parsley.Parsley.{atomic, lookAhead, many, some}
import parsley.combinator.option
import parsley.position.{offset, pos}

import morphir.langkit.core.Span
import morphir.langkit.elm.cst.*
import morphir.langkit.elm.lexer.ElmLexer.*

/**
 * Parser for Elm top-level declarations: value definitions, type aliases, custom types, ports, and infix declarations.
 */
object DeclarationParser:

  private def sameLineOrIndentedPast[A](start: (Int, Int))(p: Parsley[A]): Parsley[A] =
    lookAhead(pos.filter { case (line, col) => line == start._1 || col > start._2 }) *> p

  // -----------------------------------------------------------------------
  // Type expressions
  // -----------------------------------------------------------------------

  private val typeVariable: Parsley[CstTypeExpression] = (offset <~> ModuleParser.lowerName <~> offset).map {
    case ((s, n), e) =>
      CstTypeVariable(n)(Span.fromStartEnd(s, e))
  }

  private val typeReference: Parsley[CstTypeExpression] = (offset <~> ModuleParser.qualifiedName <~> offset).map {
    case ((s, qn), e) =>
      CstTypeReference(qn)(Span.fromStartEnd(s, e))
  }

  /**
   * The unit type, `()`.
   *
   * Atomic because it commits to `(` before it can know whether it is looking at unit or at a parenthesised type: on
   * `(List Int)` it must give the whole alternation a chance at the same input rather than failing it.
   */
  private val unitType: Parsley[CstTypeExpression] =
    atomic((offset <~> parens(Parsley.pure(())) <~> offset).map { case ((s, _), e) =>
      CstUnitType()(Span.fromStartEnd(s, e))
    })

  private val tupleType: Parsley[CstTypeExpression] =
    (offset <~> parens(typeExpression <~> some(symbol(",") *> typeExpression)) <~> offset).map {
      case ((s, (first, rest)), e) =>
        CstTupleType(first :: rest)(Span.fromStartEnd(s, e))
    }

  private val recordFieldType: Parsley[CstRecordFieldType] =
    (offset <~> ModuleParser.lowerName <~> (symbol(":") *> typeExpression) <~> offset).map {
      case (((s, n), t), e) =>
        CstRecordFieldType(n, t)(Span.fromStartEnd(s, e))
    }

  /**
   * A record type, including the empty one.
   *
   * `{}` is a legitimate Elm type — `type alias Flags = {}` is how a program with no flags says so — and reading the
   * fields with `commaSep1` used to reject it.
   */
  private val recordType: Parsley[CstTypeExpression] =
    (offset <~> braces(
      option(atomic(ModuleParser.lowerName <* symbol("|"))) <~> commaSep(recordFieldType)
    ) <~> offset).map { case ((s, (ext, fields)), e) =>
      CstRecordType(fields, ext)(Span.fromStartEnd(s, e))
    }

  private val parenthesizedType: Parsley[CstTypeExpression] =
    parens(typeExpression)

  /** An atomic type (not a function type or application). */
  val atomType: Parsley[CstTypeExpression] =
    atomic(tupleType)
      | unitType
      | recordType
      | typeReference
      | typeVariable
      | parenthesizedType

  /** A type with optional type application. */
  val appType: Parsley[CstTypeExpression] = ((offset <~> pos) <~> atomType).flatMap { case ((so, sp), con) =>
    (many(sameLineOrIndentedPast(sp)(atomType)) <~> offset).map { case (args, eo) =>
      if args.isEmpty then con
      else CstTypeApplication(con, args)(Span.fromStartEnd(so, eo))
    }
  }

  /** A full type expression including function types (`a -> b`). */
  lazy val typeExpression: Parsley[CstTypeExpression] = ((offset <~> pos) <~> appType).flatMap {
    case ((so, sp), first) =>
      (many(sameLineOrIndentedPast(sp)(symbol("->") *> appType)) <~> offset).map { case (rest, eo) =>
        // `->` is right-associative in Elm: `a -> b -> c` parses as `a -> (b -> c)`. Folding the whole chain from
        // the right keeps both the nesting and the operand order — an earlier `rest.foldRight(first)` produced
        // `FunctionType(FunctionType(a, c), b)` for that input.
        (first :: rest).reduceRight { (argument, result) =>
          CstFunctionType(argument, result)(Span.fromStartEnd(so, eo))
        }
      }
  }

  // -----------------------------------------------------------------------
  // Type annotation
  // -----------------------------------------------------------------------

  val typeAnnotation: Parsley[CstTypeAnnotation] =
    (offset <~> ModuleParser.lowerName <~> (symbol(":") *> typeExpression) <~> offset).map {
      case (((s, n), t), e) =>
        CstTypeAnnotation(n, t)(Span.fromStartEnd(s, e))
    }

  // -----------------------------------------------------------------------
  // Declarations
  // -----------------------------------------------------------------------

  private val valueDeclaration: Parsley[CstDeclaration] =
    (offset <~> option(atomic(typeAnnotation)) <~>
      ModuleParser.lowerName <~>
      many(PatternParser.atomPattern) <~>
      (symbol("=") *> ExpressionParser.expression) <~> offset).map {
      case (((((s, ann), name), params), body), e) =>
        CstValueDeclaration(ann, name, params.toIndexedSeq, body)(Span.fromStartEnd(s, e))
    }

  private val typeAliasDeclaration: Parsley[CstDeclaration] =
    (offset <~> (keyword("type") *> contextualKeyword("alias") *> ModuleParser.upperName) <~>
      many(ModuleParser.lowerName) <~>
      (symbol("=") *> typeExpression) <~> offset).map { case ((((s, name), vars), body), e) =>
      CstTypeAliasDeclaration(name, vars.toIndexedSeq, body)(Span.fromStartEnd(s, e))
    }

  /**
   * A constructor of a custom type, with its argument types.
   *
   * The arguments carry the same layout guard as a type application: a lowercase name is a perfectly good type
   * variable, so without it `Compound (List Shape)` followed by `describe : …` in column 1 reads `describe` as one more
   * argument and then trips over the `:`.
   */
  private val constructor: Parsley[CstConstructor] = ((offset <~> pos) <~> ModuleParser.upperName).flatMap {
    case ((s, sp), name) =>
      (many(sameLineOrIndentedPast(sp)(atomType)) <~> offset).map { case (params, e) =>
        CstConstructor(name, params.toIndexedSeq)(Span.fromStartEnd(s, e))
      }
  }

  private val customTypeDeclaration: Parsley[CstDeclaration] =
    (offset <~> (keyword("type") *> ModuleParser.upperName) <~>
      many(ModuleParser.lowerName) <~>
      (symbol("=") *> constructor) <~>
      many(symbol("|") *> constructor) <~> offset).map { case (((((s, name), vars), first), rest), e) =>
      CstCustomTypeDeclaration(name, vars.toIndexedSeq, (first :: rest).toIndexedSeq)(Span.fromStartEnd(s, e))
    }

  private val portDeclaration: Parsley[CstDeclaration] =
    (offset <~> (keyword("port") *> ModuleParser.lowerName) <~>
      (symbol(":") *> typeExpression) <~> offset).map { case (((s, name), t), e) =>
      CstPortDeclaration(name, t)(Span.fromStartEnd(s, e))
    }

  private val associativity: Parsley[Associativity] = (contextualKeyword("left") *> Parsley.pure(Associativity.Left))
    | (contextualKeyword("right") *> Parsley.pure(Associativity.Right))
    | (contextualKeyword("non") *> Parsley.pure(Associativity.Non))

  private val infixDeclaration: Parsley[CstDeclaration] =
    (offset <~> (contextualKeyword("infix") *> associativity) <~>
      intLiteral <~>
      parens(
        (offset <~> operator <~> offset).map { case ((s, op), e) =>
          CstName(op)(Span.fromStartEnd(s, e))
        }
      ) <~>
      (symbol("=") *> ModuleParser.lowerName) <~> offset).map { case (((((s, assoc), prec), op), fn), e) =>
      CstInfixDeclaration(assoc, prec.toInt, op, fn)(Span.fromStartEnd(s, e))
    }

  /** A top-level declaration. */
  val declaration: Parsley[CstDeclaration] =
    atomic(typeAliasDeclaration)
      | customTypeDeclaration
      | portDeclaration
      | infixDeclaration
      | valueDeclaration
