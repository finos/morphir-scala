package morphir.langkit.elm.parser

import parsley.Parsley
import parsley.Parsley.{atomic, many, some}
import parsley.combinator.option
import parsley.position.offset

import morphir.langkit.core.Span
import morphir.langkit.elm.cst.*
import morphir.langkit.elm.lexer.ElmLexer.*

/** Parser for Elm patterns used in function arguments, case branches, and let bindings. */
object PatternParser:

  // -----------------------------------------------------------------------
  // Atomic patterns
  // -----------------------------------------------------------------------

  private val anythingPat: Parsley[CstPattern] = (offset <~> symbol("_") <~> offset).map { case ((s, _), e) =>
    CstAnythingPattern()(Span.fromStartEnd(s, e))
  }

  private val intPat: Parsley[CstPattern] = (offset <~> intLiteral <~> offset).map { case ((s, v), e) =>
    CstIntPattern(v)(Span.fromStartEnd(s, e))
  }

  private val floatPat: Parsley[CstPattern] = (offset <~> floatLiteral <~> offset).map { case ((s, v), e) =>
    CstFloatPattern(v)(Span.fromStartEnd(s, e))
  }

  private val variablePat: Parsley[CstPattern] = (offset <~> ModuleParser.lowerName <~> offset).map {
    case ((s, n), e) =>
      CstVariablePattern(n)(Span.fromStartEnd(s, e))
  }

  private val unitPat: Parsley[CstPattern] =
    atomic((offset <~> parens(Parsley.pure(())) <~> offset).map { case ((s, _), e) =>
      CstUnitPattern()(Span.fromStartEnd(s, e))
    })

  private val tuplePat: Parsley[CstPattern] =
    (offset <~> parens(pattern <~> some(symbol(",") *> pattern)) <~> offset).map { case ((s, (first, rest)), e) =>
      CstTuplePattern(first :: rest)(Span.fromStartEnd(s, e))
    }

  private val listPat: Parsley[CstPattern] = (offset <~> brackets(commaSep(pattern)) <~> offset).map {
    case ((s, elems), e) =>
      CstListPattern(elems)(Span.fromStartEnd(s, e))
  }

  private val recordPat: Parsley[CstPattern] = (offset <~> braces(commaSep1(ModuleParser.lowerName)) <~> offset).map {
    case ((s, fields), e) =>
      CstRecordPattern(fields)(Span.fromStartEnd(s, e))
  }

  private val constructorPat: Parsley[CstPattern] =
    (offset <~> ModuleParser.qualifiedName <~> many(atomPattern) <~> offset).map { case (((s, name), args), e) =>
      CstConstructorPattern(name, args)(Span.fromStartEnd(s, e))
    }

  private val parenthesizedPat: Parsley[CstPattern] = (offset <~> parens(pattern) <~> offset).map { case ((s, p), e) =>
    CstParenthesizedPattern(p)(Span.fromStartEnd(s, e))
  }

  /** An atomic pattern (no cons or as). */
  val atomPattern: Parsley[CstPattern] =
    anythingPat
      | atomic(floatPat)
      | intPat
      | unitPat
      | atomic(tuplePat)
      | listPat
      | recordPat
      | constructorPat
      | variablePat
      | parenthesizedPat

  /**
   * A cons chain (`x :: rest`), or a bare atomic pattern when no `::` follows.
   *
   * `::` is right-associative in Elm, so `a :: b :: rest` is `a :: (b :: rest)`. This layer sits between
   * [[atomPattern]] and [[pattern]] because `as` binds looser than `::`: `x :: rest as whole` aliases the whole cons
   * chain.
   */
  private lazy val consPattern: Parsley[CstPattern] =
    (offset <~> atomPattern <~> many(symbol("::") *> atomPattern) <~> offset).map {
      case (((_, first), Nil), _)  => first
      case (((s, first), rest), e) =>
        (first :: rest).reduceRight { (head, tail) =>
          CstConsPattern(head, tail)(Span.fromStartEnd(s, e))
        }
    }

  /** A pattern with optional `as` alias. */
  lazy val pattern: Parsley[CstPattern] =
    (offset <~> consPattern <~> option(keyword("as") *> ModuleParser.lowerName) <~> offset).map {
      case (((s, pat), Some(alias)), e) => CstAsPattern(pat, alias)(Span.fromStartEnd(s, e))
      case (((_, pat), None), _)        => pat
    }
