package morphir.langkit.elm.parser

import parsley.Parsley
import parsley.Parsley.{atomic, many, some}
import parsley.combinator.option
import parsley.position.offset

import morphir.langkit.elm.Span
import morphir.langkit.elm.cst.*
import morphir.langkit.elm.lexer.ElmLexer.*

/** Parser for Elm patterns used in function arguments, case branches, and let bindings. */
object PatternParser:

  private def mkSpan(start: Int, end: Int): Span =
    Span(start, end - start)

  // -----------------------------------------------------------------------
  // Atomic patterns
  // -----------------------------------------------------------------------

  private val anythingPat: Parsley[CstPattern] = (offset <~> symbol("_") <~> offset).map { case ((s, _), e) =>
    CstAnythingPattern()(mkSpan(s, e))
  }

  private val intPat: Parsley[CstPattern] = (offset <~> intLiteral <~> offset).map { case ((s, v), e) =>
    CstIntPattern(v)(mkSpan(s, e))
  }

  private val floatPat: Parsley[CstPattern] = (offset <~> floatLiteral <~> offset).map { case ((s, v), e) =>
    CstFloatPattern(v)(mkSpan(s, e))
  }

  private val variablePat: Parsley[CstPattern] = (offset <~> ModuleParser.lowerName <~> offset).map {
    case ((s, n), e) =>
      CstVariablePattern(n)(mkSpan(s, e))
  }

  private val unitPat: Parsley[CstPattern] =
    atomic((offset <~> parens(Parsley.pure(())) <~> offset).map { case ((s, _), e) =>
      CstUnitPattern()(mkSpan(s, e))
    })

  private val tuplePat: Parsley[CstPattern] =
    (offset <~> parens(pattern <~> some(symbol(",") *> pattern)) <~> offset).map { case ((s, (first, rest)), e) =>
      CstTuplePattern(first :: rest)(mkSpan(s, e))
    }

  private val listPat: Parsley[CstPattern] = (offset <~> brackets(commaSep(pattern)) <~> offset).map {
    case ((s, elems), e) =>
      CstListPattern(elems)(mkSpan(s, e))
  }

  private val recordPat: Parsley[CstPattern] = (offset <~> braces(commaSep1(ModuleParser.lowerName)) <~> offset).map {
    case ((s, fields), e) =>
      CstRecordPattern(fields)(mkSpan(s, e))
  }

  private val constructorPat: Parsley[CstPattern] =
    (offset <~> ModuleParser.qualifiedName <~> many(atomPattern) <~> offset).map { case (((s, name), args), e) =>
      CstConstructorPattern(name, args)(mkSpan(s, e))
    }

  private val parenthesizedPat: Parsley[CstPattern] = (offset <~> parens(pattern) <~> offset).map { case ((s, p), e) =>
    CstParenthesizedPattern(p)(mkSpan(s, e))
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

  /** A pattern with optional `as` alias. */
  lazy val pattern: Parsley[CstPattern] =
    (offset <~> atomPattern <~> option(keyword("as") *> ModuleParser.lowerName) <~> offset).map {
      case (((s, pat), Some(alias)), e) => CstAsPattern(pat, alias)(mkSpan(s, e))
      case (((_, pat), None), _)        => pat
    }
