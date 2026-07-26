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

  private val unitType: Parsley[CstTypeExpression] = (offset <~> parens(Parsley.pure(())) <~> offset).map {
    case ((s, _), e) =>
      CstUnitType()(Span.fromStartEnd(s, e))
  }

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

  private val recordType: Parsley[CstTypeExpression] =
    (offset <~> braces(
      option(atomic(ModuleParser.lowerName <* symbol("|"))) <~> commaSep1(recordFieldType)
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
        rest.foldRight(first) { (next, acc) =>
          CstFunctionType(acc, next)(Span.fromStartEnd(so, eo))
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
    (offset <~> (keyword("type") *> keyword("alias") *> ModuleParser.upperName) <~>
      many(ModuleParser.lowerName) <~>
      (symbol("=") *> typeExpression) <~> offset).map { case ((((s, name), vars), body), e) =>
      CstTypeAliasDeclaration(name, vars.toIndexedSeq, body)(Span.fromStartEnd(s, e))
    }

  private val constructor: Parsley[CstConstructor] =
    (offset <~> ModuleParser.upperName <~> many(atomType) <~> offset).map { case (((s, name), params), e) =>
      CstConstructor(name, params.toIndexedSeq)(Span.fromStartEnd(s, e))
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

  private val associativity: Parsley[Associativity] = (keyword("left") *> Parsley.pure(Associativity.Left))
    | (keyword("right") *> Parsley.pure(Associativity.Right))
    | (keyword("non") *> Parsley.pure(Associativity.Non))

  private val infixDeclaration: Parsley[CstDeclaration] =
    (offset <~> (keyword("infix") *> associativity) <~>
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
