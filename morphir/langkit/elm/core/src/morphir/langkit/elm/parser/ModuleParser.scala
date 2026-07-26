package morphir.langkit.elm.parser

import parsley.Parsley
import parsley.Parsley.{atomic, many, some}
import parsley.combinator.option
import parsley.position.{offset, pos}

import morphir.langkit.elm.Span
import morphir.langkit.elm.cst.*
import morphir.langkit.elm.lexer.ElmLexer.*

/**
 * Parser for Elm module declarations, imports, and top-level structure.
 *
 * Elm modules follow the form:
 * {{{
 *   module Name exposing (..)
 *   import List exposing (map, filter)
 *   -- declarations follow
 * }}}
 */
object ModuleParser:

  /** Helper to build a Span from two byte offsets. */
  private def mkSpan(start: Int, end: Int): Span =
    Span(start, end - start)

  // -----------------------------------------------------------------------
  // Names
  // -----------------------------------------------------------------------

  val name: Parsley[CstName] = (offset <~> identifier <~> offset).map { case ((s, n), e) =>
    CstName(n)(mkSpan(s, e))
  }

  val lowerName: Parsley[CstName] = (offset <~> lowerIdentifier <~> offset).map { case ((s, n), e) =>
    CstName(n)(mkSpan(s, e))
  }

  val upperName: Parsley[CstName] = (offset <~> upperIdentifier <~> offset).map { case ((s, n), e) =>
    CstName(n)(mkSpan(s, e))
  }

  val qualifiedName: Parsley[CstQualifiedName] =
    (offset <~> upperName <~> many(atomic(symbol(".") *> upperName)) <~> offset).map {
      case (((s, first), rest), e) =>
        CstQualifiedName(first :: rest)(mkSpan(s, e))
    }

  val qualifiedValueName: Parsley[CstQualifiedName] =
    atomic((offset <~> many(atomic(upperName <* symbol("."))) <~> lowerName <~> offset).map {
      case (((s, prefix), last), e) =>
        CstQualifiedName(prefix :+ last)(mkSpan(s, e))
    })

  // -----------------------------------------------------------------------
  // Exposing lists
  // -----------------------------------------------------------------------

  private val exposedValue: Parsley[CstExposedItem] = (offset <~> lowerName <~> offset).map { case ((s, n), e) =>
    CstExposedValue(n)(mkSpan(s, e))
  }

  private val exposedOperator: Parsley[CstExposedItem] =
    (offset <~> parens(
      (offset <~> operator <~> offset).map { case ((s, op), e) =>
        CstName(op)(mkSpan(s, e))
      }
    ) <~> offset).map { case ((s, n), e) =>
      CstExposedOperator(n)(mkSpan(s, e))
    }

  private val exposedTypeConstructors: Parsley[CstExposedConstructors] =
    (offset <~> parens(symbol("..")) <~> offset).map { case ((s, _), e) =>
      CstExposedConstructorsAll()(mkSpan(s, e))
    }

  private val exposedType: Parsley[CstExposedItem] =
    (offset <~> upperName <~> option(exposedTypeConstructors) <~> offset).map { case (((s, n), ctors), e) =>
      CstExposedType(n, ctors)(mkSpan(s, e))
    }

  private val exposedItem: Parsley[CstExposedItem] =
    exposedType | exposedOperator | exposedValue

  val exposingList: Parsley[CstExposingList] =
    keyword("exposing") *> (
      atomic((offset <~> parens(symbol("..")) <~> offset).map { case ((s, _), e) =>
        CstExposingAll()(mkSpan(s, e))
      })
        | (offset <~> parens(commaSep1(exposedItem)) <~> offset).map { case ((s, items), e) =>
          CstExposingExplicit(items)(mkSpan(s, e))
        }
    )

  // -----------------------------------------------------------------------
  // Module declaration
  // -----------------------------------------------------------------------

  private val moduleType: Parsley[ModuleType] = (keyword("port") *> Parsley.pure(ModuleType.Port))
    | (keyword("effect") *> Parsley.pure(ModuleType.Effect))
    | Parsley.pure(ModuleType.Plain)

  val moduleDeclaration: Parsley[CstModuleDeclaration] =
    (offset <~> moduleType <* keyword("module") <~> qualifiedName <~> exposingList <~> offset).map {
      case ((((s, mt), qn), exp), e) =>
        CstModuleDeclaration(mt, qn, exp)(mkSpan(s, e))
    }

  // -----------------------------------------------------------------------
  // Imports
  // -----------------------------------------------------------------------

  val importDecl: Parsley[CstImport] =
    (offset <~> keyword("import") *> qualifiedName <~>
      option(keyword("as") *> upperName) <~>
      option(exposingList) <~> offset).map { case ((((s, modName), alias), exp), e) =>
      CstImport(modName, alias, exp)(mkSpan(s, e))
    }

  // -----------------------------------------------------------------------
  // Top-level module
  // -----------------------------------------------------------------------

  /** Parse a complete Elm module. */
  val module: Parsley[CstModule] =
    fully(
      (offset <~> moduleDeclaration <~> many(importDecl) <~>
        many(DeclarationParser.declaration) <~> offset).map { case ((((s, modDecl), imports), decls), e) =>
        CstModule(modDecl, imports.toIndexedSeq, decls.toIndexedSeq)(mkSpan(s, e))
      }
    )
