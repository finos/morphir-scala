package morphir.langkit.elm.parser

import parsley.Parsley
import parsley.Parsley.{atomic, eof, lookAhead, many, some}
import parsley.combinator.option
import parsley.errors.combinator.{fail, ErrorMethods}
import parsley.position.{offset, pos}

import morphir.langkit.core.Span
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
  // -----------------------------------------------------------------------
  // Names
  // -----------------------------------------------------------------------

  /**
   * Names in their `raw` form consume no trailing whitespace, so productions can tell whether the next token was
   * adjacent. Each has a whitespace-consuming counterpart below, which is what the rest of the grammar uses.
   */
  val rawName: Parsley[CstName] = (offset <~> raw.identifier <~> offset).map { case ((s, n), e) =>
    CstName(n)(Span.fromStartEnd(s, e))
  }

  val rawLowerName: Parsley[CstName] = (offset <~> raw.lowerIdentifier <~> offset).map { case ((s, n), e) =>
    CstName(n)(Span.fromStartEnd(s, e))
  }

  val rawUpperName: Parsley[CstName] = (offset <~> raw.upperIdentifier <~> offset).map { case ((s, n), e) =>
    CstName(n)(Span.fromStartEnd(s, e))
  }

  val name: Parsley[CstName]      = rawName <* whiteSpace
  val lowerName: Parsley[CstName] = rawLowerName <* whiteSpace
  val upperName: Parsley[CstName] = rawUpperName <* whiteSpace

  /**
   * A qualified name, whose dots must be adjacent to the names either side: `List.map`, never `List . map`.
   *
   * `elm/compiler` reserves `.` (`BadDot` in `Parse.Symbol`), so a spaced dot is not a qualification, a field access,
   * or an operator — it is an error.
   */
  val rawQualifiedName: Parsley[CstQualifiedName] =
    (offset <~> rawUpperName <~> many(atomic(raw.sym('.') *> rawUpperName)) <~> offset).map {
      case (((s, first), rest), e) =>
        CstQualifiedName(first :: rest)(Span.fromStartEnd(s, e))
    }

  val rawQualifiedValueName: Parsley[CstQualifiedName] =
    atomic((offset <~> many(atomic(rawUpperName <* raw.sym('.'))) <~> rawLowerName <~> offset).map {
      case (((s, prefix), last), e) =>
        CstQualifiedName(prefix :+ last)(Span.fromStartEnd(s, e))
    })

  val qualifiedName: Parsley[CstQualifiedName]      = rawQualifiedName <* whiteSpace
  val qualifiedValueName: Parsley[CstQualifiedName] = rawQualifiedValueName <* whiteSpace

  // -----------------------------------------------------------------------
  // Exposing lists
  // -----------------------------------------------------------------------

  private val exposedValue: Parsley[CstExposedItem] = (offset <~> lowerName <~> offset).map { case ((s, n), e) =>
    CstExposedValue(n)(Span.fromStartEnd(s, e))
  }

  private val exposedOperator: Parsley[CstExposedItem] =
    (offset <~> parens(
      (offset <~> operator <~> offset).map { case ((s, op), e) =>
        CstName(op)(Span.fromStartEnd(s, e))
      }
    ) <~> offset).map { case ((s, n), e) =>
      CstExposedOperator(n)(Span.fromStartEnd(s, e))
    }

  private val exposedTypeConstructors: Parsley[CstExposedConstructors] =
    (offset <~> parens(symbol("..")) <~> offset).map { case ((s, _), e) =>
      CstExposedConstructorsAll()(Span.fromStartEnd(s, e))
    }

  private val exposedType: Parsley[CstExposedItem] =
    (offset <~> upperName <~> option(exposedTypeConstructors) <~> offset).map { case (((s, n), ctors), e) =>
      CstExposedType(n, ctors)(Span.fromStartEnd(s, e))
    }

  private val exposedItem: Parsley[CstExposedItem] =
    exposedType | exposedOperator | exposedValue

  val exposingList: Parsley[CstExposingList] =
    keyword("exposing") *> (
      atomic((offset <~> parens(symbol("..")) <~> offset).map { case ((s, _), e) =>
        CstExposingAll()(Span.fromStartEnd(s, e))
      })
        | (offset <~> parens(commaSep1(exposedItem)) <~> offset).map { case ((s, items), e) =>
          CstExposingExplicit(items)(Span.fromStartEnd(s, e))
        }
    )

  // -----------------------------------------------------------------------
  // Module declaration
  // -----------------------------------------------------------------------

  private val moduleType: Parsley[ModuleType] = (keyword("port") *> Parsley.pure(ModuleType.Port))
    | (contextualKeyword("effect") *> Parsley.pure(ModuleType.Effect))
    | Parsley.pure(ModuleType.Plain)

  /**
   * The `where { command = …, subscription = … }` clause of an `effect module`.
   *
   * Elm takes the two keys in either order and requires at least one of them, so this reads a list and sorts it out
   * afterwards rather than spelling out the permutations.
   */
  private val effectManager: Parsley[CstEffectManager] =
    val entry: Parsley[(String, CstName)] = ((ModuleParser.lowerName <* symbol("=")) <~> upperName).map {
      case (key, value) => key.value -> value
    }

    (offset <~> (keyword("where") *> braces(commaSep1(entry))) <~> offset).collectMsg(_ =>
      Seq("an effect module's `where` clause needs `command`, `subscription`, or both")
    ) {
      case ((s, entries), e)
          if entries.map(_._1).toSet.subsetOf(Set("command", "subscription")) &&
            entries.map(_._1).distinct.size == entries.size &&
            entries.nonEmpty =>
        CstEffectManager(
          command = entries.collectFirst { case ("command", value) => value },
          subscription = entries.collectFirst { case ("subscription", value) => value }
        )(Span.fromStartEnd(s, e))
    }

  val moduleDeclaration: Parsley[CstModuleDeclaration] =
    (offset <~> moduleType <* keyword("module") <~> qualifiedName <~> option(effectManager) <~>
      exposingList <~> offset).map { case (((((s, mt), qn), manager), exp), e) =>
      CstModuleDeclaration(mt, qn, exp, manager)(Span.fromStartEnd(s, e))
    }

  // -----------------------------------------------------------------------
  // Imports
  // -----------------------------------------------------------------------

  val importDecl: Parsley[CstImport] =
    (offset <~> keyword("import") *> qualifiedName <~>
      option(keyword("as") *> upperName) <~>
      option(exposingList) <~> offset).map { case ((((s, modName), alias), exp), e) =>
      CstImport(modName, alias, exp)(Span.fromStartEnd(s, e))
    }

  // -----------------------------------------------------------------------
  // Top-level module
  // -----------------------------------------------------------------------

  /**
   * A declaration, which Elm requires to begin in column 1.
   *
   * The column is not a formatting preference: it is how the parser knows that the previous declaration's expression
   * has ended. Without it, `main = f\n    x` and a following declaration are indistinguishable from one long
   * application.
   */
  private val topLevelDeclaration: Parsley[CstDeclaration] =
    atomic(atTopLevel) *> DeclarationParser.declaration

  /**
   * What is left when the declarations run out.
   *
   * Nothing left is the happy case. Something left that would have been a perfectly good declaration had it started in
   * column 1 is the column rule being broken, and saying so beats an unexplained "unexpected token" against the first
   * character of a declaration that looks reasonable. Anything else — a stray operator, a truncated expression — is
   * described better by the ordinary end-of-input error than by a guess about indentation.
   */
  private val declarationsEnd: Parsley[Unit] =
    lookAhead(eof)
      | atomic(lookAhead(DeclarationParser.declaration).void)
      *> fail("a top-level declaration has to start in column 1")
      | Parsley.empty

  /** Parse a complete Elm module. */
  val module: Parsley[CstModule] =
    fully(
      (offset <~> moduleDeclaration <~> many(importDecl) <~>
        many(topLevelDeclaration) <~> offset).map { case ((((s, modDecl), imports), decls), e) =>
        CstModule(modDecl, imports.toIndexedSeq, decls.toIndexedSeq)(Span.fromStartEnd(s, e))
      } <* declarationsEnd
    )
