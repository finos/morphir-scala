package morphir.langkit.elm.parser

import kyo.test.*
import parsley.{Failure, Success}

import morphir.langkit.elm.Elm
import morphir.langkit.elm.cst.*

/**
 * Regression coverage for parser shapes that are easy to get subtly wrong and that a "does it parse" assertion would
 * not catch: the tree can be the wrong shape while the parse still succeeds.
 */
class AssociativitySpec extends Test[Any]:

  private def parseDecl(body: String): CstDeclaration =
    Elm.parseCst(s"module M exposing (..)\n\n$body\n") match
      case Success(m)          => m.declarations.head
      case Failure(diagnostic) =>
        throw new AssertionError(s"parse failed: $diagnostic")

  "function type associativity" - {
    "`->` associates to the right" in {
      // `a -> b -> c` is `a -> (b -> c)`, never `(a -> c) -> b`.
      val annotation = parseDecl("f : a -> b -> c\nf = 1") match
        case d: CstValueDeclaration => d.annotation.get.typeExpr
        case other                  => throw new AssertionError(s"unexpected declaration: $other")

      annotation match
        case CstFunctionType(CstTypeVariable(a), CstFunctionType(CstTypeVariable(b), CstTypeVariable(c))) =>
          assert(a.value == "a" && b.value == "b" && c.value == "c")
        case other =>
          throw new AssertionError(s"expected a -> (b -> c), got: $other")
    }

    "a single arrow is unchanged" in {
      val annotation = parseDecl("f : a -> b\nf = 1") match
        case d: CstValueDeclaration => d.annotation.get.typeExpr
        case other                  => throw new AssertionError(s"unexpected declaration: $other")

      annotation match
        case CstFunctionType(CstTypeVariable(a), CstTypeVariable(b)) => assert(a.value == "a" && b.value == "b")
        case other => throw new AssertionError(s"expected a -> b, got: $other")
    }
  }

  "cons patterns" - {
    "`::` parses and associates to the right" in {
      // `a :: b :: rest` is `a :: (b :: rest)`.
      val decl    = parseDecl("main = case xs of\n    a :: b :: rest -> a\n    [] -> 0")
      val pattern = decl match
        case d: CstValueDeclaration =>
          d.body match
            case c: CstCaseOf => c.branches.head.pattern
            case other        => throw new AssertionError(s"expected a case expression, got: $other")
        case other => throw new AssertionError(s"unexpected declaration: $other")

      pattern match
        case CstConsPattern(CstVariablePattern(a), CstConsPattern(CstVariablePattern(b), CstVariablePattern(rest))) =>
          assert(a.value == "a" && b.value == "b" && rest.value == "rest")
        case other =>
          throw new AssertionError(s"expected a :: (b :: rest), got: $other")
    }

    "`as` binds looser than `::`, aliasing the whole chain" in {
      val decl    = parseDecl("main = case xs of\n    x :: rest as whole -> x\n    [] -> 0")
      val pattern = decl match
        case d: CstValueDeclaration =>
          d.body match
            case c: CstCaseOf => c.branches.head.pattern
            case other        => throw new AssertionError(s"expected a case expression, got: $other")
        case other => throw new AssertionError(s"unexpected declaration: $other")

      pattern match
        case CstAsPattern(CstConsPattern(_, _), alias) => assert(alias.value == "whole")
        case other => throw new AssertionError(s"expected (x :: rest) as whole, got: $other")
    }

    "a pattern with no `::` is left as the bare atom" in {
      val decl    = parseDecl("main = case xs of\n    x -> x")
      val pattern = decl match
        case d: CstValueDeclaration =>
          d.body match
            case c: CstCaseOf => c.branches.head.pattern
            case other        => throw new AssertionError(s"expected a case expression, got: $other")
        case other => throw new AssertionError(s"unexpected declaration: $other")

      pattern match
        case CstVariablePattern(x) => assert(x.value == "x")
        case other                 => throw new AssertionError(s"expected a bare variable pattern, got: $other")
    }
  }
