package morphir.langkit.elm

import parsley.{Failure, Success}
import kyo.test.*

import morphir.langkit.elm.cst.CommentKind

class KruegerSpec extends Test[Any]:

  private val minimal = "module Main exposing (..)\n"

  private val richer =
    """module App exposing (..)
      |
      |import Html
      |
      |main = 42
      |""".stripMargin

  private def parseCstOrFail(src: String): morphir.langkit.elm.cst.CstModule =
    Krueger.parseCst(src) match
      case Success(m)   => m
      case Failure(msg) => throw new AssertionError(s"parse failed: $msg\nSource:\n$src")

  private def parseAstOrFail(src: String): morphir.langkit.elm.ast.Module =
    Krueger.parseAst(src) match
      case Success(m)   => m
      case Failure(msg) => throw new AssertionError(s"parse failed: $msg\nSource:\n$src")

  "Krueger" - {
    "parseCst succeeds on minimal module" in {
      val m = parseCstOrFail(minimal)
      assert(m.moduleDecl.name.parts.map(_.value) == List("Main"))
    }
    "parseAst succeeds on minimal module" in {
      val m = parseAstOrFail(minimal)
      assert(m.name.fullName == "Main")
    }
    "parseCst succeeds on richer fixture" in {
      val m = parseCstOrFail(richer)
      assert(m.moduleDecl.name.parts.map(_.value) == List("App"))
      assert(m.imports.size == 1)
      assert(m.declarations.size == 1)
    }
    "parseCst differentiates line, block, and doc comments" in {
      val m = parseCstOrFail(
        """module App exposing (..)
          |
          |-- regular line
          |{- regular block -}
          |{-| module docs -}
          |main = "-- not a comment"
          |""".stripMargin
      )
      // The doc comment is associated with the `main` declaration via trivia
      val declDoc = m.declarations.head
        .asInstanceOf[morphir.langkit.elm.cst.CstValueDeclaration]
        .trivia
        .docComment
        .map(_.text.trim)
      // Non-doc comments remain in the module trivia
      val moduleComments = m.trivia.comments.filterNot(_.kind == CommentKind.Doc)
      assert(moduleComments.map(_.kind) == IndexedSeq(CommentKind.Line, CommentKind.Block))
      assert(moduleComments.map(_.text.trim) == IndexedSeq("regular line", "regular block"))
      assert(declDoc.contains("module docs"))
    }
    "parseAst lowers imports and declarations" in {
      val m = parseAstOrFail(richer)
      assert(m.imports.map(_.moduleName.fullName) == List("Html"))
      assert(m.declarations.size == 1)
    }
    "parseCst fails on malformed input" in {
      Krueger.parseCst("module !!!") match
        case Failure(_) => succeed
        case Success(_) => throw new AssertionError("expected failure, got success")
    }
    "parseCst preserves every top-level value declaration" in {
      val src =
        """module M exposing (..)
          |
          |x = 1
          |
          |y = 2
          |
          |z = 3
          |""".stripMargin
      val m     = parseCstOrFail(src)
      val names = m.declarations.collect { case v: morphir.langkit.elm.cst.CstValueDeclaration =>
        v.name.value
      }
      assert(names == IndexedSeq("x", "y", "z"))
    }
    "parseAst preserves every top-level value declaration" in {
      val src =
        """module M exposing (..)
          |
          |x = 1
          |
          |y = 2
          |
          |z = 3
          |""".stripMargin
      val m     = parseAstOrFail(src)
      val names = m.declarations.collect { case v: morphir.langkit.elm.ast.ValueDeclaration =>
        v.name
      }
      assert(names == IndexedSeq("x", "y", "z"))
    }
  }
