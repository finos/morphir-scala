package morphir.langkit.elm.ast

import morphir.langkit.elm.Elm
import morphir.langkit.elm.ast.AstQueryableTree.given
import morphir.langkit.trees.QueryableTree
import kyo.*
import parsley.{Failure, Success}
import kyo.test.*

class KyoAstVisitorSpec extends Test[Any]:

  private val sampleSource =
    """module Main exposing (..)
      |
      |x = 1
      |""".stripMargin

  private def parsedAst: AstNode =
    Elm.parseAst(sampleSource) match
      case Success(m)   => m
      case Failure(msg) => sys.error(s"baseline parse failure: $msg")

  "KyoAstVisitor" - {
    "visit invokes callback for every AST node in pre-order" in {
      val out = KyoAstVisitor.fold(parsedAst, 0)((acc, _) => (acc + 1): Int < Any).eval
      assert(out > 0)
    }
    "visit order matches the pure AstVisitor traversal order" in {
      val qt        = QueryableTree[AstNode]
      val pureOrder = AstVisitor.foldLeft(parsedAst, Vector.empty[String]) { (acc, n) =>
        acc :+ qt.nodeType(n).toString
      }
      val kyoOrder = KyoAstVisitor
        .fold(parsedAst, Vector.empty[String]) { (acc, n) =>
          (acc :+ qt.nodeType(n).toString): Vector[String] < Any
        }
        .eval
      assert(kyoOrder == pureOrder)
    }
    "Abort.fail in callback short-circuits visitation" in {
      val out = Abort
        .run[String] {
          KyoAstVisitor.visit(parsedAst)(_ => Abort.fail("stop"))
        }
        .eval
      assert(out.toString.contains("stop"))
    }
  }
