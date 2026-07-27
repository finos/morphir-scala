package morphir.langkit.elm.cst

import morphir.langkit.elm.parser.ModuleParser
import morphir.langkit.elm.cst.CstQueryableTree.given
import morphir.langkit.trees.QueryableTree
import kyo.*
import parsley.{Failure, Success}
import kyo.test.*

class KyoCstVisitorSpec extends Test[Any]:

  private val sampleSource =
    """module Main exposing (..)
      |
      |x = 1
      |""".stripMargin

  private def parsedCst: CstNode =
    ModuleParser.module.parse(sampleSource) match
      case Success(m)   => m
      case Failure(msg) => sys.error(s"baseline parse failure: $msg")

  "KyoCstVisitor" - {
    "visit invokes callback for every CST node in pre-order" in {
      val out = KyoCstVisitor.fold(parsedCst, 0)((acc, _) => (acc + 1): Int < Any).eval
      assert(out > 0)
    }
    "visit order matches the pure CstVisitor traversal order" in {
      val qt        = QueryableTree[CstNode]
      val pureOrder = CstVisitor.foldLeft(parsedCst, Vector.empty[String]) { (acc, n) =>
        acc :+ qt.nodeType(n).toString
      }
      val kyoOrder = KyoCstVisitor
        .fold(parsedCst, Vector.empty[String]) { (acc, n) =>
          (acc :+ qt.nodeType(n).toString): Vector[String] < Any
        }
        .eval
      assert(kyoOrder == pureOrder)
    }
    "Abort.fail in callback short-circuits visitation" in {
      val out = Abort
        .run[String] {
          KyoCstVisitor.visit(parsedCst)(_ => Abort.fail("stop"))
        }
        .eval
      assert(out.toString.contains("stop"))
    }
  }
