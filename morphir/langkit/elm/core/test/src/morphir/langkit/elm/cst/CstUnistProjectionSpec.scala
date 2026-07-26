package morphir.langkit.elm.cst

import parsley.{Failure, Success}
import kyo.test.*

import morphir.langkit.elm.Krueger
import morphir.langkit.elm.cst.CstQueryableTree.given
import morphir.langkit.elm.cst.CstUnistProjection.given
import morphir.langkit.trees.unist.UnistPoint
import morphir.langkit.trees.unist.UnistProjection

class CstUnistProjectionSpec extends Test[Any]:

  private val source =
    """module App exposing (..)
      |
      |main = 42
      |""".stripMargin

  private def parse(src: String): CstModule = Krueger.parseCst(src) match
    case Success(value) => value
    case Failure(msg)   => throw AssertionError(s"parse failed: $msg")

  private val moduleTree: CstNode = parse(source)

  "CstUnistProjection" - {
    "projects parsed CST root with type, child order, fields, and source position" in {
      val node = UnistProjection.project(moduleTree, Some(source))
      assert(node.`type` == "CstModule")
      assert(node.children.map(_.`type`) == IndexedSeq("CstModuleDeclaration", "CstValueDeclaration"))
      assert(node.data.fields.keySet == Set("moduleDecl", "imports", "declarations"))
      assert(node.data.fields("moduleDecl") == IndexedSeq(0))
      assert(node.position.exists(_.start == UnistPoint(1, 1, Some(0))))
      assert(node.position.exists(_.end == UnistPoint(4, 1, Some(36))))
    }
    "projects CstName leaves with value text" in {
      val node  = UnistProjection.project(moduleTree, Some(source))
      val names = collect(node).filter(_.`type` == "CstName").flatMap(_.value)
      assert(names.contains("App"))
      assert(names.contains("main"))
    }
    "preserves QueryableTree child count on every projected node" in {
      val node = UnistProjection.project(moduleTree, Some(source))
      assert(collect(node).forall(n => n.data.childCount == n.children.size))
    }
  }

  private def collect(
      node: morphir.langkit.trees.unist.UnistNode
  ): List[morphir.langkit.trees.unist.UnistNode] =
    node :: node.children.toList.flatMap(collect)
