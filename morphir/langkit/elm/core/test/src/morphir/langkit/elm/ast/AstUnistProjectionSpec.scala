package morphir.langkit.elm.ast

import parsley.{Failure, Success}
import kyo.test.*

import morphir.langkit.elm.Elm
import morphir.langkit.elm.ast.AstQueryableTree.given
import morphir.langkit.elm.ast.AstUnistProjection.given
import morphir.langkit.trees.unist.UnistProjection

class AstUnistProjectionSpec extends Test[Any]:

  private val source =
    """module App exposing (..)
      |
      |main = 42
      |""".stripMargin

  private def parse(src: String): Module = Elm.parseAst(src) match
    case Success(value) => value
    case Failure(msg)   => throw AssertionError(s"parse failed: $msg")

  private val moduleTree: AstNode = parse(source)

  "AstUnistProjection" - {
    "projects parsed AST root with type, fields, and child count" in {
      val node = UnistProjection.project(moduleTree, Some(source))
      assert(node.`type` == "Module")
      assert(node.data.fields.keySet == Set("exposing", "manager", "imports", "declarations"))
      assert(node.data.childCount == node.children.size)
    }
    "projects an effect module's manager as a child node" in {
      val effectSource =
        """effect module Eff where { command = MyCmd, subscription = MySub } exposing (..)
          |
          |x = 1
          |""".stripMargin
      val effectTree: AstNode = parse(effectSource)
      val node                = UnistProjection.project(effectTree, Some(effectSource))
      assert(node.children.map(_.`type`).contains("EffectManager"))
    }
    "projects AST declaration and literal text values" in {
      val node   = UnistProjection.project(moduleTree, Some(source))
      val values = collect(node).flatMap(_.value)
      assert(values.contains("main"))
      assert(values.contains("42"))
    }
    "preserves AST traversal order for the module children" in {
      val node = UnistProjection.project(moduleTree, Some(source))
      assert(node.children.map(_.`type`) == IndexedSeq("ExposingAll", "ValueDeclaration"))
    }
  }

  private def collect(
      node: morphir.langkit.trees.unist.UnistNode
  ): List[morphir.langkit.trees.unist.UnistNode] =
    node :: node.children.toList.flatMap(collect)
