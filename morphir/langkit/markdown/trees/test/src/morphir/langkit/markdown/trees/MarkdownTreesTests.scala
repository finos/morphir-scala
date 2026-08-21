package morphir.langkit.markdown.trees

import kyo.*
import kyo.test.*
import morphir.langkit.markdown.{MdcNode, Parser}
import morphir.langkit.markdown.cst.{Cst, CstNode, CstParser}
import morphir.langkit.trees.{NodeTypeName, QueryableTree}
import morphir.langkit.trees.unist.UnistProjection

/**
 * Both Markdown trees through the langkit trees contract: the AST in mdast vocabulary, the CST in its own.
 *
 * What is pinned: node type names come from total matches and match the vocabulary each view declares; traversal
 * respects child order; the CST's leaf text reassembles the source through the generic contract alone; Unist
 * projections carry positions when a source is supplied and none when it is not, which is what lets generated nodes
 * live in a projected tree.
 */
class MarkdownTreesTests extends Test[Any]:

  import CstQueryableTree.given
  import MdcNodeQueryableTree.given

  private val source = "# Title\n\n- item `code`\n"

  private def ast: MdcNode.Root =
    Parser.parse(source) match
      case Result.Success(document) => document
      case other                    => throw new IllegalStateException(s"parse failed: $other")

  private def typeNames(node: MdcNode): Seq[String] =
    NodeTypeName.unwrap(QueryableTree[MdcNode].nodeType(node))
      +: QueryableTree[MdcNode].children(node).flatMap(typeNames)

  "mdast view" - {

    "names nodes in mdast vocabulary" in {
      val names = typeNames(ast)
      assert(names.head == "root")
      assert(names.contains("heading"))
      assert(names.contains("list"))
      assert(names.contains("listItem"))
      assert(names.contains("inlineCode"))
    }

    "projects to Unist with positions from the source" in {
      val projected = UnistProjection.project[MdcNode](ast, Some(source))
      assert(projected.`type` == "root")
      assert(projected.position.exists(_.start.line == 1))
      val heading = projected.children.head
      assert(heading.`type` == "heading")
      assert(heading.position.exists(p => p.start.line == 1 && p.start.column == 1))
    }

    "projects without positions when no source is supplied" in {
      val projected = UnistProjection.project[MdcNode](ast)
      assert(projected.position.isEmpty)
      assert(projected.children.forall(_.position.isEmpty))
    }

    "exposes named fields as index sets over the children" in {
      val projected = UnistProjection.project[MdcNode](ast)
      assert(projected.data.fields.get("children").exists(_.size == projected.children.size))
    }
  }

  "cst view" - {

    "names nodes in CST vocabulary" in {
      val cst   = CstParser.parse(source)
      val names =
        def walk(node: CstNode): Seq[String] =
          NodeTypeName.unwrap(QueryableTree[CstNode].nodeType(node))
            +: QueryableTree[CstNode].children(node).flatMap(walk)
        walk(cst)
      assert(names.head == "document")
      assert(names.contains("atxHeading"))
      assert(names.contains("bulletList"))
      assert(names.contains("codeSpan"))
      assert(names.contains("token"))
    }

    "reassembles the source from leaf text through the generic contract" in {
      val cst                           = CstParser.parse(source)
      def leaves(node: CstNode): String =
        QueryableTree[CstNode].text(node) match
          case Some(text) => text
          case None       => QueryableTree[CstNode].children(node).map(leaves).mkString
      assert(leaves(cst) == source)
    }

    "projects to Unist with faithful positions" in {
      val projected = UnistProjection.project[CstNode](CstParser.parse(source), Some(source))
      assert(projected.`type` == "document")
      assert(projected.position.exists(p => p.start.offset.contains(0) && p.end.offset.contains(source.length)))
    }
  }
