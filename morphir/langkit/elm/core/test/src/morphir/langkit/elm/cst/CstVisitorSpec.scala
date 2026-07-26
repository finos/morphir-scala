package morphir.langkit.elm.cst

import kyo.test.*

import morphir.langkit.elm.Span
import morphir.langkit.elm.cst.CstVisitor.*

class CstVisitorSpec extends Test[Any]:

  private val sp = Span.zero

  private class TagVisitor extends CstVisitor[String]:
    def visitNode(node: CstNode): String                      = "Node"
    override def visitName(node: CstName): String             = s"Name(${node.value})"
    override def visitComment(node: CstComment): String       = s"Comment(${node.kind})"
    override def visitIntLiteral(node: CstIntLiteral): String = s"Int(${node.value})"

    override def visitVariablePattern(node: CstVariablePattern): String =
      s"VarPat(${node.name.value})"

  private val sampleModule: CstModule =
    CstModule(
      CstModuleDeclaration(
        ModuleType.Plain,
        CstQualifiedName(List(CstName("M")(sp)))(sp),
        CstExposingAll()(sp)
      )(sp),
      IndexedSeq.empty,
      IndexedSeq.empty
    )(sp)

  "CstVisitor" - {
    "dispatch" - {
      "visit routes to the specific visitor method" in {
        val v = new TagVisitor
        assert(CstVisitor.visit(CstName("x")(sp), v) == "Name(x)")
      }
      "visit falls back to visitNode when no override is provided" in {
        val v = new TagVisitor
        assert(CstVisitor.visit(CstCharPattern('a')(sp), v) == "Node")
      }
      "visit dispatches int literal to visitIntLiteral" in {
        val v = new TagVisitor
        assert(CstVisitor.visit(CstIntLiteral(3L)(sp), v) == "Int(3)")
      }
      "visit dispatches comments to visitComment" in {
        val v = new TagVisitor
        assert(CstVisitor.visit(CstComment(CommentKind.Doc, "docs")(sp), v) == "Comment(Doc)")
      }
      "visit dispatches variable pattern to visitVariablePattern" in {
        val v = new TagVisitor
        val p = CstVariablePattern(CstName("a")(sp))(sp)
        assert(CstVisitor.visit(p, v) == "VarPat(a)")
      }
    }
    "traversal" - {
      "children returns direct children of a module" in {
        val kids = CstVisitor.children(sampleModule)
        assert(kids.size == 1) // only moduleDecl
      }
      "children returns empty list for a leaf" in
        assert(CstVisitor.children(CstName("x")(sp)).isEmpty)
      "count counts all nodes pre-order" in
        // Module + ModuleDeclaration + QualifiedName + Name + ExposingAll = 5
        assert(CstVisitor.count(sampleModule) == 5)
      "foldLeft visits pre-order" in {
        val q    = CstQualifiedName(List(CstName("x")(sp)))(sp)
        val tags = CstVisitor
          .foldLeft(q, List.empty[String])((acc, n) =>
            (n match
              case _: CstQualifiedName => "Q"
              case cn: CstName         => s"N:${cn.value}"
              case _                   => "?"
            ) :: acc
          )
          .reverse
        assert(tags == List("Q", "N:x"))
      }
      "collect picks up nodes matching a partial function" in {
        val q      = CstQualifiedName(List(CstName("a")(sp), CstName("b")(sp)))(sp)
        val values = CstVisitor.collect(q) { case x: CstName => x.value }
        assert(values == List("a", "b"))
      }
    }
    "extension methods" - {
      "node.visit delegates to CstVisitor.visit" in {
        val v             = new TagVisitor
        val node: CstNode = CstIntLiteral(7L)(sp)
        assert(node.visit(v) == "Int(7)")
      }
      "node.children delegates to CstVisitor.children" in {
        val q: CstNode = CstQualifiedName(List(CstName("a")(sp)))(sp)
        assert(q.children.size == 1)
      }
      "node.count delegates to CstVisitor.count" in {
        val node: CstNode = CstIntLiteral(1L)(sp)
        assert(node.count == 1)
      }
      "node.fold delegates to CstVisitor.foldLeft" in {
        val q: CstNode = CstQualifiedName(List(CstName("a")(sp)))(sp)
        assert(q.fold(0)((acc, _) => acc + 1) == 2)
      }
      "node.collect delegates to CstVisitor.collect" in {
        val q: CstNode = CstQualifiedName(List(CstName("x")(sp)))(sp)
        val values     = q.collect { case cn: CstName => cn.value }
        assert(values == List("x"))
      }
    }
  }
