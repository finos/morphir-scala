package morphir.langkit.elm.ast

import kyo.test.*

import morphir.langkit.elm.Span
import morphir.langkit.elm.ast.AstVisitor.*
import morphir.langkit.elm.cst.*
import morphir.langkit.elm.parser.CstLowering

class AstVisitorSpec extends Test[Any]:

  private val sp                  = Span.zero
  private def cn(name: String)    = CstName(name)(sp)
  private def cqn(parts: String*) = CstQualifiedName(parts.map(cn).toList)(sp)

  private val sampleCst: CstModule =
    CstModule(
      CstModuleDeclaration(ModuleType.Plain, cqn("M"), CstExposingAll()(sp))(sp),
      IndexedSeq(CstImport(cqn("List"), None, None)(sp)),
      IndexedSeq.empty
    )(sp)

  private val sampleAst: Module = CstLowering.lowerModule(sampleCst)

  private class TagVisitor extends AstVisitor[String]:
    def visitNode(node: AstNode): String                         = "Node"
    override def visitQualifiedName(node: QualifiedName): String = s"QN(${node.fullName})"
    override def visitIntLiteral(node: IntLiteral): String       = s"Int(${node.value})"
    override def visitImport(node: Import): String               = s"Imp(${node.moduleName.fullName})"
    override def visitModule(node: Module): String               = s"Mod(${node.name.fullName})"

  "AstVisitor" - {
    "dispatch" - {
      "visit routes to the specific visitor method" in {
        val v            = new TagVisitor
        val lit: AstNode = IntLiteral(5L)(sp)
        assert(AstVisitor.visit(lit, v) == "Int(5)")
      }
      "visit falls back to visitNode when no override is provided" in {
        val v          = new TagVisitor
        val s: AstNode = StringLiteral("hi")(sp)
        assert(AstVisitor.visit(s, v) == "Node")
      }
    }
    "traversal" - {
      "children returns direct children of a module" in
        // Module's children are: exposing :: imports ::: declarations
        assert(AstVisitor.children(sampleAst).size == 2)
      "count counts all lowered nodes" in
        // Module + ExposingAll + Import = 3
        assert(AstVisitor.count(sampleAst) == 3)
      "foldLeft visits pre-order" in {
        val tags = AstVisitor
          .foldLeft(sampleAst, List.empty[String])((acc, n) =>
            (n match
              case _: Module      => "M"
              case _: Import      => "I"
              case _: ExposingAll => "EA"
              case _              => "?"
            ) :: acc
          )
          .reverse
        assert(tags == List("M", "EA", "I"))
      }
      "collect picks up nodes matching a partial function" in {
        val imports = AstVisitor.collect(sampleAst) { case i: Import => i.moduleName.fullName }
        assert(imports == List("List"))
      }
    }
    "extension methods" - {
      "node.visit delegates to AstVisitor.visit" in {
        val v             = new TagVisitor
        val node: AstNode = IntLiteral(9L)(sp)
        assert(node.visit(v) == "Int(9)")
      }
      "node.children delegates to AstVisitor.children" in
        assert(sampleAst.children.size == 2)
      "node.count delegates to AstVisitor.count" in
        assert(sampleAst.count == 3)
      "node.fold delegates to AstVisitor.foldLeft" in
        assert(sampleAst.fold(0)((acc, _) => acc + 1) == 3)
      "node.collect delegates to AstVisitor.collect" in {
        val names = sampleAst.collect { case i: Import => i.moduleName.fullName }
        assert(names == List("List"))
      }
    }
  }
