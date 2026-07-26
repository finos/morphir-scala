package morphir.langkit.elm.parser

import kyo.test.*

import morphir.langkit.elm.Span
import morphir.langkit.elm.cst.*
import morphir.langkit.elm.ast

class CstLoweringSpec extends Test[Any]:

  private val sp                 = Span.zero
  private def n(name: String)    = CstName(name)(sp)
  private def qn(parts: String*) = CstQualifiedName(parts.map(n).toList)(sp)

  private def moduleWithDecl(decl: CstDeclaration): CstModule =
    CstModule(
      CstModuleDeclaration(ModuleType.Plain, qn("M"), CstExposingAll()(sp))(sp),
      IndexedSeq.empty,
      IndexedSeq(decl)
    )(sp)

  "CstLowering" - {
    "lowerModule maps module name, imports, and declarations" in {
      val cst = CstModule(
        CstModuleDeclaration(ModuleType.Plain, qn("Main"), CstExposingAll()(sp))(sp),
        IndexedSeq(CstImport(qn("List"), None, None)(sp)),
        IndexedSeq.empty
      )(sp)
      val m = CstLowering.lowerModule(cst)
      assert(m.name.fullName == "Main")
      assert(m.exposing.isInstanceOf[ast.ExposingAll])
      assert(m.imports.map(_.moduleName.fullName) == IndexedSeq("List"))
    }
    "lowerModule lowers an explicit exposing list" in {
      val items = List[CstExposedItem](
        CstExposedValue(n("foo"))(sp),
        CstExposedOperator(n("++"))(sp),
        CstExposedType(n("Foo"), Some(CstExposedConstructorsAll()(sp)))(sp),
        CstExposedType(n("Bar"), None)(sp)
      )
      val cst = CstModule(
        CstModuleDeclaration(ModuleType.Plain, qn("M"), CstExposingExplicit(items)(sp))(sp),
        IndexedSeq.empty,
        IndexedSeq.empty
      )(sp)
      val m   = CstLowering.lowerModule(cst)
      val exp = m.exposing match
        case e: ast.ExposingExplicit => e.items
        case _                       => Nil
      val types  = exp.collect { case t: ast.ExposedType => (t.name, t.exposeConstructors) }
      val values = exp.collect { case v: ast.ExposedValue => v.name }
      val ops    = exp.collect { case o: ast.ExposedOperator => o.name }
      assert(values == List("foo"))
      assert(ops == List("++"))
      assert(types == List(("Foo", true), ("Bar", false)))
    }
    "lowerQualifiedName flattens parts via fullName" in {
      val cst = CstModule(
        CstModuleDeclaration(ModuleType.Plain, qn("Http", "Body"), CstExposingAll()(sp))(sp),
        IndexedSeq.empty,
        IndexedSeq.empty
      )(sp)
      assert(CstLowering.lowerModule(cst).name.fullName == "Http.Body")
    }
    "lowerPattern strips CstParenthesizedPattern" in {
      val inner   = CstVariablePattern(n("x"))(sp)
      val wrapped = CstParenthesizedPattern(inner)(sp)
      val lowered = CstLowering.lowerPattern(wrapped)
      assert(lowered == ast.VariablePattern("x")(sp))
    }
    "lowerPattern strips nested parens" in {
      val nested  = CstParenthesizedPattern(CstParenthesizedPattern(CstVariablePattern(n("y"))(sp))(sp))(sp)
      val lowered = CstLowering.lowerPattern(nested)
      assert(lowered == ast.VariablePattern("y")(sp))
    }
    "lowerExpression preserves CstParenthesized as ast.Parenthesized" in {
      val wrapped = CstParenthesized(CstIntLiteral(1L)(sp))(sp)
      val lowered = CstLowering.lowerExpression(wrapped)
      assert(lowered.isInstanceOf[ast.Parenthesized])
    }
    "lowerLetBinding uses variable name when pattern is a variable" in {
      val decl = CstValueDeclaration(
        None,
        n("main"),
        IndexedSeq.empty,
        CstLetIn(
          List(
            CstLetBinding(
              None,
              CstVariablePattern(n("x"))(sp),
              Nil,
              CstIntLiteral(1L)(sp)
            )(sp)
          ),
          CstVariableRef(qn("x"))(sp)
        )(sp)
      )(sp)
      val m     = CstLowering.lowerModule(moduleWithDecl(decl))
      val letIn = m.declarations.head.asInstanceOf[ast.ValueDeclaration].body.asInstanceOf[ast.LetIn]
      assert(letIn.bindings.head.name == "x")
    }
    "lowerLetBinding falls back to <pattern> for non-variable patterns" in {
      val decl = CstValueDeclaration(
        None,
        n("main"),
        IndexedSeq.empty,
        CstLetIn(
          List(
            CstLetBinding(
              None,
              CstUnitPattern()(sp),
              Nil,
              CstIntLiteral(1L)(sp)
            )(sp)
          ),
          CstIntLiteral(2L)(sp)
        )(sp)
      )(sp)
      val m     = CstLowering.lowerModule(moduleWithDecl(decl))
      val letIn = m.declarations.head.asInstanceOf[ast.ValueDeclaration].body.asInstanceOf[ast.LetIn]
      assert(letIn.bindings.head.name == "<pattern>")
    }
    "lowerDeclaration carries a value's type annotation onto the AST" in {
      val annotated = CstValueDeclaration(
        annotation = Some(
          CstTypeAnnotation(
            n("foo"),
            CstTypeReference(qn("Int"))(sp)
          )(sp)
        ),
        name = n("foo"),
        patterns = IndexedSeq.empty,
        body = CstIntLiteral(42L)(sp)
      )(sp)
      val lowered = CstLowering.lowerDeclaration(annotated).asInstanceOf[ast.ValueDeclaration]
      assert(lowered.name == "foo")
      assert(lowered.typeAnnotation.exists(_.isInstanceOf[ast.TypeReference]))
    }
    "lowerTypeExpression lowers function types" in {
      val a       = CstTypeVariable(n("a"))(sp)
      val b       = CstTypeVariable(n("b"))(sp)
      val t       = CstFunctionType(a, b)(sp)
      val lowered = CstLowering.lowerTypeExpression(t)
      assert(lowered.isInstanceOf[ast.FunctionType])
    }
  }
