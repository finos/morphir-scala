package morphir.langkit.itest.steps

import io.cucumber.scala.{EN, ScalaDsl}

import morphir.langkit.elm.ast
import morphir.langkit.elm.cst.*
import morphir.langkit.itest.TestDriver

class DeclarationSteps(driver: TestDriver) extends ScalaDsl with EN:

  Then("the module has {int} declaration(s)") { (count: Int) =>
    val actual = driver.cst.declarations.size
    assert(actual == count, s"expected $count declarations, got $actual")
  }

  Then("declaration {int} is a value named {string}") { (index: Int, name: String) =>
    driver.cst.declarations(index - 1) match
      case v: CstValueDeclaration =>
        assert(v.name.value == name, s"expected value name [$name], got [${v.name.value}]")
      case other =>
        throw new AssertionError(s"expected CstValueDeclaration, got [${other.getClass.getSimpleName}]")
  }

  Then("declaration {int} is a type alias named {string}") { (index: Int, name: String) =>
    driver.cst.declarations(index - 1) match
      case a: CstTypeAliasDeclaration =>
        assert(a.name.value == name, s"expected alias name [$name], got [${a.name.value}]")
      case other =>
        throw new AssertionError(s"expected CstTypeAliasDeclaration, got [${other.getClass.getSimpleName}]")
  }

  Then("declaration {int} is a custom type named {string}") { (index: Int, name: String) =>
    driver.cst.declarations(index - 1) match
      case t: CstCustomTypeDeclaration =>
        assert(t.name.value == name, s"expected custom type name [$name], got [${t.name.value}]")
      case other =>
        throw new AssertionError(s"expected CstCustomTypeDeclaration, got [${other.getClass.getSimpleName}]")
  }

  Then("custom type {string} has {int} constructors") { (name: String, count: Int) =>
    val decl = driver.cst.declarations.collectFirst {
      case t: CstCustomTypeDeclaration if t.name.value == name => t
    } match
      case Some(t) => t
      case None    => throw new AssertionError(s"no custom type named [$name]")
    assert(
      decl.constructors.size == count,
      s"expected $count constructors on [$name], got ${decl.constructors.size}"
    )
  }

  Then("AST declaration {int} is a value named {string}") { (index: Int, name: String) =>
    driver.ast.declarations(index - 1) match
      case v: ast.ValueDeclaration =>
        assert(v.name == name, s"expected AST value name [$name], got [${v.name}]")
      case other =>
        throw new AssertionError(s"expected ast.ValueDeclaration, got [${other.getClass.getSimpleName}]")
  }

  Then("AST value {string} has a type annotation") { (name: String) =>
    val v = driver.ast.declarations.collectFirst {
      case v: ast.ValueDeclaration if v.name == name => v
    } match
      case Some(v) => v
      case None    => throw new AssertionError(s"no AST value named [$name]")
    assert(v.typeAnnotation.isDefined, s"expected type annotation on [$name], got none")
  }

  Then("declaration {int} has doc comment {string}") { (index: Int, expectedText: String) =>
    val decl       = driver.cst.declarations(index - 1)
    val docComment = decl.trivia.docComment
    assert(
      docComment.isDefined,
      s"expected doc comment on declaration $index, but trivia has no doc comment"
    )
    val actual = docComment.get.text.trim
    assert(
      actual == expectedText,
      s"expected doc comment [$expectedText] on declaration $index, got [$actual]"
    )
  }

  Then("declaration {int} has no doc comment") { (index: Int) =>
    val decl = driver.cst.declarations(index - 1)
    assert(
      decl.trivia.docComment.isEmpty,
      s"expected no doc comment on declaration $index, but found [${decl.trivia.docComment.map(_.text.trim)}]"
    )
  }
