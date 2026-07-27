package morphir.langkit.itest.steps

import io.cucumber.scala.{EN, ScalaDsl}

import morphir.langkit.elm.cst.*
import morphir.langkit.itest.TestDriver

class ModuleParserSteps(driver: TestDriver) extends ScalaDsl with EN:

  Given("the Elm source:") { (src: String) =>
    driver.setSource(src)
  }

  Given("the Elm fixture {string}") { (resourcePath: String) =>
    driver.setSourceFromResource(resourcePath)
  }

  When("the source is parsed") { () =>
    driver.parseCst()
  }

  When("the source is parsed to an AST") { () =>
    driver.parseAst()
  }

  Then("the module is named {string}") { (name: String) =>
    val actual = driver.cst.moduleDecl.name.parts.map(_.value).mkString(".")
    assert(actual == name, s"expected module name [$name], got [$actual]")
  }

  Then("the AST module is named {string}") { (name: String) =>
    val actual = driver.ast.name.fullName
    assert(actual == name, s"expected AST module name [$name], got [$actual]")
  }

  Then("the module is plain") { () =>
    val mt = driver.cst.moduleDecl.moduleType
    assert(mt == ModuleType.Plain, s"expected plain module, got [$mt]")
  }

  Then("the module is port") { () =>
    val mt = driver.cst.moduleDecl.moduleType
    assert(mt == ModuleType.Port, s"expected port module, got [$mt]")
  }

  Then("the module is effect") { () =>
    val mt = driver.cst.moduleDecl.moduleType
    assert(mt == ModuleType.Effect, s"expected effect module, got [$mt]")
  }

  Then("the module has {int} import(s)") { (count: Int) =>
    val actual = driver.cst.imports.size
    assert(actual == count, s"expected $count imports, got $actual")
  }

  Then("the module has {int} comment(s)") { (count: Int) =>
    val actual = driver.cst.trivia.comments.size
    assert(actual == count, s"expected $count comments, got $actual")
  }

  Then("the AST has {int} import(s)") { (count: Int) =>
    val actual = driver.ast.imports.size
    assert(actual == count, s"expected $count AST imports, got $actual")
  }

  Then("import {int} is from module {string}") { (index: Int, name: String) =>
    val imp    = driver.cst.imports(index - 1)
    val actual = imp.moduleName.parts.map(_.value).mkString(".")
    assert(actual == name, s"expected module [$name], got [$actual]")
  }

  Then("AST import {int} is from module {string}") { (index: Int, name: String) =>
    val imp    = driver.ast.imports(index - 1)
    val actual = imp.moduleName.fullName
    assert(actual == name, s"expected AST module [$name], got [$actual]")
  }

  Then("import {int} is aliased as {string}") { (index: Int, alias: String) =>
    val imp = driver.cst.imports(index - 1)
    assert(
      imp.alias.exists(_.value == alias),
      s"expected alias [$alias], got [${imp.alias.map(_.value)}]"
    )
  }

  Then("import {int} exposes values {string}") { (index: Int, csv: String) =>
    val imp    = driver.cst.imports(index - 1)
    val values = imp.exposing match
      case Some(e: CstExposingExplicit) => e.items.collect { case v: CstExposedValue => v.name.value }
      case _                            => Nil
    val expected = csv.split(",").toList.map(_.trim)
    assert(values == expected, s"expected values [$expected], got [$values]")
  }

  Then("the module doc comment is {string}") { (expectedText: String) =>
    val docComment = driver.cst.trivia.docComment
    assert(
      docComment.isDefined,
      "expected a module doc comment, but trivia has no doc comment"
    )
    val actual = docComment.get.text.trim
    assert(
      actual == expectedText,
      s"expected module doc comment [$expectedText], got [$actual]"
    )
  }

  Then("the module has no doc comment") { () =>
    assert(
      driver.cst.trivia.docComment.isEmpty,
      s"expected no module doc comment, but found [${driver.cst.trivia.docComment.map(_.text.trim)}]"
    )
  }

  Then("comment {int} is a {string} comment with text {string}") {
    (index: Int, expectedKind: String, expectedText: String) =>
      val comment = driver.cst.trivia.comments(index - 1)
      val kind    = expectedKind.trim.toLowerCase match
        case "line"  => CommentKind.Line
        case "block" => CommentKind.Block
        case "doc"   => CommentKind.Doc
        case other   => throw new AssertionError(s"unknown comment kind [$other]")
      assert(
        comment.kind == kind && comment.text.trim == expectedText,
        s"expected $kind comment [$expectedText], got ${comment.kind} [${comment.text.trim}]"
      )
  }
