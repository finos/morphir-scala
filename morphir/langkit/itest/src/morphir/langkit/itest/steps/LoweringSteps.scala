package morphir.langkit.itest.steps

import io.cucumber.scala.{EN, ScalaDsl}

import morphir.langkit.itest.TestDriver

/**
 * Lowering-specific assertions beyond what the shared module/declaration step files cover. The bulk of lowering
 * scenarios reuse phrases from ModuleParserSteps / DeclarationSteps; put anything uniquely about the CST→AST shape
 * here.
 */
class LoweringSteps(driver: TestDriver) extends ScalaDsl with EN:

  Then("the AST has {int} declaration(s)") { (count: Int) =>
    val actual = driver.ast.declarations.size
    assert(actual == count, s"expected $count AST declarations, got $actual")
  }
