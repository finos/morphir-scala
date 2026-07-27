package morphir.langkit.itest.steps

import io.cucumber.scala.{EN, ScalaDsl}

import morphir.langkit.itest.CorpusCoverage

class CorpusSteps extends ScalaDsl with EN:

  When("every conformance corpus module is parsed") { () =>
    CorpusCoverage.parseAll()
  }

  Then("every required CST node type is exercised") { () =>
    val missing = CorpusCoverage.missingNodeTypes()
    assert(
      missing.isEmpty,
      s"""the corpus exercises no instance of ${missing.size} required node type(s):
         |  ${missing.toList.sorted.mkString("\n  ")}
         |Add coverage to a module in resources/fixtures/conformance, or record the gap in the conformance plan.
         |""".stripMargin
    )
  }
