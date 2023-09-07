package org.finos.morphir.model

import org.finos.morphir.testing._
import zio.test._

object MorphirProjectSpec extends MorphirBaseSpec with ZioJsonSpec {
  def jsonSuite = suite("JSON")(
    suite("Decoding")(
      jsonDecodingTest("Simple")(
        """{
          |  "name": "My.Package",
          |  "sourceDirectory": "src",
          |  "exposedModules": ["String","List"],
          |  "localDependencies": ["./.morphir/dependencies/other-package/morphir-ir.json"]
          |}""".stripMargin
      )(
        MorphirProject(
          name = "My.Package",
          sourceDirectory = "src",
          exposedModules = Set("String", "List"),
          localDependencies = Set("./.morphir/dependencies/other-package/morphir-ir.json")
        )
      ),
      jsonDecodingTest("Should support omitting localDependencies")(
        """{
          |  "name": "My.Package",
          |  "sourceDirectory": "src",
          |  "exposedModules": ["String","List"]
          |}""".stripMargin
      )(
        MorphirProject(
          name = "My.Package",
          sourceDirectory = "src",
          exposedModules = Set("String", "List"),
          localDependencies = Set.empty
        )
      )
    ),
    suite("Round trip")(
      jsonRoundtripTest("Simple/1")(
        MorphirProject(
          name = "My.Package",
          sourceDirectory = "src",
          exposedModules = Set("Module-1"),
          localDependencies = Set("Test-2")
        )
      )
    )
  )

  def spec = suite("MorphirProjectSpec")(
    jsonSuite
  )
}
