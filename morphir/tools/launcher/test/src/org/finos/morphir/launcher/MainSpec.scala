package org.finos.morphir.launcher

import coursier.{Dependency, Module, ModuleName, Organization}
import zio.test._

object CoursierSpec extends ZIOSpecDefault {

  def fixture = new {
    val morphirVersion = MorphirVersionTest("dummyVersion", None, None)
    val coursier       = CoursierTest()
    val main           = Main(morphirVersion, coursier)
  }

  def spec = suite("Launcher")(
    test("should fetch morphir cli") {
      val f = fixture
      val expectedMorphirCliDep = Dependency(
        Module(Organization("org.finos.morphir"), ModuleName("morphir-tools-cli_3")),
        "dummyVersion"
      )
      f.main.run()
      assertTrue(f.coursier.fetched.contains(expectedMorphirCliDep))
    }
  )
}
