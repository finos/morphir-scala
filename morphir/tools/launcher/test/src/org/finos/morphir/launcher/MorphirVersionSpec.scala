package org.finos.morphir.launcher

import zio.test._

object MorphirVersionSpec extends ZIOSpecDefault {

  def spec = suite("MorphirVersion")(
    suite("version resolution order")(
      test("should resolve from env first") {
        val morphirVersion = MorphirVersionTest(
          defaultVersion = "versionA",
          versionFromEnv = Some("versionB"),
          versionFromFile = Some("versionC")
        )

        assertTrue(morphirVersion.version == morphirVersion.versionFromEnv.get)
      },
      test("should resolve from file second") {
        val morphirVersion = MorphirVersionTest(
          defaultVersion = "versionA",
          versionFromEnv = None,
          versionFromFile = Some("versionC")
        )

        assertTrue(morphirVersion.version == morphirVersion.versionFromFile.get)
      },
      test("should resolve from default third") {
        val morphirVersion = MorphirVersionTest(
          defaultVersion = "versionA",
          versionFromEnv = None,
          versionFromFile = None
        )

        assertTrue(morphirVersion.version == morphirVersion.defaultVersion)
      }
    )
  )
}
