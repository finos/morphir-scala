package org.finos.morphir.util.vfile
import zio.test._
import java.nio.file.Paths

trait VFilePathSpecPlatformSpecific { self: VFilePathSpec.type =>
  def platformSpecificSuite = suite("PlatformSpecific")(
    test("It should be possible to create from a Path") {
      val testPath = Paths.get("home", "test", "path")
      val actual   = VFilePath(testPath)
      assertTrue(actual.fullPath == testPath.toString)
    }
  )
}
