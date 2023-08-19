package org.finos.morphir.util.vfile

import zio.test._
import java.nio.file.Paths

trait VFileSpecPlatformSpecific { self: VFileSpec.type =>
  def platformSpecificSuite = suite("PlatformSpecific")(
    test("It should be possible to create a file reference") {
      val aPath  = (Paths.get("testing", "someTestFile.txt"))
      val actual = VFile.fileRef(aPath)
      assertEquals(actual.path.fullPath, aPath.toString)
    }
  )
}