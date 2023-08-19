package org.finos.morphir.util.vfile

import org.finos.morphir.testing.MorphirBaseSpec

import java.nio.file.Paths

object VFileSuite extends MorphirBaseSpec {
  def spec = suite("VFileSpec")(
    test("It should be possible to create a file reference") {
      val aPath  = (Paths.get("testing", "someTestFile.txt"))
      val actual = VFile.fileRef(aPath)
      assertEquals(actual.path.fullPath, aPath.toString)
    }
  )
}
