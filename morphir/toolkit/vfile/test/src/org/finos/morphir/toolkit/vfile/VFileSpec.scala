package org.finos.morphir
package toolkit
package vfile

import org.finos.morphir.testing.MorphirBaseSpec
import zio.test._

import java.nio.file.Paths

object VFileSpec extends MorphirBaseSpec {
  def spec = suite("VFile Spec")(
    test("It should be possible to create a file reference"){
      val aPath = (Paths.get("testing","someTestFile.txt"))
      val actual = VFile.fileRef(aPath)
      assertTrue(actual.path.fullPath == aPath.toString)
    }
  )
}
