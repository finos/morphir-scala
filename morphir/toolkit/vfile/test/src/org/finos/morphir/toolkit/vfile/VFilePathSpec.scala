package org.finos.morphir.toolkit.vfile

import java.nio.file.Paths
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test._
object VFilePathSpec extends MorphirBaseSpec {
  def spec = suite("VFilePath Spec")(
    test("It should be possible to create from a Path"){
      val testPath = Paths.get("home","test","path")
      val actual = VFilePath(testPath)
      assertTrue(actual.fullPath == testPath.toString)
    }
  )
}
