package org.finos.morphir.toolkit.vfile

import java.nio.file.Paths

class VFilePathSuite extends munit.FunSuite {
  test("It should be possible to create from a Path") {
    val testPath = Paths.get("home", "test", "path")
    val actual   = VFilePath(testPath)
    assertEquals(actual.fullPath, testPath.toString)
  }
}
