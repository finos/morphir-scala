package org.finos.morphir.toolkit
package vfile

import java.nio.file.Paths

class VFileSuite extends munit.FunSuite {
  test("It should be possible to create a file reference") {
    val aPath  = (Paths.get("testing", "someTestFile.txt"))
    val actual = VFile.fileRef(aPath)
    assertEquals(actual.path.fullPath, aPath.toString)
  }
}
