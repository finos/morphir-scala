package org.finos.morphir.foundations.platform.services

import org.finos.morphir.testing.munit.MorphirTestSuite

class FsTestSuite extends MorphirTestSuite {
  describe("For myPlatform.fs") {
    test("exists should return true for an existing file") {
      val thisFile = sourcecode.File()
      expect(myPlatform.fs.exists(thisFile))
    }
    test("exists should return false for a non-existent file") {
      val thisFile = sourcecode.File()
      val testFile = s"${thisFile}.DOES_NOT_EXIST"
      expectEquals(myPlatform.fs.exists(testFile), false)
    }
  }
}
