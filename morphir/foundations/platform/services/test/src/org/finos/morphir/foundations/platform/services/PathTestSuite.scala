package org.finos.morphir.foundations.platform.services

import org.finos.morphir.testing.munit.MorphirTestSuite

class PathTestSuite extends MorphirTestSuite {
  describe("For myPlatform.path"){
    
    test("delimiter should be Windows or Posix style") {
      val actual = myPlatform.path.delimiter
      assert(actual == ";" || actual == ":" )
    }

    test("sep should be Windows or Posix style") {
      val actual = myPlatform.path.sep
      assert(actual == "\\" || actual == "/" )
    }
    
  }
}
