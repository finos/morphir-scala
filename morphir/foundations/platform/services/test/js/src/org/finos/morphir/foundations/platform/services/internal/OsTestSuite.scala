package org.finos.morphir.foundations.platform.services.internal

import org.finos.morphir.testing.munit.*

class OsTestSuite extends MorphirTestSuite {
  describe("Node os module"){
    test("platform should return one of the expected values"){
      val expectedPlatforms = Set("aix", "darwin", "freebsd", "linux", "openbsd", "sunos", "win32")
      expect(expectedPlatforms contains os.Os.platform())
    }

    test("tmpdir should return a non-empty string"){
      expect(os.Os.tmpdir() != "")
    }
  }
}
