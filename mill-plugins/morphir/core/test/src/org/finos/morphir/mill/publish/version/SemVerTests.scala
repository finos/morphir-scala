package org.finos.morphir.mill.publish.version

import utest.*

object SemVerTests extends TestSuite {
  val tests = Tests {
    test("parses a plain release") {
      assert(SemVer.parse("0.6.0") == Some(SemVer(0, 6, 0, None)))
    }

    test("parses a milestone prerelease") {
      assert(SemVer.parse("0.6.0-M01") == Some(SemVer(0, 6, 0, Some("M01"))))
    }

    test("rejects a leading v and other non-versions") {
      assert(SemVer.parse("v0.6.0") == None)
      assert(SemVer.parse("Unreleased") == None)
      assert(SemVer.parse("0.6") == None)
    }

    test("orders by numeric component") {
      assert(SemVer.compare("0.6.0", "0.5.9") == Right(1))
      assert(SemVer.compare("0.5.9", "0.6.0") == Right(-1))
      assert(SemVer.compare("0.6.0", "0.6.0") == Right(0))
    }

    test("a prerelease sorts below its own release") {
      assert(SemVer.compare("0.6.0-M01", "0.6.0") == Right(-1))
      assert(SemVer.compare("0.6.0", "0.6.0-M01") == Right(1))
    }

    test("prereleases order lexically among themselves") {
      assert(SemVer.compare("0.6.0-M02", "0.6.0-M01") == Right(1))
    }

    test("comparing a non-version is an error naming it") {
      val result = SemVer.compare("nonsense", "0.6.0")
      assert(result.isLeft)
      assert(result.left.toOption.exists(_.contains("nonsense")))
    }
  }
}
