package org.finos.morphir.mill.publish.version

import utest.*

object ChangelogTests extends TestSuite {
  private val wellFormed =
    """# Changelog
      |
      |## [Unreleased]
      |### Added
      |- Something not yet assigned
      |
      |## [0.6.0-M01]
      |### Added
      |- The next release's shape
      |
      |## [0.5.0-M04] - 2026-07-14
      |### Fixed
      |- History
      |""".stripMargin

  val tests = Tests {
    test("the release line is the topmost undated version heading") {
      assert(Changelog.releaseLine(wellFormed, "CHANGELOG.md") == Right("0.6.0-M01"))
    }

    test("Unreleased is ignored rather than treated as a version") {
      val headings = Changelog.headings(wellFormed)
      assert(headings.map(_.version) == Seq("0.6.0-M01", "0.5.0-M04"))
    }

    test("dated headings are history") {
      val headings = Changelog.headings(wellFormed)
      assert(headings.find(_.version == "0.5.0-M04").flatMap(_.date) == Some("2026-07-14"))
      assert(headings.find(_.version == "0.6.0-M01").flatMap(_.date) == None)
    }

    test("no undated heading names the file and what to add") {
      val datedOnly =
        """# Changelog
          |
          |## [Unreleased]
          |
          |## [0.5.0-M04] - 2026-07-14
          |""".stripMargin
      val result = Changelog.releaseLine(datedOnly, "morphir/desktop/CHANGELOG.md")
      assert(result.isLeft)
      assert(result.left.toOption.exists(_.contains("morphir/desktop/CHANGELOG.md")))
      assert(result.left.toOption.exists(_.contains("undated")))
    }

    test("two undated headings is an error naming both, never a first-match guess") {
      val ambiguous =
        """# Changelog
          |
          |## [0.7.0]
          |
          |## [0.6.0-M01]
          |
          |## [0.5.0-M04] - 2026-07-14
          |""".stripMargin
      val result = Changelog.releaseLine(ambiguous, "CHANGELOG.md")
      assert(result.isLeft)
      assert(result.left.toOption.exists(_.contains("0.7.0")))
      assert(result.left.toOption.exists(_.contains("0.6.0-M01")))
    }

    test("an undated heading that is not semver is an error quoting it") {
      val bad =
        """# Changelog
          |
          |## [next]
          |""".stripMargin
      val result = Changelog.releaseLine(bad, "CHANGELOG.md")
      assert(result.isLeft)
      assert(result.left.toOption.exists(_.contains("next")))
    }

    test("an empty changelog is an error, not an empty release line") {
      val result = Changelog.releaseLine("", "CHANGELOG.md")
      assert(result.isLeft)
    }

    test("headings without brackets are read too") {
      val plain =
        """# Changelog
          |
          |## 0.6.0-M01
          |
          |## 0.5.0 - 2026-01-01
          |""".stripMargin
      assert(Changelog.releaseLine(plain, "CHANGELOG.md") == Right("0.6.0-M01"))
    }
  }
}
