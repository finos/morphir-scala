package org.finos.morphir.mill.publish.version

import utest.*

/**
 * Asserts the originals against the shared corpus in `.config/version-rules`.
 *
 * These rules are implemented twice: here, and as a port inside `.claude/skills/squire`, because a Mill script cannot
 * depend on this module. `SquireChangelogSpec` asserts the port against the same corpus, so a rule that changes on one
 * side alone fails a test on the other. See the corpus README for why the duplication is forced.
 *
 * A case belongs in the corpus rather than here whenever both implementations must agree on it. `SemVerTests`,
 * `ChangelogTests` and `TagStreamTests` keep the cases that exercise this side alone.
 */
object VersionCorpusTests extends TestSuite {
  private val corpus = VersionCorpus.load()

  val tests = Tests {
    test("parses every version the corpus accepts") {
      corpus.parseCases.foreach { expected =>
        val parsed = SemVer.parse(expected.text)
        assert(parsed == Some(SemVer(expected.major, expected.minor, expected.patch, expected.prerelease)))
      }
    }

    test("rejects every version the corpus rejects") {
      // Collected rather than asserted one at a time so a failure names every input that slipped through, each
      // carrying the rule it was meant to exercise.
      val accepted = corpus.rejectCases.filter(rejected => SemVer.parse(rejected.text).isDefined)
      assert(accepted.isEmpty)
    }

    test("orders every pair the way the corpus orders it") {
      corpus.compareCases.foreach { expected =>
        assert(SemVer.compare(expected.left, expected.right) == Right(expected.sign))
      }
    }

    test("fails a comparison naming the value that is not a version") {
      corpus.compareRejectCases.foreach { expected =>
        val result = SemVer.compare(expected.left, expected.right)
        assert(result.isLeft)
        assert(result.left.toOption.exists(_.contains(expected.messageContains)))
      }
    }

    test("derives the pattern, tag and version of every stream") {
      corpus.streamCases.foreach { expected =>
        val stream = TagStream(expected.namespace)
        assert(stream.pattern == expected.pattern)
        expected.tagFor.foreach((version, tag) => assert(stream.tagFor(version) == tag))
        expected.versionFromTag.foreach((tag, version) => assert(stream.versionFromTag(tag) == version))
      }
    }

    test("reads the release line out of every changelog the corpus accepts") {
      corpus.releaseLineCases.foreach { expected =>
        assert(Changelog.releaseLine(expected.text, expected.source) == Right(expected.releaseLine))
      }
    }

    test("fails with the exact message the corpus records") {
      corpus.releaseLineRejectCases.foreach { expected =>
        assert(Changelog.releaseLine(expected.text, expected.source) == Left(expected.message))
      }
    }

    test("lists every version heading in document order") {
      corpus.headingCases.foreach { expected =>
        val headings = Changelog.headings(expected.text).toList
        assert(headings == expected.headings.map(heading => ChangelogHeading(heading.version, heading.date)))
      }
    }
  }
}
