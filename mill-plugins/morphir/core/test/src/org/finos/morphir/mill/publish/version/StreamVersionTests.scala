package org.finos.morphir.mill.publish.version

import utest.*

object StreamVersionTests extends TestSuite {
  private val clean   = GitState(Some("v0.5.0-M04"), 12, "abc123def", dirty = false)
  private val stream  = TagStream(None)
  private val desktop = TagStream(Some("desktop"))

  val tests = Tests {
    test("main snapshots carry the distance") {
      assert(
        StreamVersion.compose("0.6.0-M01", None, clean, PublishMode.Snapshot("main"), stream) ==
          Right("0.6.0-M01-12-SNAPSHOT")
      )
    }

    test("other branches carry branch, distance and revision") {
      assert(
        StreamVersion.compose("0.6.0-M01", None, clean, PublishMode.Snapshot("develop"), stream) ==
          Right("0.6.0-M01-develop.12.gabc123-SNAPSHOT")
      )
    }

    test("a branch name is normalised") {
      assert(
        StreamVersion.compose("0.6.0", None, clean, PublishMode.Snapshot("feature/Big_Thing"), stream) ==
          Right("0.6.0-feature.big.thing.12.gabc123-SNAPSHOT")
      )
    }

    test("a release is the release line alone") {
      val onTag = GitState(Some("v0.6.0-M01"), 0, "abc123def", dirty = false)
      assert(StreamVersion.compose("0.6.0-M01", None, onTag, PublishMode.Release, stream) == Right("0.6.0-M01"))
    }

    test("a release whose tag disagrees with the changelog fails naming both") {
      val onWrongTag = GitState(Some("v0.6.0-M02"), 0, "abc123def", dirty = false)
      val result     = StreamVersion.compose("0.6.0-M01", None, onWrongTag, PublishMode.Release, stream)
      assert(result.isLeft)
      assert(result.left.toOption.exists(_.contains("v0.6.0-M02")))
      assert(result.left.toOption.exists(_.contains("0.6.0-M01")))
    }

    test("a release checks the tag of its own stream") {
      val onDesktopTag = GitState(Some("desktop/v0.3.0"), 0, "abc123def", dirty = false)
      assert(
        StreamVersion.compose("0.3.0", None, onDesktopTag, PublishMode.Release, desktop) == Right("0.3.0")
      )
    }

    test("a dirty tree refuses an explicit snapshot publish") {
      val messy  = clean.copy(dirty = true)
      val result =
        StreamVersion.compose("0.6.0-M01", None, messy, PublishMode.Snapshot("main"), stream, explicitPublish = true)
      assert(result.isLeft)
      assert(result.left.toOption.exists(_.contains("dirty")))
    }

    test("a dirty tree refuses a release even when not marked as an explicit publish") {
      val messyOnTag = GitState(Some("v0.6.0-M01"), 0, "abc123def", dirty = true)
      val result     = StreamVersion.compose("0.6.0-M01", None, messyOnTag, PublishMode.Release, stream)
      assert(result.isLeft)
      assert(result.left.toOption.exists(_.contains("dirty")))
    }

    test("a dirty tree with no publish mode produces a version marked dirty, not an error") {
      val messy  = clean.copy(dirty = true)
      val result = StreamVersion.compose("0.6.0-M01", None, messy, PublishMode.Snapshot("main"), stream)
      assert(result == Right(s"0.6.0-M01-${messy.distance}-SNAPSHOT-DIRTY${messy.revision.take(8)}"))
    }

    test("a release line below the starting version fails quoting both") {
      val result =
        StreamVersion.compose("0.4.0", Some("0.5.0"), clean, PublishMode.Snapshot("main"), stream)
      assert(result.isLeft)
      assert(result.left.toOption.exists(_.contains("0.4.0")))
      assert(result.left.toOption.exists(_.contains("0.5.0")))
    }

    test("a release line equal to the starting version is allowed") {
      assert(
        StreamVersion.compose("0.5.0", Some("0.5.0"), clean, PublishMode.Snapshot("main"), stream) ==
          Right("0.5.0-12-SNAPSHOT")
      )
    }

    test("an area with no tags yet still produces a snapshot") {
      val untagged = GitState(None, 7, "abc123def", dirty = false)
      assert(
        StreamVersion.compose("0.1.0", None, untagged, PublishMode.Snapshot("develop"), desktop) ==
          Right("0.1.0-develop.7.gabc123-SNAPSHOT")
      )
    }

    test("a revision that is too short fails, naming it") {
      val shortRevision = clean.copy(revision = "abc12")
      val result        = StreamVersion.compose("0.6.0-M01", None, shortRevision, PublishMode.Snapshot("main"), stream)
      assert(result.isLeft)
      assert(result.left.toOption.exists(_.contains("abc12")))
    }

    test("a malformed revision is rejected regardless of mode") {
      // The revision is only rendered on the non-main snapshot path, but the guard runs before the
      // mode is inspected — the same way SnapshotVersion.format validates the revision ahead of
      // branching. A release build must refuse a bad revision even though it never prints one.
      val onTag  = GitState(Some("v0.6.0-M01"), 0, "ab", dirty = false)
      val result = StreamVersion.compose("0.6.0-M01", None, onTag, PublishMode.Release, stream)
      assert(result.isLeft)
      assert(result.left.toOption.exists(_.contains("ab")))
    }

    test("a branch name that normalises to empty fails") {
      val result = StreamVersion.compose("0.6.0-M01", None, clean, PublishMode.Snapshot("///"), stream)
      assert(result.isLeft)
      assert(result.left.toOption.exists(_.contains("empty")))
    }

    test("a negative distance fails") {
      val result =
        StreamVersion.compose("0.6.0-M01", None, clean.copy(distance = -1), PublishMode.Snapshot("main"), stream)
      assert(result.isLeft)
    }

    test("resolveMode: MORPHIR_PUBLISH_MODE=snapshot with a branch stays explicit and unchanged") {
      val onTag = GitState(Some("v0.6.0-M01"), 0, "abc123def", dirty = false)
      val env   = Map("MORPHIR_PUBLISH_MODE" -> "snapshot", "MORPHIR_PUBLISH_BRANCH" -> "develop")
      assert(
        StreamVersion.resolveMode(env, onTag, stream, "0.6.0-M01", "main") ==
          Right(PublishMode.Snapshot("develop"))
      )
    }

    test("resolveMode: unset, distance zero, tag matches the release line -> Release") {
      val onTag = GitState(Some("v0.6.0-M01"), 0, "abc123def", dirty = false)
      assert(
        StreamVersion.resolveMode(Map.empty, onTag, stream, "0.6.0-M01", "main") == Right(PublishMode.Release)
      )
    }

    test("resolveMode: unset, distance zero, same-namespace tag disagrees with the release line -> error") {
      val onWrongTag = GitState(Some("v0.6.0-M02"), 0, "abc123def", dirty = false)
      val result     = StreamVersion.resolveMode(Map.empty, onWrongTag, stream, "0.6.0-M01", "main")
      assert(result.isLeft)
      assert(result.left.toOption.exists(_.contains("v0.6.0-M02")))
      assert(result.left.toOption.exists(_.contains("0.6.0-M01")))
    }

    test("resolveMode: unset, distance zero, a tag from another stream's namespace -> Snapshot, not an error") {
      // A desktop/v* tag sitting at HEAD must never block or misjudge the library stream's own
      // resolution: it belongs to a different namespace entirely, so it carries no opinion here.
      val onDesktopTag = GitState(Some("desktop/v0.3.0"), 0, "abc123def", dirty = false)
      assert(
        StreamVersion.resolveMode(Map.empty, onDesktopTag, stream, "0.6.0-M01", "main") ==
          Right(PublishMode.Snapshot("main"))
      )
    }

    test("resolveMode: unset, distance greater than zero -> Snapshot even though the tag matches") {
      val aheadOfTag = GitState(Some("v0.6.0-M01"), 3, "abc123def", dirty = false)
      assert(
        StreamVersion.resolveMode(Map.empty, aheadOfTag, stream, "0.6.0-M01", "develop") ==
          Right(PublishMode.Snapshot("develop"))
      )
    }

    test("resolveMode: unset, no tag in this stream at all -> Snapshot") {
      // distance 0 is deliberate: it is the value that would otherwise allow Release, so the only
      // thing forcing Snapshot here is the absent tag, not a nonzero distance.
      val untagged = GitState(None, 0, "abc123def", dirty = false)
      assert(
        StreamVersion.resolveMode(Map.empty, untagged, stream, "0.6.0-M01", "main") ==
          Right(PublishMode.Snapshot("main"))
      )
    }

    test("resolveMode: an unsupported MORPHIR_PUBLISH_MODE value is still an error") {
      val onTag  = GitState(Some("v0.6.0-M01"), 0, "abc123def", dirty = false)
      val result = StreamVersion.resolveMode(Map("MORPHIR_PUBLISH_MODE" -> "bogus"), onTag, stream, "0.6.0-M01", "main")
      assert(result.isLeft)
      assert(result.left.toOption.exists(_.contains("bogus")))
    }
  }
}
