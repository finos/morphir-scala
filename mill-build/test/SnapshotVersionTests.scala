//| moduleDeps: ["//mill-build/src/millbuild/SnapshotVersion.scala"]

import mill.util.VcsVersion
import millbuild.SnapshotVersion

def assertEquals[A](actual: A, expected: A): Unit =
  assert(actual == expected, s"Expected $expected, got $actual")

def assertLeft(result: Either[String, String], clue: String): Unit =
  assert(result.isLeft, s"Expected failure for $clue, got $result")

def state(
    tag: Option[String] = Some("v0.5.0-M04"),
    distance: Int = 57,
    revision: String = "bd4cd2cc1234567890abcdef1234567890abcdef",
    dirtyHash: Option[String] = None
): VcsVersion.State =
  VcsVersion.State(revision, tag, distance, dirtyHash, Some(VcsVersion.git))

@main def runSnapshotVersionTests(): Unit =
  assertEquals(
    SnapshotVersion.format(state(), "develop"),
    Right("0.5.0-M04-develop.57.gbd4cd2-SNAPSHOT")
  )

  assertEquals(
    SnapshotVersion.format(state(), "main"),
    Right("0.5.0-M04-57-SNAPSHOT")
  )
  assertEquals(
    SnapshotVersion.format(state(tag = Some("v0.5.0")), "main"),
    Right("0.5.0-57-SNAPSHOT")
  )
  assertEquals(
    SnapshotVersion.format(state(tag = None), "main"),
    Right("0.0.0-57-SNAPSHOT")
  )
  assertEquals(
    SnapshotVersion.format(state(distance = 0), "MAIN"),
    Right("0.5.0-M04-0-SNAPSHOT")
  )
  assertEquals(
    SnapshotVersion.select(
      state(),
      Map("MORPHIR_PUBLISH_MODE" -> "snapshot", "MORPHIR_PUBLISH_BRANCH" -> "main")
    ),
    Right("0.5.0-M04-57-SNAPSHOT")
  )

  assertEquals(
    SnapshotVersion.format(state(tag = Some("v0.5.0")), "develop"),
    Right("0.5.0-develop.57.gbd4cd2-SNAPSHOT")
  )
  assertEquals(
    SnapshotVersion.format(state(tag = Some("v0.5.0-alpha.1")), "develop"),
    Right("0.5.0-alpha.1-develop.57.gbd4cd2-SNAPSHOT")
  )
  assertEquals(
    SnapshotVersion.format(state(tag = None), "develop"),
    Right("0.0.0-develop.57.gbd4cd2-SNAPSHOT")
  )
  assertEquals(
    SnapshotVersion.format(state(distance = 0), "develop"),
    Right("0.5.0-M04-develop.0.gbd4cd2-SNAPSHOT")
  )
  assertEquals(
    SnapshotVersion.format(state(), "Feature///HELLO__World--"),
    Right("0.5.0-M04-feature.hello.world.57.gbd4cd2-SNAPSHOT")
  )
  assertEquals(
    SnapshotVersion.format(state(revision = "ABCDEF012345678901234567890123456789ABCD"), "DEVELOP"),
    Right("0.5.0-M04-develop.57.gabcdef-SNAPSHOT")
  )
  assertEquals(
    SnapshotVersion.format(state(revision = "1234567"), "develop"),
    Right("0.5.0-M04-develop.57.g123456-SNAPSHOT")
  )

  assertLeft(SnapshotVersion.format(state(), "---"), "an empty normalized branch")
  assertLeft(SnapshotVersion.format(state(revision = "123456"), "develop"), "a short revision")
  assertLeft(SnapshotVersion.format(state(revision = "123456g"), "develop"), "a nonhex revision")
  assertLeft(
    SnapshotVersion.format(state(revision = "12345678901234567890123456789012345678901"), "develop"),
    "an overlong revision"
  )
  assertLeft(SnapshotVersion.format(state(dirtyHash = Some("dirty")), "develop"), "a dirty state")
  assertLeft(SnapshotVersion.format(state(distance = -1), "develop"), "negative distance")
  assertLeft(SnapshotVersion.format(state(tag = Some("release-five")), "develop"), "a non-version tag")
  Seq(
    "v01.2.3",
    "v1.02.3",
    "v1.2.03",
    "v0.5.0-alpha.",
    "v0.5.0-alpha..beta",
    "v0.5.0-01"
  ).foreach { tag =>
    assertLeft(SnapshotVersion.format(state(tag = Some(tag)), "develop"), s"invalid SemVer tag $tag")
  }

  assertEquals(SnapshotVersion.select(state(), Map.empty), Right(state().format()))
  assertEquals(
    SnapshotVersion.select(state(), Map("MORPHIR_PUBLISH_MODE" -> "")),
    Right(state().format())
  )
  assertEquals(
    SnapshotVersion.select(
      state(),
      Map("MORPHIR_PUBLISH_MODE" -> "snapshot", "MORPHIR_PUBLISH_BRANCH" -> "Develop")
    ),
    Right("0.5.0-M04-develop.57.gbd4cd2-SNAPSHOT")
  )
  assertLeft(
    SnapshotVersion.select(state(), Map("MORPHIR_PUBLISH_MODE" -> "snapshot")),
    "snapshot mode without a branch"
  )
  assertLeft(
    SnapshotVersion.select(
      state(),
      Map("MORPHIR_PUBLISH_MODE" -> "snapshot", "MORPHIR_PUBLISH_BRANCH" -> "")
    ),
    "snapshot mode with an empty branch"
  )
  assertLeft(
    SnapshotVersion.select(state(), Map("MORPHIR_PUBLISH_MODE" -> "release")),
    "an unknown publish mode"
  )
