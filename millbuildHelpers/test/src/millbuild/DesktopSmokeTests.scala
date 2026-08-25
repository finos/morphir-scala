package millbuild

import utest.*

object DesktopSmokeTests extends TestSuite {
  private val assertions = DesktopSmoke.expectedAssertions.iterator.map(_ -> true).toMap

  val tests = Tests {
    test("expected assertions are the desktop smoke contract") {
      assert(
        DesktopSmoke.expectedAssertions == Set(
          "clearedAfterFailure",
          "clearedAfterSessionSuccess",
          "clearedAfterSuccess",
          "disconnectedThroughButton",
          "mountedRenderer",
          "rememberFalseReadLive",
          "rememberReadLive",
          "rememberTrueReadLive",
          "removedStoredCredentialThroughButton",
          "rendererConsoleSentinelFree",
          "retainedOnFailure",
          "retainedOnSessionSuccess",
          "retainedOnSuccess",
          "safeConnectedStatus",
          "safeRejectedError",
          "safeSessionStatus",
          "submittedThroughForm",
          "transientDomSentinelFree"
        )
      )
    }

    test("results round trip through uPickle") {
      val result = DesktopSmoke.Result(assertions)
      assert(upickle.default.read[DesktopSmoke.Result](upickle.default.write(result)) == result)
    }

    test("result reads the flat JavaScript smoke result wire format") {
      val json = """{"mountedRenderer":true,"safeRejectedError":false}"""
      assert(
        upickle.default.read[DesktopSmoke.Result](json) == DesktopSmoke.Result(
          Map("mountedRenderer" -> true, "safeRejectedError" -> false)
        )
      )
    }

    test("result writes assertions as a flat JSON object") {
      val json = ujson.read(
        upickle.default.write(DesktopSmoke.Result(assertions))
      )

      assert(json.obj.keySet == DesktopSmoke.expectedAssertions)
      assert(json.obj.values.forall(_ == ujson.True))
      assert(!json.obj.contains("assertions"))
    }

    test("validate accepts exactly the expected successful assertions") {
      assert(DesktopSmoke.validate(DesktopSmoke.Result(assertions)) == Right(()))
    }

    test("validate reports missing and extra assertion keys deterministically") {
      val missing = DesktopSmoke.Result(assertions - "mountedRenderer")
      val extra   = DesktopSmoke.Result(assertions.updated("unexpectedAssertion", true))

      assert(
        DesktopSmoke.validate(missing) == Left(
          "desktop smoke assertion keys differ: missing [mountedRenderer]; extra []"
        )
      )
      assert(
        DesktopSmoke.validate(extra) == Left(
          "desktop smoke assertion keys differ: missing []; extra [unexpectedAssertion]"
        )
      )
    }

    test("validate reports the first failed assertion deterministically") {
      val failed = assertions.updated("safeSessionStatus", false).updated("clearedAfterFailure", false)
      assert(
        DesktopSmoke.validate(DesktopSmoke.Result(failed)) ==
          Left("desktop smoke assertion failed: clearedAfterFailure")
      )
    }

    test("redact replaces every sentinel occurrence and leaves harmless text alone") {
      assert(DesktopSmoke.redact("before-secret-middle-secret-after", "secret") ==
        "before-<redacted>-middle-<redacted>-after")
      assert(DesktopSmoke.redact("harmless", "secret") == "harmless")
    }

    test("artifacts retain their named paths") {
      val root      = os.temp.dir(prefix = "desktop-smoke-artifacts-", deleteOnExit = true)
      val artifacts = DesktopSmoke.Artifacts(
        screenshot = root / "screenshot.png",
        result = root / "result.json",
        processLog = root / "process.log",
        rendererLog = root / "renderer.log"
      )

      assert(artifacts.screenshot == root / "screenshot.png")
      assert(artifacts.result == root / "result.json")
      assert(artifacts.processLog == root / "process.log")
      assert(artifacts.rendererLog == root / "renderer.log")
    }

    test("safeRunRoot accepts an existing ordinary descendant") {
      val base      = os.temp.dir(prefix = "desktop-smoke-safe-base-", deleteOnExit = true)
      val candidate = base / "task-42" / "run"
      os.makeDir.all(candidate)

      val physicalCandidate = os.Path(candidate.toNIO.toRealPath())
      assert(DesktopSmoke.safeRunRoot(base, candidate) == Right(physicalCandidate))

      DesktopSmoke.safeRunRoot(base, candidate).foreach(os.remove.all)
      assert(!os.exists(candidate))
    }

    test("safeRunRoot rejects the base itself and lexical non-descendants") {
      val root    = os.temp.dir(prefix = "desktop-smoke-lexical-", deleteOnExit = true)
      val base    = root / "base"
      val sibling = root / "sibling"
      os.makeDir.all(base)
      os.makeDir.all(sibling)

      assert(DesktopSmoke.safeRunRoot(base, base).isLeft)
      assert(DesktopSmoke.safeRunRoot(base, sibling).isLeft)
    }

    test("safeRunRoot rejects a symbolic-link base") {
      val root     = os.temp.dir(prefix = "desktop-smoke-linked-base-", deleteOnExit = true)
      val realBase = root / "real-base"
      val linked   = root / "linked-base"
      os.makeDir.all(realBase / "run")
      java.nio.file.Files.createSymbolicLink(linked.toNIO, realBase.toNIO)

      assert(DesktopSmoke.safeRunRoot(linked, linked / "run").isLeft)
    }

    test("safeRunRoot rejects a symbolic-link candidate") {
      val root     = os.temp.dir(prefix = "desktop-smoke-linked-candidate-", deleteOnExit = true)
      val base     = root / "base"
      val external = root / "external"
      val linked   = base / "linked-run"
      os.makeDir.all(base)
      os.makeDir.all(external)
      java.nio.file.Files.createSymbolicLink(linked.toNIO, external.toNIO)

      assert(DesktopSmoke.safeRunRoot(base, linked).isLeft)
    }

    test("safeRunRoot rejects a symbolic-link traversal component") {
      val root     = os.temp.dir(prefix = "desktop-smoke-linked-component-", deleteOnExit = true)
      val base     = root / "base"
      val external = root / "external"
      val linked   = base / "linked"
      os.makeDir.all(base)
      os.makeDir.all(external / "run")
      java.nio.file.Files.createSymbolicLink(linked.toNIO, external.toNIO)

      assert(DesktopSmoke.safeRunRoot(base, linked / "run").isLeft)
    }

    test("physical containment rejects a resolved path outside the resolved base") {
      val root     = os.temp.dir(prefix = "desktop-smoke-physical-escape-", deleteOnExit = true)
      val base     = root / "base"
      val external = root / "external"
      os.makeDir.all(base)
      os.makeDir.all(external)

      assert(
        DesktopSmoke.validatePhysicalContainment(base.toNIO.toRealPath(), external.toNIO.toRealPath()) ==
          Left("desktop smoke run root must be physically contained by its base")
      )
    }
  }
}
