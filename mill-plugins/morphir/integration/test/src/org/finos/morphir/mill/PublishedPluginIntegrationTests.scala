package org.finos.morphir.mill

import mill.testkit.IntegrationTester
import utest.*

object PublishedPluginIntegrationTests extends TestSuite {
  private val expectedVersion   = "0.0.0-published-test-SNAPSHOT"
  private val expectedArtifacts = Seq(
    "mill-morphir-toolchain_mill1_3",
    "mill-morphir-javascript_mill1_3",
    "mill-morphir-elm-tooling_mill1_3",
    "mill-morphir-core_mill1_3",
    "mill-morphir-elm_mill1_3"
  )

  private def resourceRoot: os.Path =
    os.Path(sys.env("MILL_TEST_RESOURCE_DIR")) / "published-consumer"

  private def requireSuccess(result: IntegrationTester.EvalResult): Unit =
    if (!result.isSuccess) throw new java.lang.AssertionError(result.debugString)

  private def cachedInputHash(tester: IntegrationTester, selector: String): Int =
    tester.out(selector).cached.inputsHash

  private def outputString(tester: IntegrationTester, selector: String, field: String): String =
    tester.out(selector).json(field).str

  private def pathRefLocation(value: String): String = value.split(":", 4).last

  private def exercise(daemonMode: Boolean): Unit = {
    val repositories = sys
      .env("MORPHIR_PUBLISHED_TEST_REPOSITORIES")
      .split(java.io.File.pathSeparator)
      .toSeq
      .map(os.Path(_))
    assert(repositories.size == expectedArtifacts.size)
    repositories.zip(expectedArtifacts).foreach { case (repository, artifact) =>
      assert(
        os.exists(
          repository / "org" / "finos" / "morphir" / "mill" / artifact / expectedVersion /
            s"$artifact-$expectedVersion.jar"
        )
      )
    }

    val tester = IntegrationTester(
      daemonMode = daemonMode,
      workspaceSourcePath = resourceRoot,
      millExecutable = os.Path(sys.env("MILL_EXECUTABLE_PATH"))
    )
    try {
      val initial = tester.eval("scala-consumer.run")
      requireSuccess(initial)
      assert(initial.out.contains("published-consumer:"))
      assert(initial.out.contains(":true"))

      val isolation = tester.eval("verifyPublishedIsolation")
      requireSuccess(isolation)
      assert(isolation.out.contains("published-artifacts-only"))

      val initialConsumerHash = cachedInputHash(tester, "elm-consumer.morphirIR")
      val initialCompileHash  = cachedInputHash(tester, "scala-consumer.compile")
      val cachedRun           = tester.eval("scala-consumer.run")
      requireSuccess(cachedRun)
      assert(cachedInputHash(tester, "elm-consumer.morphirIR") == initialConsumerHash)
      assert(cachedInputHash(tester, "scala-consumer.compile") == initialCompileHash)
      assert(!cachedRun.err.contains("compiling"))

      tester.modifyFile(
        tester.workspacePath / "elm-consumer" / "src" / "Unpublished" / "Consumer" / "Main.elm",
        _.replace("unpublishedSourceValue + 1", "unpublishedSourceValue + 2")
      )
      requireSuccess(tester.eval("scala-consumer.run"))
      val sourceConsumerHash = cachedInputHash(tester, "elm-consumer.morphirIR")
      assert(sourceConsumerHash != initialConsumerHash)
      assert(cachedInputHash(tester, "scala-consumer.compile") != initialCompileHash)

      tester.modifyFile(
        tester.workspacePath / "elm-consumer" / "morphir.json",
        _ + "\n"
      )
      requireSuccess(tester.eval("elm-consumer.morphirIR"))
      val configConsumerHash = cachedInputHash(tester, "elm-consumer.morphirIR")
      assert(configConsumerHash != sourceConsumerHash)

      val dependencyHashBefore = cachedInputHash(tester, "elm-dependency.morphirIR")
      val dependencyPathBefore = outputString(tester, "elm-dependency.morphirIR", "path")
      val dependencyShaBefore  = outputString(tester, "elm-dependency.morphirIR", "sha256")
      tester.modifyFile(
        tester.workspacePath / "elm-dependency" / "src" / "Unpublished" / "Source" / "Dependency.elm",
        _.replace("=\n    41", "=\n    42")
      )
      requireSuccess(tester.eval("scala-consumer.run"))
      assert(cachedInputHash(tester, "elm-dependency.morphirIR") != dependencyHashBefore)
      assert(
        pathRefLocation(outputString(tester, "elm-dependency.morphirIR", "path")) ==
          pathRefLocation(dependencyPathBefore)
      )
      assert(outputString(tester, "elm-dependency.morphirIR", "sha256") != dependencyShaBefore)
      assert(cachedInputHash(tester, "elm-consumer.morphirIR") != configConsumerHash)

      val lockHashBefore = cachedInputHash(tester, "packages.morphirElmInstall")
      tester.modifyFile(tester.workspacePath / "tool" / "package-lock.json", _ + "\n")
      requireSuccess(tester.eval("packages.morphirElmInstall"))
      assert(cachedInputHash(tester, "packages.morphirElmInstall") != lockHashBefore)

      tester.modifyFile(tester.workspacePath / "node-version.txt", _ => "0.0.0\n")
      val invalidToolVersion = tester.eval("elm-consumer.morphirIR")
      assert(!invalidToolVersion.isSuccess)
      assert(invalidToolVersion.err.contains("Unsupported Node version '0.0.0'"))
      tester.modifyFile(tester.workspacePath / "node-version.txt", _ => "24.19.0\n")
      requireSuccess(tester.eval("scala-consumer.run"))
    } finally tester.close()
  }

  val tests = Tests {
    test("daemon resolves published plugins and preserves invalidation") {
      exercise(daemonMode = true)
    }

    test("clean daemon resolves published plugins and preserves invalidation") {
      exercise(daemonMode = false)
    }
  }
}
