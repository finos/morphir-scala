package org.finos.morphir.mill

import java.nio.file.Paths

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

  private def resourceRoot: os.Path = {
    val buildFile = Option(getClass.getClassLoader.getResource("published-consumer/build.mill"))
      .getOrElse(throw new java.lang.AssertionError("Published consumer fixture is not a test resource"))
    os.Path(Paths.get(buildFile.toURI)) / os.up
  }

  private final case class PublishedInputs(repository: os.Path, millExecutable: os.Path) {
    val environment: Map[String, String] = Map(
      "MORPHIR_PUBLISHED_TEST_REPOSITORIES" -> repository.toString,
      "COURSIER_REPOSITORIES"               -> Seq(
        repository.toNIO.toUri.toASCIIString,
        sys.env.getOrElse("COURSIER_REPOSITORIES", "ivy2Local|central")
      ).mkString("|")
    )
  }

  private def publishedInputs: PublishedInputs = {
    val executable = Option(getClass.getClassLoader.getResource("mill-executable.jar"))
      .getOrElse(throw new java.lang.AssertionError("Published consumer inputs are not test-owned resources"))
    val root = os.Path(Paths.get(executable.toURI)) / os.up
    PublishedInputs(root / "repository", root / "mill-executable.jar")
  }

  private def requireSuccess(result: IntegrationTester.EvalResult): Unit =
    if (!result.isSuccess) throw new java.lang.AssertionError(result.debugString)

  private def cachedInputHash(tester: IntegrationTester, selector: String): Int =
    tester.out(selector).cached.inputsHash

  private def outputString(tester: IntegrationTester, selector: String, field: String): String =
    tester.out(selector).json(field).str

  private def pathRefLocation(value: String): String = value.split(":", 4).last

  private def evaluate(
      tester: IntegrationTester,
      inputs: PublishedInputs,
      selector: String
  ): IntegrationTester.EvalResult =
    tester.eval(selector, env = inputs.environment)

  private def exercise(daemonMode: Boolean): Unit = {
    val inputs = publishedInputs
    expectedArtifacts.foreach { artifact =>
      assert(
        os.exists(
          inputs.repository / "org" / "finos" / "morphir" / "mill" / artifact / expectedVersion /
            s"$artifact-$expectedVersion.jar"
        )
      )
    }

    val tester = IntegrationTester(
      daemonMode = daemonMode,
      workspaceSourcePath = resourceRoot,
      millExecutable = inputs.millExecutable
    )
    try {
      val initial = evaluate(tester, inputs, "scala-consumer.run")
      requireSuccess(initial)
      assert(initial.out.contains("published-consumer:"))
      assert(initial.out.contains(":true"))

      val isolation = evaluate(tester, inputs, "verifyPublishedIsolation")
      requireSuccess(isolation)
      assert(isolation.out.contains("published-artifacts-only"))

      val initialConsumerHash  = cachedInputHash(tester, "elm-consumer.morphirIR")
      val initialCompileHash   = cachedInputHash(tester, "scala-consumer.compile")
      val morphirCacheSentinel =
        tester.workspacePath / "out" / "elm-consumer" / "morphirIR.dest" / "evaluation-sentinel"
      os.write.over(morphirCacheSentinel, "cache-probe")
      val cachedRun = evaluate(tester, inputs, "scala-consumer.run")
      requireSuccess(cachedRun)
      assert(cachedInputHash(tester, "elm-consumer.morphirIR") == initialConsumerHash)
      assert(cachedInputHash(tester, "scala-consumer.compile") == initialCompileHash)
      assert(os.read(morphirCacheSentinel) == "cache-probe")
      assert(!cachedRun.err.contains("compiling"))

      tester.modifyFile(
        tester.workspacePath / "elm-consumer" / "src" / "Unpublished" / "Consumer" / "Main.elm",
        _.replace("unpublishedSourceValue + 1", "unpublishedSourceValue + 2")
      )
      requireSuccess(evaluate(tester, inputs, "scala-consumer.run"))
      assert(!os.exists(morphirCacheSentinel))
      val sourceConsumerHash = cachedInputHash(tester, "elm-consumer.morphirIR")
      assert(sourceConsumerHash != initialConsumerHash)
      assert(cachedInputHash(tester, "scala-consumer.compile") != initialCompileHash)

      tester.modifyFile(
        tester.workspacePath / "elm-consumer" / "morphir.json",
        _ + "\n"
      )
      requireSuccess(evaluate(tester, inputs, "elm-consumer.morphirIR"))
      val configConsumerHash = cachedInputHash(tester, "elm-consumer.morphirIR")
      assert(configConsumerHash != sourceConsumerHash)

      val dependencyHashBefore = cachedInputHash(tester, "elm-dependency.morphirIR")
      val dependencyPathBefore = outputString(tester, "elm-dependency.morphirIR", "path")
      val dependencyShaBefore  = outputString(tester, "elm-dependency.morphirIR", "sha256")
      tester.modifyFile(
        tester.workspacePath / "elm-dependency" / "src" / "Unpublished" / "Source" / "Dependency.elm",
        _.replace("=\n    41", "=\n    42")
      )
      requireSuccess(evaluate(tester, inputs, "scala-consumer.run"))
      assert(cachedInputHash(tester, "elm-dependency.morphirIR") != dependencyHashBefore)
      assert(
        pathRefLocation(outputString(tester, "elm-dependency.morphirIR", "path")) ==
          pathRefLocation(dependencyPathBefore)
      )
      assert(outputString(tester, "elm-dependency.morphirIR", "sha256") != dependencyShaBefore)
      assert(cachedInputHash(tester, "elm-consumer.morphirIR") != configConsumerHash)

      val lockHashBefore = cachedInputHash(tester, "packages.morphirElmInstall")
      tester.modifyFile(tester.workspacePath / "tool" / "package-lock.json", _ + "\n")
      requireSuccess(evaluate(tester, inputs, "packages.morphirElmInstall"))
      assert(cachedInputHash(tester, "packages.morphirElmInstall") != lockHashBefore)

      tester.modifyFile(tester.workspacePath / "node-version.txt", _ => "0.0.0\n")
      val invalidToolVersion = evaluate(tester, inputs, "elm-consumer.morphirIR")
      assert(!invalidToolVersion.isSuccess)
      assert(invalidToolVersion.err.contains("Unsupported Node version '0.0.0'"))
      tester.modifyFile(tester.workspacePath / "node-version.txt", _ => "24.19.0\n")
      requireSuccess(evaluate(tester, inputs, "scala-consumer.run"))
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
