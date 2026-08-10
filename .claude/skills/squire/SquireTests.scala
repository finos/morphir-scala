//| scalaVersion: 3.8.4
//| mainClass: kyo.test.runner.Cli
//| resources: [test-resources]
//| moduleDeps: [squire.scala, SquireCellar.scala, SquireRepo.scala, SquireTracking.scala]
//| mvnDeps:
//| - io.getkyo::kyo-test-api:1.0.0-RC6
//| - io.getkyo::kyo-test-runner:1.0.0-RC6

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import caseapp.core.parser.Parser
import kyo.*
import kyo.test.*
import scala.jdk.CollectionConverters.*

final case class LaunchedSquire(exitCode: Int, stdout: String, stderr: String)

final case class TaskInvocation(program: String, arguments: Chunk[String])

object SquireLauncherFixtures:
  def run(
      skillDirectory: java.nio.file.Path,
      arguments: Seq[String],
      environment: Map[String, String] = Map.empty,
      launcher: String = "squire"
  ): LaunchedSquire =
    val command =
      if launcher.endsWith(".bat") then Seq("cmd", "/d", "/c", skillDirectory.resolve(launcher).toString)
      else Seq(skillDirectory.resolve(launcher).toString)
    val builder = new ProcessBuilder((command ++ arguments)*)
      .directory(skillDirectory.toFile)
    builder.environment().putAll(environment.asJava)
    val process = builder.start()
    val stdout  = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val stderr  = new String(process.getErrorStream.readAllBytes(), StandardCharsets.UTF_8)
    LaunchedSquire(process.waitFor(), stdout, stderr)

  def executable(path: java.nio.file.Path, text: String): Unit =
    Files.writeString(path, text, StandardCharsets.UTF_8)
    if !path.toFile.setExecutable(true) then throw new AssertionError(s"could not make $path executable")

object SquireCiPolicy:
  val SupportedBranches = List("main", "0.4.x", "develop")
  val PublishPredicate  =
    "github.repository == 'finos/morphir-scala' && " +
      "(github.ref == 'refs/heads/main' || " +
      "github.ref == 'refs/heads/0.4.x' || " +
      "github.ref == 'refs/heads/develop' || " +
      "startsWith(github.ref, 'refs/tags/'))"
  val CachePredicate =
    "github.ref == 'refs/heads/main' || " +
      "github.ref == 'refs/heads/0.4.x' || " +
      "github.ref == 'refs/heads/develop' || " +
      "startsWith(github.ref, 'refs/tags/')"
  val SnapshotCommands = List(
    "echo \"MORPHIR_PUBLISH_MODE=snapshot\" >> \"$GITHUB_ENV\"",
    "echo \"MORPHIR_PUBLISH_BRANCH=${GITHUB_REF_NAME}\" >> \"$GITHUB_ENV\""
  )

  private def fail(message: String): Nothing = throw new AssertionError(message)

  private def expect(condition: Boolean, message: String): Unit =
    if !condition then fail(message)

  private def leadingSpaces(line: String): Int = line.length - line.dropWhile(_ == ' ').length

  def indentedBlock(text: String, header: String, indent: Int): String =
    val lines          = text.linesIterator.toList
    val expectedHeader = " " * indent + header
    val start          = lines.indexWhere(_ == expectedHeader)
    if start < 0 then fail(s"missing block: $header")
    lines.drop(start + 1).takeWhile(line => line.trim.isEmpty || leadingSpaces(line) > indent).mkString("\n")

  def inlineList(block: String, key: String): List[String] =
    val value = block.linesIterator.map(_.trim).find(_.startsWith(s"$key:")).map(_.drop(key.length + 1).trim)
      .getOrElse(fail(s"missing inline list: $key"))
    expect(value.startsWith("[") && value.endsWith("]"), s"invalid inline list: $key")
    value.drop(1).dropRight(1).split(',').iterator.map(_.trim.stripPrefix("\"").stripSuffix("\"")
      .stripPrefix("'").stripSuffix("'")).filter(_.nonEmpty).toList

  def scalar(block: String, key: String): String =
    block.linesIterator.map(_.trim).find(_.startsWith(s"$key:")).map(_.drop(key.length + 1).trim)
      .getOrElse(fail(s"missing scalar: $key"))

  def normalizeExpression(expression: String): String = expression.split("\\s+").filter(_.nonEmpty).mkString(" ")

  def count(text: String, needle: String): Int =
    Iterator.iterate(text.indexOf(needle)) { index =>
      if index < 0 then -1 else text.indexOf(needle, index + needle.length)
    }.takeWhile(_ >= 0).size

  private val YamlKey = """^\s*(?:\"([^\"]+)\"|'([^']+)'|([A-Za-z][A-Za-z0-9_-]*))\s*:.*$""".r

  def hasYamlKey(block: String, key: String): Boolean =
    block.linesIterator.exists {
      case YamlKey(doubleQuoted, singleQuoted, bare) => List(doubleQuoted, singleQuoted, bare).contains(key)
      case _                                         => false
    }

  def yamlSequenceEntries(block: String, indent: Int): List[String] =
    block.linesIterator.filter { line =>
      val item = line.drop(indent)
      leadingSpaces(line) == indent && (item == "-" || item.startsWith("- "))
    }.toList

  def replaceOnce(text: String, oldValue: String, newValue: String): String =
    val index = text.indexOf(oldValue)
    if index < 0 then fail(s"mutation target not found: $oldValue")
    text.substring(0, index) + newValue + text.substring(index + oldValue.length)

  def publishBlock(workflow: String): String =
    expect(workflow.linesIterator.count(_ == "  publish:") == 1, "workflow must contain exactly one publish job")
    indentedBlock(workflow, "publish:", 2)

  def assertBranchPolicy(workflow: String): Unit =
    val events = indentedBlock(workflow, "on:", 0)
    List("pull_request:", "push:").foreach { event =>
      val branches = inlineList(indentedBlock(events, event, 2), "branches")
      expect(branches == SupportedBranches, s"$event branches were $branches")
    }

  def assertPublishPolicy(workflow: String): Unit =
    val publish = publishBlock(workflow)
    expect(scalar(publish, "needs") == "[ci]", "publish must depend only on aggregate ci")
    expect(scalar(publish, "if") == PublishPredicate, "publish predicate does not match the release allowlist")
    expect(
      publish.linesIterator.count(_ == "      - name: Release") == 1,
      "publish job must contain exactly one Release step"
    )
    val release = indentedBlock(publish, "- name: Release", 6)
    expect(
      count(release, "mise run publish:sonatype") == 1,
      "Release step must contain the Sonatype publish invocation"
    )
    expect(
      count(workflow, "mise run publish:sonatype") == 1,
      "workflow must contain exactly one Sonatype publish invocation"
    )

  def assertSnapshotPolicy(workflow: String): Unit =
    val publish  = publishBlock(workflow)
    val snapshot = indentedBlock(publish, "- name: Configure develop snapshot version", 6)
    expect(
      scalar(snapshot, "if") == "github.ref == 'refs/heads/develop'",
      "snapshot step must run only on develop"
    )
    val lines    = snapshot.linesIterator.toList
    val runIndex = lines.indexWhere(_.trim == "run: |")
    if runIndex < 0 then fail("snapshot step must have a literal run block")
    val commands = lines.drop(runIndex + 1).takeWhile(line => line.trim.isEmpty || leadingSpaces(line) > 8)
      .map(_.drop(10))
    expect(commands == SnapshotCommands, s"unexpected snapshot commands: $commands")
    expect(
      publish.indexOf("- name: Configure develop snapshot version") < publish.indexOf("- name: Release"),
      "snapshot configuration must precede Release"
    )
    List("MORPHIR_PUBLISH_MODE=snapshot", "MORPHIR_PUBLISH_BRANCH=${GITHUB_REF_NAME}").foreach { assignment =>
      expect(count(workflow, assignment) == 1, s"snapshot assignment must occur exactly once: $assignment")
    }

  def assertCachePolicy(workflow: String): Unit =
    List("test-js:" -> "Cache JS build output", "test-jvm:" -> "Cache JVM build output").foreach {
      case (jobName, stepName) =>
        val job       = indentedBlock(workflow, jobName, 2)
        val step      = indentedBlock(job, s"- name: $stepName", 6)
        val condition = scalar(step, "if")
        expect(
          normalizeExpression(condition) == normalizeExpression(CachePredicate),
          s"$stepName has an unapproved condition: $condition"
        )
    }

  def assertReadOnlyPermissions(workflow: String): Unit =
    val permissions = indentedBlock(workflow, "permissions:", 0)
    expect(
      permissions.linesIterator.filter(_.trim.nonEmpty).toList == List("  contents: read"),
      "workflow permissions must be exactly contents: read"
    )

  def assertMorphirCapabilityPolicy(workflow: String): Unit =
    val commands = List(
      "mill-morphir-unit:" -> "'mill-plugins.morphir.{toolchain,javascript,elm-tooling,core,elm}.__.test'",
      "mill-morphir-integration:" -> "mill-plugins.morphir.integration.test",
      "morphir-elm-projects:" -> "examples.morphir-elm-projects.__.morphirIR",
      "runtime-generated-fixtures:" -> "morphir.runtime.classic.jvm.test.generatedRuntimeFixtures",
      "runtime-tests:" -> "morphir.runtime.classic.jvm.test.verifyRuntimeTestDiscovery"
    )
    commands.foreach { case (job, command) =>
      val block = indentedBlock(workflow, job, 2)
      expect(block.contains(command), s"$job must run $command")
    }
    val unit = indentedBlock(workflow, "mill-morphir-unit:", 2)
    expect(!unit.contains("mill-plugins.morphir.integration"), "unit selector must exclude integration")
    List(
      "mill-morphir-integration:" -> "[mill-morphir-unit]",
      "morphir-elm-projects:" -> "[mill-morphir-unit]",
      "runtime-generated-fixtures:" -> "[morphir-elm-projects]",
      "runtime-tests:" -> "[runtime-generated-fixtures]"
    ).foreach { case (job, dependency) =>
      expect(
        scalar(indentedBlock(workflow, job, 2), "needs") == dependency,
        s"$job must depend on $dependency"
      )
    }

  def assertJvmPlatformPolicy(workflow: String, buildMill: String, task: String): Unit =
    val testJvm = indentedBlock(workflow, "test-jvm:", 2)
    val runJvm  = indentedBlock(testJvm, "- name: Run JVM tests", 6)
    expect(scalar(runJvm, "run") == "mise run test:jvm-platform", "generic JVM CI must use test:jvm-platform")
    expect(
      task.linesIterator.map(_.trim).contains("./mill -i Alias/run testJVMPlatform"),
      "test:jvm-platform must invoke Alias/run testJVMPlatform"
    )
    val expectedMembers = List(
      "morphir.jvm.__.compile",
      "morphir.{contrib.knowledge,extensibility,intelligence.sdk,interop.borer,interop.zio.json,kit.kyo,langkit.core,langkit.elm.compiler.api,langkit.elm.core,langkit.trees,lib.interop,model,model.lowering,naming,testing.generators,testing.zio,tests,tools}.jvm.__.compile",
      "morphir.jvm.publishArtifacts",
      "morphir.{contrib.knowledge,extensibility,interop.borer,interop.zio.json,lib.interop,model,model.lowering,naming,tests,tools}.jvm.publishArtifacts",
      "morphir.{contrib.knowledge,intelligence.sdk,interop.borer,interop.zio.json,kit.kyo,langkit.core,langkit.elm.compiler.api,langkit.elm.core,langkit.trees,model,model.lowering,tests}.jvm.test",
      "morphir.langkit.itest.testCached"
    )
    val definitions = "(?m)^\\s*def testJVMPlatform\\b".r.findAllMatchIn(buildMill).size
    expect(definitions == 1, s"build must provide exactly one testJVMPlatform alias, found $definitions")
    val aliasPattern = "(?ms)^\\s*def testJVMPlatform\\s*=\\s*alias\\(\\s*\\n(.*?)^\\s*\\)\\s*$".r
    val aliasBody = aliasPattern.findFirstMatchIn(buildMill).map(_.group(1))
      .getOrElse(fail("testJVMPlatform must have a parseable alias body"))
    val memberPattern = "^\\s*\"([^\"]+)\"(,?)\\s*$".r
    val lines         = aliasBody.linesIterator.toList
    val members = lines.zipWithIndex.map { case (line, index) =>
      line match
        case memberPattern(member, comma) =>
          val expectedComma = if index < lines.size - 1 then "," else ""
          expect(comma == expectedComma, "testJVMPlatform members must use canonical separators")
          member
        case _ => fail("testJVMPlatform contains a non-literal member")
    }
    expect(members == expectedMembers, s"unexpected testJVMPlatform members: $members")

  def assertJvmTargetInventory(inventory: Map[String, Set[String]]): Unit =
    val classicPrefix = "morphir.runtime.classic.jvm"
    val compileSelectors = List(
      "morphir.jvm.__.compile",
      "morphir.{contrib.knowledge,extensibility,intelligence.sdk,interop.borer,interop.zio.json,kit.kyo,langkit.core,langkit.elm.compiler.api,langkit.elm.core,langkit.trees,lib.interop,model,model.lowering,naming,testing.generators,testing.zio,tests,tools}.jvm.__.compile"
    )
    val publishSelectors = List(
      "morphir.jvm.publishArtifacts",
      "morphir.{contrib.knowledge,extensibility,interop.borer,interop.zio.json,lib.interop,model,model.lowering,naming,tests,tools}.jvm.publishArtifacts"
    )
    val testSelectors = List(
      "morphir.{contrib.knowledge,intelligence.sdk,interop.borer,interop.zio.json,kit.kyo,langkit.core,langkit.elm.compiler.api,langkit.elm.core,langkit.trees,model,model.lowering,tests}.jvm.test"
    )

    def targets(selector: String): Set[String] =
      inventory.getOrElse(selector, fail(s"missing resolved JVM selector: $selector"))

    def selected(selectors: List[String]): Set[String] = selectors.iterator.flatMap(targets).toSet

    def assertParity(kind: String, broadSelector: String, selectors: List[String]): Unit =
      val broad    = targets(broadSelector)
      val expected = broad.filterNot(_.startsWith(classicPrefix))
      val actual   = selected(selectors)
      expect(actual == expected, s"JVM $kind selectors differ from current non-classic targets")
      expect(!actual.exists(_.startsWith(classicPrefix)), s"JVM $kind selectors must exclude classic runtime")

    assertParity("compile", "morphir.__.jvm.__.compile", compileSelectors)
    assertParity("publish", "morphir.__.jvm.publishArtifacts", publishSelectors)
    assertParity("test", "morphir.__.jvm.__.test", testSelectors)
    expect(
      targets("morphir.__.jvm.__.test").contains("morphir.runtime.classic.jvm.test"),
      "broad JVM test inventory must include the separately gated classic runtime target"
    )

  def assertMorphirCachePolicy(workflow: String): Unit =
    val pluginOutputs = List(
      "out/mill-plugins/morphir/",
      "!out/mill-plugins/morphir/**/testForked.dest/**",
      "!out/mill-plugins/morphir/**/testOnly.dest/**"
    )
    val generatedOutputs = List("out/examples/morphir-elm-projects/", "out/morphir-elm/")
    val runtimeOutputs   = List("out/morphir/runtime/classic/jvm/test/")
    val expected = List(
      "mill-morphir-unit:" -> List(
        "Cache verified Morphir tool downloads" -> List("~/.cache/morphir-scala"),
        "Cache Mill capability outputs"         -> pluginOutputs
      ),
      "mill-morphir-integration:" -> List(
        "Cache verified Morphir tool downloads" -> List("~/.cache/morphir-scala"),
        "Cache Mill capability outputs"         -> pluginOutputs
      ),
      "morphir-elm-projects:" -> List(
        "Cache verified Morphir tool downloads" -> List("~/.cache/morphir-scala"),
        "Restore Mill plugin outputs"           -> List("out/mill-plugins/morphir/"),
        "Cache Mill capability outputs"         -> generatedOutputs
      ),
      "runtime-generated-fixtures:" -> List(
        "Cache verified Morphir tool downloads" -> List("~/.cache/morphir-scala"),
        "Restore generated Morphir IR outputs"  -> generatedOutputs,
        "Cache Mill capability outputs"         -> runtimeOutputs
      ),
      "runtime-tests:" -> List(
        "Cache verified Morphir tool downloads" -> List("~/.cache/morphir-scala"),
        "Restore generated Morphir IR outputs"  -> generatedOutputs,
        "Cache Mill capability outputs"         -> runtimeOutputs
      )
    )
    expected.foreach { case (jobName, expectedSteps) =>
      val job         = indentedBlock(workflow, jobName, 2)
      val actualSteps = actionCacheSteps(job)
      expect(actualSteps == expectedSteps, s"$jobName cache paths were $actualSteps")
    }

  private def actionCacheSteps(job: String): List[(String, List[String])] =
    val lines      = job.linesIterator.toList
    val stepStarts = lines.indices.filter(index => lines(index).startsWith("      - "))
    stepStarts.zip(stepStarts.drop(1) :+ lines.size).flatMap { case (start, end) =>
      val step = lines.slice(start, end).mkString("\n")
      val cacheAction = step.linesIterator.map(_.trim.stripPrefix("- ")).exists(line =>
        line.startsWith("uses: actions/cache@") || line.startsWith("uses: actions/cache/restore@")
      )
      if cacheAction then
        val name = step.linesIterator.collectFirst {
          case line if line.startsWith("      - name: ") => line.stripPrefix("      - name: ")
        }.getOrElse("<unnamed cache action>")
        Some(name -> cachePaths(step))
      else None
    }.toList

  private def cachePaths(step: String): List[String] =
    scalar(step, "path") match
      case "|" =>
        indentedBlock(step, "path: |", 10).linesIterator.map(_.trim).filter(_.nonEmpty).toList
      case path => List(path)

  def assertSquireCiPolicy(workflow: String): Unit =
    val jobName  = "squire-policy:"
    val stepName = "Test Squire and release policy"
    expect(
      workflow.linesIterator.count(_ == s"  $jobName") == 1,
      "workflow must contain exactly one squire-policy job"
    )
    val lint = indentedBlock(workflow, "lint:", 2)
    expect(!lint.contains("mise run test:squire"), "lint must not run Squire policy")

    val policy = indentedBlock(workflow, jobName, 2)
    val stepStarts = yamlSequenceEntries(policy, 6)
    val headers = policy.linesIterator.collect {
      case line if line.startsWith("      - name: ") => line.stripPrefix("      - name: ")
    }.toList
    expect(stepStarts.size == 5, s"unexpected squire-policy step count: ${stepStarts.size}")
    expect(
      headers == List(
        "Checkout current branch",
        "Setup Scala and Java",
        "Cache scala dependencies",
        "Setup mise",
        stepName
      ),
      s"unexpected squire-policy steps: $headers"
    )
    expect(!hasYamlKey(policy, "needs"), "squire-policy must run in parallel")
    expect(!hasYamlKey(policy, "fetch-depth"), "squire-policy must use the default shallow checkout")

    val step = indentedBlock(policy, s"- name: $stepName", 6)
    expect(scalar(step, "run") == "mise run test:squire", s"$stepName must run test:squire exactly")
    expect(count(workflow, "mise run test:squire") == 1, "workflow must invoke test:squire exactly once")

    val ci        = indentedBlock(workflow, "ci:", 2)
    val aggregate = inlineList(ci, "needs")
    expect(aggregate.count(_ == "squire-policy") == 1, "ci must depend on squire-policy exactly once")
    expect(scalar(ci, "if") == "${{ always() }}", "ci must always run after its dependencies")
    val aggregateStep = indentedBlock(ci, "- name: Verify required CI jobs succeeded", 6)
    val requiredResults = aggregate.map(job => s"test \"$${{ needs.$job.result }}\" = \"success\"")
    val resultAssertions = aggregateStep.linesIterator.map(_.trim).filter(_.startsWith("test ")).toList
    expect(
      resultAssertions == requiredResults,
      "ci must fail unless every required job result is success"
    )

  def replaceInJob(workflow: String, jobName: String, oldValue: String, newValue: String): String =
    val job = indentedBlock(workflow, jobName, 2)
    if !job.contains(oldValue) then fail(s"mutation target not found in $jobName: $oldValue")
    replaceOnce(workflow, job, replaceOnce(job, oldValue, newValue))

  def rejects(validator: String => Unit, workflow: String): Boolean =
    scala.util.Try(validator(workflow)).isFailure

class SquireCliSpec extends Test[Any]:
  "commands" - {
    "expose the complete unified command tree" in {
      val expected = Set(
        List("ai", "env", "info"),
        List("doctor"),
        List("cellar", "get"),
        List("cellar", "search"),
        List("cellar", "deps"),
        List("reference", "repo", "add"),
        List("reference", "repo", "list"),
        List("reference", "repo", "status"),
        List("reference", "repo", "remove"),
        List("branch", "refresh"),
        List("tracking", "status"),
        List("tracking", "sync"),
        List("tracking", "doctor"),
        List("spec", "sync"),
        List("spec", "export"),
        List("schemas", "build"),
        List("schemas", "compare"),
        List("schemas", "validate")
      )

      assert(SquireApp.commands.flatMap(_.names).toSet == expected)
    }
  }

  "environment info" - {
    "writes JSON for the full report and nothing for check mode" in {
      for
        root <- SquireFixtures.scratch("env-cli")
        platform   = SquireFixtures.platform(root, SquireEnv.CheckResult(Present(true), "ok", 0.0))
        fullOutput = new StringBuilder
        fullExit <- SquireCli.runEnvInfo(AiEnvInfoOpts(), root, platform, value => fullOutput.append(value))
        skippedOutput = new StringBuilder
        skippedExit <- SquireCli.runEnvInfo(
          AiEnvInfoOpts(check = Some("var-folders")),
          root,
          platform,
          value => skippedOutput.append(value)
        )
        blockedPlatform = SquireFixtures.platform(root, SquireEnv.CheckResult(Present(false), "blocked", 0.0))
        blockedOutput   = new StringBuilder
        blockedExit <- SquireCli.runEnvInfo(
          AiEnvInfoOpts(check = Some("jvm-network")),
          root,
          blockedPlatform,
          value => blockedOutput.append(value)
        )
        fractionalOutput = new StringBuilder
        fractionalExit <-
          SquireCli.runEnvInfo(AiEnvInfoOpts(timeout = 0.5), root, platform, value => fractionalOutput.append(value))
        json       = fullOutput.result()
        legacyKeys =
          List("generated_at", "claude_code", "ci", "checks", "sandboxed", "claude_settings", "recommendation")
        ordered = legacyKeys.map(json.indexOf).sliding(2).forall {
          case List(left, right) => left >= 0 && left < right; case _ => true
        }
      yield assert(
        fullExit == 0 && json.contains("\"generated_at\": \"1970-01-01T00:00:00+0000\"") &&
          json.contains("\"jvm_network\"") && !json.contains("python_network") &&
          json.contains("\"entrypoint\": null") && json.contains("\"session_id\": null") && ordered &&
          fractionalExit == 0 && fractionalOutput.nonEmpty && skippedExit == 0 && skippedOutput.isEmpty &&
          blockedExit == 1 && blockedOutput.isEmpty
      )
    }

    "resolves the repository root for routed diagnostics" in {
      for
        root <- SquireFixtures.scratch("env-cli-root")
        from = root / ".claude" / "skills" / "squire"
        _ <- Sync.defer {
          Files.createDirectories(from.toJava)
          Files.createDirectory((root / ".git").toJava)
        }
        resolved <- SquireCli.projectRoot(from)
      yield assert(resolved == root)
    }
  }

  "Cellar and reference routing" - {
    "forward process exits and command output through the unified CLI" in {
      for
        root <- SquireFixtures.scratch("cli-routing")
        settings = root / ".config" / "squire" / "settings.local.yaml"
        _ <- Sync.defer {
          Files.createDirectories(settings.parent.get.toJava)
          Files.writeString(settings.toJava, "cellar:\n  binary: /opt/cellar\n")
        }
        cellarOutput = new StringBuilder
        cellarError  = new StringBuilder
        cellarRunner = RuleRunner(request => ProcessResult(request, 7, "cellar out", "cellar err"))
        cellarExit <- SquireCli.runCellar(
          CellarAction.Deps("mill-scalalib"),
          root,
          cellarRunner,
          TestSquirePlatform(),
          value => cellarOutput.append(value),
          value => cellarError.append(value)
        )
        manifest = ReferenceManifest(List(SquireRepoFixtures.repo("mill")))
        _ <- SquireRepo.saveManifest(root, manifest)
        listOut = new StringBuilder
        listExit <- SquireCli.runReferenceList(
          ReferenceRepoListOpts(json = true),
          root,
          RuleRunner(SquireRepoFixtures.ok),
          value => listOut.append(value)
        )
      yield assert(
        cellarExit == 7 && cellarOutput.result() == "cellar out" && cellarError.result() == "cellar err" &&
          cellarRunner.requests.head.argv == Chunk("/opt/cellar", "deps", "com.lihaoyi:mill-scalalib_3:0.12.0") &&
          listExit == 0 && SquireJson.decode[ReferenceManifest](listOut.result().trim) == Result.Success(manifest)
      )
    }
  }

  "branch refresh routing" - {
    "renders text and typed JSON results" in {
      val textRunner = BranchRecordingRunner(SquireBranchFixtures.successfulProofResponses)
      val jsonRunner = BranchRecordingRunner(SquireBranchFixtures.successfulProofResponses)
      val text       = new StringBuilder
      val json       = new StringBuilder
      for
        textExit <- SquireCli.runBranch(
          BranchRefreshOpts(dryRun = true),
          textRunner,
          value => text.append(value)
        )
        jsonExit <- SquireCli.runBranch(
          BranchRefreshOpts(dryRun = true, json = true),
          jsonRunner,
          value => json.append(value)
        )
      yield assert(
        textExit == 0 && text.result() ==
          s"validated: develop ${SquireBranchFixtures.targetSha} -> ${SquireBranchFixtures.sourceSha}\n" &&
          jsonExit == 0 &&
          SquireJson.decode[RefreshResult](json.result().trim) == Result.Success(
            RefreshResult(
              "validated",
              "develop",
              SquireBranchFixtures.targetSha,
              SquireBranchFixtures.sourceSha,
              Present(42)
            )
          )
      )
    }
  }

  "tracking routing" - {
    "renders quiet checks sync and doctor through the typed tracking boundary" in {
      import SquireTrackingFixtures.*
      for
        root <- SquireFixtures.scratch("tracking-cli")
        _    <- beads(root)
        _    <- Sync.defer {
          Files.writeString((root / "AGENTS.md").toJava, SquireTracking.pointer + "\n")
          Files.writeString((root / "CLAUDE.md").toJava, SquireTracking.pointer + "\n")
        }
        statusOutput = new StringBuilder
        status <- SquireCli.runTrackingStatus(
          TrackingStatusOpts(quiet = true),
          root,
          runner(gitShared, bdVersion),
          TestSquirePlatform(Present("bd")),
          value => statusOutput.append(value)
        )
        checkOutput = new StringBuilder
        check <- SquireCli.runTrackingStatus(
          TrackingStatusOpts(check = Some("off")),
          root,
          runner(gitShared, bdVersion),
          TestSquirePlatform(Present("bd")),
          value => checkOutput.append(value)
        )
        syncOutput = new StringBuilder
        sync <- SquireCli.runTrackingSync(TrackingSyncOpts(check = true), root, value => syncOutput.append(value))
        doctorOutput = new StringBuilder
        doctor <- SquireCli.runTrackingDoctor(
          root,
          runner(gitShared, bdVersion),
          TestSquirePlatform(Present("bd")),
          value => doctorOutput.append(value)
        )
      yield assert(status == 0 && statusOutput.result() == "beads\n" && check == 1 && checkOutput.isEmpty &&
        sync == 0 && syncOutput.result().contains("OK - AGENTS.md") && doctor == 0 && doctorOutput.result().contains(
          "guidance"
        ))
    }

    "rejects invalid status checks and conflicting sync modes before output or process work" in {
      for
        root <- SquireFixtures.scratch("tracking-cli-invalid")
        statusOutput = new StringBuilder
        status <- SquireCli.runTrackingStatus(
          TrackingStatusOpts(check = Some("invalid")),
          root,
          RuleRunner(SquireTrackingFixtures.unexpected),
          TestSquirePlatform(),
          value => statusOutput.append(value)
        )
        syncOutput = new StringBuilder
        sync <- SquireCli.runTrackingSync(
          TrackingSyncOpts(check = true, diff = true),
          root,
          value => syncOutput.append(value)
        )
      yield assert(status == 2 && statusOutput.isEmpty && sync == 2 && syncOutput.isEmpty)
    }

    "preserves the legacy tracking status JSON shape byte for byte" in {
      import SquireTrackingFixtures.*
      for
        root <- SquireFixtures.scratch("tracking-cli-legacy-json")
        _    <- beads(root)
        output = new StringBuilder
        exit <- SquireCli.runTrackingStatus(
          TrackingStatusOpts(),
          root,
          runner(gitShared, bdVersion),
          TestSquirePlatform(Present("bd")),
          value => output.append(value)
        )
        expected = """{
                     |  "configured_mode": "auto",
                     |  "effective_mode": "beads",
                     |  "reason": "bd is installed and .beads/ exists (mode: auto)",
                     |  "bd": {
                     |    "installed": true,
                     |    "version": "bd 0.42.0"
                     |  },
                     |  "beads_dir_present": true,
                     |  "workspace": {
                     |    "is_worktree": false,
                     |    "local_store": false,
                     |    "status": "shared"
                     |  },
                     |  "settings_file": ".config/squire/settings.local.yaml",
                     |  "settings_file_present": false,
                     |  "guidance_doc": "docs/task-tracking.md",
                     |  "guidance_drift": []
                     |}
                     |""".stripMargin
      yield assert(exit == 0 && output.result() == expected)
    }
  }

  "doctor routing" - {
    "returns one for blockers and zero for healthy refused or absent daemon findings" in {
      val healthy = SquireDoctor.DoctorReport(
        Chunk(
          SquireDoctor.Finding("mill_daemon", "NO_DAEMON", "not running", false),
          SquireDoctor.Finding("mill_daemon", "REFUSED", "not accepting connections", false),
          SquireDoctor.Finding("project", "OK", "healthy", false)
        )
      )
      val blocked = SquireDoctor.DoctorReport(
        Chunk(SquireDoctor.Finding("var_folders", "BLOCKED", "cannot write", true))
      )
      for
        healthyExit <- SquireCli.printDoctor(healthy)
        blockedExit <- SquireCli.printDoctor(blocked)
      yield
        val healthyCode: Any = healthyExit
        val blockedCode: Any = blockedExit
        assert(healthyCode == 0 && blockedCode == 1)
    }
  }

  "command boundary" - {
    "records exact exits and renders one structured domain error" in {
      val exits                             = scala.collection.mutable.ArrayBuffer.empty[Int]
      val errors                            = new StringBuilder
      val failure: Int < Abort[SquireError] = Abort.fail(
        SquireError.Failure("repo", "manifest entry is missing", Present("choose a configured repository name"))
      )
      for
        _ <- SquireCli.runCommand(0, value => errors.append(value), code => exits.append(code))
        _ <- SquireCli.runCommand(1, value => errors.append(value), code => exits.append(code))
        _ <- SquireCli.runCommand(2, value => errors.append(value), code => exits.append(code))
        _ <- SquireCli.runCommand(7, value => errors.append(value), code => exits.append(code))
        _ <- SquireCli.runCommand(failure, value => errors.append(value), code => exits.append(code))
      yield assert(
        exits.toList == List(0, 1, 2, 7, 1) &&
          errors.result() == "ERROR [repo]: manifest entry is missing\n  choose a configured repository name\n"
      )
    }
  }

  "schema routing" - {
    "uses build and compare defaults while keeping JSON stdout clean" in {
      for
        root <- SquireFixtures.scratch("schemas-cli")
        source = root / "schemas"
        _ <- Sync.defer {
          Files.createDirectories(source.toJava)
          Files.writeString((source / "morphir-ir-v4.yaml").toJava, "$id: https://example.test/v4.yaml\ntype: object\n")
        }
        buildOutput = new StringBuilder
        buildExit <- SquireCli.runSchemasBuild(
          SchemasBuildOpts(from = Some(source.toString), json = true),
          root,
          value => buildOutput.append(value),
          _ => ()
        )
        buildReport   = SquireJson.decode[SchemaReport](buildOutput.result().trim)
        generated     = root / ".dev" / "out" / "squire" / "schemas" / "morphir-ir-v4.json"
        compareOutput = new StringBuilder
        compareExit <- SquireCli.runSchemasCompare(
          SchemasCompareOpts(from = Some(source.toString), json = true),
          root,
          value => compareOutput.append(value),
          _ => ()
        )
        compareReport = SquireJson.decode[SchemaReport](compareOutput.result().trim)
      yield assert(
        buildExit == 0 && buildReport.exists(report => report.to == generated.parent.get.toString && report.ok) &&
          Files.exists(generated.toJava) && compareExit == 1 &&
          compareReport.exists(report => report.to == source.toString && !report.ok) &&
          !buildOutput.result().contains("written\nwritten")
      )
    }
  }

class SquireMetaSpec extends Test[Any]:
  private val skillDirectory = java.nio.file.Paths.get(java.lang.System.getProperty("user.dir"))

  private def read(name: String): String =
    Files.readString(skillDirectory.resolve(name), StandardCharsets.UTF_8)

  "launchers" - {
    "run the single-file application without a Mill server or ticker" in {
      assert(read("squire").contains("--no-server --ticker false squire.scala"))
      assert(read("squire.bat").contains("--no-server --ticker false squire.scala"))
    }
    "preserve the activated validator executable across Mill's PATH rewrite" in {
      assert(read("squire").contains("command -v jsonschema") && read("squire").contains("SQUIRE_JSONSCHEMA_BIN"))
      assert(read("squire.bat").contains("where jsonschema") && read("squire.bat").contains("SQUIRE_JSONSCHEMA_BIN"))
    }

    "honours the POSIX exit-file contract and propagates Mill failure without a handoff" in Sync.defer {
      val root     = Files.createTempDirectory("squire-wrapper-contract-")
      val launcher = root.resolve("squire")
      val mill     = root.resolve("mill")
      Files.copy(skillDirectory.resolve("squire"), launcher)
      SquireLauncherFixtures.executable(launcher, Files.readString(launcher, StandardCharsets.UTF_8))
      SquireLauncherFixtures.executable(
        mill,
        """#!/bin/sh
          |if [ -n "${SQUIRE_TEST_RECORDED_EXIT:-}" ]; then
          |  printf '%s\n' "$SQUIRE_TEST_RECORDED_EXIT" > "$SQUIRE_EXIT_FILE"
          |fi
          |exit "${SQUIRE_TEST_MILL_EXIT:-0}"
          |""".stripMargin
      )
      val recorded = List(0, 1, 2, 7).map(code =>
        SquireLauncherFixtures.run(
          root,
          Seq.empty,
          Map("SQUIRE_TEST_RECORDED_EXIT" -> code.toString, "SQUIRE_TEST_MILL_EXIT" -> "0")
        ).exitCode
      )
      val millFailure = SquireLauncherFixtures.run(
        root,
        Seq.empty,
        Map("SQUIRE_TEST_MILL_EXIT" -> "23")
      )
      assert(recorded == List(0, 1, 2, 7) && millFailure.exitCode == 23)
    }

    "defines the same validated temporary exit-file contract on Windows" in {
      val launcher = read("squire.bat")
      assert(
        launcher.contains("SQUIRE_EXIT_FILE") && launcher.contains("SQUIRE_RECORDED_EXIT") &&
          launcher.contains("SQUIRE_MILL_EXIT") && launcher.contains("del /q") &&
          launcher.contains("goto no_recorded_exit") &&
          launcher.contains("--no-server --ticker false squire.scala")
      )
    }
  }

  "Scala source policy" - {
    "marks every concrete Squire case class final" in {
      val stream     = Files.list(skillDirectory)
      val violations =
        try
          stream.iterator().asScala
            .filter(path => path.getFileName.toString.endsWith(".scala"))
            .flatMap(path =>
              Files.readAllLines(path, StandardCharsets.UTF_8).asScala.iterator.zipWithIndex.map {
                case (line, index) => (path.getFileName.toString, index + 1, line.trim)
              }
            )
            .collect {
              case (file, line, source)
                  if source.startsWith("case class ") || source.startsWith("private case class ") =>
                s"$file:$line: $source"
            }
            .toList
        finally stream.close()
      assert(violations.isEmpty, violations.mkString("non-final case classes:\n", "\n", ""))
    }
  }

  "Mill version" - {
    "keeps the approved standalone Squire pin" in
      assert(read(".mill-version").trim == "1.2.0-RC1-24-042146")
  }

  "suite registry" - {
    "lists every suite declared by this test file" in {
      val registry = read("test-resources/META-INF/services/kyo.test.Test")
        .linesIterator
        .map(_.trim)
        .filter(line => line.nonEmpty && !line.startsWith("#"))
        .toSet
      assert(
        registry == Set(
          "SquireCliSpec",
          "SquireCiPolicySpec",
          "SquireMisePolicySpec",
          "SquireMigrationSpec",
          "SquireMetaSpec",
          "SquireModelSpec",
          "SquireProcessSpec",
          "SquireEnvSpec",
          "SquireDoctorSpec",
          "SquireCellarSpec",
          "SquireRepoSpec",
          "SquireBranchSpec",
          "SquireTrackingSpec",
          "SquireSchemasSpec",
          "SquireSpecSpec"
        )
      )
    }
  }

class SquireCiPolicySpec extends Test[Any]:
  import SquireCiPolicy.*

  private val skillDirectory = java.nio.file.Paths.get(java.lang.System.getProperty("user.dir"))
  private val repositoryRoot = Path(skillDirectory.resolve("../../..").normalize.toString)
  private val workflow       = Files.readString(
    skillDirectory.resolve("../../../.github/workflows/ci.yml").normalize,
    StandardCharsets.UTF_8
  )
  private val buildMill = Files.readString(
    skillDirectory.resolve("../../../build.mill").normalize,
    StandardCharsets.UTF_8
  )
  private val jvmPlatformTask = Files.readString(
    skillDirectory.resolve("../../../.config/mise/tasks/test/jvm-platform").normalize,
    StandardCharsets.UTF_8
  )
  private val jvmTargetSelectors = List(
    "morphir.__.jvm.__.compile",
    "morphir.jvm.__.compile",
    "morphir.{contrib.knowledge,extensibility,intelligence.sdk,interop.borer,interop.zio.json,kit.kyo,langkit.core,langkit.elm.compiler.api,langkit.elm.core,langkit.trees,lib.interop,model,model.lowering,naming,testing.generators,testing.zio,tests,tools}.jvm.__.compile",
    "morphir.__.jvm.publishArtifacts",
    "morphir.jvm.publishArtifacts",
    "morphir.{contrib.knowledge,extensibility,interop.borer,interop.zio.json,lib.interop,model,model.lowering,naming,tests,tools}.jvm.publishArtifacts",
    "morphir.__.jvm.__.test",
    "morphir.{contrib.knowledge,intelligence.sdk,interop.borer,interop.zio.json,kit.kyo,langkit.core,langkit.elm.compiler.api,langkit.elm.core,langkit.trees,model,model.lowering,tests}.jvm.test"
  )

  private def resolveJvmTarget(selector: String): Set[String] < (Async & Abort[SquireError]) =
    LiveProcessRunner.run(
      ProcessRequest(
        Chunk((repositoryRoot / "mill").toString, "--ticker", "false", "resolve", selector),
        Present(repositoryRoot)
      )
    ).flatMap {
      case ProcessResult(_, 0, stdout, _) =>
        stdout.linesIterator.filter(_.startsWith("morphir.")).toSet
      case result =>
        Abort.fail(
          SquireError.Failure(
            "ci-policy",
            s"could not resolve JVM selector $selector (exit ${result.exitCode})",
            Present(result.stderr.trim)
          )
        )
    }

  private def resolveJvmTargetInventory: Map[String, Set[String]] < (Async & Abort[SquireError]) =
    Kyo.foreach(Chunk.from(jvmTargetSelectors)) { selector =>
      resolveJvmTarget(selector).map(selector -> _)
    }.map(_.toList.toMap)

  "hosted CI policy" - {
    "targets the exact supported pull-request and push branches" in {
      assertBranchPolicy(workflow)
      assert(true)
    }

    "waits for aggregate CI and owns one guarded release path" in {
      assertPublishPolicy(workflow)
      assert(true)
    }

    "scopes the exact snapshot configuration to develop" in {
      assertSnapshotPolicy(workflow)
      assert(true)
    }

    "retains every release ref on the JS and JVM cache saves" in {
      assertCachePolicy(workflow)
      assert(true)
    }

    "restricts workflow permissions to read-only contents" in {
      assertReadOnlyPermissions(workflow)

      val permissionMutations = List(
        replaceOnce(workflow, "permissions:\n  contents: read", "permissions:\n  contents: write"),
        replaceOnce(workflow, "permissions:\n  contents: read", "permissions:\n  contents: read\n  packages: write")
      )
      assert(permissionMutations.forall(rejects(assertReadOnlyPermissions, _)))
    }

    "preserves the Morphir CI capability graph" in {
      assertMorphirCapabilityPolicy(workflow)

      val capabilityMutations = List(
        assertMorphirCapabilityPolicy -> replaceInJob(
          workflow,
          "mill-morphir-unit:",
          "'mill-plugins.morphir.{toolchain,javascript,elm-tooling,core,elm}.__.test'",
          "'mill-plugins.morphir.__.test'"
        ),
        ((workflow: String) => assertJvmPlatformPolicy(workflow, buildMill, jvmPlatformTask)) -> replaceInJob(
          workflow,
          "test-jvm:",
          "mise run test:jvm-platform",
          "mise run test:jvm"
        ),
        assertMorphirCapabilityPolicy -> replaceInJob(
          workflow,
          "mill-morphir-integration:",
          "needs: [mill-morphir-unit]",
          "needs: []"
        ),
        assertMorphirCapabilityPolicy -> replaceInJob(
          workflow,
          "runtime-tests:",
          "needs: [runtime-generated-fixtures]",
          "needs: [test-jvm]"
        )
      )
      assert(capabilityMutations.forall((validator, mutation) => rejects(validator, mutation)))
    }

    "keeps generic JVM CI on the non-classic platform alias" in {
      assertJvmPlatformPolicy(workflow, buildMill, jvmPlatformTask)

      val missingPublishAliasMutation = replaceOnce(
        buildMill,
        "    \"morphir.jvm.publishArtifacts\",\n",
        ""
      )
      val addedAliasMutation = replaceOnce(
        buildMill,
        "    \"morphir.langkit.itest.testCached\"",
        "    \"morphir.future.jvm.test\",\n    \"morphir.langkit.itest.testCached\""
      )
      val reorderedAliasMutation = replaceOnce(
        buildMill,
        "    \"morphir.jvm.__.compile\",\n" +
          "    \"morphir.{contrib.knowledge,extensibility,intelligence.sdk,interop.borer,interop.zio.json,kit.kyo,langkit.core,langkit.elm.compiler.api,langkit.elm.core,langkit.trees,lib.interop,model,model.lowering,naming,testing.generators,testing.zio,tests,tools}.jvm.__.compile\",",
        "    \"morphir.{contrib.knowledge,extensibility,intelligence.sdk,interop.borer,interop.zio.json,kit.kyo,langkit.core,langkit.elm.compiler.api,langkit.elm.core,langkit.trees,lib.interop,model,model.lowering,naming,testing.generators,testing.zio,tests,tools}.jvm.__.compile\",\n" +
          "    \"morphir.jvm.__.compile\","
      )
      val runtimeAliasMutation = replaceOnce(
        buildMill,
        "\"morphir.langkit.itest.testCached\"",
        "\"morphir.langkit.itest.testCached\", \"morphir.runtime.classic.jvm.test\""
      )
      val taskMutation = replaceOnce(
        jvmPlatformTask,
        "./mill -i Alias/run testJVMPlatform",
        "./mill -i Alias/run testJVM"
      )
      val aliasMutations = List(
        missingPublishAliasMutation,
        addedAliasMutation,
        reorderedAliasMutation,
        runtimeAliasMutation
      )
      aliasMutations.zipWithIndex.foreach { case (mutation, index) =>
        assert(
          rejects(assertJvmPlatformPolicy(workflow, _, jvmPlatformTask), mutation),
          s"JVM alias mutation $index must be rejected"
        )
      }
      assert(rejects(assertJvmPlatformPolicy(workflow, buildMill, _), taskMutation))
    }

    "resolves every current non-classic JVM compile publish and test target" in {
      val selectedCompile = jvmTargetSelectors(2)
      val broadCompile    = jvmTargetSelectors.head
      for
        inventory <- resolveJvmTargetInventory
        _         <- Sync.defer(assertJvmTargetInventory(inventory))
        missingSelectedTarget = inventory.updated(
          selectedCompile,
          inventory(selectedCompile) - "morphir.model.jvm.compile"
        )
        addedCurrentTarget = inventory.updated(
          broadCompile,
          inventory(broadCompile) + "morphir.future.jvm.compile"
        )
      yield assert(
        scala.util.Try(assertJvmTargetInventory(missingSelectedTarget)).isFailure &&
          scala.util.Try(assertJvmTargetInventory(addedCurrentTarget)).isFailure
      )
    }

    "keeps Morphir caches scoped to reusable capability outputs" in {
      assertMorphirCachePolicy(workflow)

      val morphirJobs = List(
        "mill-morphir-unit:",
        "mill-morphir-integration:",
        "morphir-elm-projects:",
        "runtime-generated-fixtures:",
        "runtime-tests:"
      )
      val downloadCacheMutations = morphirJobs.map { job =>
        replaceInJob(workflow, job, "path: ~/.cache/morphir-scala", "path: ~/.cache/unverified-morphir")
      }
      val approvedDownloadPathOutsideNamedStep = morphirJobs.map { job =>
        replaceInJob(
          replaceInJob(workflow, job, "path: ~/.cache/morphir-scala", "path: ~/.cache/unverified-morphir"),
          job,
          "- name: Run capability",
          "- name: Document approved cache path\n" +
            "        run: echo 'path: ~/.cache/morphir-scala'\n" +
            "      - name: Run capability"
        )
      }
      val movedRuntimeOutput = replaceInJob(
        replaceInJob(
          workflow,
          "runtime-generated-fixtures:",
          "path: out/morphir/runtime/classic/jvm/test/",
          "path: out/morphir/runtime/classic/jvm/test-fixtures/"
        ),
        "morphir-elm-projects:",
        "out/morphir-elm/",
        "out/morphir-elm/\n            out/morphir/runtime/classic/jvm/test/"
      )
      val integrationExtraOutput = replaceInJob(
        workflow,
        "mill-morphir-integration:",
        "            out/mill-plugins/morphir/\n",
        "            out/mill-plugins/morphir/\n            out/\n"
      )
      val runtimeGeneratedExtraOutput = replaceInJob(
        workflow,
        "runtime-generated-fixtures:",
        "path: out/morphir/runtime/classic/jvm/test/",
        "path: |\n            out/morphir/runtime/classic/jvm/test/\n            out/"
      )
      val runtimeTestsExtraOutput = replaceInJob(
        workflow,
        "runtime-tests:",
        "path: out/morphir/runtime/classic/jvm/test/",
        "path: |\n            out/morphir/runtime/classic/jvm/test/\n            out/"
      )
      val runtimeGeneratedOutputOutsideStep = replaceInJob(
        replaceInJob(
          workflow,
          "runtime-generated-fixtures:",
          "path: out/morphir/runtime/classic/jvm/test/",
          "path: out/"
        ),
        "runtime-generated-fixtures:",
        "run: ./mill -i morphir.runtime.classic.jvm.test.generatedRuntimeFixtures",
        "run: |\n          echo out/morphir/runtime/classic/jvm/test/\n          ./mill -i morphir.runtime.classic.jvm.test.generatedRuntimeFixtures"
      )
      val runtimeTestsOutputOutsideStep = replaceInJob(
        replaceInJob(
          workflow,
          "runtime-tests:",
          "path: out/morphir/runtime/classic/jvm/test/",
          "path: out/"
        ),
        "runtime-tests:",
        "./mill -i morphir.runtime.classic.jvm.test.verifyRuntimeTestDiscovery",
        "echo out/morphir/runtime/classic/jvm/test/\n          ./mill -i morphir.runtime.classic.jvm.test.verifyRuntimeTestDiscovery"
      )
      val unnamedCacheAction = replaceInJob(
        workflow,
        "mill-morphir-integration:",
        "- name: Run capability",
        "- uses: actions/cache@v6\n" +
          "        with:\n" +
          "          path: out/\n" +
          "          key: injected-unnamed-cache\n" +
          "      - name: Run capability"
      )
      val unnamedRestoreAction = replaceInJob(
        workflow,
        "morphir-elm-projects:",
        "- name: Run capability",
        "- uses: actions/cache/restore@v6\n" +
          "        with:\n" +
          "          path: out/mill-plugins/morphir/\n" +
          "          key: injected-unnamed-restore\n" +
          "      - name: Run capability"
      )
      val cacheMutations = downloadCacheMutations ++ approvedDownloadPathOutsideNamedStep ++ List(
        replaceInJob(workflow, "mill-morphir-unit:", "out/mill-plugins/morphir/", "out/"),
        replaceInJob(workflow, "mill-morphir-unit:", "!out/mill-plugins/morphir/**/testForked.dest/**\n", ""),
        replaceInJob(workflow, "mill-morphir-unit:", "!out/mill-plugins/morphir/**/testOnly.dest/**\n", ""),
        replaceInJob(workflow, "morphir-elm-projects:", "out/examples/morphir-elm-projects/", "out/examples/"),
        replaceInJob(workflow, "morphir-elm-projects:", "out/morphir-elm/", "out/"),
        integrationExtraOutput,
        runtimeGeneratedExtraOutput,
        runtimeTestsExtraOutput,
        runtimeGeneratedOutputOutsideStep,
        runtimeTestsOutputOutsideStep,
        movedRuntimeOutput
      )
      cacheMutations.zipWithIndex.foreach { case (mutation, index) =>
        assert(rejects(assertMorphirCachePolicy, mutation), s"cache mutation $index must be rejected")
      }
      val acceptedUnnamedMutations = List(unnamedCacheAction, unnamedRestoreAction).zipWithIndex.collect {
        case (mutation, index) if !rejects(assertMorphirCachePolicy, mutation) => index
      }
      assert(acceptedUnnamedMutations.isEmpty, s"unnamed cache mutations accepted: $acceptedUnnamedMutations")
    }

    "runs Squire policy in a dedicated parallel CI job" in {
      assertSquireCiPolicy(workflow)

      val policy     = indentedBlock(workflow, "squire-policy:", 2)
      val policyJob  = s"  squire-policy:\n$policy"
      val policyStep = s"      - name: Test Squire and release policy\n" +
        indentedBlock(policy, "- name: Test Squire and release policy", 6)
      val checkout = "      - name: Checkout current branch\n" +
        "        uses: actions/checkout@v7.0.1"
      val aggregate      = inlineList(indentedBlock(workflow, "ci:", 2), "needs")
      val aggregateNeeds = s"needs: [${aggregate.mkString(", ")}]"
      val requiredResult = (job: String) => s"test \"$${{ needs.$job.result }}\" = \"success\""
      val mutations = List(
        "missing job" -> replaceOnce(workflow, policyJob, ""),
        "duplicate job" -> replaceOnce(workflow, policyJob, s"$policyJob\n$policyJob"),
        "step moved into lint" -> replaceOnce(
          replaceOnce(workflow, policyStep, ""),
          "      - name: Lint code\n        run: mise run lint",
          "      - name: Lint code\n        run: mise run lint\n" + policyStep
        ),
        "step moved into another job" -> (replaceOnce(workflow, policyStep, "") +
          "\n  bypass-policy:\n" +
          "    runs-on: ubuntu-latest\n" +
          "    steps:\n" +
          policyStep),
        "changed command" -> {
          val changedPolicy = replaceOnce(policyJob, "mise run test:squire", "mise run lint")
          replaceOnce(workflow, policyJob, changedPolicy)
        },
        "job dependency added" -> replaceOnce(
          workflow,
          "  squire-policy:\n",
          "  squire-policy:\n    needs: [lint]\n"
        ),
        "full-history checkout added" -> replaceOnce(
          workflow,
          policyJob,
          replaceOnce(policyJob, checkout, s"$checkout\n        with:\n          fetch-depth: 0")
        ),
        "unnamed run step added" -> replaceOnce(
          workflow,
          policyJob,
          s"$policyJob\n      - run: echo bypass"
        ),
        "unnamed uses step added" -> replaceOnce(
          workflow,
          policyJob,
          s"$policyJob\n      - uses: actions/checkout@v7.0.1"
        ),
        "dash-only unnamed run step added" -> replaceOnce(
          workflow,
          policyJob,
          s"$policyJob\n      -\n        run: echo bypass"
        ),
        "quoted policy dependency added" -> replaceOnce(
          workflow,
          "  squire-policy:\n",
          "  squire-policy:\n    \"needs\": [lint]\n"
        ),
        "spaced policy dependency added" -> replaceOnce(
          workflow,
          "  squire-policy:\n",
          "  squire-policy:\n    needs : [lint]\n"
        ),
        "quoted full-history checkout added" -> replaceOnce(
          workflow,
          policyJob,
          replaceOnce(policyJob, checkout, s"$checkout\n        with:\n          \"fetch-depth\": 0")
        ),
        "spaced full-history checkout added" -> replaceOnce(
          workflow,
          policyJob,
          replaceOnce(policyJob, checkout, s"$checkout\n        with:\n          fetch-depth : 0")
        ),
        "aggregate dependency removed" -> replaceOnce(
          workflow,
          aggregateNeeds,
          s"needs: [${aggregate.filterNot(_ == "squire-policy").mkString(", ")}]"
        ),
        "aggregate dependency duplicated" -> replaceOnce(
          workflow,
          aggregateNeeds,
          s"needs: [${aggregate.flatMap(name => if name == "squire-policy" then List(name, name) else List(name)).mkString(", ")}]"
        ),
        "aggregate always removed" -> replaceOnce(workflow, "    if: ${{ always() }}\n", ""),
        "squire-policy failure guard removed" -> replaceOnce(workflow, requiredResult("squire-policy"), ""),
        "squire-policy result handling weakened" -> replaceOnce(
          workflow,
          requiredResult("squire-policy"),
          s"test \"$${{ needs.squire-policy.result }}\" != \"failure\""
        ),
        "squire-policy failure guard weakened with || true" -> replaceOnce(
          workflow,
          requiredResult("squire-policy"),
          s"${requiredResult("squire-policy")} || true"
        ),
        "squire-policy failure guard weakened with ; true" -> replaceOnce(
          workflow,
          requiredResult("squire-policy"),
          s"${requiredResult("squire-policy")}; true"
        )
      )
      mutations.foreach { case (name, mutation) =>
        assert(rejects(assertSquireCiPolicy, mutation), s"$name must be rejected")
      }
    }

    "keeps Mill Morphir dogfood and generated-runtime work in ordered CI jobs" in {
      val required = List(
        "mill-morphir-unit:"         -> "mill-plugins.morphir.{toolchain,javascript,elm-tooling,core,elm}.__.test",
        "mill-morphir-integration:"  -> "mill-plugins.morphir.integration.test",
        "morphir-elm-projects:"      -> "examples.morphir-elm-projects.__.morphirIR",
        "runtime-generated-fixtures:" -> "morphir.runtime.classic.jvm.test.generatedRuntimeFixtures",
        "runtime-tests:"             -> "morphir.runtime.classic.jvm.test.verifyRuntimeTestDiscovery"
      )
      val dependencies = List(
        "mill-morphir-integration:"  -> "needs: [mill-morphir-unit]",
        "morphir-elm-projects:"      -> "needs: [mill-morphir-unit]",
        "runtime-generated-fixtures:" -> "needs: [morphir-elm-projects]",
        "runtime-tests:"             -> "needs: [runtime-generated-fixtures]"
      )
      assert(
        required.forall((job, command) => indentedBlock(workflow, job, 2).contains(command)) &&
          dependencies.forall((job, dependency) => indentedBlock(workflow, job, 2).contains(dependency)) &&
          workflow.contains("path: ~/.cache/morphir-scala")
      )
    }

    "rejects representative branch snapshot and publish regressions" in {
      val pushWithExtraBranch = replaceOnce(
        workflow,
        "  push:\n    branches: [\"main\", \"0.4.x\", \"develop\"]",
        "  push:\n    branches: [\"main\", \"0.4.x\", \"develop\", \"feature\"]"
      )
      val broadSnapshotCondition = replaceOnce(
        workflow,
        "        if: github.ref == 'refs/heads/develop'\n        run: |",
        "        if: github.ref == 'refs/heads/develop' || github.ref == 'refs/heads/main'\n        run: |"
      )
      val extraSnapshotWrite = replaceOnce(
        workflow,
        s"          ${SnapshotCommands(1)}",
        s"          ${SnapshotCommands(1)}\n          echo EXTRA=true >> \"$$GITHUB_ENV\""
      )
      val duplicateSnapshotAssignment = workflow +
        "\nenv:\n  DUPLICATE: \"MORPHIR_PUBLISH_MODE=snapshot\"\n"
      val duplicatePublishPath = replaceOnce(
        workflow,
        "          mise run publish:sonatype",
        "          mise run publish:sonatype\n          mise run publish:sonatype"
      )
      val unguardedPublishPath = replaceOnce(
        workflow,
        "          mise run publish:sonatype",
        "          echo release command moved"
      ) +
        "\n  unguarded-publish:\n" +
        "    runs-on: ubuntu-latest\n" +
        "    steps:\n" +
        "      - name: Bypass Release\n" +
        "        run: mise run publish:sonatype\n"

      val mutations = List(
        (assertBranchPolicy, pushWithExtraBranch),
        (assertSnapshotPolicy, broadSnapshotCondition),
        (assertSnapshotPolicy, extraSnapshotWrite),
        (assertSnapshotPolicy, duplicateSnapshotAssignment),
        (assertPublishPolicy, duplicatePublishPath),
        (assertPublishPolicy, unguardedPublishPath)
      )
      assert(mutations.forall((validator, mutation) => rejects(validator, mutation)))
    }

    "rejects every required cache predicate removed from either cache job" in {
      val predicates = List(
        "github.ref == 'refs/heads/main'",
        "github.ref == 'refs/heads/0.4.x'",
        "github.ref == 'refs/heads/develop'",
        "startsWith(github.ref, 'refs/tags/')"
      )
      val mutations =
        for
          job       <- List("test-js:", "test-jvm:")
          predicate <- predicates
        yield replaceInJob(workflow, job, predicate, "false")
      assert(mutations.forall(rejects(assertCachePolicy, _)))
    }

    "rejects disabled or broadened conditions on either cache job" in {
      val conditions = List(s"false && ($CachePredicate)", s"($CachePredicate) || true")
      val mutations  =
        for
          job       <- List("test-js:", "test-jvm:")
          condition <- conditions
        yield replaceInJob(workflow, job, CachePredicate, condition)
      assert(mutations.forall(rejects(assertCachePolicy, _)))
    }
  }

class SquireMisePolicySpec extends Test[Any]:
  private val skillDirectory = Path(java.lang.System.getProperty("user.dir"))
  private val repositoryRoot = skillDirectory / ".." / ".." / ".."
  private val miseExecutable = Option(java.lang.System.getenv("SQUIRE_MISE_BIN")).filter(_.nonEmpty).getOrElse("mise")
  private val buildElmScript = repositoryRoot / ".config" / "mise" / "tasks" / "build" / "elm"
  private val buildEvaluatorScript = repositoryRoot / ".config" / "mise" / "tasks" / "build" / "morphir-elm"
  private val setupScript = repositoryRoot / ".config" / "mise" / "tasks" / "setup"
  private val localCiScript = repositoryRoot / ".config" / "mise" / "tasks" / "ci" / "local"
  private val expectedDependencies = List(
    "lint",
    "test:squire",
    "test:jvm-platform",
    "test:js",
    "test:native"
  )
  private val expectedBuildElm = Chunk(
    TaskInvocation(
      "mill",
      Chunk(
        "--ticker",
        "false",
        "-k",
        "examples.morphir-elm-projects.__.morphirIR",
        "+",
        "morphir-elm.sdks.__.morphirIR"
      )
    )
  )
  private val expectedBuildEvaluator = Chunk(
    TaskInvocation(
      "mill",
      Chunk("--ticker", "false", "examples.morphir-elm-projects.evaluator-tests.morphirIR")
    )
  )
  private val expectedSetup = Chunk(TaskInvocation("bun", Chunk("install", "--ignore-scripts")))
  private val expectedLocalCi = Chunk(
    TaskInvocation("mill", Chunk("-i", "-k", "mill-plugins.morphir.{toolchain,javascript,elm-tooling,core,elm}.__.test")),
    TaskInvocation("mill", Chunk("-i", "mill-plugins.morphir.integration.test")),
    TaskInvocation(
      "mill",
      Chunk("-i", "-k", "examples.morphir-elm-projects.__.morphirIR", "+", "morphir-elm.sdks.__.morphirIR")
    ),
    TaskInvocation("mill", Chunk("-i", "morphir.runtime.classic.jvm.test.generatedRuntimeFixtures")),
    TaskInvocation("mill", Chunk("-i", "morphir.runtime.classic.jvm.test.verifyRuntimeTestDiscovery")),
    TaskInvocation("mill", Chunk("-i", "morphir.runtime.classic.jvm.test"))
  )

  private def runMise(arguments: String*): ProcessResult < (Async & Abort[SquireError]) =
    LiveProcessRunner.run(ProcessRequest(Chunk(miseExecutable) ++ Chunk.from(arguments), Present(repositoryRoot)))

  private def validateTaskScript(scriptText: String): Either[SquireError, Unit] =
    val scriptLines = scriptText.linesIterator.toList
    val executableLines = scriptLines match
      case "#!/usr/bin/env bash" :: remaining => remaining
      case _                                    => scriptLines
    val executableText = executableLines.filterNot(_.trim.startsWith("#")).mkString("\n")
    val absoluteReference = "(?<![A-Za-z0-9_.-])(/[^\\s\\\"';&|]+|\\.\\./[^\\s\\\"';&|]+)".r
      .findFirstMatchIn(executableText).map(_.group(1))
    absoluteReference match
      case Some(reference) =>
        Left(
          SquireError.Failure(
            "mise-policy",
            s"task preflight rejected absolute program reference: $reference",
            Present(reference)
          )
        )
      case None if "(?m)(?:^|[;&|]\\s*)command\\s+-p\\b".r.findFirstIn(executableText).nonEmpty =>
        Left(
          SquireError.Failure(
            "mise-policy",
            "task preflight rejected command -p PATH escape",
            Present("command -p")
          )
        )
      case None => Right(())

  private def runTaskScript(
      script: Path,
      scriptText: String,
      expectedPrograms: Set[String],
      executionStarted: () => Unit = () => ()
  ): Chunk[TaskInvocation] < (Async & Sync & Abort[SquireError]) =
    validateTaskScript(scriptText) match
      case Left(error) => Abort.fail(error)
      case Right(_) =>
        Scope.run {
          for
            root <- SquireFixtures.scopedScratch(s"task-${script.toJava.getFileName}")
            log <- Sync.defer {
              val bin = root / "bin"
              val log = root / "task-invocations.bin"
              Files.createDirectories(bin.toJava)
              SquireLauncherFixtures.executable((root / "task-script").toJava, scriptText)
              SquireLauncherFixtures.executable((root / "mill").toJava, fakeTaskTool("mill"))
              List("bun", "npm", "npx").foreach(program =>
                SquireLauncherFixtures.executable((bin / program).toJava, fakeTaskTool(program))
              )
              SquireLauncherFixtures.executable((bin / "bash").toJava, "#!/bin/sh\nexec /usr/bin/bash \"$@\"\n")
              log
            }
            _ <- Sync.defer(executionStarted())
            result <- LiveProcessRunner.run(
              ProcessRequest(
                Chunk(
                  "/usr/bin/env",
                  s"PATH=${(root / "bin").toString}",
                  s"SQUIRE_TASK_LOG=${log.toString}",
                  s"SQUIRE_APPROVED_TASK_PROGRAMS=${expectedPrograms.toList.sorted.mkString(",")}",
                  "bash",
                  (root / "task-script").toString
                ),
                Present(root)
              )
            )
            _ <- result match
              case ProcessResult(_, 0, _, _) => Sync.defer(())
              case _ =>
                Abort.fail(
                  SquireError.Failure(
                    "mise-policy",
                    s"task script ${script.toString} failed with exit ${result.exitCode}: ${result.stderr.trim}",
                    Present(result.stderr.trim)
                  )
                )
            decoded <- Sync.defer(decodeTaskInvocations(log))
            invocations <- decoded match
              case Right(value) => Sync.defer(value)
              case Left(error)  => Abort.fail(error)
          yield invocations
        }

  private def fakeTaskTool(program: String): String =
    s"""#!/bin/sh
set -eu
printf '%s\\0%s\\0' '$program' "$$#" >> "$$SQUIRE_TASK_LOG"
for argument in "$$@"; do
  printf '%s\\0' "$$argument" >> "$$SQUIRE_TASK_LOG"
done
printf '\\n' >> "$$SQUIRE_TASK_LOG"
case ",$${SQUIRE_APPROVED_TASK_PROGRAMS}," in
  *,$program,*) exit 0 ;;
  *) echo "unapproved program: $program" >&2; exit 97 ;;
esac
"""

  private def decodeTaskInvocations(log: Path): Either[SquireError, Chunk[TaskInvocation]] =
    try
      val bytes = Files.readAllBytes(log.toJava)
      var offset = 0
      val decoded = scala.collection.mutable.ArrayBuffer.empty[TaskInvocation]

      def readField(): String =
        val start = offset
        while offset < bytes.length && bytes(offset) != 0 do offset += 1
        if offset == bytes.length then throw new IllegalArgumentException("unterminated task invocation field")
        val field = new String(bytes, start, offset - start, StandardCharsets.UTF_8)
        offset += 1
        field

      while offset < bytes.length do
        val program = readField()
        val count = readField().toIntOption.getOrElse(throw new IllegalArgumentException("invalid task argument count"))
        val arguments = Chunk.from(List.fill(count)(readField()))
        if offset == bytes.length || bytes(offset) != '\n' then
          throw new IllegalArgumentException("task invocation record is missing its newline terminator")
        offset += 1
        decoded += TaskInvocation(program, arguments)
      Right(Chunk.from(decoded))
    catch
      case error: Exception =>
        Left(SquireError.Failure("mise-policy", "could not decode task invocation record", Present(error.getMessage)))

  private def failureContains[A](outcome: Result[SquireError, A], fragments: String*): Boolean =
    outcome match
      case Result.Failure(error) => fragments.forall(error.getMessage.contains)
      case Result.Success(_)     => false

  private def taskScratchRoots(scriptName: String): Set[Path] =
    val temporaryRoot = java.nio.file.Path.of(java.lang.System.getProperty("java.io.tmpdir"))
    val prefix        = s"squire-task-$scriptName-"
    val stream        = Files.list(temporaryRoot)
    try
      stream.iterator.asScala.filter(path => path.getFileName.toString.startsWith(prefix))
        .map(path => Path(path.toString)).toSet
    finally stream.close()

  private def morphirElmManifests: List[Path] =
    val examples = repositoryRoot / "examples" / "morphir-elm-projects"
    val stream = Files.walk(examples.toJava)
    try
      (repositoryRoot / "package.json") :: stream.iterator.asScala
        .filter(path => path.getFileName.toString == "package.json")
        .map(path => Path(path.toString))
        .toList
    finally stream.close()

  private def packageManifestIsSafe(json: String): Boolean =
    SquireJson.decode[Structure.Value](json) match
      case Result.Success(value) =>
        !recordFieldContains(value, "devDependencies", "morphir-elm") &&
          !recordFieldContains(value, "scripts", "make")
      case Result.Failure(_) => false

  private def recordFieldContains(value: Structure.Value, field: String, key: String): Boolean =
    value match
      case Structure.Value.Record(fields) =>
        fields.collect { case (`field`, Structure.Value.Record(entries)) => entries }
          .exists(_.exists(_._1 == key))
      case _ => false

  private def stringField(json: String, name: String): String =
    val pattern = ("\\\"" + java.util.regex.Pattern.quote(name) + "\\\"\\s*:\\s*\\\"([^\\\"]*)\\\"").r
    pattern.findFirstMatchIn(json).map(_.group(1)).getOrElse(throw new AssertionError(s"missing JSON field: $name"))

  private def stringArray(json: String, name: String): List[String] =
    val pattern = ("(?s)\\\"" + java.util.regex.Pattern.quote(name) + "\\\"\\s*:\\s*\\[(.*?)\\]").r
    val body    = pattern.findFirstMatchIn(json).map(_.group(1))
      .getOrElse(throw new AssertionError(s"missing JSON array: $name"))
    "\\\"([^\\\"]+)\\\"".r.findAllMatchIn(body).map(_.group(1)).toList

  "Mise task policy" - {
    "resolves Squire metadata and the exact local CI dependency list" in {
      for
        ciInfo     <- runMise("task", "info", "ci:local", "--json")
        squireInfo <- runMise("task", "info", "test:squire", "--json")
        dryRun     <- runMise("run", "--dry-run", "ci:local")
      yield assert(
        ciInfo.exitCode == 0 && squireInfo.exitCode == 0 && dryRun.exitCode == 0 &&
          stringArray(ciInfo.stdout, "depends") == expectedDependencies &&
          stringField(ciInfo.stdout, "description") == "Run the core CI workflow locally" &&
          stringField(squireInfo.stdout, "description") == "Test Squire commands and build/release policy" &&
          (dryRun.stdout + dryRun.stderr).contains("test:squire")
      )
    }

    "runs Elm build and setup scripts through only their approved tools" in {
      for
        buildElm <- runTaskScript(
          buildElmScript,
          Files.readString(buildElmScript.toJava, StandardCharsets.UTF_8),
          Set("mill")
        )
        buildEvaluator <- runTaskScript(
          buildEvaluatorScript,
          Files.readString(buildEvaluatorScript.toJava, StandardCharsets.UTF_8),
          Set("mill")
        )
        setup <- runTaskScript(setupScript, Files.readString(setupScript.toJava, StandardCharsets.UTF_8), Set("bun"))
      yield assert(buildElm == expectedBuildElm && buildEvaluator == expectedBuildEvaluator && setup == expectedSetup)
    }

    "runs every local-CI Morphir capability through its dedicated Mill invocation" in {
      runTaskScript(
        localCiScript,
        Files.readString(localCiScript.toJava, StandardCharsets.UTF_8),
        Set("mill")
      ).map(invocations => assert(invocations == expectedLocalCi))
    }

    "rejects executed task mutations that add package tooling or change the approved invocation sequences" in {
      val buildElmText  = Files.readString(buildElmScript.toJava, StandardCharsets.UTF_8)
      val setupText     = Files.readString(setupScript.toJava, StandardCharsets.UTF_8)
      val localCiText   = Files.readString(localCiScript.toJava, StandardCharsets.UTF_8)
      val addBun        = buildElmText + "\nbun install\n"
      val addNpm        = buildElmText + "\nnpm install\n"
      val removeSdk     = buildElmText.replace("    + \"morphir-elm.sdks.__.morphirIR\"\n", "")
      val broadUnit     = localCiText.replace(
        "mill-plugins.morphir.{toolchain,javascript,elm-tooling,core,elm}.__.test",
        "mill-plugins.morphir.__.test"
      )
      val collapseLocal = localCiText.replace(
        "./mill -i morphir.runtime.classic.jvm.test.generatedRuntimeFixtures\n" +
          "./mill -i morphir.runtime.classic.jvm.test.verifyRuntimeTestDiscovery\n",
        "./mill -i morphir.runtime.classic.jvm.test\n"
      )
      val enableHooks = setupText.replace(" --ignore-scripts", "")
      for
        bunResult <- Abort.run[SquireError](runTaskScript(buildElmScript, addBun, Set("mill")))
        npmResult <- Abort.run[SquireError](runTaskScript(buildElmScript, addNpm, Set("mill")))
        withoutSdk <- runTaskScript(buildElmScript, removeSdk, Set("mill"))
        broadUnitInvocations <- runTaskScript(localCiScript, broadUnit, Set("mill"))
        collapsedInvocations <- runTaskScript(localCiScript, collapseLocal, Set("mill"))
        hooksEnabled <- runTaskScript(setupScript, enableHooks, Set("bun"))
      yield assert(
        failureContains(bunResult, "unapproved program", "bun") &&
          failureContains(npmResult, "unapproved program", "npm") &&
          withoutSdk != expectedBuildElm &&
          broadUnitInvocations != expectedLocalCi &&
          collapsedInvocations != expectedLocalCi &&
          hooksEnabled != expectedSetup
      )
    }

    "cleans task harness scratch after success and Abort" in {
      val successName = "cleanup-success"
      val abortName   = "cleanup-abort"
      for
        successBefore <- Sync.defer(taskScratchRoots(successName))
        success <- runTaskScript(
          Path(successName),
          "#!/usr/bin/env bash\n./mill clean\n",
          Set("mill")
        )
        successAfter <- Sync.defer(taskScratchRoots(successName))
        abortBefore  <- Sync.defer(taskScratchRoots(abortName))
        aborted <- Abort.run[SquireError](
          runTaskScript(
            Path(abortName),
            "#!/usr/bin/env bash\nnpm install\n",
            Set("mill")
          )
        )
        abortAfter = taskScratchRoots(abortName)
        successLeaks = successAfter -- successBefore
        abortLeaks   = abortAfter -- abortBefore
        _ <- Sync.defer((successLeaks ++ abortLeaks).foreach(SquireFixtures.deleteRecursively))
      yield assert(
        success == Chunk(TaskInvocation("mill", Chunk("clean"))) &&
          aborted.isFailure && successLeaks.isEmpty && abortLeaks.isEmpty
      )
    }

    "rejects absolute package-tool escapes before execution or recording" in {
      for
        escapes = Chunk(
          ("npm", "/usr/bin/npm"),
          ("bun", "/home/linuxbrew/.linuxbrew/bin/bun"),
          ("npx", "/usr/bin/npx")
        )
        variants = escapes.flatMap { case (program, absoluteProgram) =>
          Chunk(
            (program, absoluteProgram, "with-shebang", s"#!/usr/bin/env bash\n$absoluteProgram --version\n"),
            (program, absoluteProgram, "without-shebang", s"$absoluteProgram --version\n")
          )
        }
        approvedShebangAccepted = validateTaskScript("#!/usr/bin/env bash\n./mill clean\n").isRight
        proofs <- Kyo.foreach(variants) { case (program, _, variant, scriptText) =>
          val scriptName = s"absolute-$program-$variant"
          val preflight = validateTaskScript(scriptText)
          var executionStarted = false
          preflight match
            case Left(_) =>
              for
                before <- Sync.defer(taskScratchRoots(scriptName))
                result <- Abort.run[SquireError](
                  runTaskScript(Path(scriptName), scriptText, Set(program), () => executionStarted = true)
                )
                after <- Sync.defer(taskScratchRoots(scriptName))
              yield failureContains(result, "absolute program reference", program) &&
                !executionStarted && after == before
            case Right(_) => Sync.defer(false)
        }
      yield assert(approvedShebangAccepted && proofs.forall(identity))
    }

    "semantically rejects forbidden Morphir Elm package manifest fields" in {
      val forbiddenDevDependency = """{"devDependencies":{"morphir-elm":"1.0.0"},"scripts":{}}"""
      val forbiddenScript = """{"devDependencies":{},"scripts":{"make":"make"}}"""
      assert(
        morphirElmManifests.forall(path => packageManifestIsSafe(Files.readString(path.toJava, StandardCharsets.UTF_8))) &&
          !packageManifestIsSafe(forbiddenDevDependency) &&
          !packageManifestIsSafe(forbiddenScript)
      )
    }
  }

class SquireMigrationSpec extends Test[Any]:
  private val skillDirectory      = java.nio.file.Paths.get(java.lang.System.getProperty("user.dir"))
  private val repositoryRoot      = skillDirectory.resolve("../../..").normalize
  private val maintainedConsumers = List(
    ".claude/skills/squire/SKILL.md",
    ".claude/skills/squire/README.md",
    ".claude/skills/squire/references/branch.md",
    ".claude/skills/squire/references/cellar.md",
    ".claude/skills/squire/references/doctor.md",
    ".claude/skills/squire/references/env.md",
    ".claude/skills/squire/references/repo.md",
    ".claude/skills/squire/references/spec-sync.md",
    ".claude/skills/squire/references/tracking.md",
    ".config/mise/tasks/test/squire",
    ".config/mise/tasks/schemas/build",
    ".config/mise/tasks/schemas/check",
    ".config/mise/tasks/ci/local",
    ".config/mise/config.toml",
    ".config/squire/settings.local.yaml.template",
    ".github/workflows/ci.yml",
    "scripts/lib/mill-flags.sh",
    "AGENTS.md",
    "CLAUDE.md",
    "CONTRIBUTING.md",
    "docs/task-tracking.md",
    "kb/bundles/morphir/morphir-ir-v4-draft/schema-conformance.md"
  )

  "migration completeness" - {
    "removes every Python and TypeScript implementation and test from Squire" in {
      val stream      = Files.walk(skillDirectory)
      val legacyFiles =
        try stream.iterator.asScala.filter { path =>
            val rendered = path.toString
            rendered.endsWith(".py") || rendered.endsWith(".ts")
          }.map(skillDirectory.relativize).map(_.toString).toList.sorted
        finally stream.close()
      if legacyFiles.nonEmpty then
        throw new AssertionError(s"legacy Squire files remain: ${legacyFiles.mkString(", ")}")
      assert(legacyFiles.isEmpty)
    }

    "routes every maintained consumer through the unified launcher" in {
      val maintainedConsumerText = maintainedConsumers.map { relative =>
        relative -> Files.readString(repositoryRoot.resolve(relative), StandardCharsets.UTF_8)
      }
      val stale = List(
        "python3 .claude/skills/squire",
        "${CLAUDE_PLUGIN_ROOT}/scripts/",
        "bun .claude/skills/squire",
        "schemas-to-json.ts"
      ).flatMap { token =>
        maintainedConsumerText.collect { case (path, text) if text.contains(token) => s"$path: $token" }
      }
      if stale.nonEmpty then throw new AssertionError(s"stale Squire invocations remain: ${stale.mkString(", ")}")
      assert(
        !maintainedConsumerText.exists(_._2.contains("python3 .claude/skills/squire")) &&
          !maintainedConsumerText.exists(_._2.contains("${CLAUDE_PLUGIN_ROOT}/scripts/")) &&
          !maintainedConsumerText.exists(_._2.contains("bun .claude/skills/squire")) &&
          !maintainedConsumerText.exists(_._2.contains("schemas-to-json.ts"))
      )
    }

    "keeps the documented schema parity command on the compare parser" in {
      val evidence = Files.readString(
        repositoryRoot.resolve("kb/bundles/morphir/morphir-ir-v4-draft/schema-conformance.md"),
        StandardCharsets.UTF_8
      )
      val documentedSource = ".refs/finos/morphir/website/static/schemas"
      val parsed           = Parser[SchemasCompareOpts].parse(Seq("--from", documentedSource))
      assert(
        evidence.contains(
          s"`.claude/skills/squire/squire schemas compare --from $documentedSource`"
        ) &&
          !evidence.contains("schemas build --check") &&
          parsed == Right((SchemasCompareOpts(from = Some(documentedSource)), Seq.empty)) &&
          SquireApp.SchemasCompareCmd.names.contains(List("schemas", "compare"))
      )
    }
  }

class SquireBranchSpec extends Test[Any]:
  import SquireBranchFixtures.*

  "CLI options" - {
    "default to develop without dry-run or JSON" in {
      val parsed = Parser[BranchRefreshOpts].parse(Seq.empty)
      assert(parsed == Right((BranchRefreshOpts(), Seq.empty)))
    }

    "accept a named target and reject a bare positional target before any process" in {
      val named      = Parser[BranchRefreshOpts].parse(Seq("--target", "release-line"))
      val positional = SquireApp.BranchRefreshCmd.parser.detailedParse(Seq("release-line"))
      positional match
        case Left(_)                     => assert(false)
        case Right((options, remaining)) =>
          val runner = BranchRecordingRunner(Map.empty)
          Abort.run[SquireError](
            SquireCli.runBranch(options, remaining.all, runner, _ => ())
          ).map { outcome =>
            assert(
              named == Right((BranchRefreshOpts(target = "release-line"), Seq.empty)) &&
                failureContains(outcome, "unexpected positional arguments", "release-line") &&
                runner.requests.isEmpty
            )
          }
    }
  }

  "proof pipeline" - {
    "short-circuits equal remote refs before GitHub and never pushes" in {
      val sha    = "a" * 40
      val runner = BranchRecordingRunner(
        baseResponses.updated(resolveSource, ok(resolveSource, sha + "\n"))
          .updated(resolveTarget, ok(resolveTarget, sha + "\n"))
      )
      Abort.run[SquireError](SquireBranch.refresh("develop", dryRun = false, runner)).map { outcome =>
        assert(
          outcome == Result.Success(RefreshResult("already-current", "develop", sha, sha, Absent)) &&
            !runner.requests.exists(_.argv.headOption.contains("gh")) &&
            neverPushed(runner)
        )
      }
    }

    "fetches both exact remote-tracking refspecs in a single-branch clone" in {
      for
        root <- SquireFixtures.scratch("branch-fetch")
        origin = root / "origin.git"
        seed   = root / "seed"
        clone  = root / "clone"
        _ <- Sync.defer {
          git(root, "init", "--bare", origin.toString)
          git(root, "init", "-b", "main", seed.toString)
          git(seed, "config", "user.name", "Branch Refresh Test")
          git(seed, "config", "user.email", "branch-refresh@example.invalid")
          Files.writeString((seed / "main.txt").toJava, "main one\n")
          git(seed, "add", "main.txt")
          git(seed, "commit", "-m", "main one")
          git(seed, "remote", "add", "origin", origin.toString)
          git(seed, "push", "-u", "origin", "main")
          git(seed, "switch", "-c", "develop")
          Files.writeString((seed / "develop.txt").toJava, "develop one\n")
          git(seed, "add", "develop.txt")
          git(seed, "commit", "-m", "develop one")
          git(seed, "push", "-u", "origin", "develop")
          git(root, "clone", "--single-branch", "--branch", "main", origin.toString, clone.toString)
          git(seed, "switch", "main")
          Files.writeString((seed / "main.txt").toJava, "main two\n")
          git(seed, "add", "main.txt")
          git(seed, "commit", "-m", "main two")
          git(seed, "push", "origin", "main")
          git(seed, "switch", "develop")
          Files.writeString((seed / "develop.txt").toJava, "develop two\n")
          git(seed, "add", "develop.txt")
          git(seed, "commit", "-m", "develop two")
          git(seed, "push", "origin", "develop")
        }
        mainTip     = git(origin, "rev-parse", "refs/heads/main").trim
        developTip  = git(origin, "rev-parse", "refs/heads/develop").trim
        pullRequest =
          s"""[{"number":42,"headRefOid":"$developTip","mergeCommit":{"oid":"$mainTip"}}]"""
        runner = LocalGitBranchRunner(clone, Map(repoView -> "finos/morphir-scala\n", prList -> pullRequest))
        outcome <- Abort.run[SquireError](SquireBranch.refresh("develop", dryRun = true, runner))
        fetchedMain    = git(clone, "rev-parse", "refs/remotes/origin/main").trim
        fetchedDevelop = git(clone, "rev-parse", "refs/remotes/origin/develop").trim
      yield assert(
        outcome == Result.Success(RefreshResult("validated", "develop", developTip, mainTip, Present(42))) &&
          fetchedMain == mainTip && fetchedDevelop == developTip &&
          runner.requests.map(_.argv).contains(fetch)
      )
    }

    "rejects each pre-proof command failure with operation and PR context before push" in {
      val cases = Chunk(
        (checkTarget, "validate target branch", "invalid ref"),
        (fetch, "fetch origin branches", "network unavailable"),
        (resolveSource, "resolve remote refs", "missing origin main")
      )
      Kyo.foreach(cases) { case (command, operation, detail) =>
        val runner = BranchRecordingRunner(baseResponses.updated(command, failed(command, detail)))
        Abort.run[SquireError](SquireBranch.refresh("develop", dryRun = false, runner)).map { outcome =>
          assert(
            failureContains(outcome, operation, "develop", "develop-to-main PR", detail) &&
              !runner.requests.exists(_.argv.headOption.contains("gh")) && neverPushed(runner)
          )
        }
      }
    }

    "validates an exact target head with a reachable merge without push" in {
      val runner = BranchRecordingRunner(successfulProofResponses)
      Abort.run[SquireError](SquireBranch.refresh("develop", dryRun = true, runner)).map { outcome =>
        assert(
          outcome == Result.Success(
            RefreshResult("validated", "develop", targetSha, sourceSha, Present(42))
          ) && runner.requests.map(_.argv).contains(ancestor) && neverPushed(runner)
        )
      }
    }

    "rejects every GitHub proof shape and command failure before push" in {
      val otherHead = matchingPullRequest.replace(targetSha, "4" * 40)
      val cases     = Chunk(
        (proofResponses(s"[$otherHead]"), Chunk("could not find", "head SHA exactly matches")),
        (
          proofResponses(s"[${matchingPullRequest.replace(s"{\"oid\":\"$mergeSha\"}", "null")}]"),
          Chunk("merge commit")
        ),
        (proofResponses(s"[${matchingPullRequest.replace("\"number\":42,", "")}]"), Chunk("number", "integer")),
        (
          proofResponses(s"[${matchingPullRequest.replace("\"number\":42", "\"number\":\"42\"")}]"),
          Chunk("number", "integer")
        ),
        (proofResponses("not json"), Chunk("JSON")),
        (proofResponses("{\"number\":42}"), Chunk("array")),
        (
          proofResponses("[]").updated(repoView, failed(repoView, "gh unavailable")),
          Chunk("identify repository", "gh unavailable")
        ),
        (
          proofResponses("[]").updated(prList, failed(prList, "PR listing unavailable")),
          Chunk("list merged PRs", "PR listing unavailable")
        )
      )
      Kyo.foreach(cases) { case (responses, fragments) =>
        val runner = BranchRecordingRunner(responses)
        Abort.run[SquireError](SquireBranch.refresh("develop", dryRun = true, runner)).map { outcome =>
          assert(
            failureContains(outcome, (Chunk("develop", "develop-to-main PR") ++ fragments)*) &&
              neverPushed(runner)
          )
        }
      }
    }

    "rejects unreachable merge ancestry before push" in {
      val runner = BranchRecordingRunner(
        proofResponses(s"[$matchingPullRequest]").updated(ancestor, failed(ancestor, "not an ancestor"))
      )
      Abort.run[SquireError](SquireBranch.refresh("develop", dryRun = true, runner)).map { outcome =>
        assert(
          failureContains(
            outcome,
            "verify merge ancestry",
            "develop",
            "develop-to-main PR",
            "origin/main",
            "not an ancestor"
          ) && neverPushed(runner)
        )
      }
    }

    "rejects main as a target without invoking a process" in {
      val runner = BranchRecordingRunner(Map.empty)
      Abort.run[SquireError](SquireBranch.refresh("main", dryRun = false, runner)).map { outcome =>
        assert(failureContains(outcome, "target branch must not be main") && runner.requests.isEmpty)
      }
    }
  }

  "leased update boundary" - {
    "uses the exact force-with-lease request only after every proof" in {
      val runner = BranchRecordingRunner(successfulProofResponses.updated(push, ok(push)))
      Abort.run[SquireError](SquireBranch.refresh("develop", dryRun = false, runner)).map { outcome =>
        assert(
          outcome == Result.Success(RefreshResult("updated", "develop", targetSha, sourceSha, Present(42))) &&
            runner.requests.map(_.argv) == Chunk(
              checkTarget,
              fetch,
              resolveSource,
              resolveTarget,
              repoView,
              prList,
              ancestor,
              push
            )
        )
      }
    }

    "surfaces a lease rejection without retry or unleased force" in {
      val runner = BranchRecordingRunner(successfulProofResponses.updated(push, failed(push, "lease rejected")))
      Abort.run[SquireError](SquireBranch.refresh("develop", dryRun = false, runner)).map { outcome =>
        val pushes = runner.requests.map(_.argv).filter(_.take(2) == Chunk("git", "push"))
        assert(
          failureContains(outcome, "push leased update", "develop", "develop-to-main PR", "lease rejected") &&
            pushes == Chunk(push)
        )
      }
    }
  }

class SquireModelSpec extends Test[Any]:
  "JSON" - {
    "pretty JSON preserves field order and escapes control characters" in {
      val value = Structure.Value.Record(
        Chunk(
          "z" -> Structure.Value.Str("line\n\"quoted\""),
          "a" -> Structure.Value.Sequence(
            Chunk(Structure.Value.Integer(1), Structure.Value.Bool(true))
          )
        )
      )
      assert(
        SquireJson.pretty(value) == "{\n  \"z\": \"line\\n\\\"quoted\\\"\",\n  \"a\": [\n    1,\n    true\n  ]\n}\n"
      )
    }

    "rejects Bytes as non-deterministic JSON" in
      assert(isRejectedAsNonDeterministicJson(Structure.Value.Bytes(Span.from(Array[Byte](1, 2, 3)))))

    "rejects Instant as non-deterministic JSON" in
      assert(isRejectedAsNonDeterministicJson(Structure.Value.Instant(java.time.Instant.EPOCH)))

    "rejects Duration as non-deterministic JSON" in
      assert(isRejectedAsNonDeterministicJson(Structure.Value.Duration(java.time.Duration.ZERO)))
  }

  "paths" - {
    "resolveUnder rejects a sibling prefix" in {
      val base    = Path("/tmp/squire-path-test/.refs")
      val sibling = Path("/tmp/squire-path-test/.refs-escaped/repo")
      assert(SquirePaths.resolveUnder(sibling, base).isFailure)
    }

    "resolveUnder rejects an in-base symlink that escapes" in {
      for
        root <- SquireFixtures.scratch("path")
        base    = root / ".refs"
        outside = root / "outside"
        link    = base / "link-outside"
        _ <- Sync.defer {
          Files.createDirectories(base.toJava)
          Files.createDirectories(outside.toJava)
          Files.createSymbolicLink(link.toJava, outside.toJava)
        }
        result = SquirePaths.resolveUnder(link / "repo", base)
      yield assert(result.isFailure)
    }
  }

  private def isRejectedAsNonDeterministicJson(value: Structure.Value): Boolean =
    try
      SquireJson.pretty(value)
      false
    catch
      case SquireError.Failure(area, message, _) =>
        area == "json" && message == "value cannot be represented as deterministic JSON"

class SquireSchemasSpec extends Test[Any]:
  private val skillDirectory = java.nio.file.Paths.get(java.lang.System.getProperty("user.dir"))

  "conversion" - {
    "preserves YAML scalar types, order, Unicode, escapes, mappings, and sequences while rewriting only top-level id" in {
      val yaml =
        """$id: https://example.test/schema.yaml
          |nested:
          |  $id: keep.yaml
          |nothing: null
          |yes: true
          |no: false
          |integer: 42
          |negative: -7
          |decimal: 1.25
          |exponential: 1.2e3
          |unicode: "λ café"
          |escaped: "line\n\"quoted\"\\slash"
          |mapping:
          |  z: first
          |  a: second
          |sequence:
          |  - null
          |  - false
          |  - 3
          |  - 2.5
          |""".stripMargin
      val expected =
        """{
          |  "$id": "https://example.test/schema.json",
          |  "nested": {
          |    "$id": "keep.yaml"
          |  },
          |  "nothing": null,
          |  "yes": true,
          |  "no": false,
          |  "integer": 42,
          |  "negative": -7,
          |  "decimal": 1.25,
          |  "exponential": 1200,
          |  "unicode": "λ café",
          |  "escaped": "line\n\"quoted\"\\slash",
          |  "mapping": {
          |    "z": "first",
          |    "a": "second"
          |  },
          |  "sequence": [
          |    null,
          |    false,
          |    3,
          |    2.5
          |  ]
          |}
          |""".stripMargin
      assert(SquireSchemas.convert(yaml) == Result.Success(expected))
    }

    "matches Bun JSON.stringify bytes for every legacy numeric boundary" in {
      val yaml =
        """$id: https://example.test/numeric.yaml
          |ordinaryInt: 42
          |ordinaryNegative: -7
          |ordinaryDecimal: 1.25
          |integralDecimal: 1.0
          |ordinaryExponent: 1.2e3
          |lowerFixed: 1e-6
          |lowerScientific: 1e-7
          |upperFixed: 1e20
          |upperScientific: 1E21
          |explicitPositiveExponent: 1E+7
          |upperNegativeExponent: 1E-7
          |positiveZero: 0
          |negativeZero: -0
          |negativeDecimalZero: -0.0
          |negativeExponentZero: -0e5
          |unsafeInteger: 9007199254740993
          |beyondLong: 9223372036854775808
          |notANumber: .nan
          |positiveInfinity: .inf
          |negativeInfinity: -.inf
          |""".stripMargin
      val expected =
        """{
          |  "$id": "https://example.test/numeric.json",
          |  "ordinaryInt": 42,
          |  "ordinaryNegative": -7,
          |  "ordinaryDecimal": 1.25,
          |  "integralDecimal": 1,
          |  "ordinaryExponent": 1200,
          |  "lowerFixed": 0.000001,
          |  "lowerScientific": 1e-7,
          |  "upperFixed": 100000000000000000000,
          |  "upperScientific": 1e+21,
          |  "explicitPositiveExponent": 10000000,
          |  "upperNegativeExponent": 1e-7,
          |  "positiveZero": 0,
          |  "negativeZero": 0,
          |  "negativeDecimalZero": 0,
          |  "negativeExponentZero": 0,
          |  "unsafeInteger": 9007199254740992,
          |  "beyondLong": 9223372036854776000,
          |  "notANumber": null,
          |  "positiveInfinity": null,
          |  "negativeInfinity": null
          |}
          |""".stripMargin

      assert(scala.util.Try(SquireSchemas.convert(yaml)).toOption == Some(Result.Success(expected)))
    }

    "returns malformed conversions as typed failures" in {
      val result = scala.util.Try(SquireSchemas.convert("value: !!int nope\n")).toOption
      assert(result.exists(_.isFailure))
    }

    "keeps overflow recovery compatible with YAML anchors and aliases" in {
      val yaml     = "value: &big 9223372036854775808\nalias: *big\n"
      val expected =
        """{
          |  "value": 9223372036854776000,
          |  "alias": 9223372036854776000
          |}
          |""".stripMargin
      assert(SquireSchemas.convert(yaml) == Result.Success(expected))
    }

    "matches Bun bytes for non-finite numeric anchors and every alias shape" in {
      val huge = "9" * 400
      val yaml =
        s"""positive: &positive $huge
           |positiveAlias: *positive
           |nested:
           |  positiveAlias: *positive
           |sequence:
           |  - *positive
           |  - &negative -$huge
           |  - *negative
           |negativeAlias: *negative
           |negativeNested:
           |  value: *negative
           |""".stripMargin
      val expected =
        """{
          |  "positive": null,
          |  "positiveAlias": null,
          |  "nested": {
          |    "positiveAlias": null
          |  },
          |  "sequence": [
          |    null,
          |    null,
          |    null
          |  ],
          |  "negativeAlias": null,
          |  "negativeNested": {
          |    "value": null
          |  }
          |}
          |""".stripMargin

      assert(scala.util.Try(SquireSchemas.convert(yaml)).toOption == Some(Result.Success(expected)))
    }

    "matches the committed v4 golden bytes" in {
      val yaml = Files.readString(skillDirectory.resolve("test-resources/schemas/morphir-ir-v4.yaml"))
      val json = Files.readString(skillDirectory.resolve("test-resources/schemas/morphir-ir-v4.json"))
      assert(SquireSchemas.convert(yaml) == Result.Success(json))
    }

    "matches every available reference-checkout IR golden" in {
      val root      = skillDirectory.resolve("../../..").normalize
      val mirror    = root.resolve("kb/bundles/morphir/morphir-upstream/sources/website/static/schemas")
      val reference = root.resolve(".refs/finos/morphir/website/static/schemas")
      if !Files.isDirectory(reference) then assert(true)
      else
        val stream = Files.list(mirror)
        try
          val names = stream.iterator.asScala
            .map(_.getFileName.toString)
            .filter(name => name.startsWith("morphir-ir-") && name.endsWith(".yaml"))
            .toList
            .sorted
          assert(names.forall { name =>
            val target = reference.resolve(name.stripSuffix(".yaml") + ".json")
            Files.isRegularFile(target) &&
            SquireSchemas.convert(Files.readString(mirror.resolve(name))) == Result.Success(Files.readString(target))
          })
        finally stream.close()
    }
  }

  "build and compare" - {
    "selects IR-only or all Morphir YAML in basename order and creates output directories" in {
      for
        root <- SquireFixtures.scratch("schemas-build")
        source = root / "source"
        out    = root / "nested" / "generated"
        allOut = root / "all"
        _ <- Sync.defer {
          Files.createDirectories(source.toJava)
          Files.writeString((source / "morphir-ir-z.yaml").toJava, "z: 1\n")
          Files.writeString((source / "morphir-config-v1.yaml").toJava, "config: true\n")
          Files.writeString((source / "morphir-ir-a.yaml").toJava, "a: 2\n")
          Files.writeString((source / "ignored.yaml").toJava, "ignored: true\n")
        }
        ir  <- SquireSchemas.build(source, out, all = false)
        all <- SquireSchemas.build(source, allOut, all = true)
      yield assert(
        ir.outcomes.map(_.file) == List("morphir-ir-a.json", "morphir-ir-z.json") &&
          ir.outcomes.forall(_.status == "written") && Files.isDirectory(out.toJava) &&
          all.outcomes.map(_.file) == List("morphir-config-v1.json", "morphir-ir-a.json", "morphir-ir-z.json")
      )
    }

    "reports identical, drifted, missing, and the all-missing summary with nonzero comparison status" in {
      for
        root <- SquireFixtures.scratch("schemas-compare")
        source  = root / "source"
        target  = root / "target"
        missing = root / "missing"
        _ <- Sync.defer {
          Files.createDirectories(source.toJava)
          Files.createDirectories(target.toJava)
          Files.createDirectories(missing.toJava)
          Files.writeString((source / "morphir-ir-a.yaml").toJava, "a: 1\n")
          Files.writeString((source / "morphir-ir-b.yaml").toJava, "b: 2\n")
          Files.writeString((source / "morphir-config-v1.yaml").toJava, "config: true\n")
          Files.writeString((target / "morphir-ir-a.json").toJava, "{\n  \"a\": 1\n}\n")
          Files.writeString((target / "morphir-ir-b.json").toJava, "{}\n")
        }
        compared   <- SquireSchemas.compare(source, target, all = true)
        allMissing <- SquireSchemas.compare(source, missing, all = false)
      yield assert(
        compared.outcomes.map(outcome => outcome.file -> outcome.status) == List(
          "morphir-config-v1.json" -> "missing",
          "morphir-ir-a.json"      -> "identical",
          "morphir-ir-b.json"      -> "drifted"
        ) && !compared.ok && SquireSchemas.exitCode(compared) == 1 &&
          allMissing.outcomes.forall(_.status == "missing") &&
          SquireSchemas.renderText(allMissing).contains("no generated JSON under")
      )
    }

    "fails explicitly when no input names match" in {
      for
        root <- SquireFixtures.scratch("schemas-empty")
        source = root / "source"
        _      <- Sync.defer(Files.createDirectories(source.toJava))
        result <- Abort.run[SquireError](SquireSchemas.build(source, root / "out", all = false))
      yield assert(result.isFailure)
    }
  }

  "validation" - {
    "fails when jsonschema is unavailable" in {
      for
        root <- SquireFixtures.scratch("schemas-no-jsonschema")
        runner = RuleRunner(request => ProcessResult(request, 127, "", "not found"))
        result <- Abort.run[SquireError](SquireSchemas.validate(root, root, root, runner))
      yield assert(result.isFailure && runner.requests.map(_.argv) == Chunk(Chunk("jsonschema", "--version")))
    }

    "distinguishes an incompatible validator command from an invalid schema" in {
      for
        root <- SquireFixtures.scratch("schemas-incompatible-jsonschema")
        yaml = root / "morphir-ir-v4.yaml"
        _ <- Sync.defer(Files.writeString(yaml.toJava, "type: object\n"))
        runner = RuleRunner { request =>
          if request.argv == Chunk("/tools/jsonschema", "--version") then ProcessResult(request, 0, "4.10.3\n", "")
          else ProcessResult(request, 2, "", "usage: jsonschema\njsonschema: error: unrecognized arguments")
        }
        result <- Abort.run[SquireError](
          SquireSchemas.validate(root, root, root, runner, jsonschema = "/tools/jsonschema")
        )
      yield assert(result.isFailure && runner.requests.map(_.argv) == Chunk(
        Chunk("/tools/jsonschema", "--version"),
        Chunk("/tools/jsonschema", "metaschema", yaml.toString)
      ))
    }

    "reports sorted metaschema and document outcomes, skips non-documents, and keeps validation non-gating" in {
      for
        root <- SquireFixtures.scratch("schemas-validate")
        yaml      = root / "yaml"
        generated = root / "generated"
        documents = root / "documents"
        _ <- Sync.defer {
          Files.createDirectories(yaml.toJava)
          Files.createDirectories(generated.toJava)
          Files.createDirectories(documents.toJava)
          Files.writeString((yaml / "morphir-ir-z.yaml").toJava, "type: object\n")
          Files.writeString((yaml / "morphir-ir-a.yaml").toJava, "type: object\n")
          Files.writeString((generated / "morphir-ir-v4.json").toJava, "{}\n")
          Files.writeString((documents / "a-invalid.json").toJava, "{\"formatVersion\":\"4.2\"}\n")
          Files.writeString((documents / "b-malformed.json").toJava, "{not-json")
          Files.writeString((documents / "c-array.json").toJava, "[]\n")
          Files.writeString((documents / "d-missing-schema.json").toJava, "{\"formatVersion\":\"9.0\"}\n")
          Files.writeString((documents / "z-valid.json").toJava, "{\"formatVersion\":4}\n")
        }
        runner = RuleRunner { request =>
          val name = request.argv.lastOption.getOrElse("")
          if request.argv == Chunk("jsonschema", "--version") then ProcessResult(request, 0, "v0", "")
          else if request.argv.lift(1).contains("metaschema") && name.endsWith("morphir-ir-z.yaml") then
            ProcessResult(request, 1, "", "bad schema")
          else if request.argv.lift(1).contains("validate") && name.endsWith("a-invalid.json") then
            ProcessResult(request, 2, "", s"fail: $name\nerror: Schema validation failure")
          else ProcessResult(request, 0, "", "")
        }
        report <- SquireSchemas.validate(yaml, generated, documents, runner)
      yield assert(
        report.ok && SquireSchemas.exitCode(report) == 0 &&
          report.outcomes.map(outcome => outcome.file -> outcome.status) == List(
            "morphir-ir-a.yaml"     -> "metaschema-valid",
            "morphir-ir-z.yaml"     -> "metaschema-invalid",
            "a-invalid.json"        -> "invalid",
            "b-malformed.json"      -> "skipped",
            "c-array.json"          -> "skipped",
            "d-missing-schema.json" -> "skipped",
            "z-valid.json"          -> "valid"
          ) &&
          runner.requests.exists(_.argv == Chunk(
            "jsonschema",
            "validate",
            (generated / "morphir-ir-v4.json").toString,
            (documents / "z-valid.json").toString
          )) &&
          runner.requests.forall(!_.argv.exists(_.contains("python")))
      )
    }

    "aborts when unexpected exits mimic Sourcemeta's validation failure" in {
      for
        root <- SquireFixtures.scratch("schemas-fatal-validation")
        yaml = root / "morphir-ir-v4.yaml"
        _       <- Sync.defer(Files.writeString(yaml.toJava, "type: object\n"))
        results <- Kyo.foreach(Chunk(42, 127)) { exitCode =>
          val runner = RuleRunner { request =>
            if request.argv == Chunk("jsonschema", "--version") then ProcessResult(request, 0, "v0", "")
            else
              ProcessResult(
                request,
                exitCode,
                "",
                s"fail: ${request.argv.lastOption.getOrElse("")}\nerror: Schema validation failure\n"
              )
          }
          Abort.run[SquireError](SquireSchemas.validate(root, root, root, runner))
        }
      yield assert(results.forall(_.isFailure))
    }

    "rejects every exit-2 result outside the exact target-matched Sourcemeta shape" in {
      for
        root <- SquireFixtures.scratch("schemas-exact-validation-failure")
        yaml = root / "morphir-ir-v4.yaml"
        _       <- Sync.defer(Files.writeString(yaml.toJava, "type: object\n"))
        results <- Kyo.foreach(Chunk("leading", "trailing", "mismatched", "empty", "stdout")) { shape =>
          val runner = RuleRunner { request =>
            if request.argv == Chunk("jsonschema", "--version") then ProcessResult(request, 0, "v0", "")
            else
              val target = request.argv.lastOption.getOrElse("")
              val stderr = shape match
                case "leading"    => s"fatal: transport failed\nfail: $target\nerror: Schema validation failure"
                case "trailing"   => s"fail: $target\nerror: Schema validation failure\nfatal: transport failed"
                case "mismatched" => s"fail: ${root / "unrelated.yaml"}\nerror: Schema validation failure"
                case "empty"      => "fail: \nerror: Schema validation failure"
                case _            => s"fail: $target\nerror: Schema validation failure"
              ProcessResult(request, 2, if shape == "stdout" then "unexpected stdout" else "", stderr)
          }
          Abort.run[SquireError](SquireSchemas.validate(root, root, root, runner))
        }
      yield assert(results.forall(_.isFailure))
    }

    "matches Sourcemeta's absolute normalized target for a relative request" in {
      for
        root <- SquireFixtures.scratch("schemas-relative-validation-target")
        yaml = root / "morphir-ir-v4.yaml"
        _ <- Sync.defer(Files.writeString(yaml.toJava, "type: object\n"))
        relativeRoot = Path(skillDirectory.relativize(root.toJava).toString)
        runner       = RuleRunner { request =>
          if request.argv == Chunk("jsonschema", "--version") then ProcessResult(request, 0, "v0", "")
          else
            val requested = java.nio.file.Paths.get(request.argv.lastOption.getOrElse(""))
            val target    = skillDirectory.resolve(requested).toAbsolutePath.normalize
            ProcessResult(request, 2, "", s"fail: $target\nerror: Schema validation failure")
        }
        report <- SquireSchemas.validate(relativeRoot, relativeRoot, relativeRoot, runner)
      yield assert(report.outcomes.map(_.status) == List("metaschema-invalid"))
    }

    "accepts installed Sourcemeta structured diagnostics only as complete detail triplets" in {
      for
        root <- SquireFixtures.scratch("schemas-structured-validation-failure")
        yaml = root / "morphir-ir-v4.yaml"
        _ <- Sync.defer(Files.writeString(yaml.toJava, "type: object\n"))
        runner = RuleRunner { request =>
          if request.argv == Chunk("jsonschema", "--version") then ProcessResult(request, 0, "v0", "")
          else
            val target = request.argv.lastOption.getOrElse("")
            ProcessResult(
              request,
              2,
              "",
              s"""fail: $target
                 |error: Schema validation failure
                 |  The value was expected to be of type boolean, or object but it was of type string
                 |    at instance location "/description" (line 83, column 7)
                 |    at evaluate path "/properties/description/type"
                 |  The object value was expected to validate against the defined properties subschemas
                 |    at instance location "" (line 1, column 1)
                 |    at evaluate path "/properties"
                 |""".stripMargin
            )
        }
        report <- SquireSchemas.validate(root, root, root, runner)
      yield assert(report.outcomes.map(_.status) == List("metaschema-invalid"))
    }
  }

class SquireSpecSpec extends Test[Any]:
  import SquireSpecFixtures.*

  "repository and checkout boundaries" - {
    "discovers the repository by its sibling kb launcher and reports the unified add hint" in {
      for
        root <- rootWithoutCheckout("spec-root")
        nested = root / ".claude" / "skills" / "squire" / "nested"
        _ <- Sync.defer(Files.createDirectories(nested.toJava))
        platform = TestSpecPlatform()
        found <- SquireSpec.findRepoRoot(nested, platform)
        runner = RuleRunner(unexpected)
        report <- SquireSpec.sync(SpecSyncOptions(noFetch = true), root, runner, platform)
        checkout = report.steps.find(_.step == "checkout")
      yield assert(
        found == Present(root) && !report.ok && runner.requests.isEmpty &&
          checkout.exists(step =>
            step.status == "failed" &&
              step.detail.contains("no reference checkout of finos/morphir") &&
              step.hint.exists(
                _ ==
                  "add one with:\n    squire reference repo add https://github.com/finos/morphir --sparse docs website tests/bdd wit"
              )
          ) && safe(runner)
      )
    }

    "warns for an incomplete sparse checkout but refuses prune before kb work" in {
      for
        root <- preparedRoot("spec-sparse", Chunk("docs"), schemas = false)
        checkout    = root / SquireSpec.CheckoutRel
        pruneRunner = syncRunner(root, sparse = true)
        prune <- SquireSpec.sync(SpecSyncOptions(prune = true, noFetch = true), root, pruneRunner, TestSpecPlatform())
        warningRunner = syncRunner(root, sparse = true)
        warning <- SquireSpec.sync(SpecSyncOptions(noFetch = true), root, warningRunner, TestSpecPlatform())
        warningStep = warning.steps.find(_.step == "checkout")
      yield assert(
        !prune.ok && prune.steps.last.status == "failed" && prune.steps.last.detail.contains("--prune would delete") &&
          pruneRunner.requests.map(_.argv) == Chunk(revParse(checkout), sparseConfig(checkout)) &&
          warning.ok && warningStep.exists(step =>
            step.status == "ok" && step.detail.contains("missing website, tests/bdd, wit")
          ) && safe(pruneRunner) && safe(warningRunner)
      )
    }
  }

  "sync orchestration" - {
    "refuses a dirty fetch and honours no-fetch and dry-run without refresh mutation" in {
      for
        dirtyRoot <- preparedRoot("spec-dirty")
        dirtyCheckout = dirtyRoot / SquireSpec.CheckoutRel
        dirtyRunner   = syncRunner(dirtyRoot, dirty = " M exported.yaml\n")
        dirty       <- SquireSpec.sync(SpecSyncOptions(ref = "v4"), dirtyRoot, dirtyRunner, TestSpecPlatform())
        noFetchRoot <- preparedRoot("spec-no-fetch")
        noFetchRunner = syncRunner(noFetchRoot)
        noFetch <- SquireSpec.sync(SpecSyncOptions(noFetch = true), noFetchRoot, noFetchRunner, TestSpecPlatform())
        dryRoot <- preparedRoot("spec-dry")
        dryRunner = syncRunner(dryRoot)
        dry <- SquireSpec.sync(SpecSyncOptions(ref = "release", dryRun = true), dryRoot, dryRunner, TestSpecPlatform())
        dryKb = kb(dryRoot, "sync", "pull", "--dry-run")
      yield assert(
        !dirty.ok && dirty.steps.last.step == "fetch" && dirty.steps.last.detail.contains("uncommitted changes") &&
          dirtyRunner.requests.map(_.argv) == Chunk(
            revParse(dirtyCheckout),
            sparseConfig(dirtyCheckout),
            dirtyStatus(dirtyCheckout)
          ) &&
          noFetch.ok && noFetch.steps.find(_.step == "fetch").exists(step =>
            step.status == "skipped" && step.detail == "--no-fetch"
          ) && !noFetchRunner.requests.exists(_.argv == dirtyStatus(noFetchRoot / SquireSpec.CheckoutRel)) &&
          dry.ok && dry.steps.find(_.step == "fetch").exists(step =>
            step.status == "skipped" && step.detail == "--dry-run"
          ) && dryRunner.requests.exists(_.argv == dryKb) &&
          !dryRunner.requests.exists(request => request.argv.contains("fetch") || request.argv.contains("checkout")) &&
          safe(dirtyRunner) && safe(noFetchRunner) && safe(dryRunner)
      )
    }

    "uses the exact shallow fetch and detached checkout then parses status and pull options" in {
      for
        root <- preparedRoot("spec-fetch")
        checkout = root / SquireSpec.CheckoutRel
        runner   = syncRunner(root, statusJson = "banner\n{\"summary\":{\"clean\":2,\"local-only\":1}}\n")
        report <- SquireSpec.sync(
          SpecSyncOptions(ref = "v4-draft", theirs = true, prune = true, json = true),
          root,
          runner,
          TestSpecPlatform()
        )
        fetch        = Chunk("git", "-C", checkout.toString, "fetch", "--depth", "1", "origin", "v4-draft")
        detach       = Chunk("git", "-C", checkout.toString, "checkout", "--detach", "FETCH_HEAD")
        pull         = kb(root, "sync", "pull", "--theirs", "--prune", "--json")
        statusResult = report.steps.find(_.step == "status").flatMap(_.result.toOption)
      yield assert(
        report.ok && runner.requests.map(_.argv).contains(fetch) && runner.requests.map(_.argv).contains(detach) &&
          runner.requests.map(_.argv).contains(pull) && statusResult.exists(recordHas(_, "summary")) &&
          report.steps.map(_.step) == List("checkout", "fetch", "status", "pull", "check") && safe(runner)
      )
    }

    "reports malformed status, pull failure, and final check failure without continuing past the failed step" in {
      for
        statusRoot <- preparedRoot("spec-status-fail")
        statusRunner = syncRunner(statusRoot, statusJson = "not json\n")
        status   <- SquireSpec.sync(SpecSyncOptions(noFetch = true), statusRoot, statusRunner, TestSpecPlatform())
        pullRoot <- preparedRoot("spec-pull-fail")
        pullRunner = syncRunner(pullRoot, pullExit = 9)
        pull      <- SquireSpec.sync(SpecSyncOptions(noFetch = true), pullRoot, pullRunner, TestSpecPlatform())
        checkRoot <- preparedRoot("spec-check-fail")
        checkRunner = syncRunner(checkRoot, checkExit = 1)
        check <-
          SquireSpec.sync(SpecSyncOptions(noFetch = true, json = true), checkRoot, checkRunner, TestSpecPlatform())
      yield assert(
        !status.ok && status.steps.last.step == "status" && !statusRunner.requests.exists(_.argv == kb(
          statusRoot,
          "sync",
          "pull"
        )) &&
          !pull.ok && pull.steps.last.step == "pull" && !pullRunner.requests.exists(
            _.argv.headOption.contains("check")
          ) &&
          !check.ok && check.steps.last.step == "check" && check.steps.last.status == "failed" &&
          safe(statusRunner) && safe(pullRunner) && safe(checkRunner)
      )
    }

    "fails closed on repository sparse dirty and kb status probe failures" in {
      for
        headRoot <- preparedRoot("spec-head-probe")
        headCheckout = headRoot / SquireSpec.CheckoutRel
        headRunner   = syncRunner(headRoot, headExit = 128, headError = "not a repository")
        head      <- SquireSpec.sync(SpecSyncOptions(prune = true), headRoot, headRunner, TestSpecPlatform())
        emptyRoot <- preparedRoot("spec-empty-head")
        emptyRunner = syncRunner(emptyRoot, headOutput = "")
        empty      <- SquireSpec.sync(SpecSyncOptions(prune = true), emptyRoot, emptyRunner, TestSpecPlatform())
        sparseRoot <- preparedRoot("spec-sparse-probe", Chunk("docs"), schemas = false)
        sparseCheckout = sparseRoot / SquireSpec.CheckoutRel
        sparseRunner   = syncRunner(sparseRoot, sparseExit = 128, sparseError = "config failed")
        sparse <- SquireSpec.sync(
          SpecSyncOptions(prune = true, noFetch = true),
          sparseRoot,
          sparseRunner,
          TestSpecPlatform()
        )
        unsetRoot <- preparedRoot("spec-sparse-unset")
        unsetRunner = syncRunner(unsetRoot, sparseExit = 1)
        unset     <- SquireSpec.sync(SpecSyncOptions(noFetch = true), unsetRoot, unsetRunner, TestSpecPlatform())
        dirtyRoot <- preparedRoot("spec-dirty-probe")
        dirtyCheckout = dirtyRoot / SquireSpec.CheckoutRel
        dirtyRunner   = syncRunner(dirtyRoot, dirtyExit = 128, dirtyError = "status failed")
        dirty      <- SquireSpec.sync(SpecSyncOptions(prune = true), dirtyRoot, dirtyRunner, TestSpecPlatform())
        statusRoot <- preparedRoot("spec-kb-status-probe")
        statusRunner = syncRunner(statusRoot, statusExit = 7)
        status <- SquireSpec.sync(
          SpecSyncOptions(prune = true, noFetch = true),
          statusRoot,
          statusRunner,
          TestSpecPlatform()
        )
      yield assert(
        !head.ok && head.steps.last.step == "checkout" && headRunner.requests.map(_.argv) == Chunk(
          revParse(headCheckout)
        ) &&
          !empty.ok && empty.steps.last.step == "checkout" && emptyRunner.requests.size == 1 &&
          !sparse.ok && sparse.steps.last.step == "checkout" && sparseRunner.requests.map(_.argv) == Chunk(
            revParse(sparseCheckout),
            sparseConfig(sparseCheckout)
          ) && !sparseRunner.requests.exists(_.argv == kb(sparseRoot, "sync", "pull", "--prune")) &&
          unset.ok && unset.steps.find(_.step == "checkout").exists(_.status == "ok") &&
          !dirty.ok && dirty.steps.last.step == "fetch" && dirtyRunner.requests.map(_.argv) == Chunk(
            revParse(dirtyCheckout),
            sparseConfig(dirtyCheckout),
            dirtyStatus(dirtyCheckout)
          ) && !dirtyRunner.requests.exists(_.argv.contains("fetch")) &&
          !status.ok && status.steps.last.step == "status" &&
          !statusRunner.requests.exists(_.argv == kb(statusRoot, "sync", "pull", "--prune")) &&
          List(headRunner, emptyRunner, sparseRunner, unsetRunner, dirtyRunner, statusRunner).forall(safe)
      )
    }
  }

  "export orchestration" - {
    "validates the target then requests kb JSON push with every option and extracts written paths" in {
      for
        root <- rootWithoutCheckout("spec-export-target")
        missing       = root / "missing"
        missingRunner = RuleRunner(unexpected)
        missingReport <- SquireSpec.`export`(
          SpecExportOptions(to = Present(missing)),
          root,
          missingRunner,
          TestSpecPlatform()
        )
        checkout <- standaloneCheckout(root / "target")
        runner = exportRunner(
          root,
          checkout,
          written = List("docs/spec.md", "website/static/schemas/morphir-ir-v4.yaml")
        )
        report <- SquireSpec.`export`(
          SpecExportOptions(
            to = Present(checkout),
            dryRun = true,
            includeDiverged = true,
            noBranch = true,
            json = true
          ),
          root,
          runner,
          TestSpecPlatform()
        )
        expected   = kb(root, "sync", "push", "--to", checkout.toString, "--dry-run", "--include-diverged", "--json")
        pushResult = report.steps.find(_.step == "push").flatMap(_.result.toOption)
      yield assert(
        !missingReport.ok && missingRunner.requests.isEmpty && missingReport.steps.last.step == "checkout" &&
          report.ok && runner.requests.map(_.argv).take(2) == Chunk(gitCheckoutProbe(checkout), expected) &&
          pushResult.exists(recordHas(_, "actions")) &&
          report.steps.find(_.step == "push").exists(_.detail.contains("2 written path(s)")) &&
          report.steps.find(_.step == "branch").exists(_.status == "skipped") && safe(missingRunner) && safe(runner)
      )
    }

    "rejects a non-Git target before kb push" in {
      for
        root     <- rootWithoutCheckout("spec-non-git-target")
        checkout <- standaloneCheckout(root / "ordinary-directory")
        runner = exportRunner(root, checkout, gitCheckoutExit = 128)
        report <- SquireSpec.`export`(
          SpecExportOptions(to = Present(checkout)),
          root,
          runner,
          TestSpecPlatform()
        )
      yield assert(
        !report.ok && report.steps.last.step == "checkout" &&
          runner.requests.map(_.argv) == Chunk(gitCheckoutProbe(checkout)) &&
          !runner.requests.exists(_.argv.take(3) == kb(root, "sync", "push").take(3)) && safe(runner)
      )
    }

    "fails push when ownership JSON is absent malformed non-object or structurally invalid" in {
      val cases = Chunk(
        ("absent", "", false),
        ("malformed", "{", false),
        ("non-object", "[]", true),
        ("scalar", "7", true),
        ("missing-actions", "{}", true),
        ("duplicate-actions", "{\"actions\":[],\"actions\":[]}", true),
        ("actions-object", "{\"actions\":{}}", true),
        ("action-scalar", "{\"actions\":[\"wrote\"]}", true),
        ("wrote-missing-path", "{\"actions\":[{\"verb\":\"wrote\"}]}", true),
        ("other-missing-path", "{\"actions\":[{\"verb\":\"held back\"}]}", true),
        ("wrote-empty-path", "{\"actions\":[{\"verb\":\"wrote\",\"path\":\"\"}]}", true),
        ("wrote-non-string-path", "{\"actions\":[{\"verb\":\"wrote\",\"path\":7}]}", true),
        ("non-string-verb", "{\"actions\":[{\"verb\":7,\"path\":\"website/static/schemas/x.yaml\"}]}", true)
      )
      Kyo.foreach(cases) { case (label, payload, parsedEvidence) =>
        for
          root     <- rootWithoutCheckout(s"spec-push-json-$label")
          checkout <- standaloneCheckout(root / "checkout", schemas = true)
          runner = exportRunner(root, checkout, pushStdout = Some(payload))
          report <- SquireSpec.`export`(
            SpecExportOptions(to = Present(checkout), noBranch = true),
            root,
            runner,
            TestSpecPlatform()
          )
          push = report.steps.find(_.step == "push")
        yield !report.ok && push.exists(step => step.status == "failed" && step.result.isDefined == parsedEvidence) &&
          report.steps.last.step == "status" &&
          !runner.requests.exists(_.argv.headOption.contains("jsonschema")) && safe(runner)
      }.map(results => assert(results.forall(identity)))
    }

    "creates reuses or fails the review branch without ever committing or pushing" in {
      for
        root            <- rootWithoutCheckout("spec-branches")
        createdCheckout <- standaloneCheckout(root / "created")
        createdRunner = exportRunner(root, createdCheckout)
        created <-
          SquireSpec.`export`(SpecExportOptions(to = Present(createdCheckout)), root, createdRunner, TestSpecPlatform())
        reusedCheckout <- standaloneCheckout(root / "reused")
        reusedRunner = exportRunner(root, reusedCheckout, createBranchExit = 1)
        reused <-
          SquireSpec.`export`(SpecExportOptions(to = Present(reusedCheckout)), root, reusedRunner, TestSpecPlatform())
        failedCheckout <- standaloneCheckout(root / "failed")
        failedRunner = exportRunner(root, failedCheckout, createBranchExit = 1, reuseBranchExit = 1)
        failed <-
          SquireSpec.`export`(SpecExportOptions(to = Present(failedCheckout)), root, failedRunner, TestSpecPlatform())
      yield assert(
        created.ok && created.steps.find(_.step == "branch").exists(step =>
          step.status == "ok" && step.detail.contains("created")
        ) &&
          reused.ok && reused.steps.find(_.step == "branch").exists(step =>
            step.status == "ok" && step.detail.contains("already existed")
          ) &&
          !failed.ok && failed.steps.find(_.step == "branch").exists(_.status == "failed") &&
          failed.steps.last.step == "status" &&
          failedRunner.requests.exists(_.argv == Chunk("git", "-C", failedCheckout.toString, "status", "--short")) &&
          safe(createdRunner) && safe(reusedRunner) && safe(failedRunner)
      )
    }

    "preserves partial push and runner Abort failures while always recording final status" in {
      for
        pushRoot     <- rootWithoutCheckout("spec-partial-push")
        pushCheckout <- standaloneCheckout(pushRoot / "checkout")
        partialPayload = "{\"actions\":[{\"verb\":\"wrote\",\"path\":\"docs/spec.md\"}]}"
        pushRunner     = exportRunner(
          pushRoot,
          pushCheckout,
          pushExit = 9,
          pushStdout = Some(partialPayload),
          changed = " M docs/spec.md\n"
        )
        push <- SquireSpec.`export`(
          SpecExportOptions(to = Present(pushCheckout)),
          pushRoot,
          pushRunner,
          TestSpecPlatform()
        )
        abortRoot     <- rootWithoutCheckout("spec-aborted-push")
        abortCheckout <- standaloneCheckout(abortRoot / "checkout")
        abortBase   = exportResponse(abortRoot, abortCheckout, changed = " M docs/spec.md\n")
        abortRunner = AbortRuleRunner { request =>
          if request.argv.take(3) == kb(abortRoot, "sync", "push").take(3) then
            Abort.fail(SquireError.Failure("process", "push launch failed"))
          else abortBase(request)
        }
        aborted <- Abort.run[SquireError](
          SquireSpec.`export`(
            SpecExportOptions(to = Present(abortCheckout)),
            abortRoot,
            abortRunner,
            TestSpecPlatform()
          )
        )
      yield assert(
        !push.ok && push.steps.find(_.step == "push").exists(step =>
          step.status == "failed" && step.result.exists(recordHas(_, "actions"))
        ) && push.steps.last.step == "status" &&
          !pushRunner.requests.exists(_.argv.contains("switch")) &&
          aborted.exists(report =>
            !report.ok && report.steps.find(_.step == "push").exists(step =>
              step.status == "failed" && step.detail.contains("push launch failed")
            ) && report.steps.last.step == "status"
          ) && safe(pushRunner) && safe(abortRunner)
      )
    }

    "contains validator launch Abort and makes nonzero final status fail the report" in {
      for
        validatorRoot     <- rootWithoutCheckout("spec-validator-abort")
        validatorCheckout <- standaloneCheckout(validatorRoot / "checkout", schemas = true)
        validatorBase   = exportResponse(validatorRoot, validatorCheckout)
        validatorRunner = AbortRuleRunner { request =>
          if request.argv.headOption.contains("jsonschema") then
            Abort.fail(SquireError.Failure("process", "validator launch failed"))
          else validatorBase(request)
        }
        validator <- Abort.run[SquireError](
          SquireSpec.`export`(
            SpecExportOptions(to = Present(validatorCheckout), noBranch = true),
            validatorRoot,
            validatorRunner,
            TestSpecPlatform()
          )
        )
        statusRoot     <- rootWithoutCheckout("spec-status-exit")
        statusCheckout <- standaloneCheckout(statusRoot / "checkout", schemas = true)
        statusRunner = exportRunner(statusRoot, statusCheckout, statusExit = 7)
        status <- SquireSpec.`export`(
          SpecExportOptions(to = Present(statusCheckout), noBranch = true),
          statusRoot,
          statusRunner,
          TestSpecPlatform()
        )
      yield assert(
        validator.exists(report =>
          !report.ok && report.steps.find(_.step == "validator:jsonschema fmt").exists(step =>
            step.status == "failed" && step.detail.contains("validator launch failed")
          ) && report.steps.last.step == "status"
        ) &&
          !status.ok && status.steps.last.step == "status" && status.steps.last.status == "failed" &&
          status.steps.last.detail.contains("exited 7") && safe(validatorRunner) && safe(statusRunner)
      )
    }

    "expands validator globs and classifies YAML unsupported, pre-existing, and owned failures" in {
      for
        preRoot     <- rootWithoutCheckout("spec-preexisting")
        preCheckout <- standaloneCheckout(preRoot / "preexisting", schemas = true)
        preRunner   = exportRunner(preRoot, preCheckout, validatorFailure = Some("lint"))
        prePlatform = TestSpecPlatform()
        pre <- SquireSpec.`export`(
          SpecExportOptions(to = Present(preCheckout), noBranch = true),
          preRoot,
          preRunner,
          prePlatform
        )
        ownRoot     <- rootWithoutCheckout("spec-owned")
        ownCheckout <- standaloneCheckout(ownRoot / "owned", schemas = true)
        ownRunner = exportRunner(
          ownRoot,
          ownCheckout,
          written = List("website/static/schemas/morphir-ir-v4.yaml"),
          validatorFailure = Some("lint")
        )
        own <- SquireSpec.`export`(
          SpecExportOptions(to = Present(ownCheckout), noBranch = true),
          ownRoot,
          ownRunner,
          TestSpecPlatform()
        )
        yamlRoot     <- rootWithoutCheckout("spec-yaml-unsupported")
        yamlCheckout <- standaloneCheckout(yamlRoot / "yaml", schemas = true)
        yamlRunner = exportRunner(yamlRoot, yamlCheckout, yamlUnsupported = true)
        yaml <- SquireSpec.`export`(
          SpecExportOptions(to = Present(yamlCheckout), noBranch = true),
          yamlRoot,
          yamlRunner,
          TestSpecPlatform()
        )
        expanded = Chunk(
          "jsonschema",
          "lint",
          "website/static/schemas/morphir-ir-v4.yaml"
        )
      yield assert(
        pre.ok && pre.steps.find(_.step == "validator:jsonschema lint").exists(_.status == "pre-existing") &&
          !own.ok && own.steps.find(_.step == "validator:jsonschema lint").exists(_.status == "failed") &&
          yaml.ok && yaml.steps.find(_.step == "validator:jsonschema fmt").exists(step =>
            step.status == "skipped" && step.detail.contains("does not support YAML")
          ) &&
          preRunner.requests.exists(_.argv == expanded) && safe(preRunner) && safe(ownRunner) && safe(yamlRunner)
      )
    }

    "skips absent tools paths and globs while replacing Bun with in-process schema comparison" in {
      for
        toolRoot     <- rootWithoutCheckout("spec-no-tool")
        toolCheckout <- standaloneCheckout(toolRoot / "no-tool", schemas = true)
        toolRunner   = exportRunner(toolRoot, toolCheckout)
        toolPlatform = TestSpecPlatform(executables = Set.empty)
        toolReport <- SquireSpec.`export`(
          SpecExportOptions(to = Present(toolCheckout), noBranch = true),
          toolRoot,
          toolRunner,
          toolPlatform
        )
        pathRoot     <- rootWithoutCheckout("spec-no-path")
        pathCheckout <- standaloneCheckout(pathRoot / "no-path", schemas = false)
        pathRunner   = exportRunner(pathRoot, pathCheckout)
        pathPlatform = TestSpecPlatform()
        pathReport <- SquireSpec.`export`(
          SpecExportOptions(to = Present(pathCheckout), noBranch = true),
          pathRoot,
          pathRunner,
          pathPlatform
        )
        globRoot     <- rootWithoutCheckout("spec-no-glob")
        globCheckout <- standaloneCheckout(globRoot / "no-glob", schemas = true, yaml = false)
        globRunner   = exportRunner(globRoot, globCheckout)
        globPlatform = TestSpecPlatform()
        globReport <- SquireSpec.`export`(
          SpecExportOptions(to = Present(globCheckout), noBranch = true),
          globRoot,
          globRunner,
          globPlatform
        )
        schemaDir = toolCheckout / "website" / "static" / "schemas"
      yield assert(
        toolReport.ok && toolReport.steps.count(step =>
          step.step.startsWith("validator:jsonschema") && step.status == "skipped"
        ) == 3 && toolPlatform.schemaRequests == List(schemaDir) &&
          pathReport.ok && pathReport.steps.count(step =>
            step.step.startsWith("validator:") && step.status == "skipped"
          ) == 4 &&
          pathPlatform.schemaRequests.isEmpty &&
          globReport.ok && globReport.steps.find(
            _.step == "validator:jsonschema lint"
          ).exists(_.detail == "no matching files") &&
          globReport.steps.find(_.step == "validator:schemas json in step").exists(_.detail == "no matching files") &&
          globPlatform.schemaRequests.isEmpty &&
          List(toolRunner, pathRunner, globRunner).forall(runner =>
            !runner.requests.exists(_.argv.headOption.contains("bun")) && safe(runner)
          )
      )
    }

    "gates owned schema drift, still records final status, and never requests commit or push" in {
      for
        root     <- rootWithoutCheckout("spec-schema-drift")
        checkout <- standaloneCheckout(root / "checkout", schemas = true)
        runner = exportRunner(
          root,
          checkout,
          written = List("website/static/schemas/morphir-ir-v4.yaml"),
          changed = " M website/static/schemas/morphir-ir-v4.yaml\n"
        )
        platform = TestSpecPlatform(schemaOk = false)
        report <- SquireSpec.`export`(
          SpecExportOptions(to = Present(checkout), noBranch = true),
          root,
          runner,
          platform
        )
        status = report.steps.find(_.step == "status")
      yield assert(
        !report.ok && report.steps.find(_.step == "validator:schemas json in step").exists(_.status == "failed") &&
          status.exists(step => step.status == "ok" && step.detail == "1 changed path(s)") &&
          platform.schemaRequests == List(checkout / "website" / "static" / "schemas") && safe(runner)
      )
    }

    "contains typed schema comparison failures and still records final status" in {
      for
        root     <- rootWithoutCheckout("spec-schema-error")
        checkout <- standaloneCheckout(root / "checkout", schemas = true)
        runner = exportRunner(
          root,
          checkout,
          written = List("website/static/schemas/morphir-ir-v4.yaml"),
          changed = " M website/static/schemas/morphir-ir-v4.yaml\n"
        )
        platform = TestSpecPlatform(schemaFailure = Some("could not decode YAML schema"))
        outcome <- Abort.run[SquireError](
          SquireSpec.`export`(
            SpecExportOptions(to = Present(checkout), noBranch = true),
            root,
            runner,
            platform
          )
        )
      yield assert(
        outcome.exists(report =>
          !report.ok &&
            report.steps.find(_.step == "validator:schemas json in step").exists(step =>
              step.status == "failed" && step.detail.contains("could not decode YAML schema")
            ) &&
            report.steps.last.step == "status"
        ) && safe(runner)
      )
    }
  }

  "CLI reporting" - {
    "renders step text or one clean typed JSON report and forwards the aggregate exit" in {
      for
        textRoot <- preparedRoot("spec-cli-text")
        textOut = new StringBuilder
        textErr = new StringBuilder
        textExit <- SquireCli.runSpecSync(
          SpecSyncOpts(noFetch = true),
          textRoot,
          syncRunner(textRoot),
          TestSpecPlatform(),
          value => textOut.append(value),
          value => textErr.append(value)
        )
        jsonRoot <- preparedRoot("spec-cli-json")
        jsonOut = new StringBuilder
        jsonErr = new StringBuilder
        jsonExit <- SquireCli.runSpecSync(
          SpecSyncOpts(noFetch = true, json = true),
          jsonRoot,
          syncRunner(jsonRoot),
          TestSpecPlatform(),
          value => jsonOut.append(value),
          value => jsonErr.append(value)
        )
        decoded = SquireJson.decode[SpecReport](jsonOut.result().trim)
      yield assert(
        textExit == 0 && textOut.result().contains("[1/5] reference checkout") && textOut.result().contains(
          "Import complete"
        ) &&
          textErr.isEmpty && jsonExit == 0 && decoded.exists(report => report.command == "spec-sync" && report.ok) &&
          !jsonOut.result().contains("[1/5]") && jsonErr.isEmpty
      )
    }

    "reports the primary export failure after successful or failed final status" in
      Kyo.foreach(Chunk(("status-ok", 0), ("status-failed", 7))) { case (label, statusExit) =>
        for
          root     <- rootWithoutCheckout(s"spec-cli-export-$label")
          checkout <- standaloneCheckout(root / "checkout", schemas = true)
          base   = exportResponse(root, checkout, statusExit = statusExit)
          runner = AbortRuleRunner { request =>
            if request.argv.headOption.contains("jsonschema") then
              Abort.fail(SquireError.Failure("process", "validator launch failed"))
            else base(request)
          }
          out = new StringBuilder
          err = new StringBuilder
          result <- Abort.run[SquireError](
            SquireCli.runSpecExport(
              SpecExportOpts(to = Some(checkout.toString), noBranch = true, json = true),
              root,
              runner,
              TestSpecPlatform(),
              value => out.append(value),
              value => err.append(value)
            )
          )
          decoded = SquireJson.decode[SpecReport](out.result().trim)
        yield result == Result.Success(1) && decoded.exists(report =>
          !report.ok && report.command == "spec-export" &&
            report.steps.find(_.status == "failed").exists(_.detail == "validator launch failed") &&
            report.steps.lastOption.exists(step =>
              step.step == "status" && step.status == (if statusExit == 0 then "ok" else "failed")
            )
        ) && out.result().trim.linesIterator.size == 1 &&
          err.result() == "ERROR: validator launch failed\n" && safe(runner)
      }.map(results => assert(results.forall(identity)))
  }

class SquireProcessSpec extends Test[Any]:
  "process runner" - {
    "recording runner preserves argv cwd stdout stderr and exit" in {
      val request  = ProcessRequest(Chunk("git", "status"), Present(Path("/repo")))
      val expected = ProcessResult(request, 7, "out", "err")
      val runner   = RecordingRunner(Chunk(expected))
      runner.run(request).map(result => assert(result == expected && runner.requests == Chunk(request)))
    }

    "live runner captures stdout and stderr separately" in {
      val outputBytes = 128 * 1024
      for
        root <- SquireFixtures.scratch("process")
        source = root / "ProcessProbe.java"
        _ <- source.write(
          s"class ProcessProbe { public static void main(String[] a) { String out = \"o\".repeat($outputBytes); String err = \"e\".repeat($outputBytes); System.out.print(out); System.err.print(err); System.exit(7); } }"
        )
        outcome <- Abort.run[SquireError | Timeout](
          Async.timeout(5.seconds)(
            LiveProcessRunner.run(ProcessRequest(Chunk(SquireFixtures.javaExecutable, SquirePaths.render(source))))
          )
        )
        result = outcome match
          case Result.Success(value) =>
            value.exitCode == 7 &&
            // Java launchers may prepend inherited option diagnostics to either stream in CI.
            value.stdout.endsWith("o".repeat(outputBytes)) &&
            value.stderr.endsWith("e".repeat(outputBytes))
          case Result.Failure(_) => false
      yield assert(result)
    }
  }

object SquireFixtures:
  val javaExecutable: String =
    java.nio.file.Path.of(java.lang.System.getProperty("java.home"), "bin", "java").toString

  def scratch(name: String): Path < Sync =
    Sync.defer(Path(java.nio.file.Files.createTempDirectory(s"squire-$name-").toString))

  def deleteRecursively(root: Path): Unit =
    if Files.exists(root.toJava) then
      val stream = Files.walk(root.toJava)
      try stream.iterator.asScala.toList.reverse.foreach(Files.deleteIfExists)
      finally stream.close()

  def scopedScratch(name: String): Path < (Scope & Sync) =
    Scope.acquireRelease(scratch(name))(root => Sync.defer(deleteRecursively(root)))

  def platform(
      root: Path,
      jvmResult: SquireEnv.CheckResult,
      environment: Map[String, String] = Map.empty,
      home: Option[Path] = None,
      managed: Chunk[Path] = Chunk.empty,
      varFolders: Option[Path] = None,
      jvmTempDirectory: Maybe[Path] = Absent,
      daemonProbe: Int => SquireEnv.DaemonProbe = _ => SquireEnv.DaemonProbe.Open,
      writeProbe: Path => Unit = path => Files.writeString(path.toJava, "squire probe"),
      deleteProbe: Path => Unit = path => Files.deleteIfExists(path.toJava)
  ): TestEnvPlatform =
    TestEnvPlatform(
      environment,
      home.getOrElse(root / "home"),
      managed,
      varFolders.getOrElse(root),
      jvmTempDirectory.orElse(Present(varFolders.getOrElse(root))),
      _ => jvmResult,
      daemonProbe,
      writeProbe,
      deleteProbe
    )

  def writeDaemonFiles(root: Path, portFile: Option[Int], logPort: Option[Int]): Unit < Sync =
    Sync.defer {
      val daemon = root / "out" / "mill-daemon"
      Files.createDirectories(daemon.toJava)
      portFile.foreach(port => Files.writeString((daemon / "socketPort").toJava, port.toString))
      logPort.foreach(port => Files.writeString((daemon / "server.log").toJava, s"listening on port $port\n"))
    }

final class RecordingRunner(responses: Chunk[ProcessResult]) extends ProcessRunner:
  private var index                   = 0
  var requests: Chunk[ProcessRequest] = Chunk.empty

  def run(request: ProcessRequest): ProcessResult < (Async & Abort[SquireError]) =
    requests = requests.append(request)
    val response = responses(index)
    index += 1
    response

trait SpecRecordedRunner:
  def requests: Chunk[ProcessRequest]

final class RuleRunner(response: ProcessRequest => ProcessResult) extends ProcessRunner with SpecRecordedRunner:
  var requests: Chunk[ProcessRequest] = Chunk.empty

  def run(request: ProcessRequest): ProcessResult < (Async & Abort[SquireError]) =
    requests = requests.append(request)
    response(request)

final class AbortRuleRunner(response: ProcessRequest => ProcessResult < Abort[SquireError])
    extends ProcessRunner
    with SpecRecordedRunner:
  var requests: Chunk[ProcessRequest] = Chunk.empty

  def run(request: ProcessRequest): ProcessResult < (Async & Abort[SquireError]) =
    requests = requests.append(request)
    response(request)

object AbortRuleRunner:
  def apply(response: ProcessRequest => ProcessResult < Abort[SquireError]): AbortRuleRunner =
    new AbortRuleRunner(response)

final case class TestSpecPlatform(
    executables: Set[String] = Set("jsonschema"),
    schemaOk: Boolean = true,
    schemaFailure: Option[String] = None
) extends SquireSpecPlatform:
  var schemaRequests: List[Path] = Nil

  def exists(path: Path): Boolean < Sync          = Sync.defer(Files.exists(path.toJava))
  def isDirectory(path: Path): Boolean < Sync     = Sync.defer(Files.isDirectory(path.toJava))
  def isSymlink(path: Path): Boolean < Sync       = Sync.defer(Files.isSymbolicLink(path.toJava))
  def resolve(path: Path): Path < Sync            = Sync.defer(Path(path.toJava.toAbsolutePath.normalize.toString))
  def findExecutable(name: String): Maybe[String] =
    if executables.contains(name) then Present(name) else Absent
  def glob(cwd: Path, pattern: String): Chunk[String] < Sync = LiveSquireSpecPlatform.glob(cwd, pattern)
  def compareSchemas(directory: Path): SchemaReport < (Sync & Abort[SquireError]) =
    schemaRequests = schemaRequests :+ directory
    schemaFailure match
      case Some(message) => Abort.fail(SquireError.Failure("schemas", message))
      case None          =>
        SchemaReport(
          "schemas-to-json",
          directory.toString,
          directory.toString,
          check = true,
          ok = schemaOk,
          List(SchemaOutcome("morphir-ir-v4.json", if schemaOk then "identical" else "drifted"))
        )

object SquireSpecFixtures:
  val head: String = "a" * 40

  def rootWithoutCheckout(name: String): Path < Sync =
    for
      root <- SquireFixtures.scratch(name)
      _    <- Sync.defer {
        val launcher = root / ".claude" / "skills" / "kb" / "kb"
        Files.createDirectories(launcher.parent.get.toJava)
        Files.writeString(launcher.toJava, "#!/bin/sh\n")
      }
    yield root

  def preparedRoot(
      name: String,
      sparse: Chunk[String] = SquireSpec.SparsePaths,
      schemas: Boolean = true
  ): Path < Sync =
    for
      root <- rootWithoutCheckout(name)
      _    <- standaloneCheckout(root / SquireSpec.CheckoutRel, sparse, schemas)
    yield root

  def standaloneCheckout(
      checkout: Path,
      sparse: Chunk[String] = SquireSpec.SparsePaths,
      schemas: Boolean = false,
      yaml: Boolean = true
  ): Path < Sync =
    Sync.defer {
      Files.createDirectories((checkout / ".git").toJava)
      sparse.foreach(path => Files.createDirectories((checkout / path).toJava))
      if schemas then
        val directory = checkout / "website" / "static" / "schemas"
        Files.createDirectories(directory.toJava)
        if yaml then
          Files.writeString((directory / "morphir-ir-v4.yaml").toJava, "type: object\n")
          Files.writeString((directory / "morphir-ir-v4.json").toJava, "{\n  \"type\": \"object\"\n}\n")
      checkout
    }

  def unexpected(request: ProcessRequest): ProcessResult =
    ProcessResult(request, 99, "", s"unexpected: ${request.argv.mkString(" ")}")

  def syncRunner(
      root: Path,
      sparse: Boolean = false,
      dirty: String = "",
      statusJson: String = "{\"summary\":{\"clean\":1}}\n",
      pullExit: Int = 0,
      checkExit: Int = 0,
      headExit: Int = 0,
      headOutput: String = head + "\n",
      headError: String = "",
      sparseExit: Int = 0,
      sparseError: String = "",
      dirtyExit: Int = 0,
      dirtyError: String = "",
      statusExit: Int = 0
  ): RuleRunner =
    val checkout = root / SquireSpec.CheckoutRel
    RuleRunner { request =>
      val argv = request.argv
      if argv == revParse(checkout) then ProcessResult(request, headExit, headOutput, headError)
      else if argv == sparseConfig(checkout) then
        val output =
          if sparseExit == 1 && !sparse && sparseError.isEmpty then "" else if sparse then "true\n" else "false\n"
        ProcessResult(request, sparseExit, output, sparseError)
      else if argv == dirtyStatus(checkout) then ProcessResult(request, dirtyExit, dirty, dirtyError)
      else if argv.take(6) == Chunk("git", "-C", checkout.toString, "fetch", "--depth", "1") then ok(request)
      else if argv == Chunk("git", "-C", checkout.toString, "checkout", "--detach", "FETCH_HEAD") then ok(request)
      else if argv == kb(root, "sync", "status", "--json") then ProcessResult(request, statusExit, statusJson, "")
      else if argv.take(3) == kb(root, "sync", "pull").take(3) then
        ProcessResult(request, pullExit, "{\"actions\":[]}", if pullExit == 0 then "" else "pull failed")
      else if argv.take(2) == kb(root, "check").take(2) then
        ProcessResult(request, checkExit, "{\"findings\":[]}", if checkExit == 0 then "" else "check failed")
      else unexpected(request)
    }

  def exportRunner(
      root: Path,
      checkout: Path,
      written: List[String] = Nil,
      createBranchExit: Int = 0,
      reuseBranchExit: Int = 0,
      validatorFailure: Option[String] = None,
      yamlUnsupported: Boolean = false,
      changed: String = "",
      gitCheckoutExit: Int = 0,
      pushExit: Int = 0,
      pushStdout: Option[String] = None,
      statusExit: Int = 0
  ): RuleRunner =
    RuleRunner(
      exportResponse(
        root,
        checkout,
        written,
        createBranchExit,
        reuseBranchExit,
        validatorFailure,
        yamlUnsupported,
        changed,
        gitCheckoutExit,
        pushExit,
        pushStdout,
        statusExit
      )
    )

  def exportResponse(
      root: Path,
      checkout: Path,
      written: List[String] = Nil,
      createBranchExit: Int = 0,
      reuseBranchExit: Int = 0,
      validatorFailure: Option[String] = None,
      yamlUnsupported: Boolean = false,
      changed: String = "",
      gitCheckoutExit: Int = 0,
      pushExit: Int = 0,
      pushStdout: Option[String] = None,
      statusExit: Int = 0
  ): ProcessRequest => ProcessResult =
    request =>
      val argv = request.argv
      if argv == gitCheckoutProbe(checkout) then
        ProcessResult(request, gitCheckoutExit, if gitCheckoutExit == 0 then "true\n" else "", "not a work tree")
      else if argv.take(3) == kb(root, "sync", "push").take(3) then
        val actions = written.map(path => s"{\"verb\":\"wrote\",\"path\":\"$path\"}").mkString(",")
        ProcessResult(
          request,
          pushExit,
          pushStdout.getOrElse(s"{\"actions\":[$actions]}\n"),
          if pushExit == 0 then "" else "push failed"
        )
      else if argv == Chunk("git", "-C", checkout.toString, "switch", "-c", SquireSpec.DefaultExportBranch) then
        ProcessResult(request, createBranchExit, "", if createBranchExit == 0 then "" else "already exists")
      else if argv == Chunk("git", "-C", checkout.toString, "switch", SquireSpec.DefaultExportBranch) then
        ProcessResult(request, reuseBranchExit, "", if reuseBranchExit == 0 then "" else "cannot switch")
      else if argv == Chunk("git", "-C", checkout.toString, "status", "--short") then
        ProcessResult(request, statusExit, changed, if statusExit == 0 then "" else "status failed")
      else if argv.headOption.contains("jsonschema") then
        val operation = argv.lift(1).getOrElse("")
        if yamlUnsupported && operation == "fmt" then ProcessResult(request, 1, "", "does not support YAML")
        else if validatorFailure.contains(operation) then ProcessResult(request, 1, "", s"$operation failed")
        else ok(request)
      else unexpected(request)

  def kb(root: Path, args: String*): Chunk[String] =
    Chunk((root / ".claude" / "skills" / "kb" / "kb").toString) ++ Chunk.from(args)

  def revParse(checkout: Path): Chunk[String]     = Chunk("git", "-C", checkout.toString, "rev-parse", "HEAD")
  def sparseConfig(checkout: Path): Chunk[String] =
    Chunk("git", "-C", checkout.toString, "config", "--get", "core.sparseCheckout")
  def dirtyStatus(checkout: Path): Chunk[String]      = Chunk("git", "-C", checkout.toString, "status", "--porcelain")
  def gitCheckoutProbe(checkout: Path): Chunk[String] =
    Chunk("git", "-C", checkout.toString, "rev-parse", "--is-inside-work-tree")

  def ok(request: ProcessRequest, stdout: String = ""): ProcessResult = ProcessResult(request, 0, stdout, "")

  def recordHas(value: Structure.Value, field: String): Boolean = value match
    case Structure.Value.Record(fields) => fields.exists(_._1 == field)
    case _                              => false

  def safe(runner: SpecRecordedRunner): Boolean =
    !runner.requests.exists { request =>
      request.argv.headOption.contains("git") &&
      request.argv.exists(argument => argument == "commit" || argument == "push")
    }

final class BranchRecordingRunner(responses: Map[Chunk[String], ProcessResult]) extends ProcessRunner:
  var requests: Chunk[ProcessRequest] = Chunk.empty

  def run(request: ProcessRequest): ProcessResult < (Async & Abort[SquireError]) =
    requests = requests.append(request)
    responses(request.argv).copy(request = request)

object BranchRecordingRunner:
  def apply(responses: Map[Chunk[String], ProcessResult]): BranchRecordingRunner =
    new BranchRecordingRunner(responses)

final class LocalGitBranchRunner(cwd: Path, responses: Map[Chunk[String], String]) extends ProcessRunner:
  var requests: Chunk[ProcessRequest] = Chunk.empty

  def run(request: ProcessRequest): ProcessResult < (Async & Abort[SquireError]) =
    requests = requests.append(request)
    if request.argv.headOption.contains("git") then
      LiveProcessRunner.run(request.copy(cwd = Present(cwd))).map(_.copy(request = request))
    else
      ProcessResult(request, 0, responses(request.argv), "")

object SquireBranchFixtures:
  val sourceSha: String = "1" * 40
  val targetSha: String = "2" * 40
  val mergeSha: String  = "3" * 40

  val checkTarget: Chunk[String] = Chunk("git", "check-ref-format", "--branch", "develop")
  val fetch: Chunk[String]       = Chunk(
    "git",
    "fetch",
    "--prune",
    "origin",
    "+refs/heads/main:refs/remotes/origin/main",
    "+refs/heads/develop:refs/remotes/origin/develop"
  )
  val resolveSource: Chunk[String] = Chunk("git", "rev-parse", "refs/remotes/origin/main")
  val resolveTarget: Chunk[String] = Chunk("git", "rev-parse", "refs/remotes/origin/develop")
  val repoView: Chunk[String]      = Chunk(
    "gh",
    "repo",
    "view",
    "--json",
    "nameWithOwner",
    "--jq",
    ".nameWithOwner"
  )
  val prList: Chunk[String] = Chunk(
    "gh",
    "pr",
    "list",
    "--repo",
    "finos/morphir-scala",
    "--base",
    "main",
    "--head",
    "develop",
    "--state",
    "merged",
    "--limit",
    "100",
    "--json",
    "number,headRefOid,mergeCommit,url,mergedAt"
  )
  val ancestor: Chunk[String] =
    Chunk("git", "merge-base", "--is-ancestor", mergeSha, "refs/remotes/origin/main")
  val push: Chunk[String] = Chunk(
    "git",
    "push",
    s"--force-with-lease=refs/heads/develop:$targetSha",
    "origin",
    "refs/remotes/origin/main:refs/heads/develop"
  )

  val matchingPullRequest: String =
    s"""{"number":42,"headRefOid":"$targetSha","mergeCommit":{"oid":"$mergeSha"},"url":"https://github.com/finos/morphir-scala/pull/42","mergedAt":"2026-08-07T12:00:00Z"}"""

  def ok(argv: Chunk[String], stdout: String = ""): ProcessResult =
    ProcessResult(ProcessRequest(argv), 0, stdout, "")

  def failed(argv: Chunk[String], detail: String): ProcessResult =
    ProcessResult(ProcessRequest(argv), 1, "", detail)

  val baseResponses: Map[Chunk[String], ProcessResult] = Map(
    checkTarget   -> ok(checkTarget, "develop\n"),
    fetch         -> ok(fetch),
    resolveSource -> ok(resolveSource, sourceSha + "\n"),
    resolveTarget -> ok(resolveTarget, targetSha + "\n")
  )

  def proofResponses(pullRequests: String): Map[Chunk[String], ProcessResult] =
    baseResponses ++ Map(
      repoView -> ok(repoView, "finos/morphir-scala\n"),
      prList   -> ok(prList, pullRequests)
    )

  val successfulProofResponses: Map[Chunk[String], ProcessResult] =
    proofResponses(s"[$matchingPullRequest]").updated(ancestor, ok(ancestor))

  def neverPushed(runner: BranchRecordingRunner): Boolean =
    !runner.requests.exists(_.argv.take(2) == Chunk("git", "push"))

  def failureContains[A](outcome: Result[SquireError, A], fragments: String*): Boolean =
    outcome match
      case Result.Failure(error) => fragments.forall(error.getMessage.contains)
      case Result.Success(_)     => false

  def git(cwd: Path, args: String*): String =
    val process =
      new ProcessBuilder((Seq("git") ++ args)*).directory(cwd.toJava.toFile).redirectErrorStream(true).start()
    val output = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val exit   = process.waitFor()
    if exit != 0 then throw new RuntimeException(s"git ${args.mkString(" ")} failed: ${output.trim}")
    output

final case class TestSquirePlatform(
    executable: Maybe[String] = Absent,
    instant: java.time.Instant = java.time.Instant.parse("2026-08-08T12:34:56Z")
) extends SquirePlatform:
  def findExecutable(name: String): Maybe[String] = executable
  def now: java.time.Instant                      = instant

class SquireCellarSpec extends Test[Any]:
  "settings" - {
    "use defaults when absent and decode Kyo YAML when present" in {
      for
        root <- SquireFixtures.scratch("cellar-settings")
        absent   = SquireCellar.loadSettings(root)
        settings = root / ".config" / "squire" / "settings.local.yaml"
        _ <- Sync.defer {
          Files.createDirectories(settings.parent.get.toJava)
          Files.writeString(
            settings.toJava,
            "cellar:\n  repositories:\n    - https://repo.example/maven\n  binary: /opt/cellar\n"
          )
        }
        loaded = SquireCellar.loadSettings(root)
      yield assert(
        absent == Result.Success(CellarSettings()) &&
          loaded == Result.Success(CellarSettings(List("https://repo.example/maven"), Some("/opt/cellar")))
      )
    }

    "report malformed Kyo YAML instead of silently discarding it" in {
      for
        root <- SquireFixtures.scratch("cellar-settings-invalid")
        settings = root / ".config" / "squire" / "settings.local.yaml"
        _ <- Sync.defer {
          Files.createDirectories(settings.parent.get.toJava)
          Files.writeString(settings.toJava, "cellar: [unterminated")
        }
      yield assert(SquireCellar.loadSettings(root).isFailure)
    }
  }

  "coordinates and execution" - {
    "expand repository flags and every legacy coordinate alias" in {
      val aliases = Map(
        "case-app:2.1.0" -> "com.github.alexarchambault:case-app_3:2.1.0",
        "kyo-case-app"   -> "io.getkyo:kyo-case-app_3:1.0.0-RC5",
        "kyo-schema"     -> "io.getkyo:kyo-schema_3:1.0.0-RC5",
        "kyo-zio"        -> "io.getkyo:kyo-zio_3:1.0.0-RC5",
        "zio:2.1.26"     -> "dev.zio:zio_3:2.1.26",
        "zio-cli"        -> "dev.zio:zio-cli_3:0.8.1",
        "mill-scalalib"  -> "com.lihaoyi:mill-scalalib_3:0.12.0",
        "scala3-library" -> "org.scala-lang:scala3-library_3:3.8.4"
      )
      assert(
        aliases.forall((alias, coordinate) => SquireCellar.resolveCoordinate(alias) == coordinate) &&
          SquireCellar.resolveCoordinate("group:artifact:1") == "group:artifact:1" &&
          SquireCellar.repositoryFlags(CellarSettings(List("one", "two"))) ==
          Chunk("--repository", "one", "--repository", "two")
      )
    }

    "prefer the configured binary then PATH and fail when neither exists" in
      assert(
        SquireCellar.executable(
          CellarSettings(binary = Some("/opt/cellar")),
          TestSquirePlatform(Present("/usr/bin/cellar"))
        ) ==
          Result.Success("/opt/cellar") &&
          SquireCellar.executable(CellarSettings(), TestSquirePlatform(Present("/usr/bin/cellar"))) ==
          Result.Success("/usr/bin/cellar") &&
          SquireCellar.executable(CellarSettings(), TestSquirePlatform()).isFailure
      )

    "build exact argv for get search and deps options" in {
      val settings = CellarSettings(List("https://repo.example/maven"))
      val flags    = Chunk("--repository", "https://repo.example/maven")
      assert(
        SquireCellar.command(
          CellarAction.Get(
            "kyo-case-app",
            "kyo.KyoCommand",
            hideInherited = true,
            groupInherited = true,
            limit = Some(20)
          ),
          settings,
          "/usr/bin/cellar"
        ) == ProcessRequest(
          Chunk("/usr/bin/cellar", "get-external") ++ flags ++
            Chunk(
              "io.getkyo:kyo-case-app_3:1.0.0-RC5",
              "kyo.KyoCommand",
              "--hide-inherited",
              "--group-inherited",
              "--limit",
              "20"
            )
        ) &&
          SquireCellar.command(CellarAction.Search("zio:2.1.26", "provide", Some(7)), settings, "cellar") ==
          ProcessRequest(Chunk("cellar", "search-external") ++ flags ++
            Chunk("dev.zio:zio_3:2.1.26", "provide", "--limit", "7")) &&
          SquireCellar.command(CellarAction.Deps("mill-scalalib"), CellarSettings(), "cellar") ==
          ProcessRequest(Chunk("cellar", "deps", "com.lihaoyi:mill-scalalib_3:0.12.0"))
      )
    }

    "validates an absolute writable temp directory and passes it to the native process" in {
      for
        root <- SquireFixtures.scratch("cellar-temp")
        relative = Path("relative-temp")
        missing  = root / "missing"
        valid    <- SquireCellar.validateTempDirectory(Some(root.toString))
        invalidRelative <- Abort.run[SquireError](SquireCellar.validateTempDirectory(Some(relative.toString)))
        invalidMissing  <- Abort.run[SquireError](SquireCellar.validateTempDirectory(Some(missing.toString)))
      yield assert(
        valid == Present(root) && invalidRelative.isFailure && invalidMissing.isFailure &&
          SquireCellar.command(CellarAction.Deps("mill-scalalib"), CellarSettings(), "cellar", valid) ==
          ProcessRequest(
            Chunk(
              "cellar",
              s"-Djava.io.tmpdir=$root",
              "deps",
              "com.lihaoyi:mill-scalalib_3:0.12.0"
            )
          )
      )
    }

    "preserve legacy zero and negative limit argv semantics" in
      assert(
        SquireCellar.command(CellarAction.Search("coord", "query", Some(0)), CellarSettings(), "cellar") ==
          ProcessRequest(Chunk("cellar", "search-external", "coord", "query")) &&
          SquireCellar.command(CellarAction.Search("coord", "query", Some(-2)), CellarSettings(), "cellar") ==
          ProcessRequest(Chunk("cellar", "search-external", "coord", "query", "--limit", "-2"))
      )

    "resolve bare executables with POSIX names and Windows PATHEXT" in {
      for
        root <- SquireFixtures.scratch("cellar-executable")
        posixBin   = root / "posix-bin"
        windowsBin = root / "windows-bin"
        _ <- Sync.defer {
          Files.createDirectories(posixBin.toJava)
          Files.createDirectories(windowsBin.toJava)
          Files.writeString((posixBin / "cellar").toJava, "probe")
          Files.writeString((windowsBin / "cellar.CMD").toJava, "probe")
        }
        posix = SquireExecutableLookup.find(
          "cellar",
          List(posixBin),
          windows = false,
          pathExtensions = Nil,
          candidate => Files.isRegularFile(candidate.toJava)
        )
        windows = SquireExecutableLookup.find(
          "cellar",
          List(windowsBin),
          windows = true,
          pathExtensions = List(".EXE", ".CMD", ".BAT"),
          candidate => Files.isRegularFile(candidate.toJava)
        )
      yield assert(posix == Present((posixBin / "cellar").toString) &&
        windows == Present((windowsBin / "cellar.CMD").toString))
    }
  }

class SquireRepoSpec extends Test[Any]:
  "naming and clone requests" - {
    "extract names and organizations from HTTPS SSH and local sources" in {
      for
        root <- SquireFixtures.scratch("repo-names")
        local = root / "finos" / "morphir-scala"
        _ <- Sync.defer(Files.createDirectories(local.toJava))
      yield assert(
        SquireRepo.nameFrom("https://github.com/finos/morphir.git") == "morphir" &&
          SquireRepo.orgFrom("https://github.com/finos/morphir.git") == Some("finos") &&
          SquireRepo.nameFrom("git@github.com:com-lihaoyi/mill.git") == "mill" &&
          SquireRepo.orgFrom("git@github.com:com-lihaoyi/mill.git") == Some("com-lihaoyi") &&
          SquireRepo.nameFrom(local.toString) == "morphir-scala" &&
          SquireRepo.orgFrom(local.toString) == Some("finos")
      )
    }

    "reject duplicate names depth/full combinations and sparse non-clones" in {
      val manifest = ReferenceManifest(List(SquireRepoFixtures.repo("mill")))
      assert(
        SquireRepo.validate(ReferenceAdd("https://github.com/other/mill"), manifest).isFailure &&
          SquireRepo.validate(
            ReferenceAdd("https://github.com/finos/morphir", depth = Some(2), full = true),
            ReferenceManifest()
          ).isFailure &&
          SquireRepo.validate(
            ReferenceAdd("/tmp/morphir", strategy = "symlink", sparse = List("docs")),
            ReferenceManifest()
          ).isFailure &&
          SquireRepo.validate(
            ReferenceAdd("/tmp/morphir", strategy = "worktree", sparse = List("docs")),
            ReferenceManifest()
          ).isFailure
      )
    }

    "reject empty dot separator and normalization-alias path components" in {
      val invalidNames = List("", ".", "..", "nested/repo", "nested\\repo", "one/../repo")
      assert(
        invalidNames.forall(name =>
          SquireRepo.validate(
            ReferenceAdd("https://github.com/finos/morphir", name = Some(name)),
            ReferenceManifest()
          ).isFailure
        ) &&
          SquireRepo.validate(ReferenceAdd("https://github.com/../morphir"), ReferenceManifest()).isFailure
      )
    }

    "prefer gh only for authenticated GitHub clones and preserve exact Git fallback argv" in {
      val options       = ReferenceAdd("https://github.com/finos/morphir", ref = Some("main"), depth = Some(3))
      val dest          = Path("/repo/.refs/finos/morphir")
      val authenticated = ProcessResult(ProcessRequest(Chunk("gh", "auth", "status")), 1, "", "✓ Logged in")
      val rejected      = authenticated.copy(stdout = "", stderr = "not logged in")
      assert(
        SquireRepo.ghAuthenticated(authenticated) && !SquireRepo.ghAuthenticated(rejected) &&
          SquireRepo.cloneRequest(options, dest, useGh = true) == ProcessRequest(
            Chunk(
              "gh",
              "repo",
              "clone",
              options.urlOrPath,
              dest.toString,
              "--",
              "--depth",
              "3",
              "--branch",
              "main",
              "--single-branch"
            )
          ) &&
          SquireRepo.cloneRequest(options, dest, useGh = false) == ProcessRequest(
            Chunk(
              "git",
              "clone",
              "--depth",
              "3",
              "--branch",
              "main",
              "--single-branch",
              options.urlOrPath,
              dest.toString
            )
          )
      )
    }

    "compose sparse clone and checkout argv" in {
      val options =
        ReferenceAdd("https://github.com/finos/morphir", full = true, sparse = List("docs", "website", "wit"))
      val dest = Path("/repo/.refs/finos/morphir")
      assert(
        SquireRepo.cloneRequest(options, dest, useGh = false) == ProcessRequest(
          Chunk("git", "clone", "--filter=blob:none", "--sparse", options.urlOrPath, dest.toString)
        ) &&
          SquireRepo.sparseRequest(dest, options.sparse) == ProcessRequest(
            Chunk("git", "-C", dest.toString, "sparse-checkout", "set", "docs", "website", "wit")
          )
      )
    }

    "preserve legacy zero and negative clone-depth argv semantics" in {
      val dest = Path("/repo/.refs/finos/morphir")
      assert(
        SquireRepo.cloneRequest(ReferenceAdd("url", depth = Some(0)), dest, useGh = false) ==
          ProcessRequest(Chunk("git", "clone", "--depth", "1", "url", dest.toString)) &&
          SquireRepo.cloneRequest(ReferenceAdd("url", depth = Some(-2)), dest, useGh = false) ==
          ProcessRequest(Chunk("git", "clone", "--depth", "-2", "url", dest.toString))
      )
    }
  }

  "control root and manifest" - {
    "reject a symlinked control root throughout the read lifecycle" in {
      for
        root <- SquireFixtures.scratch("repo-control-root-read")
        refs     = root / ".refs"
        external = root / "external-refs"
        entry    = SquireRepoFixtures.repo("mill")
        original = SquireJson.encode(ReferenceManifest(List(entry))) + "\n"
        _ <- Sync.defer {
          Files.createDirectories(external.toJava)
          Files.writeString((external / "manifest.json").toJava, original)
          Files.createSymbolicLink(refs.toJava, external.toJava)
        }
        runner = RuleRunner(SquireRepoFixtures.ok)
        loaded   <- Abort.run[SquireError](SquireRepo.loadManifest(root))
        listed   <- Abort.run[SquireError](SquireRepo.list(root, asJson = false, runner))
        detailed <- Abort.run[SquireError](SquireRepo.status(root, None, runner))
        removed  <- Abort.run[SquireError](
          SquireRepo.remove("mill", keepFiles = false, root, runner, TestSquirePlatform())
        )
        after <- Sync.defer(Files.readString((external / "manifest.json").toJava))
      yield assert(
        loaded.isFailure && listed.isFailure && detailed.isFailure && removed.isFailure &&
          runner.requests.isEmpty && after == original && Files.isSymbolicLink(refs.toJava)
      )
    }

    "reject a symlinked control root before write lifecycle effects" in {
      for
        saveRoot <- SquireFixtures.scratch("repo-control-root-save")
        saveRefs     = saveRoot / ".refs"
        saveExternal = saveRoot / "external-refs"
        original     = "{\"repos\":[]}\n"
        _ <- Sync.defer {
          Files.createDirectories(saveExternal.toJava)
          Files.writeString((saveExternal / "manifest.json").toJava, original)
          Files.createSymbolicLink(saveRefs.toJava, saveExternal.toJava)
        }
        saved <- Abort.run[SquireError](
          SquireRepo.saveManifest(saveRoot, ReferenceManifest(List(SquireRepoFixtures.repo("mill"))))
        )
        savedAfter <- Sync.defer(Files.readString((saveExternal / "manifest.json").toJava))
        addRoot    <- SquireFixtures.scratch("repo-control-root-add")
        addRefs     = addRoot / ".refs"
        addExternal = addRoot / "external-refs"
        _ <- Sync.defer {
          Files.createDirectories(addExternal.toJava)
          Files.writeString((addExternal / "manifest.json").toJava, original)
          Files.createSymbolicLink(addRefs.toJava, addExternal.toJava)
        }
        runner = RuleRunner(SquireRepoFixtures.cloneResponse)
        added <- Abort.run[SquireError](
          SquireRepo.add(
            ReferenceAdd("https://github.com/finos/morphir"),
            addRoot,
            runner,
            TestSquirePlatform()
          )
        )
        addedAfter <- Sync.defer(Files.readString((addExternal / "manifest.json").toJava))
      yield assert(
        saved.isFailure && added.isFailure && savedAfter == original && addedAfter == original &&
          runner.requests.isEmpty && !Files.exists(
            (addExternal / "finos").toJava,
            java.nio.file.LinkOption.NOFOLLOW_LINKS
          )
      )
    }

    "reject a final manifest symlink throughout the read lifecycle" in {
      for
        root <- SquireFixtures.scratch("repo-manifest-link-read")
        refs     = root / ".refs"
        external = root / "external-manifest.json"
        entry    = SquireRepoFixtures.repo("mill")
        original = SquireJson.encode(ReferenceManifest(List(entry))) + "\n"
        _ <- Sync.defer {
          Files.createDirectories(refs.toJava)
          Files.writeString(external.toJava, original)
          Files.createSymbolicLink((refs / "manifest.json").toJava, external.toJava)
        }
        runner = RuleRunner(SquireRepoFixtures.ok)
        loaded   <- Abort.run[SquireError](SquireRepo.loadManifest(root))
        listed   <- Abort.run[SquireError](SquireRepo.list(root, asJson = false, runner))
        detailed <- Abort.run[SquireError](SquireRepo.status(root, None, runner))
        removed  <- Abort.run[SquireError](
          SquireRepo.remove("mill", keepFiles = false, root, runner, TestSquirePlatform())
        )
        after <- Sync.defer(Files.readString(external.toJava))
      yield assert(
        loaded.isFailure && listed.isFailure && detailed.isFailure && removed.isFailure &&
          runner.requests.isEmpty && after == original && Files.isSymbolicLink((refs / "manifest.json").toJava)
      )
    }

    "reject a final manifest symlink before write lifecycle effects" in {
      for
        saveRoot <- SquireFixtures.scratch("repo-manifest-link-save")
        saveRefs     = saveRoot / ".refs"
        saveExternal = saveRoot / "external-manifest.json"
        original     = "{\"repos\":[]}\n"
        _ <- Sync.defer {
          Files.createDirectories(saveRefs.toJava)
          Files.writeString(saveExternal.toJava, original)
          Files.createSymbolicLink((saveRefs / "manifest.json").toJava, saveExternal.toJava)
        }
        saved <- Abort.run[SquireError](
          SquireRepo.saveManifest(saveRoot, ReferenceManifest(List(SquireRepoFixtures.repo("mill"))))
        )
        savedAfter <- Sync.defer(Files.readString(saveExternal.toJava))
        addRoot    <- SquireFixtures.scratch("repo-manifest-link-add")
        addRefs     = addRoot / ".refs"
        addExternal = addRoot / "external-manifest.json"
        _ <- Sync.defer {
          Files.createDirectories(addRefs.toJava)
          Files.writeString(addExternal.toJava, original)
          Files.createSymbolicLink((addRefs / "manifest.json").toJava, addExternal.toJava)
        }
        runner = RuleRunner(SquireRepoFixtures.cloneResponse)
        added <- Abort.run[SquireError](
          SquireRepo.add(
            ReferenceAdd("https://github.com/finos/morphir"),
            addRoot,
            runner,
            TestSquirePlatform()
          )
        )
        addedAfter <- Sync.defer(Files.readString(addExternal.toJava))
      yield assert(
        saved.isFailure && added.isFailure && savedAfter == original && addedAfter == original &&
          runner.requests.isEmpty && !Files.exists((addRefs / "finos").toJava, java.nio.file.LinkOption.NOFOLLOW_LINKS)
      )
    }
  }

  "add and manifest" - {
    "reject an existing destination before invoking clone" in {
      for
        root <- SquireFixtures.scratch("repo-add-collision")
        dest = root / ".refs" / "finos" / "morphir"
        _ <- Sync.defer(Files.createDirectories(dest.toJava))
        runner = RuleRunner(SquireRepoFixtures.ok)
        result <- Abort.run[SquireError](
          SquireRepo.add(
            ReferenceAdd("https://github.com/finos/morphir"),
            root,
            runner,
            TestSquirePlatform()
          )
        )
      yield assert(result.isFailure && runner.requests.isEmpty && Files.isDirectory(dest.toJava))
    }

    "record clone metadata and round-trip it through Kyo Schema JSON" in {
      for
        root <- SquireFixtures.scratch("repo-clone")
        runner = RuleRunner(SquireRepoFixtures.cloneResponse)
        added <- SquireRepo.add(
          ReferenceAdd("https://github.com/finos/morphir", ref = Some("main"), sparse = List("docs", "wit")),
          root,
          runner,
          TestSquirePlatform()
        )
        loaded <- SquireRepo.loadManifest(root)
      yield assert(
        added.name == "morphir" && added.org == Some("finos") && added.path == "finos/morphir" &&
          added.strategy == "clone" && added.url == Some("https://github.com/finos/morphir") &&
          added.ref == Some("main") && added.commit == Some("abc123") && added.depth == Some(1) &&
          added.sparse == List("docs", "wit") && added.added == "2026-08-08T12:34:56Z" &&
          loaded == ReferenceManifest(List(added)) &&
          runner.requests.exists(_.argv == Chunk(
            "git",
            "-C",
            (root / ".refs" / "finos" / "morphir").toString,
            "sparse-checkout",
            "set",
            "docs",
            "wit"
          ))
      )
    }

    "create local symlinks and worktrees with their exact metadata commands" in {
      for
        root <- SquireFixtures.scratch("repo-local")
        source = root / "sources" / "finos" / "morphir"
        _ <- Sync.defer(Files.createDirectories(source.toJava))
        symlinkRunner = RuleRunner(SquireRepoFixtures.localMetadataResponse)
        linked <-
          SquireRepo.add(ReferenceAdd(source.toString, strategy = "symlink"), root, symlinkRunner, TestSquirePlatform())
        worktreeRunner = RuleRunner(SquireRepoFixtures.localMetadataResponse)
        worktree <- SquireRepo.add(
          ReferenceAdd(source.toString, name = Some("morphir-v4"), ref = Some("v4"), strategy = "worktree"),
          root,
          worktreeRunner,
          TestSquirePlatform()
        )
        linkPath     = root / ".refs" / linked.path
        worktreePath = root / ".refs" / worktree.path
      yield assert(
        Files.isSymbolicLink(linkPath.toJava) &&
          Files.readSymbolicLink(linkPath.toJava) == source.toJava.toRealPath() &&
          linked.source == Some(source.toJava.toRealPath().toString) && linked.ref == Some("main") &&
          linked.commit == Some("abc123") &&
          worktree.path == "finos/.worktrees/morphir/morphir-v4" &&
          worktreeRunner.requests.head.argv ==
          Chunk(
            "git",
            "-C",
            source.toJava.toRealPath().toString,
            "worktree",
            "add",
            worktreePath.toJava.toAbsolutePath.normalize.toString,
            "v4"
          )
      )
    }
  }

  "list and status" - {
    "emit manifest JSON and report missing dirty drift and sparse state with filtered exits" in {
      for
        root <- SquireFixtures.scratch("repo-status")
        refs = root / ".refs"
        _ <- Sync.defer {
          Files.createDirectories((refs / "finos" / "dirty").toJava)
          Files.createDirectories((refs / "finos" / "drift").toJava)
          Files.createDirectories((refs / "finos" / "sparse").toJava)
        }
        manifest = ReferenceManifest(
          List(
            SquireRepoFixtures.repo("missing", commit = Some("abc123")),
            SquireRepoFixtures.repo("dirty", commit = Some("abc123")),
            SquireRepoFixtures.repo("drift", commit = Some("old123")),
            SquireRepoFixtures.repo("sparse", commit = Some("abc123"), sparse = List("docs", "wit"))
          )
        )
        _ <- SquireRepo.saveManifest(root, manifest)
        runner = RuleRunner(SquireRepoFixtures.statusResponse)
        json    <- SquireRepo.list(root, asJson = true, runner)
        all     <- SquireRepo.status(root, None, runner)
        sparse  <- SquireRepo.status(root, Some("sparse"), runner)
        missing <- SquireRepo.status(root, Some("missing"), runner)
        unknown <- SquireRepo.status(root, Some("unknown"), runner)
      yield assert(
        SquireJson.decode[ReferenceManifest](json.trim) == Result.Success(manifest) &&
          all.exitCode == 1 && all.output.contains("MISSING") && all.output.contains("DIRTY") &&
          all.output.contains("DRIFT") &&
          all.output.contains("sparse:   docs wit") &&
          sparse.exitCode == 0 && sparse.output.contains("in sync with manifest") &&
          missing.exitCode == 1 && unknown.exitCode == 1 && unknown.output.contains("not in manifest")
      )
    }

    "share legacy disk labels and make non-Git and failed Git probes unhealthy" in {
      for
        root <- SquireFixtures.scratch("repo-status-labels")
        refs   = root / ".refs"
        target = root / "target-repo"
        _ <- Sync.defer {
          List("ok", "modified", "nongit", "git-error").foreach(name =>
            Files.createDirectories((refs / "finos" / name).toJava)
          )
          Files.createDirectories(target.toJava)
          Files.createSymbolicLink((refs / "finos" / "linked").toJava, target.toJava)
          Files.createSymbolicLink((refs / "finos" / "broken").toJava, (root / "absent-target").toJava)
        }
        manifest = ReferenceManifest(List(
          SquireRepoFixtures.repo("ok", sparse = List("docs")),
          SquireRepoFixtures.repo("modified", commit = Some("old123")),
          SquireRepoFixtures.repo("nongit"),
          SquireRepoFixtures.repo("git-error"),
          SquireRepoFixtures.repo("linked", strategy = "symlink", source = Some(target.toString)),
          SquireRepoFixtures.repo("broken", strategy = "symlink", source = Some((root / "absent-target").toString)),
          SquireRepoFixtures.repo("missing")
        ))
        _ <- SquireRepo.saveManifest(root, manifest)
        runner = RuleRunner(SquireRepoFixtures.legacyStatusResponse)
        listed   <- SquireRepo.list(root, asJson = false, runner)
        detailed <- SquireRepo.status(root, None, runner)
      yield assert(
        listed.contains("OK (abc123) [sparse]") &&
          listed.contains("MODIFIED (was old123, now new123)") &&
          listed.contains("DIR_NO_GIT") && listed.contains("GIT_ERROR") &&
          listed.contains("symlink →") && listed.contains("BROKEN_SYMLINK") && listed.contains("MISSING") &&
          detailed.exitCode == 1 && detailed.output.contains("DIR_NO_GIT") && detailed.output.contains("GIT_ERROR")
      )
    }

    "reject escaped and intermediate-link manifest paths before filesystem or Git inspection" in {
      for
        root <- SquireFixtures.scratch("repo-status-path-policy")
        refs    = root / ".refs"
        outside = root / "outside"
        _ <- Sync.defer {
          Files.createDirectories(refs.toJava)
          Files.createDirectories(outside.toJava)
          Files.createSymbolicLink((refs / "redirect").toJava, outside.toJava)
        }
        manifest = ReferenceManifest(List(
          SquireRepoFixtures.repo("lexical", path = "../outside"),
          SquireRepoFixtures.repo("redirected", path = "redirect/repo")
        ))
        _ <- SquireRepo.saveManifest(root, manifest)
        runner = RuleRunner(SquireRepoFixtures.ok)
        report <- SquireRepo.status(root, None, runner)
        listed <- SquireRepo.list(root, asJson = false, runner)
      yield assert(report.exitCode == 1 && report.output.contains("INVALID_PATH") && listed.contains("INVALID_PATH") &&
        runner.requests.isEmpty)
    }
  }

  "remove" - {
    "keep files on request and safely delete clones with empty-parent pruning" in {
      for
        root <- SquireFixtures.scratch("repo-remove-clone")
        dest = root / ".refs" / "finos" / "mill"
        _ <- Sync.defer {
          Files.createDirectories(dest.toJava)
          Files.writeString((dest / "README.md").toJava, "fixture")
        }
        entry = SquireRepoFixtures.repo("mill")
        _ <- SquireRepo.saveManifest(root, ReferenceManifest(List(entry)))
        _ <- SquireRepo.remove("mill", keepFiles = true, root, RuleRunner(SquireRepoFixtures.ok), TestSquirePlatform())
        kept <- dest.exists
        _    <- SquireRepo.saveManifest(root, ReferenceManifest(List(entry)))
        _ <- SquireRepo.remove("mill", keepFiles = false, root, RuleRunner(SquireRepoFixtures.ok), TestSquirePlatform())
        deleted <- dest.exists
        parent  <- (root / ".refs" / "finos").exists
      yield assert(kept && !deleted && !parent)
    }

    "keep-files preserves empty checkout parents and reject symlink paths lexically outside refs" in {
      for
        root <- SquireFixtures.scratch("repo-remove-lexical-escape")
        emptyParent = root / ".refs" / "finos"
        outside     = root / "outside"
        outsideLink = root / "outside-link"
        _ <- Sync.defer {
          Files.createDirectories(emptyParent.toJava)
          Files.createDirectories(outside.toJava)
          Files.createSymbolicLink(outsideLink.toJava, outside.toJava)
        }
        keptEntry = SquireRepoFixtures.repo("missing")
        _ <- SquireRepo.saveManifest(root, ReferenceManifest(List(keptEntry)))
        _ <-
          SquireRepo.remove("missing", keepFiles = true, root, RuleRunner(SquireRepoFixtures.ok), TestSquirePlatform())
        emptyParentKept <- emptyParent.exists
        escapedEntry = SquireRepoFixtures.repo(
          "outside-link",
          path = "../outside-link",
          strategy = "symlink",
          source = Some(outside.toString)
        )
        _       <- SquireRepo.saveManifest(root, ReferenceManifest(List(escapedEntry)))
        escaped <- Abort.run[SquireError](
          SquireRepo.remove(
            "outside-link",
            keepFiles = false,
            root,
            RuleRunner(SquireRepoFixtures.ok),
            TestSquirePlatform()
          )
        )
      yield assert(emptyParentKept && escaped.isFailure && Files.isSymbolicLink(outsideLink.toJava))
    }

    "unlink escaped symlinks without deleting targets and reject escaped recursive deletion" in {
      for
        root <- SquireFixtures.scratch("repo-remove-escape")
        outside = root / "outside"
        link    = root / ".refs" / "finos" / "link"
        _ <- Sync.defer {
          Files.createDirectories(link.parent.get.toJava)
          Files.createDirectories(outside.toJava)
          Files.writeString((outside / "keep.txt").toJava, "keep")
          Files.createSymbolicLink(link.toJava, outside.toJava)
        }
        symlinkEntry = SquireRepoFixtures.repo("link", strategy = "symlink", source = Some(outside.toString))
        _ <- SquireRepo.saveManifest(root, ReferenceManifest(List(symlinkEntry)))
        _ <- SquireRepo.remove("link", keepFiles = false, root, RuleRunner(SquireRepoFixtures.ok), TestSquirePlatform())
        targetKept <- (outside / "keep.txt").exists
        linkGone   <- link.exists(followLinks = false).map(!_)
        _          <- Sync.defer {
          Files.createDirectories(link.parent.get.toJava)
          Files.createSymbolicLink(link.toJava, outside.toJava)
        }
        cloneEntry = SquireRepoFixtures.repo("link")
        _       <- SquireRepo.saveManifest(root, ReferenceManifest(List(cloneEntry)))
        escaped <- Abort.run[SquireError](SquireRepo.remove(
          "link",
          keepFiles = false,
          root,
          RuleRunner(SquireRepoFixtures.ok),
          TestSquirePlatform()
        ))
        targetStillKept <- (outside / "keep.txt").exists
      yield assert(targetKept && linkGone && escaped.isFailure && targetStillKept && Files.isSymbolicLink(link.toJava))
    }

    "reject intermediate redirects and final clone aliases without touching their targets" in {
      for
        root <- SquireFixtures.scratch("repo-remove-aliases")
        refs       = root / ".refs"
        outside    = root / "outside"
        outsideDir = root / "outside-dir"
        targetRepo = refs / "target-repo"
        _ <- Sync.defer {
          Files.createDirectories(refs.toJava)
          Files.createDirectories(outside.toJava)
          Files.createDirectories(outsideDir.toJava)
          Files.createDirectories(targetRepo.toJava)
          Files.writeString((targetRepo / "keep.txt").toJava, "keep")
          Files.createSymbolicLink((refs / "redirect").toJava, outsideDir.toJava)
          Files.createSymbolicLink((outsideDir / "victim").toJava, outside.toJava)
        }
        redirected = SquireRepoFixtures.repo(
          "evil",
          path = "redirect/victim",
          strategy = "symlink",
          source = Some(outside.toString)
        )
        _                <- SquireRepo.saveManifest(root, ReferenceManifest(List(redirected)))
        redirectedResult <- Abort.run[SquireError](
          SquireRepo.remove("evil", keepFiles = false, root, RuleRunner(SquireRepoFixtures.ok), TestSquirePlatform())
        )
        victimKept = Files.isSymbolicLink((outsideDir / "victim").toJava)
        _ <- Sync.defer(Files.createSymbolicLink((refs / "alias").toJava, targetRepo.toJava))
        alias = SquireRepoFixtures.repo("alias", path = "alias")
        _           <- SquireRepo.saveManifest(root, ReferenceManifest(List(alias)))
        aliasResult <- Abort.run[SquireError](
          SquireRepo.remove("alias", keepFiles = false, root, RuleRunner(SquireRepoFixtures.ok), TestSquirePlatform())
        )
      yield assert(
        redirectedResult.isFailure && victimKept && aliasResult.isFailure &&
          Files.isSymbolicLink((refs / "alias").toJava) && Files.exists((targetRepo / "keep.txt").toJava)
      )
    }

    "handle dangling final links by strategy and reject dangling intermediate links" in {
      for
        root <- SquireFixtures.scratch("repo-remove-dangling")
        refs = root / ".refs"
        _ <- Sync.defer {
          Files.createDirectories(refs.toJava)
          Files.createSymbolicLink((refs / "dangling-link").toJava, (root / "missing-target").toJava)
        }
        symlinkEntry = SquireRepoFixtures.repo(
          "dangling-link",
          path = "dangling-link",
          strategy = "symlink",
          source = Some((root / "missing-target").toString)
        )
        _ <- SquireRepo.saveManifest(root, ReferenceManifest(List(symlinkEntry)))
        _ <- SquireRepo.remove(
          "dangling-link",
          keepFiles = false,
          root,
          RuleRunner(SquireRepoFixtures.ok),
          TestSquirePlatform()
        )
        symlinkRemoved = !Files.exists((refs / "dangling-link").toJava, java.nio.file.LinkOption.NOFOLLOW_LINKS)
        _ <- Sync.defer {
          Files.createSymbolicLink((refs / "dangling-clone").toJava, (root / "missing-target").toJava)
          Files.createSymbolicLink((refs / "dangling-parent").toJava, (root / "missing-parent").toJava)
        }
        cloneEntry = SquireRepoFixtures.repo("dangling-clone", path = "dangling-clone")
        _           <- SquireRepo.saveManifest(root, ReferenceManifest(List(cloneEntry)))
        cloneResult <- Abort.run[SquireError](
          SquireRepo.remove(
            "dangling-clone",
            keepFiles = false,
            root,
            RuleRunner(SquireRepoFixtures.ok),
            TestSquirePlatform()
          )
        )
        intermediateEntry = SquireRepoFixtures.repo(
          "nested",
          path = "dangling-parent/nested",
          strategy = "symlink",
          source = Some("unused")
        )
        _                  <- SquireRepo.saveManifest(root, ReferenceManifest(List(intermediateEntry)))
        intermediateResult <- Abort.run[SquireError](
          SquireRepo.remove("nested", keepFiles = false, root, RuleRunner(SquireRepoFixtures.ok), TestSquirePlatform())
        )
      yield assert(
        symlinkRemoved && cloneResult.isFailure && Files.isSymbolicLink((refs / "dangling-clone").toJava) &&
          intermediateResult.isFailure && Files.isSymbolicLink((refs / "dangling-parent").toJava)
      )
    }

    "delete the exact in-base worktree directory when its source is missing" in {
      for
        root <- SquireFixtures.scratch("repo-remove-worktree-fallback")
        dest   = root / ".refs" / "finos" / ".worktrees" / "morphir" / "v4"
        source = root / "missing-source"
        _ <- Sync.defer {
          Files.createDirectories(dest.toJava)
          Files.writeString((dest / "marker").toJava, "fixture")
        }
        entry = SquireRepoFixtures.repo(
          "v4",
          path = "finos/.worktrees/morphir/v4",
          strategy = "worktree",
          source = Some(source.toString),
          ref = Some("v4")
        )
        _ <- SquireRepo.saveManifest(root, ReferenceManifest(List(entry)))
        runner = RuleRunner(SquireRepoFixtures.ok)
        _ <- SquireRepo.remove("v4", keepFiles = false, root, runner, TestSquirePlatform())
      yield assert(!Files.exists(dest.toJava, java.nio.file.LinkOption.NOFOLLOW_LINKS) && runner.requests.isEmpty)
    }

    "remove worktrees through Git and prune their empty hierarchy" in {
      for
        root <- SquireFixtures.scratch("repo-remove-worktree")
        source = root / "source" / "finos" / "morphir"
        dest   = root / ".refs" / "finos" / ".worktrees" / "morphir" / "v4"
        _ <- Sync.defer {
          Files.createDirectories(source.toJava)
          Files.createDirectories(dest.toJava)
        }
        entry = SquireRepoFixtures.repo(
          "v4",
          path = "finos/.worktrees/morphir/v4",
          strategy = "worktree",
          source = Some(source.toString),
          ref = Some("v4")
        )
        _ <- SquireRepo.saveManifest(root, ReferenceManifest(List(entry)))
        runner = RuleRunner { request =>
          if request.argv.contains("worktree") then Files.delete(dest.toJava)
          ProcessResult(request, 0, "", "")
        }
        _      <- SquireRepo.remove("v4", keepFiles = false, root, runner, TestSquirePlatform())
        parent <- (root / ".refs" / "finos").exists
      yield assert(
        runner.requests.head.argv ==
          Chunk("git", "-C", source.toString, "worktree", "remove", "--force", dest.toString) && !parent
      )
    }
  }

object SquireRepoFixtures:
  def repo(
      name: String,
      path: String = "",
      strategy: String = "clone",
      source: Option[String] = None,
      ref: Option[String] = Some("main"),
      commit: Option[String] = Some("abc123"),
      sparse: List[String] = Nil
  ): ReferenceRepo =
    ReferenceRepo(
      name = name,
      org = Some("finos"),
      path = if path.nonEmpty then path else s"finos/$name",
      added = "2026-08-08T12:34:56Z",
      strategy = strategy,
      url = if strategy == "clone" then Some(s"https://github.com/finos/$name") else None,
      source = source,
      ref = ref,
      commit = commit,
      depth = if strategy == "clone" then Some(1) else None,
      sparse = sparse
    )

  def ok(request: ProcessRequest): ProcessResult = ProcessResult(request, 0, "", "")

  def cloneResponse(request: ProcessRequest): ProcessResult =
    val argv = request.argv
    if argv == Chunk("gh", "auth", "status") then ProcessResult(request, 1, "", "not logged in")
    else if argv.contains("symbolic-ref") then ProcessResult(request, 0, "main\n", "")
    else if argv.contains("rev-parse") then ProcessResult(request, 0, "abc123\n", "")
    else ProcessResult(request, 0, "", "")

  def localMetadataResponse(request: ProcessRequest): ProcessResult =
    if request.argv.contains("symbolic-ref") then ProcessResult(request, 0, "main\n", "")
    else if request.argv.contains("rev-parse") then ProcessResult(request, 0, "abc123\n", "")
    else ProcessResult(request, 0, "", "")

  def statusResponse(request: ProcessRequest): ProcessResult =
    val path = request.argv.lift(2).getOrElse("")
    if request.argv.contains("rev-parse") then
      ProcessResult(request, 0, if path.endsWith("drift") then "new123\n" else "abc123\n", "")
    else if request.argv.contains("symbolic-ref") then ProcessResult(request, 0, "main\n", "")
    else if request.argv.contains("status") then
      ProcessResult(request, 0, if path.endsWith("dirty") then " M README.md\n" else "", "")
    else ProcessResult(request, 0, "", "")

  def legacyStatusResponse(request: ProcessRequest): ProcessResult =
    val path = request.argv.lift(2).getOrElse("")
    if request.argv.contains("rev-parse") then
      if path.endsWith("nongit") then ProcessResult(request, 128, "", "not a git repository")
      else ProcessResult(request, 0, if path.endsWith("modified") then "new123\n" else "abc123\n", "")
    else if request.argv.contains("symbolic-ref") then ProcessResult(request, 0, "main\n", "")
    else if request.argv.contains("status") then
      ProcessResult(request, if path.endsWith("git-error") then 2 else 0, "", "status failed")
    else ProcessResult(request, 0, "", "")

class SquireEnvSpec extends Test[Any]:
  "environment report" - {
    "detects Claude Code and CI without confusing either with a sandbox" in {
      for
        root   <- SquireFixtures.scratch("env-detection")
        report <- SquireEnv.report(
          1.seconds,
          SquireFixtures.platform(
            root,
            SquireEnv.CheckResult(Present(true), "loopback bind+accept+connect succeeded", 0.01),
            environment = Map(
              "CLAUDECODE"                -> "1",
              "CLAUDE_CODE_ENTRYPOINT"    -> "cli",
              "CLAUDE_CODE_SESSION_ID"    -> "session-1",
              "CLAUDE_CODE_CHILD_SESSION" -> "1",
              "GITHUB_ACTIONS"            -> "true"
            )
          ),
          root
        )
        serialized = SquireJson.encode(report)
      yield assert(
        report.claudeCode.detected &&
          report.claudeCode.entrypoint == Present("cli") &&
          report.claudeCode.sessionId == Present("session-1") &&
          report.claudeCode.childSession &&
          report.ci &&
          !report.sandboxed &&
          serialized.contains("jvm_network")
      )
    }

    "merges every settings level and tolerates malformed files" in {
      for
        root <- SquireFixtures.scratch("env-settings")
        managed = root / "managed-settings.json"
        home    = root / "home"
        _ <- Sync.defer {
          Files.createDirectories((home / ".claude").toJava)
          Files.createDirectories((root / ".claude").toJava)
          Files.writeString(
            managed.toJava,
            """{"sandbox":{"enabled":true,"network":{"allowedDomains":["managed.example"]}}}"""
          )
          Files.writeString((home / ".claude" / "settings.json").toJava, "{}")
          Files.writeString(
            (root / ".claude" / "settings.json").toJava,
            """{"unknown":true,"sandbox":{"enabled":true,"network":{"allowedDomains":["project.example"]}}}"""
          )
          Files.writeString((root / ".claude" / "settings.local.json").toJava, "not json")
        }
        report <- SquireEnv.report(
          1.seconds,
          SquireFixtures.platform(
            root,
            SquireEnv.CheckResult(Present(true), "ok", 0.0),
            home = Some(home),
            managed = Chunk(managed)
          ),
          root
        )
        settings = report.claudeSettings
      yield assert(
        settings.sandboxEnabled == Map(
          "managed"       -> Present(true),
          "user"          -> Absent,
          "project"       -> Present(true),
          "project_local" -> Absent
        ) &&
          settings.networkAllowedDomains == Chunk("managed.example", "project.example") &&
          settings.networkDeniedDomains == Chunk.empty &&
          settings.sources("managed").present &&
          !settings.sources("user").present &&
          settings.sources("project").present &&
          !settings.sources("project_local").present
      )
    }
  }

  "checks" - {
    "returns JVM loopback success failure and timeout outcomes" in {
      for
        root    <- SquireFixtures.scratch("env-network")
        success <- SquireEnv.check(
          SquireEnv.CheckKind.JvmNetwork,
          1.seconds,
          SquireFixtures.platform(root, SquireEnv.CheckResult(Present(true), "ok", 0.0))
        )
        failure <- SquireEnv.check(
          SquireEnv.CheckKind.JvmNetwork,
          1.seconds,
          SquireFixtures.platform(root, SquireEnv.CheckResult(Present(false), "SocketException: refused", 0.0))
        )
        timeout <- SquireEnv.check(
          SquireEnv.CheckKind.JvmNetwork,
          1.seconds,
          SquireFixtures.platform(
            root,
            SquireEnv.CheckResult(Present(false), "JVM loopback probe hung past 1s timeout", 1.0)
          )
        )
      yield assert(success && !failure && !timeout)
    }

    "probes the effective JVM temp directory and cleans a successful probe" in {
      for
        root   <- SquireFixtures.scratch("env-var-folders")
        absent <- SquireEnv.check(
          SquireEnv.CheckKind.VarFolders,
          1.seconds,
          SquireFixtures.platform(
            root,
            SquireEnv.CheckResult(Present(true), "ok", 0.0),
            jvmTempDirectory = Present(root / "absent")
          )
        )
        writable <- SquireEnv.check(
          SquireEnv.CheckKind.VarFolders,
          1.seconds,
          SquireFixtures.platform(root, SquireEnv.CheckResult(Present(true), "ok", 0.0), varFolders = Some(root))
        )
        probeExists <- (root / ".squire-env-probe").exists
        report <- SquireEnv.report(
          1.seconds,
          SquireFixtures.platform(
            root,
            SquireEnv.CheckResult(Present(true), "ok", 0.0),
            jvmTempDirectory = Present(root)
          ),
          root
        )
      yield assert(
        !absent && writable && !probeExists &&
          report.checks("var_folders_writable").detail.contains(root.toString) &&
          !report.checks("var_folders_writable").detail.contains("/var/folders")
      )
    }

    "reports an unavailable check when the JVM temp property is absent" in {
      for
        root <- SquireFixtures.scratch("env-var-folders-unavailable")
        platform = TestEnvPlatform(
          Map.empty,
          root / "home",
          Chunk.empty,
          root,
          Absent,
          _ => SquireEnv.CheckResult(Present(true), "ok", 0.0),
          _ => SquireEnv.DaemonProbe.Open,
          path => Files.writeString(path.toJava, "squire probe"),
          path => Files.deleteIfExists(path.toJava)
        )
        check <- SquireEnv.check(SquireEnv.CheckKind.VarFolders, 1.seconds, platform)
        report <- SquireEnv.report(1.seconds, platform, root)
      yield assert(
        check &&
          report.checks("var_folders_writable").ok == Absent &&
          report.checks("var_folders_writable").detail == "effective JVM temp directory is unavailable — check skipped"
      )
    }

    "reports a blocked var folders write without leaving a probe" in {
      for
        root <- SquireFixtures.scratch("env-var-folders-blocked")
        platform = SquireFixtures.platform(
          root,
          SquireEnv.CheckResult(Present(true), "ok", 0.0),
          writeProbe = _ => throw java.nio.file.AccessDeniedException("/var/folders/.squire-env-probe")
        )
        blocked     <- SquireEnv.check(SquireEnv.CheckKind.VarFolders, 1.seconds, platform)
        probeExists <- (root / ".squire-env-probe").exists
      yield assert(!blocked && !probeExists)
    }

    "cleans a probe created before a write failure" in {
      for
        root <- SquireFixtures.scratch("env-var-folders-partial")
        platform = SquireFixtures.platform(
          root,
          SquireEnv.CheckResult(Present(true), "ok", 0.0),
          writeProbe = path => {
            Files.writeString(path.toJava, "partial"); throw java.nio.file.AccessDeniedException(path.toString)
          }
        )
        partial <- SquireEnv.check(SquireEnv.CheckKind.VarFolders, 1.seconds, platform)
        removed <- (root / ".squire-env-probe").exists
      yield assert(!partial && !removed)
    }

    "reports a delete probe failure without throwing" in {
      for
        root <- SquireFixtures.scratch("env-var-folders-delete")
        platform = SquireFixtures.platform(
          root,
          SquireEnv.CheckResult(Present(true), "ok", 0.0),
          deleteProbe = _ => throw java.nio.file.AccessDeniedException("/var/folders/.squire-env-probe")
        )
        check  <- SquireEnv.check(SquireEnv.CheckKind.VarFolders, 1.seconds, platform)
        report <- SquireEnv.report(1.seconds, platform, root)
      yield assert(!check && report.checks("var_folders_writable").detail.startsWith("could not clean probe file:"))
    }
  }

class SquireDoctorSpec extends Test[Any]:
  "daemon diagnostics" - {
    "prefers the daemon port file over the server log" in {
      for
        root <- SquireFixtures.scratch("doctor-port-file")
        _    <- SquireFixtures.writeDaemonFiles(root, portFile = Some(41001), logPort = Some(41002))
        platform = SquireFixtures.platform(root, SquireEnv.CheckResult(Present(true), "ok", 0.0))
        report <- SquireDoctor.run(root, RecordingRunner(Chunk.empty), platform)
      yield assert(report.finding("mill_daemon").exists(_.code == "PORT_OPEN") && platform.daemonPorts == Chunk(41001))
    }

    "uses the latest server log port when no port file exists" in {
      for
        root <- SquireFixtures.scratch("doctor-server-log")
        _    <- SquireFixtures.writeDaemonFiles(root, portFile = None, logPort = Some(41003))
        platform = SquireFixtures.platform(root, SquireEnv.CheckResult(Present(true), "ok", 0.0))
        report <- SquireDoctor.run(root, RecordingRunner(Chunk.empty), platform)
      yield assert(report.finding("mill_daemon").exists(_.code == "PORT_OPEN") && platform.daemonPorts == Chunk(41003))
    }

    "distinguishes no daemon sandbox and refused daemon connections" in {
      for
        root     <- SquireFixtures.scratch("doctor-daemon-states")
        noDaemon <- SquireDoctor.run(
          root,
          RecordingRunner(Chunk.empty),
          SquireFixtures.platform(root, SquireEnv.CheckResult(Present(true), "ok", 0.0))
        )
        _       <- SquireFixtures.writeDaemonFiles(root, portFile = Some(41004), logPort = None)
        sandbox <- SquireDoctor.run(
          root,
          RecordingRunner(Chunk.empty),
          SquireFixtures.platform(
            root,
            SquireEnv.CheckResult(Present(true), "ok", 0.0),
            daemonProbe = _ => SquireEnv.DaemonProbe.Sandbox("Operation not permitted")
          )
        )
        refused <- SquireDoctor.run(
          root,
          RecordingRunner(Chunk.empty),
          SquireFixtures.platform(
            root,
            SquireEnv.CheckResult(Present(true), "ok", 0.0),
            daemonProbe = _ => SquireEnv.DaemonProbe.Refused("Connection refused")
          )
        )
      yield assert(
        noDaemon.finding("mill_daemon").exists(_.code == "NO_DAEMON") &&
          sandbox.finding("mill_daemon").exists(_.code == "SANDBOX") &&
          refused.finding("mill_daemon").exists(_.code == "REFUSED")
      )
    }
  }

  "project diagnostics" - {
    "accepts Mill-owned setup YAML main class plugin wiring and effective JVM temp" in {
      for
        root <- SquireFixtures.scratch("doctor-project")
        _    <- Sync.defer {
          Files.createDirectories((root / ".config" / "mise" / "tasks").toJava)
          Files.createDirectories((root / "morphir").toJava)
          Files.writeString((root / ".config" / "mise" / "tasks" / "setup").toJava, "bun install --ignore-scripts\n")
          Files.writeString((root / "package.json").toJava, "{}\n")
          Files.writeString(
            (root / "morphir" / "package.mill.yaml").toJava,
            "mainClass: org.finos.morphir.Main\n"
          )
          val plugin = root / "mill-plugins" / "morphir"
          val modules = List("toolchain", "javascript", "elm-tooling", "core", "elm", "integration")
          modules.foreach(name => Files.createDirectories((plugin / name).toJava))
          Files.writeString(
            (plugin / "package.mill").toJava,
            modules.map(name => s"object ${if name.contains('-') then s"`$name`" else name} extends Module\n").mkString +
              "publishLocalTestRepo publishedPluginRepositories\n"
          )
          val integration = plugin / "integration" / "test" / "src" / "org" / "finos" / "morphir" / "mill" / "PublishedPluginIntegrationTests.scala"
          Files.createDirectories(integration.parent.get.toJava)
          Files.writeString(integration.toJava, "COURSIER_REPOSITORIES millExecutable\n")
          val consumer = plugin / "integration" / "resources" / "published-consumer" / "build.mill"
          Files.createDirectories(consumer.parent.get.toJava)
          Files.writeString(consumer.toJava, "MORPHIR_PUBLISHED_TEST_REPOSITORIES\n")
        }
        report <- SquireDoctor.run(
          root,
          RecordingRunner(Chunk.empty),
          SquireFixtures.platform(root, SquireEnv.CheckResult(Present(true), "ok", 0.0))
        )
      yield assert(
        report.finding("setup").exists(_.code == "OK") &&
          report.finding("main_class_task").exists(_.code == "OK") &&
          report.finding("mill_morphir").exists(_.code == "OK") &&
          report.finding("jvm_temp").exists(_.code == "OK")
      )
    }

    "blocks missing Mill Morphir modules and a relative acquisition cache override" in {
      for
        root <- SquireFixtures.scratch("doctor-project-invalid")
        platform = SquireFixtures.platform(
          root,
          SquireEnv.CheckResult(Present(true), "ok", 0.0),
          environment = Map("MORPHIR_NODE_CACHE" -> "relative-cache")
        )
        report <- SquireDoctor.run(root, RecordingRunner(Chunk.empty), platform)
      yield assert(
        report.finding("mill_morphir").exists(finding => finding.blocked && finding.message.contains("plugin modules")) &&
          report.finding("acquisition_cache").exists(finding => finding.blocked && finding.message.contains("absolute"))
      )
    }

    "detects corrupt acquisition cache content and stale metabuild output" in {
      for
        root <- SquireFixtures.scratch("doctor-cache-metabuild")
        cache = root / "cache"
        digest = "0" * 64
        source = root / "build.mill"
        compiled = root / "out" / "mill-build" / "compile.dest" / "classes" / "build.class"
        _ <- Sync.defer {
          Files.createDirectories((cache / "sha256").toJava)
          Files.writeString((cache / "sha256" / digest).toJava, "not the named digest")
          Files.createDirectories(compiled.parent.get.toJava)
          Files.writeString(compiled.toJava, "compiled")
          Files.writeString(source.toJava, "package build\n")
          Files.setLastModifiedTime(compiled.toJava, java.nio.file.attribute.FileTime.fromMillis(1_700_000_000_000L))
          Files.setLastModifiedTime(source.toJava, java.nio.file.attribute.FileTime.fromMillis(1_700_000_060_000L))
        }
        report <- SquireDoctor.run(
          root,
          RecordingRunner(Chunk.empty),
          SquireFixtures.platform(
            root,
            SquireEnv.CheckResult(Present(true), "ok", 0.0),
            environment = Map("MORPHIR_NODE_CACHE" -> cache.toString)
          )
        )
      yield assert(
        report.finding("acquisition_cache").exists(finding => finding.blocked && finding.code == "CORRUPT") &&
          report.finding("metabuild").exists(finding => finding.blocked && finding.code == "STALE")
      )
    }

    "bounds oversized acquisition cache diagnostics without declaring content corrupt" in {
      var allocatedRoot = Option.empty[Path]
      for
        report <- Scope.run {
          for
            root <- SquireFixtures.scopedScratch("doctor-cache-bounded")
            _    <- Sync.defer { allocatedRoot = Some(root) }
            cache = root / "cache"
            digest = "0" * 64
            _ <- Sync.defer {
              val entry = cache / "sha256" / digest
              Files.createDirectories(entry.parent.get.toJava)
              val channel = Files.newByteChannel(
                entry.toJava,
                java.nio.file.StandardOpenOption.CREATE_NEW,
                java.nio.file.StandardOpenOption.WRITE
              )
              try
                channel.position(65L * 1024 * 1024 - 1L)
                channel.write(java.nio.ByteBuffer.wrap(Array[Byte](0)))
              finally channel.close()
            }
            report <- SquireDoctor.run(
              root,
              RecordingRunner(Chunk.empty),
              SquireFixtures.platform(
                root,
                SquireEnv.CheckResult(Present(true), "ok", 0.0),
                environment = Map("MORPHIR_NODE_CACHE" -> cache.toString)
              )
            )
          yield report
        }
        rootRemoved <- Sync.defer(allocatedRoot.exists(root => !Files.exists(root.toJava)))
        _           <- Sync.defer(allocatedRoot.foreach(SquireFixtures.deleteRecursively))
      yield assert(
        report.finding("acquisition_cache").exists(finding =>
          !finding.blocked && finding.code == "NOTICE" && finding.message.contains("oversized entry")
        ) && rootRemoved
      )
    }

    "treats a digest-named symlink to matching external content as corrupt" in Scope.run {
      for
        externalRoot <- SquireFixtures.scopedScratch("doctor-cache-symlink-external")
        root         <- SquireFixtures.scopedScratch("doctor-cache-symlink")
        cache = root / "cache"
        digest = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        externalTarget = externalRoot / "matching-content"
        _ <- Sync.defer {
          Files.writeString(externalTarget.toJava, "")
          Files.createDirectories((cache / "sha256").toJava)
          Files.createSymbolicLink((cache / "sha256" / digest).toJava, externalTarget.toJava)
        }
        report <- SquireDoctor.run(
          root,
          RecordingRunner(Chunk.empty),
          SquireFixtures.platform(
            root,
            SquireEnv.CheckResult(Present(true), "ok", 0.0),
            environment = Map("MORPHIR_NODE_CACHE" -> cache.toString)
          )
        )
        externalTargetIntact <- Sync.defer(Files.isRegularFile(externalTarget.toJava))
      yield assert(
        report.finding("acquisition_cache").exists(finding => finding.blocked && finding.code == "CORRUPT") &&
          externalTargetIntact
      )
    }

    "requires scoped cleanup for cache fixtures after success and abort" in {
      var completedRoot = Option.empty[Path]
      var abortedRoot   = Option.empty[Path]
      for
        _ <- Scope.run {
          for
            root <- SquireFixtures.scopedScratch("doctor-cache-clean-success")
            _    <- Sync.defer { completedRoot = Some(root) }
          yield ()
        }
        completedRemoved <- Sync.defer(completedRoot.exists(root => !Files.exists(root.toJava)))
        aborted <- Abort.run[String](
          Scope.run {
            for
              root <- SquireFixtures.scopedScratch("doctor-cache-clean-abort")
              _    <- Sync.defer { abortedRoot = Some(root) }
              _    <- Abort.fail("deliberate cleanup regression")
            yield ()
          }
        )
        abortedRemoved <- Sync.defer(abortedRoot.exists(root => !Files.exists(root.toJava)))
        _ <- Sync.defer {
          completedRoot.foreach(SquireFixtures.deleteRecursively)
          abortedRoot.foreach(SquireFixtures.deleteRecursively)
        }
      yield assert(completedRemoved && aborted.isFailure && abortedRemoved)
    }

    "validates a relative cache override before honoring disabled mode and skips corrupt cache content" in Scope.run {
      for
        root <- SquireFixtures.scopedScratch("doctor-cache-disabled")
        cache = root / "cache"
        digest = "0" * 64
        _ <- Sync.defer {
          Files.createDirectories((cache / "sha256").toJava)
          Files.writeString((cache / "sha256" / digest).toJava, "not the named digest")
        }
        disabled <- SquireDoctor.run(
          root,
          RecordingRunner(Chunk.empty),
          SquireFixtures.platform(
            root,
            SquireEnv.CheckResult(Present(true), "ok", 0.0),
            environment = Map(
              "MORPHIR_NODE_CACHE" -> cache.toString,
              "MORPHIR_NODE_DISABLE_MACHINE_CACHE" -> "true"
            )
          )
        )
        relativeDisabled <- SquireDoctor.run(
          root,
          RecordingRunner(Chunk.empty),
          SquireFixtures.platform(
            root,
            SquireEnv.CheckResult(Present(true), "ok", 0.0),
            environment = Map(
              "MORPHIR_NODE_CACHE" -> "relative-cache",
              "MORPHIR_NODE_DISABLE_MACHINE_CACHE" -> "true"
            )
          )
        )
      yield assert(
        disabled.finding("acquisition_cache").exists(finding => !finding.blocked && finding.code == "DISABLED") &&
          relativeDisabled.finding("acquisition_cache").exists(finding => finding.blocked && finding.code == "INVALID")
      )
    }

    "bounds acquisition cache inspection at 256 directory entries" in Scope.run {
      for
        root <- SquireFixtures.scopedScratch("doctor-cache-entry-limit")
        cache = root / "cache"
        _ <- Sync.defer {
          val digestRoot = cache / "sha256"
          Files.createDirectories(digestRoot.toJava)
          (1 to 257).foreach(index => Files.writeString((digestRoot / f"entry-$index%03d").toJava, "fixture"))
        }
        report <- SquireDoctor.run(
          root,
          RecordingRunner(Chunk.empty),
          SquireFixtures.platform(
            root,
            SquireEnv.CheckResult(Present(true), "ok", 0.0),
            environment = Map("MORPHIR_NODE_CACHE" -> cache.toString)
          )
        )
      yield assert(
        report.finding("acquisition_cache").exists(finding =>
          !finding.blocked && finding.code == "NOTICE" && finding.message.contains("directory entry limit reached (256)")
        )
      )
    }

    "blocks non-regular digest entries and bounds unreadable valid entries" in Scope.run {
      val emptyDigest = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
      for
        root <- SquireFixtures.scopedScratch("doctor-cache-unreadable")
        cache = root / "cache"
        digestRoot = cache / "sha256"
        unreadable = digestRoot / emptyDigest
        _ <- Sync.defer {
          Files.createDirectories((digestRoot / ("1" * 64)).toJava)
          Files.writeString(unreadable.toJava, "")
        }
        structural <- SquireDoctor.run(
          root,
          RecordingRunner(Chunk.empty),
          SquireFixtures.platform(
            root,
            SquireEnv.CheckResult(Present(true), "ok", 0.0),
            environment = Map("MORPHIR_NODE_CACHE" -> cache.toString)
          )
        )
        _ <- Sync.defer(Files.delete((digestRoot / ("1" * 64)).toJava))
        _ <- Scope.acquireRelease(
          Sync.defer {
            val original = Files.getPosixFilePermissions(unreadable.toJava)
            Files.setPosixFilePermissions(
              unreadable.toJava,
              java.util.EnumSet.noneOf(classOf[java.nio.file.attribute.PosixFilePermission])
            )
            original
          }
        )(original => Sync.defer(Files.setPosixFilePermissions(unreadable.toJava, original)))
        report <- SquireDoctor.run(
          root,
          RecordingRunner(Chunk.empty),
          SquireFixtures.platform(
            root,
            SquireEnv.CheckResult(Present(true), "ok", 0.0),
            environment = Map("MORPHIR_NODE_CACHE" -> cache.toString)
          )
        )
      yield assert(
        structural.finding("acquisition_cache").exists(finding => finding.blocked && finding.code == "CORRUPT") &&
          report.finding("acquisition_cache").exists(finding =>
            !finding.blocked && finding.code == "NOTICE" && finding.message.contains("unreadable or changed during inspection")
          )
      )
    }

    "bounds acquisition cache hashing after 256 MiB" in Scope.run {
      val entryBytes = 64L * 1024 * 1024
      for
        root <- SquireFixtures.scopedScratch("doctor-cache-total-budget")
        cache = root / "cache"
        _ <- Sync.defer {
          val digestRoot = cache / "sha256"
          Files.createDirectories(digestRoot.toJava)
          (1 to 5).foreach { index =>
            val entry = digestRoot / s"entry-$index"
            val channel = Files.newByteChannel(
              entry.toJava,
              java.nio.file.StandardOpenOption.CREATE_NEW,
              java.nio.file.StandardOpenOption.WRITE
            )
            try
              channel.write(java.nio.ByteBuffer.wrap(Array(index.toByte)))
              channel.position(entryBytes - 1L)
              channel.write(java.nio.ByteBuffer.wrap(Array[Byte](0)))
            finally channel.close()
            val digest = java.security.MessageDigest.getInstance("SHA-256")
            digest.update(index.toByte)
            val zeroes = Array.ofDim[Byte](1024 * 1024)
            var remaining = entryBytes - 1L
            while remaining > 0L do
              val count = math.min(remaining, zeroes.length.toLong).toInt
              digest.update(zeroes, 0, count)
              remaining -= count
            val name = digest.digest().map(byte => f"${byte & 0xff}%02x").mkString
            Files.move(entry.toJava, (digestRoot / name).toJava)
          }
        }
        report <- SquireDoctor.run(
          root,
          RecordingRunner(Chunk.empty),
          SquireFixtures.platform(
            root,
            SquireEnv.CheckResult(Present(true), "ok", 0.0),
            environment = Map("MORPHIR_NODE_CACHE" -> cache.toString)
          )
        )
      yield assert(
        report.finding("acquisition_cache").exists(finding =>
          !finding.blocked && finding.code == "NOTICE" && finding.message.contains("total hash budget reached (268435456 bytes)")
        )
      )
    }
  }

class SquireTrackingSpec extends Test[Any]:
  import SquireTrackingFixtures.*

  "tracking resolution" - {
    "defaults absent settings to auto and reports unavailable when bd is missing" in {
      for
        root   <- SquireFixtures.scratch("tracking-absent")
        report <- SquireTracking.resolve(root, runner(gitFailure, bdFailure), TestSquirePlatform())
      yield assert(report.configuredMode == TrackingMode.Auto && report.effectiveMode == TrackingMode.Unavailable &&
        report.reason == "bd is not on PATH")
    }

    "honours auto beads off and YAML boolean mode settings" in {
      for
        root         <- SquireFixtures.scratch("tracking-modes")
        _            <- settings(root, "auto")
        _            <- beads(root)
        auto         <- SquireTracking.resolve(root, runner(gitShared, bdVersion), TestSquirePlatform(Present("bd")))
        _            <- settings(root, "beads")
        forced       <- SquireTracking.resolve(root, runner(gitShared, bdVersion), TestSquirePlatform(Present("bd")))
        _            <- settings(root, "off")
        off          <- SquireTracking.resolve(root, runner(gitFailure, bdFailure), TestSquirePlatform())
        _            <- settings(root, "false")
        booleanOff   <- SquireTracking.resolve(root, runner(gitFailure, bdFailure), TestSquirePlatform())
        _            <- settings(root, "true")
        booleanBeads <- SquireTracking.resolve(root, runner(gitShared, bdVersion), TestSquirePlatform(Present("bd")))
      yield assert(auto.effectiveMode == TrackingMode.Beads && forced.effectiveMode == TrackingMode.Beads &&
        off.effectiveMode == TrackingMode.Off && booleanOff.configuredMode == TrackingMode.Off &&
        booleanBeads.configuredMode == TrackingMode.Beads)
    }

    "warns for invalid or unavailable forced beads settings" in {
      for
        root        <- SquireFixtures.scratch("tracking-warnings")
        _           <- settings(root, "unknown")
        invalid     <- SquireTracking.resolve(root, runner(gitFailure, bdFailure), TestSquirePlatform())
        _           <- settings(root, "beads")
        unavailable <- SquireTracking.resolve(root, runner(gitFailure, bdFailure), TestSquirePlatform())
      yield assert(invalid.configuredMode == TrackingMode.Auto && invalid.warning.exists(_.contains("unrecognised")) &&
        unavailable.warning.exists(_.contains("tracking.mode is 'beads'")))
    }

    "reports guidance drift independently for both agent instruction files" in {
      for
        root <- SquireFixtures.scratch("tracking-guidance-drift")
        _    <- Sync.defer {
          Files.writeString(
            (root / "AGENTS.md").toJava,
            "<!-- BEGIN BEADS INTEGRATION -->old<!-- END BEADS INTEGRATION -->"
          )
          Files.writeString((root / "CLAUDE.md").toJava, "no pointer")
        }
        report <- SquireTracking.resolve(root, runner(gitFailure, bdFailure), TestSquirePlatform())
      yield assert(report.guidanceDrift.map(_.file).toSet == Set("AGENTS.md", "CLAUDE.md"))
    }

    "distinguishes missing beads git worktree and workspace fallback states" in {
      for
        root         <- SquireFixtures.scratch("tracking-workspaces")
        missingBeads <- SquireTracking.resolve(root, runner(gitShared, bdVersion), TestSquirePlatform(Present("bd")))
        _            <- beads(root)
        localStore = root / ".beads" / "embeddeddolt"
        _     <- Sync.defer(Files.createDirectories(localStore.toJava))
        local <- SquireTracking.resolve(
          root,
          runner(gitWorktree(root / ".git", root / ".git"), bdVersion),
          TestSquirePlatform(Present("bd"))
        )
        _ <- Sync.defer(Files.delete(localStore.toJava))
        main   = root / "main"
        common = main / ".git"
        _      <- Sync.defer(Files.createDirectories(common.toJava))
        _      <- Sync.defer(Files.createDirectories((main / ".beads").toJava))
        _      <- Sync.defer(Files.writeString((main / ".beads" / "config.yaml").toJava, "prefix: morphir\n"))
        shared <- SquireTracking.resolve(
          root,
          runner(gitWorktree(root / "worktree-git", common), bdVersion),
          TestSquirePlatform(Present("bd"))
        )
        _          <- Sync.defer(Files.delete((main / ".beads" / "config.yaml").toJava))
        unresolved <- SquireTracking.resolve(
          root,
          runner(gitWorktree(root / "worktree-git", common), bdVersion),
          TestSquirePlatform(Present("bd"))
        )
      yield assert(missingBeads.effectiveMode == TrackingMode.Unavailable && local.workspace.status == "local" &&
        shared.workspace.status == "shared" && unresolved.effectiveMode == TrackingMode.Unavailable &&
        unresolved.workspace.remedy.exists(_.contains("bd bootstrap")))
    }

    "treats bd version failure and a non repository as unavailable without mutating beads" in {
      for
        root   <- SquireFixtures.scratch("tracking-failures")
        _      <- beads(root)
        failed <- SquireTracking.resolve(root, runner(gitFailure, bdFailure), TestSquirePlatform(Present("bd")))
      yield assert(failed.effectiveMode == TrackingMode.Unavailable && failed.workspace.status == "no-repo")
    }
  }

  "guidance rewrite" - {
    "removes both beads marker families and replaces the pointer with one trailing newline" in {
      val input = "before\n<!-- BEGIN BEADS INTEGRATION -->old<!-- END BEADS INTEGRATION -->\n" +
        "<!-- BEGIN BEADS CODEX SETUP -->old<!-- END BEADS CODEX SETUP -->\n" +
        "<!-- BEGIN MORPHIR TRACKING -->stale<!-- END MORPHIR TRACKING -->\n\n"
      val rewrite = SquireTracking.rewriteGuidance(input)
      assert(rewrite.removedBeadsBlocks == 2 && !rewrite.text.contains("BEGIN BEADS") &&
        rewrite.text.contains(".claude/skills/squire/squire tracking status --quiet") && rewrite.text.endsWith("\n") &&
        !rewrite.text.endsWith("\n\n"))
    }

    "appends the pointer and is idempotent" in {
      val first  = SquireTracking.rewriteGuidance("agent instructions\n")
      val second = SquireTracking.rewriteGuidance(first.text)
      assert(first.changed && second.text == first.text && !second.changed)
    }

    "checks diffs applies only when requested and reports missing targets" in {
      for
        root   <- SquireFixtures.scratch("tracking-guidance")
        _      <- Sync.defer(Files.writeString((root / "AGENTS.md").toJava, "stale\n"))
        check  <- SquireTracking.syncGuidance(root, SquireTracking.GuidanceMode.Check)
        diff   <- SquireTracking.syncGuidance(root, SquireTracking.GuidanceMode.Diff)
        before <- Sync.defer(Files.readString((root / "AGENTS.md").toJava))
        apply  <- SquireTracking.syncGuidance(root, SquireTracking.GuidanceMode.Apply)
        after  <- Sync.defer(Files.readString((root / "AGENTS.md").toJava))
        second <- SquireTracking.syncGuidance(root, SquireTracking.GuidanceMode.Apply)
      yield assert(check.exitCode == 1 && diff.exitCode == 1 && diff.output.contains("--- a/AGENTS.md") &&
        before == "stale\n" && apply.exitCode == 1 && after.contains("BEGIN MORPHIR TRACKING") &&
        apply.missing == Chunk("CLAUDE.md") && second.changed.isEmpty)
    }

    "rejects symlinked guidance targets before apply check or doctor can follow them" in {
      for
        root <- SquireFixtures.scratch("tracking-guidance-symlink")
        outside  = root / "outside"
        external = outside / "AGENTS.md"
        _ <- Sync.defer {
          Files.createDirectories(outside.toJava)
          Files.writeString(external.toJava, "outside\n")
          Files.createSymbolicLink((root / "AGENTS.md").toJava, external.toJava)
          Files.writeString((root / "CLAUDE.md").toJava, SquireTracking.pointer + "\n")
        }
        checked <- SquireTracking.syncGuidance(root, SquireTracking.GuidanceMode.Check)
        applied <- SquireTracking.syncGuidance(root, SquireTracking.GuidanceMode.Apply)
        doctorOutput = new StringBuilder
        doctor <- SquireCli.runTrackingDoctor(
          root,
          runner(gitFailure, bdFailure),
          TestSquirePlatform(),
          value => doctorOutput.append(value)
        )
        after <- Sync.defer(Files.readString(external.toJava))
      yield assert(checked.exitCode == 1 && applied.exitCode == 1 && doctor == 1 && after == "outside\n" &&
        checked.output.contains("unsafe") && applied.output.contains("unsafe") && doctorOutput.result().contains(
          "unsafe"
        ))
    }

    "rejects a symlinked repository root before reading its guidance targets" in {
      for
        root <- SquireFixtures.scratch("tracking-guidance-intermediate")
        outside  = root / "outside"
        alias    = root / "alias"
        external = outside / "AGENTS.md"
        _ <- Sync.defer {
          Files.createDirectories(outside.toJava)
          Files.writeString(external.toJava, "outside\n")
          Files.writeString((outside / "CLAUDE.md").toJava, SquireTracking.pointer + "\n")
          Files.createSymbolicLink(alias.toJava, outside.toJava)
        }
        result <- SquireTracking.syncGuidance(alias, SquireTracking.GuidanceMode.Apply)
        after  <- Sync.defer(Files.readString(external.toJava))
      yield assert(result.exitCode == 1 && result.output.contains("unsafe") && after == "outside\n")
    }
  }

object SquireTrackingFixtures:
  val gitShared: ProcessRequest => ProcessResult  = request => ProcessResult(request, 0, ".git\n.git\n", "")
  val gitFailure: ProcessRequest => ProcessResult = request => ProcessResult(request, 1, "", "not a repository")
  val bdVersion: ProcessRequest => ProcessResult  = request => ProcessResult(request, 0, "bd 0.42.0\n", "")
  val bdFailure: ProcessRequest => ProcessResult  = request => ProcessResult(request, 1, "", "failed")
  val unexpected: ProcessRequest => ProcessResult =
    request => throw new AssertionError(s"unexpected process: ${request.argv}")

  def runner(git: ProcessRequest => ProcessResult, bd: ProcessRequest => ProcessResult): RuleRunner =
    RuleRunner(request => if request.argv.headOption.contains("git") then git(request) else bd(request))

  def gitWorktree(gitDir: Path, commonDir: Path): ProcessRequest => ProcessResult =
    request => ProcessResult(request, 0, s"$gitDir\n$commonDir\n", "")

  def settings(root: Path, mode: String): Unit < Sync =
    Sync.defer {
      val path = root / ".config" / "squire" / "settings.local.yaml"
      Files.createDirectories(path.parent.get.toJava)
      Files.writeString(path.toJava, s"tracking:\n  mode: $mode\n")
    }

  def beads(root: Path): Unit < Sync =
    Sync.defer(Files.createDirectories((root / ".beads").toJava))

final class TestEnvPlatform(
    val environment: Map[String, String],
    val home: Path,
    val managedSettingsCandidates: Chunk[Path],
    val varFolders: Path,
    override val jvmTempDirectory: Maybe[Path],
    val jvmProbe: Duration => SquireEnv.CheckResult,
    val daemonProbe: Int => SquireEnv.DaemonProbe,
    val writeProbeFn: Path => Unit,
    val deleteProbeFn: Path => Unit
) extends SquireEnv.Platform:
  var daemonPorts: Chunk[Int] = Chunk.empty

  def now: java.time.Instant          = java.time.Instant.EPOCH
  override def zone: java.time.ZoneId = java.time.ZoneOffset.UTC

  def probeJvmNetwork(timeout: Duration): SquireEnv.CheckResult = jvmProbe(timeout)

  def probeDaemon(port: Int): SquireEnv.DaemonProbe =
    daemonPorts = daemonPorts.append(port)
    daemonProbe(port)

  override def writeProbe(path: Path): Unit  = writeProbeFn(path)
  override def deleteProbe(path: Path): Unit = deleteProbeFn(path)
