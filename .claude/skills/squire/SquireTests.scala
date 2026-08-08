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
        _ <- beads(root)
        _ <- Sync.defer {
          Files.writeString((root / "AGENTS.md").toJava, SquireTracking.pointer + "\n")
          Files.writeString((root / "CLAUDE.md").toJava, SquireTracking.pointer + "\n")
        }
        statusOutput = new StringBuilder
        status <- SquireCli.runTrackingStatus(
          TrackingStatusOpts(quiet = true), root, runner(gitShared, bdVersion), TestSquirePlatform(Present("bd")), value => statusOutput.append(value)
        )
        checkOutput = new StringBuilder
        check <- SquireCli.runTrackingStatus(
          TrackingStatusOpts(check = Some("off")), root, runner(gitShared, bdVersion), TestSquirePlatform(Present("bd")), value => checkOutput.append(value)
        )
        syncOutput = new StringBuilder
        sync <- SquireCli.runTrackingSync(TrackingSyncOpts(check = true), root, value => syncOutput.append(value))
        doctorOutput = new StringBuilder
        doctor <- SquireCli.runTrackingDoctor(root, runner(gitShared, bdVersion), TestSquirePlatform(Present("bd")), value => doctorOutput.append(value))
      yield assert(status == 0 && statusOutput.result() == "beads\n" && check == 1 && checkOutput.isEmpty &&
        sync == 0 && syncOutput.result().contains("OK - AGENTS.md") && doctor == 0 && doctorOutput.result().contains("guidance"))
    }

    "rejects invalid status checks and conflicting sync modes before output or process work" in {
      for
        root <- SquireFixtures.scratch("tracking-cli-invalid")
        statusOutput = new StringBuilder
        status <- SquireCli.runTrackingStatus(
          TrackingStatusOpts(check = Some("invalid")), root, RuleRunner(SquireTrackingFixtures.unexpected), TestSquirePlatform(), value => statusOutput.append(value)
        )
        syncOutput = new StringBuilder
        sync <- SquireCli.runTrackingSync(TrackingSyncOpts(check = true, diff = true), root, value => syncOutput.append(value))
      yield assert(status == 2 && statusOutput.isEmpty && sync == 2 && syncOutput.isEmpty)
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
  }

  "Mill version" - {
    "matches the repository version" in {
      val repositoryVersion =
        Files.readString(skillDirectory.resolve("../../../.mill-version"), StandardCharsets.UTF_8).trim
      assert(read(".mill-version").trim == repositoryVersion)
    }
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
          "SquireMetaSpec",
          "SquireModelSpec",
          "SquireProcessSpec",
          "SquireEnvSpec",
          "SquireDoctorSpec",
          "SquireCellarSpec",
          "SquireRepoSpec",
          "SquireBranchSpec",
          "SquireTrackingSpec"
        )
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
            value.stdout == "o".repeat(outputBytes) &&
            value.stderr == "e".repeat(outputBytes)
          case Result.Failure(_) => false
      yield assert(result)
    }
  }

object SquireFixtures:
  val javaExecutable: String =
    java.nio.file.Path.of(java.lang.System.getProperty("java.home"), "bin", "java").toString

  def scratch(name: String): Path < Sync =
    Sync.defer(Path(java.nio.file.Files.createTempDirectory(s"squire-$name-").toString))

  def platform(
      root: Path,
      jvmResult: SquireEnv.CheckResult,
      environment: Map[String, String] = Map.empty,
      home: Option[Path] = None,
      managed: Chunk[Path] = Chunk.empty,
      varFolders: Option[Path] = None,
      daemonProbe: Int => SquireEnv.DaemonProbe = _ => SquireEnv.DaemonProbe.Open,
      writeProbe: Path => Unit = path => Files.writeString(path.toJava, "squire probe"),
      deleteProbe: Path => Unit = path => Files.deleteIfExists(path.toJava)
  ): TestEnvPlatform =
    TestEnvPlatform(
      environment,
      home.getOrElse(root / "home"),
      managed,
      varFolders.getOrElse(root),
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

final class RuleRunner(response: ProcessRequest => ProcessResult) extends ProcessRunner:
  var requests: Chunk[ProcessRequest] = Chunk.empty

  def run(request: ProcessRequest): ProcessResult < (Async & Abort[SquireError]) =
    requests = requests.append(request)
    response(request)

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

    "skips an absent var folders directory and cleans a successful probe" in {
      for
        root   <- SquireFixtures.scratch("env-var-folders")
        absent <- SquireEnv.check(
          SquireEnv.CheckKind.VarFolders,
          1.seconds,
          SquireFixtures.platform(
            root,
            SquireEnv.CheckResult(Present(true), "ok", 0.0),
            varFolders = Some(root / "absent")
          )
        )
        writable <- SquireEnv.check(
          SquireEnv.CheckKind.VarFolders,
          1.seconds,
          SquireFixtures.platform(root, SquireEnv.CheckResult(Present(true), "ok", 0.0), varFolders = Some(root))
        )
        probeExists <- (root / ".squire-env-probe").exists
      yield assert(absent && writable && !probeExists)
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
    "checks elm tooling guard main class task wrapper and var folders access" in {
      for
        root <- SquireFixtures.scratch("doctor-project")
        _    <- Sync.defer {
          Files.createDirectories((root / ".config" / "mise" / "tasks").toJava)
          Files.createDirectories((root / "morphir").toJava)
          Files.writeString((root / ".config" / "mise" / "tasks" / "setup").toJava, "ELM_TOOLING_INSTALL=1")
          Files.writeString(
            (root / "morphir" / "package.mill").toJava,
            "override def mainClass = Task { Some(\"main\") }"
          )
        }
        report <- SquireDoctor.run(
          root,
          RecordingRunner(Chunk.empty),
          SquireFixtures.platform(root, SquireEnv.CheckResult(Present(true), "ok", 0.0))
        )
      yield assert(
        report.finding("elm_tooling_guard").exists(_.code == "OK") &&
          report.finding("main_class_task").exists(_.code == "OK") &&
          report.finding("var_folders").exists(_.code == "OK")
      )
    }
  }

class SquireTrackingSpec extends Test[Any]:
  import SquireTrackingFixtures.*

  "tracking resolution" - {
    "defaults absent settings to auto and reports unavailable when bd is missing" in {
      for
        root <- SquireFixtures.scratch("tracking-absent")
        report <- SquireTracking.resolve(root, runner(gitFailure, bdFailure), TestSquirePlatform())
      yield assert(report.configuredMode == TrackingMode.Auto && report.effectiveMode == TrackingMode.Unavailable &&
        report.reason == "bd is not on PATH")
    }

    "honours auto beads off and YAML boolean mode settings" in {
      for
        root <- SquireFixtures.scratch("tracking-modes")
        _ <- settings(root, "auto")
        _ <- beads(root)
        auto <- SquireTracking.resolve(root, runner(gitShared, bdVersion), TestSquirePlatform(Present("bd")))
        _ <- settings(root, "beads")
        forced <- SquireTracking.resolve(root, runner(gitShared, bdVersion), TestSquirePlatform(Present("bd")))
        _ <- settings(root, "off")
        off <- SquireTracking.resolve(root, runner(gitFailure, bdFailure), TestSquirePlatform())
        _ <- settings(root, "false")
        booleanOff <- SquireTracking.resolve(root, runner(gitFailure, bdFailure), TestSquirePlatform())
        _ <- settings(root, "true")
        booleanBeads <- SquireTracking.resolve(root, runner(gitShared, bdVersion), TestSquirePlatform(Present("bd")))
      yield assert(auto.effectiveMode == TrackingMode.Beads && forced.effectiveMode == TrackingMode.Beads &&
        off.effectiveMode == TrackingMode.Off && booleanOff.configuredMode == TrackingMode.Off &&
        booleanBeads.configuredMode == TrackingMode.Beads)
    }

    "warns for invalid or unavailable forced beads settings" in {
      for
        root <- SquireFixtures.scratch("tracking-warnings")
        _ <- settings(root, "unknown")
        invalid <- SquireTracking.resolve(root, runner(gitFailure, bdFailure), TestSquirePlatform())
        _ <- settings(root, "beads")
        unavailable <- SquireTracking.resolve(root, runner(gitFailure, bdFailure), TestSquirePlatform())
      yield assert(invalid.configuredMode == TrackingMode.Auto && invalid.warning.exists(_.contains("unrecognised")) &&
        unavailable.warning.exists(_.contains("tracking.mode is 'beads'")))
    }

    "reports guidance drift independently for both agent instruction files" in {
      for
        root <- SquireFixtures.scratch("tracking-guidance-drift")
        _ <- Sync.defer {
          Files.writeString((root / "AGENTS.md").toJava, "<!-- BEGIN BEADS INTEGRATION -->old<!-- END BEADS INTEGRATION -->")
          Files.writeString((root / "CLAUDE.md").toJava, "no pointer")
        }
        report <- SquireTracking.resolve(root, runner(gitFailure, bdFailure), TestSquirePlatform())
      yield assert(report.guidanceDrift.map(_.file).toSet == Set("AGENTS.md", "CLAUDE.md"))
    }

    "distinguishes missing beads git worktree and workspace fallback states" in {
      for
        root <- SquireFixtures.scratch("tracking-workspaces")
        missingBeads <- SquireTracking.resolve(root, runner(gitShared, bdVersion), TestSquirePlatform(Present("bd")))
        _ <- beads(root)
        localStore = root / ".beads" / "embeddeddolt"
        _ <- Sync.defer(Files.createDirectories(localStore.toJava))
        local <- SquireTracking.resolve(root, runner(gitWorktree(root / ".git", root / ".git"), bdVersion), TestSquirePlatform(Present("bd")))
        _ <- Sync.defer(Files.delete(localStore.toJava))
        main = root / "main"
        common = main / ".git"
        _ <- Sync.defer(Files.createDirectories(common.toJava))
        _ <- Sync.defer(Files.createDirectories((main / ".beads").toJava))
        _ <- Sync.defer(Files.writeString((main / ".beads" / "config.yaml").toJava, "prefix: morphir\n"))
        shared <- SquireTracking.resolve(root, runner(gitWorktree(root / "worktree-git", common), bdVersion), TestSquirePlatform(Present("bd")))
        _ <- Sync.defer(Files.delete((main / ".beads" / "config.yaml").toJava))
        unresolved <- SquireTracking.resolve(root, runner(gitWorktree(root / "worktree-git", common), bdVersion), TestSquirePlatform(Present("bd")))
      yield assert(missingBeads.effectiveMode == TrackingMode.Unavailable && local.workspace.status == "local" &&
        shared.workspace.status == "shared" && unresolved.effectiveMode == TrackingMode.Unavailable &&
        unresolved.workspace.remedy.exists(_.contains("bd bootstrap")))
    }

    "treats bd version failure and a non repository as unavailable without mutating beads" in {
      for
        root <- SquireFixtures.scratch("tracking-failures")
        _ <- beads(root)
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
      val first = SquireTracking.rewriteGuidance("agent instructions\n")
      val second = SquireTracking.rewriteGuidance(first.text)
      assert(first.changed && second.text == first.text && !second.changed)
    }

    "checks diffs applies only when requested and reports missing targets" in {
      for
        root <- SquireFixtures.scratch("tracking-guidance")
        _ <- Sync.defer(Files.writeString((root / "AGENTS.md").toJava, "stale\n"))
        check <- SquireTracking.syncGuidance(root, SquireTracking.GuidanceMode.Check)
        diff <- SquireTracking.syncGuidance(root, SquireTracking.GuidanceMode.Diff)
        before <- Sync.defer(Files.readString((root / "AGENTS.md").toJava))
        apply <- SquireTracking.syncGuidance(root, SquireTracking.GuidanceMode.Apply)
        after <- Sync.defer(Files.readString((root / "AGENTS.md").toJava))
        second <- SquireTracking.syncGuidance(root, SquireTracking.GuidanceMode.Apply)
      yield assert(check.exitCode == 1 && diff.exitCode == 1 && diff.output.contains("--- a/AGENTS.md") &&
        before == "stale\n" && apply.exitCode == 1 && after.contains("BEGIN MORPHIR TRACKING") &&
        apply.missing == Chunk("CLAUDE.md") && second.changed.isEmpty)
    }

    "rejects symlinked guidance targets before apply check or doctor can follow them" in {
      for
        root <- SquireFixtures.scratch("tracking-guidance-symlink")
        outside = root / "outside"
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
        doctor <- SquireCli.runTrackingDoctor(root, runner(gitFailure, bdFailure), TestSquirePlatform(), value => doctorOutput.append(value))
        after <- Sync.defer(Files.readString(external.toJava))
      yield assert(checked.exitCode == 1 && applied.exitCode == 1 && doctor == 1 && after == "outside\n" &&
        checked.output.contains("unsafe") && applied.output.contains("unsafe") && doctorOutput.result().contains("unsafe"))
    }

    "rejects a symlinked repository root before reading its guidance targets" in {
      for
        root <- SquireFixtures.scratch("tracking-guidance-intermediate")
        outside = root / "outside"
        alias = root / "alias"
        external = outside / "AGENTS.md"
        _ <- Sync.defer {
          Files.createDirectories(outside.toJava)
          Files.writeString(external.toJava, "outside\n")
          Files.writeString((outside / "CLAUDE.md").toJava, SquireTracking.pointer + "\n")
          Files.createSymbolicLink(alias.toJava, outside.toJava)
        }
        result <- SquireTracking.syncGuidance(alias, SquireTracking.GuidanceMode.Apply)
        after <- Sync.defer(Files.readString(external.toJava))
      yield assert(result.exitCode == 1 && result.output.contains("unsafe") && after == "outside\n")
    }
  }

object SquireTrackingFixtures:
  val gitShared: ProcessRequest => ProcessResult = request => ProcessResult(request, 0, ".git\n.git\n", "")
  val gitFailure: ProcessRequest => ProcessResult = request => ProcessResult(request, 1, "", "not a repository")
  val bdVersion: ProcessRequest => ProcessResult = request => ProcessResult(request, 0, "bd 0.42.0\n", "")
  val bdFailure: ProcessRequest => ProcessResult = request => ProcessResult(request, 1, "", "failed")
  val unexpected: ProcessRequest => ProcessResult = request => throw new AssertionError(s"unexpected process: ${request.argv}")

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
