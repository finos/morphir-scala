//| scalaVersion: 3.8.4
//| mainClass: kyo.test.runner.Cli
//| resources: [test-resources]
//| moduleDeps: [squire.scala]
//| mvnDeps:
//| - io.getkyo::kyo-test-api:1.0.0-RC6
//| - io.getkyo::kyo-test-runner:1.0.0-RC6

import java.nio.charset.StandardCharsets
import java.nio.file.Files
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
        root     <- SquireFixtures.scratch("env-cli")
        platform = SquireFixtures.platform(root, SquireEnv.CheckResult(Present(true), "ok", 0.0))
        fullOutput = new StringBuilder
        fullExit <- SquireCli.runEnvInfo(AiEnvInfoOpts(), root, platform, value => fullOutput.append(value))
        skippedOutput = new StringBuilder
        skippedExit <- SquireCli.runEnvInfo(AiEnvInfoOpts(check = Some("var-folders")), root, platform, value => skippedOutput.append(value))
        blockedPlatform = SquireFixtures.platform(root, SquireEnv.CheckResult(Present(false), "blocked", 0.0))
        blockedOutput = new StringBuilder
        blockedExit <- SquireCli.runEnvInfo(AiEnvInfoOpts(check = Some("jvm-network")), root, blockedPlatform, value => blockedOutput.append(value))
        fractionalOutput = new StringBuilder
        fractionalExit <- SquireCli.runEnvInfo(AiEnvInfoOpts(timeout = 0.5), root, platform, value => fractionalOutput.append(value))
        json = fullOutput.result()
        legacyKeys = List("generated_at", "claude_code", "ci", "checks", "sandboxed", "claude_settings", "recommendation")
        ordered = legacyKeys.map(json.indexOf).sliding(2).forall { case List(left, right) => left >= 0 && left < right; case _ => true }
      yield assert(
        fullExit == 0 && json.contains("\"generated_at\": \"1970-01-01T00:00:00+0000\"") && json.contains("\"jvm_network\"") && !json.contains("python_network") &&
          json.contains("\"entrypoint\": null") && json.contains("\"session_id\": null") && ordered &&
          fractionalExit == 0 && fractionalOutput.nonEmpty && skippedExit == 0 && skippedOutput.isEmpty && blockedExit == 1 && blockedOutput.isEmpty
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
          "SquireDoctorSpec"
        )
      )
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

    "rejects Bytes as non-deterministic JSON" in {
      assert(isRejectedAsNonDeterministicJson(Structure.Value.Bytes(Span.from(Array[Byte](1, 2, 3)))))
    }

    "rejects Instant as non-deterministic JSON" in {
      assert(isRejectedAsNonDeterministicJson(Structure.Value.Instant(java.time.Instant.EPOCH)))
    }

    "rejects Duration as non-deterministic JSON" in {
      assert(isRejectedAsNonDeterministicJson(Structure.Value.Duration(java.time.Duration.ZERO)))
    }
  }

  "paths" - {
    "resolveUnder rejects a sibling prefix" in {
      val base    = Path("/tmp/squire-path-test/.refs")
      val sibling = Path("/tmp/squire-path-test/.refs-escaped/repo")
      assert(SquirePaths.resolveUnder(sibling, base).isFailure)
    }

    "resolveUnder rejects an in-base symlink that escapes" in {
      for
        root    <- SquireFixtures.scratch("path")
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
        root   <- SquireFixtures.scratch("process")
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
    TestEnvPlatform(environment, home.getOrElse(root / "home"), managed, varFolders.getOrElse(root), _ => jvmResult, daemonProbe, writeProbe, deleteProbe)

  def writeDaemonFiles(root: Path, portFile: Option[Int], logPort: Option[Int]): Unit < Sync =
    Sync.defer {
      val daemon = root / "out" / "mill-daemon"
      Files.createDirectories(daemon.toJava)
      portFile.foreach(port => Files.writeString((daemon / "socketPort").toJava, port.toString))
      logPort.foreach(port => Files.writeString((daemon / "server.log").toJava, s"listening on port $port\n"))
    }

final class RecordingRunner(responses: Chunk[ProcessResult]) extends ProcessRunner:
  private var index = 0
  var requests: Chunk[ProcessRequest] = Chunk.empty

  def run(request: ProcessRequest): ProcessResult < (Async & Abort[SquireError]) =
    requests = requests.append(request)
    val response = responses(index)
    index += 1
    response

class SquireEnvSpec extends Test[Any]:
  "environment report" - {
    "detects Claude Code and CI without confusing either with a sandbox" in {
      for
        root <- SquireFixtures.scratch("env-detection")
        report <- SquireEnv.report(
          1.seconds,
          SquireFixtures.platform(
            root,
            SquireEnv.CheckResult(Present(true), "loopback bind+accept+connect succeeded", 0.01),
            environment = Map(
              "CLAUDECODE" -> "1",
              "CLAUDE_CODE_ENTRYPOINT" -> "cli",
              "CLAUDE_CODE_SESSION_ID" -> "session-1",
              "CLAUDE_CODE_CHILD_SESSION" -> "1",
              "GITHUB_ACTIONS" -> "true"
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
          Files.writeString(managed.toJava, """{"sandbox":{"enabled":true,"network":{"allowedDomains":["managed.example"]}}}""")
          Files.writeString((home / ".claude" / "settings.json").toJava, "{}")
          Files.writeString((root / ".claude" / "settings.json").toJava, """{"unknown":true,"sandbox":{"enabled":true,"network":{"allowedDomains":["project.example"]}}}""")
          Files.writeString((root / ".claude" / "settings.local.json").toJava, "not json")
        }
        report <- SquireEnv.report(
          1.seconds,
          SquireFixtures.platform(root, SquireEnv.CheckResult(Present(true), "ok", 0.0), home = Some(home), managed = Chunk(managed)),
          root
        )
        settings = report.claudeSettings
      yield assert(
        settings.sandboxEnabled == Map(
          "managed" -> Present(true),
          "user" -> Absent,
          "project" -> Present(true),
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
        root <- SquireFixtures.scratch("env-network")
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
          SquireFixtures.platform(root, SquireEnv.CheckResult(Present(false), "JVM loopback probe hung past 1s timeout", 1.0))
        )
      yield assert(success && !failure && !timeout)
    }

    "skips an absent var folders directory and cleans a successful probe" in {
      for
        root <- SquireFixtures.scratch("env-var-folders")
        absent <- SquireEnv.check(
          SquireEnv.CheckKind.VarFolders,
          1.seconds,
          SquireFixtures.platform(root, SquireEnv.CheckResult(Present(true), "ok", 0.0), varFolders = Some(root / "absent"))
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
        blocked <- SquireEnv.check(SquireEnv.CheckKind.VarFolders, 1.seconds, platform)
        probeExists <- (root / ".squire-env-probe").exists
      yield assert(!blocked && !probeExists)
    }

    "cleans a probe created before a write failure" in {
      for
        root <- SquireFixtures.scratch("env-var-folders-partial")
        platform = SquireFixtures.platform(
          root,
          SquireEnv.CheckResult(Present(true), "ok", 0.0),
          writeProbe = path => { Files.writeString(path.toJava, "partial"); throw java.nio.file.AccessDeniedException(path.toString) }
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
        check <- SquireEnv.check(SquireEnv.CheckKind.VarFolders, 1.seconds, platform)
        report <- SquireEnv.report(1.seconds, platform, root)
      yield assert(!check && report.checks("var_folders_writable").detail.startsWith("could not clean probe file:"))
    }
  }

class SquireDoctorSpec extends Test[Any]:
  "daemon diagnostics" - {
    "prefers the daemon port file over the server log" in {
      for
        root <- SquireFixtures.scratch("doctor-port-file")
        _ <- SquireFixtures.writeDaemonFiles(root, portFile = Some(41001), logPort = Some(41002))
        platform = SquireFixtures.platform(root, SquireEnv.CheckResult(Present(true), "ok", 0.0))
        report <- SquireDoctor.run(root, RecordingRunner(Chunk.empty), platform)
      yield assert(report.finding("mill_daemon").exists(_.code == "PORT_OPEN") && platform.daemonPorts == Chunk(41001))
    }

    "uses the latest server log port when no port file exists" in {
      for
        root <- SquireFixtures.scratch("doctor-server-log")
        _ <- SquireFixtures.writeDaemonFiles(root, portFile = None, logPort = Some(41003))
        platform = SquireFixtures.platform(root, SquireEnv.CheckResult(Present(true), "ok", 0.0))
        report <- SquireDoctor.run(root, RecordingRunner(Chunk.empty), platform)
      yield assert(report.finding("mill_daemon").exists(_.code == "PORT_OPEN") && platform.daemonPorts == Chunk(41003))
    }

    "distinguishes no daemon sandbox and refused daemon connections" in {
      for
        root <- SquireFixtures.scratch("doctor-daemon-states")
        noDaemon <- SquireDoctor.run(root, RecordingRunner(Chunk.empty), SquireFixtures.platform(root, SquireEnv.CheckResult(Present(true), "ok", 0.0)))
        _ <- SquireFixtures.writeDaemonFiles(root, portFile = Some(41004), logPort = None)
        sandbox <- SquireDoctor.run(
          root,
          RecordingRunner(Chunk.empty),
          SquireFixtures.platform(root, SquireEnv.CheckResult(Present(true), "ok", 0.0), daemonProbe = _ => SquireEnv.DaemonProbe.Sandbox("Operation not permitted"))
        )
        refused <- SquireDoctor.run(
          root,
          RecordingRunner(Chunk.empty),
          SquireFixtures.platform(root, SquireEnv.CheckResult(Present(true), "ok", 0.0), daemonProbe = _ => SquireEnv.DaemonProbe.Refused("Connection refused"))
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
        _ <- Sync.defer {
          Files.createDirectories((root / ".config" / "mise" / "tasks").toJava)
          Files.createDirectories((root / "morphir").toJava)
          Files.writeString((root / ".config" / "mise" / "tasks" / "setup").toJava, "ELM_TOOLING_INSTALL=1")
          Files.writeString((root / "morphir" / "package.mill").toJava, "override def mainClass = Task { Some(\"main\") }")
        }
        report <- SquireDoctor.run(root, RecordingRunner(Chunk.empty), SquireFixtures.platform(root, SquireEnv.CheckResult(Present(true), "ok", 0.0)))
      yield assert(
        report.finding("elm_tooling_guard").exists(_.code == "OK") &&
          report.finding("main_class_task").exists(_.code == "OK") &&
          report.finding("var_folders").exists(_.code == "OK")
      )
    }
  }

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

  def now: java.time.Instant = java.time.Instant.EPOCH
  override def zone: java.time.ZoneId = java.time.ZoneOffset.UTC

  def probeJvmNetwork(timeout: Duration): SquireEnv.CheckResult = jvmProbe(timeout)

  def probeDaemon(port: Int): SquireEnv.DaemonProbe =
    daemonPorts = daemonPorts.append(port)
    daemonProbe(port)

  override def writeProbe(path: Path): Unit = writeProbeFn(path)
  override def deleteProbe(path: Path): Unit = deleteProbeFn(path)
