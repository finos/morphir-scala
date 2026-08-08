//| scalaVersion: 3.8.4
//| mainClass: kyo.test.runner.Cli
//| resources: [test-resources]
//| moduleDeps: [squire.scala, SquireCellar.scala, SquireRepo.scala]
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
          "SquireRepoSpec"
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
