package millbuild

import java.nio.charset.StandardCharsets

import scala.concurrent.duration.*

import utest.*

object DesktopSmokeTests extends TestSuite {
  private val assertions = DesktopSmoke.expectedAssertions.iterator.map(_ -> true).toMap

  private def validatedRunRoot(prefix: String): DesktopSmoke.SafeRunRoot = {
    val base = os.temp.dir(prefix = prefix, deleteOnExit = true)
    val run  = base / "run"
    os.makeDir.all(run)
    DesktopSmoke.safeRunRoot(base, run).toOption.get
  }

  private def writeSuccessfulArtifacts(paths: DesktopSmoke.Artifacts): Unit = {
    os.write.over(paths.screenshot, Array[Byte](1, 2, 3), createFolders = true)
    os.write.over(paths.result, upickle.default.write(DesktopSmoke.Result(assertions)))
    os.write.over(paths.processLog, "electron output\n")
    os.write.over(paths.rendererLog, "renderer output\n")
  }

  @annotation.tailrec
  private def awaitDead(pid: Long, attempts: Int = 100): Unit =
    if attempts <= 0 || ProcessHandle.of(pid).isEmpty || !ProcessHandle.of(pid).get().isAlive then ()
    else {
      Thread.sleep(25L)
      awaitDead(pid, attempts - 1)
    }

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

    test("child environment removes Electron Node mode and adds the internal sentinel") {
      val configured = DesktopSmoke.childEnvironment(
        Map("PATH" -> "/bin", "ELECTRON_RUN_AS_NODE" -> "1")
      )
      assert(configured("PATH") == "/bin")
      assert(!configured.contains("ELECTRON_RUN_AS_NODE"))
      assert(configured.contains("MORPHIR_DESKTOP_SMOKE_SENTINEL"))
    }

    test("platform token normalizes operating system and architecture inputs") {
      assert(DesktopSmoke.platformToken("Mac OS X", "aarch64") == "darwin-arm64")
      assert(DesktopSmoke.platformToken("Linux", "x86_64") == "linux-x64")
      assert(DesktopSmoke.platformToken("Windows 11", "amd64") == "windows-x64")
    }

    test("platform process boundaries and npm commands are constructed explicitly") {
      val root    = os.temp.dir(prefix = "desktop-smoke-boundary-plan-", deleteOnExit = true)
      val command = Seq("tool with spaces", "percent%", "bang!", "caret^", "amp&", "pipe|", "(group)", "quote\"")
      val darwin  = DesktopSmoke.processBoundary("darwin-arm64", command, root, None).toOption.get
      val linux   = DesktopSmoke.processBoundary("linux-x64", command, root, Some("/usr/bin/setsid")).toOption.get
      val windows = DesktopSmoke.processBoundary("windows-x64", command, root, None).toOption.get

      assert(darwin.kind == DesktopSmoke.BoundaryKind.DarwinProcessGroup)
      assert(darwin.launchCommand.take(2) == Seq("/bin/sh", "-c"))
      assert(darwin.launchCommand.contains("tool with spaces"))
      assert(linux.kind == DesktopSmoke.BoundaryKind.LinuxSession)
      assert(linux.launchCommand == Seq("/usr/bin/setsid") ++ command)
      assert(DesktopSmoke.processBoundary("linux-x64", command, root, None).isLeft)
      assert(windows.kind == DesktopSmoke.BoundaryKind.WindowsTaskkillBestEffort)
      assert(!windows.launchCommand.contains("cmd.exe"))
      assert(windows.launchCommand.takeRight(command.size) == command)
      assert(windows.marker.nonEmpty)
      assert(windows.completion.nonEmpty)
      assert(windows.launchCommand(1).endsWith("WindowsProcessBoundary.java"))
      val generated = windows.generatedSource.get
      assert(generated.path == root / "WindowsProcessBoundary.java")
      assert(generated.content.contains("new ProcessBuilder(command)"))
      assert(generated.content.contains("Files.writeString(marker"))
      assert(generated.content.contains("Files.writeString(completion"))
      assert(!generated.content.contains("cmd.exe"))
      assert(!generated.content.contains("call "))
      assert(DesktopSmoke.npmCommand("darwin-arm64") == Seq("npm", "ci", "--ignore-scripts"))
      assert(
        DesktopSmoke.npmCommand("windows-x64") ==
          Seq("cmd.exe", "/d", "/s", "/c", "npm ci --ignore-scripts")
      )
    }

    test("generated Java boundary preserves metacharacter arguments without a shell") {
      val root      = os.temp.dir(prefix = "desktop-smoke-java-boundary-", deleteOnExit = true)
      val weird     = Seq("space value", "percent%", "bang!", "caret^", "amp&", "pipe|", "(group)", "quote\"")
      val command   = Seq("/usr/bin/printf", "<%s>\\n") ++ weird
      val boundary  = DesktopSmoke.processBoundary("windows-x64", command, root, None).toOption.get
      val generated = boundary.generatedSource.get
      val stdout    = root / "stdout.log"
      val hostJava  = java.nio.file.Paths.get(System.getProperty("java.home"), "bin", "java").toString
      os.write.over(generated.path, generated.content)
      val hostLaunch = hostJava +: boundary.launchCommand.tail
      val process    = new ProcessBuilder(hostLaunch*)
        .redirectOutput(stdout.toIO)
        .redirectError(ProcessBuilder.Redirect.INHERIT)
        .start()

      @annotation.tailrec
      def awaitCompletion(attempts: Int): Unit =
        if boundary.completion.exists(os.isFile(_)) then ()
        else if attempts <= 0 then throw new java.lang.AssertionError("Java boundary did not complete")
        else {
          Thread.sleep(20L)
          awaitCompletion(attempts - 1)
        }

      try {
        awaitCompletion(250)
        assert(boundary.marker.exists(path => os.read(path).trim.toLong > 1L))
        assert(boundary.completion.exists(path => os.read(path).trim == "0"))
        assert(os.read.lines(stdout) == weird.map(value => s"<$value>"))
      } finally {
        process.destroyForcibly()
        process.waitFor(5L, java.util.concurrent.TimeUnit.SECONDS)
      }
    }

    test("assembled bundles omit stale source map trailers") {
      val linked = "const ready = true;\n//# sourceMappingURL=main.js.map\n"
      assert(DesktopSmoke.withoutSourceMapTrailer(linked) == "const ready = true;\n")
      assert(DesktopSmoke.withoutSourceMapTrailer("const ready = true;\n") == "const ready = true;\n")
    }

    test("artifacts use the stable task-owned layout") {
      val root      = validatedRunRoot("desktop-smoke-artifacts-")
      val artifacts = DesktopSmoke.artifacts(root)

      assert(artifacts.screenshot == root.path / "artifacts" / "screenshot.png")
      assert(artifacts.result == root.path / "artifacts" / "result.json")
      assert(artifacts.processLog == root.path / "artifacts" / "process.log")
      assert(artifacts.rendererLog == root.path / "artifacts" / "renderer.log")
    }

    test("decode rejects malformed non-flat and invalid assertion results without exposing the sentinel") {
      val sentinel    = DesktopSmoke.launchEnvironment("MORPHIR_DESKTOP_SMOKE_SENTINEL")
      val malformed   = DesktopSmoke.decodeResult(s"{not-json-$sentinel")
      val nested      = DesktopSmoke.decodeResult("""{"assertions":{"mountedRenderer":true}}""")
      val falseResult = DesktopSmoke.decodeResult(
        upickle.default.write(DesktopSmoke.Result(assertions.updated("mountedRenderer", false)))
      )

      assert(malformed.left.exists(message => !message.contains(sentinel)))
      assert(nested.isLeft)
      assert(falseResult == Left("desktop smoke assertion failed: mountedRenderer"))
    }

    test("verify process and artifacts accepts a successful complete run") {
      val paths = DesktopSmoke.artifacts(validatedRunRoot("desktop-smoke-verify-success-"))
      os.makeDir.all(paths.screenshot / os.up)
      writeSuccessfulArtifacts(paths)

      assert(DesktopSmoke.verifyProcessAndArtifacts(0, paths) == Right(DesktopSmoke.Result(assertions)))
      os.write.over(paths.processLog, Array.emptyByteArray)
      os.write.over(paths.rendererLog, Array.emptyByteArray)
      assert(DesktopSmoke.verifyProcessAndArtifacts(0, paths) == Right(DesktopSmoke.Result(assertions)))
    }

    test("verify process and artifacts rejects a bad exit and every missing required artifact") {
      val paths = DesktopSmoke.artifacts(validatedRunRoot("desktop-smoke-verify-failure-"))
      os.makeDir.all(paths.screenshot / os.up)
      writeSuccessfulArtifacts(paths)
      assert(DesktopSmoke.verifyProcessAndArtifacts(9, paths).left.exists(_.contains("exit 9")))

      Seq(paths.screenshot, paths.result, paths.processLog, paths.rendererLog).foreach { missing =>
        writeSuccessfulArtifacts(paths)
        os.remove(missing)
        assert(DesktopSmoke.verifyProcessAndArtifacts(0, paths).left.exists(_.contains(missing.last)))
      }
    }

    test("verify process and artifacts rejects empty screenshot and result files") {
      val paths = DesktopSmoke.artifacts(validatedRunRoot("desktop-smoke-verify-empty-"))
      os.makeDir.all(paths.screenshot / os.up)
      writeSuccessfulArtifacts(paths)
      os.write.over(paths.screenshot, Array.emptyByteArray)
      assert(
        DesktopSmoke.verifyProcessAndArtifacts(0, paths) ==
          Left("desktop smoke required artifact is empty: screenshot.png")
      )

      writeSuccessfulArtifacts(paths)
      os.write.over(paths.result, Array.emptyByteArray)
      assert(
        DesktopSmoke.verifyProcessAndArtifacts(0, paths) ==
          Left("desktop smoke required artifact is empty: result.json")
      )
    }

    test("scan detects sentinel bytes in text binary artifacts and nested user data with redacted diagnostics") {
      val root     = validatedRunRoot("desktop-smoke-scan-")
      val paths    = DesktopSmoke.artifacts(root)
      val userData = root.path / "user-data"
      val sentinel = DesktopSmoke.launchEnvironment("MORPHIR_DESKTOP_SMOKE_SENTINEL")
      os.makeDir.all(paths.screenshot / os.up)
      os.makeDir.all(userData / "nested")
      writeSuccessfulArtifacts(paths)

      val leakTargets = Seq(
        paths.processLog  -> sentinel.getBytes(StandardCharsets.UTF_8),
        paths.rendererLog -> sentinel.getBytes(StandardCharsets.UTF_8),
        paths.result      -> sentinel.getBytes(StandardCharsets.UTF_8),
        paths.screenshot  -> (Array[Byte](0, 1) ++ sentinel.getBytes(StandardCharsets.UTF_8) ++ Array[Byte](2)),
        (userData / "nested" / "credential.bin") -> sentinel.getBytes(StandardCharsets.UTF_8)
      )
      leakTargets.foreach { case (target, bytes) =>
        writeSuccessfulArtifacts(paths)
        os.remove.all(userData)
        os.makeDir.all(userData / "nested")
        os.write.over(target, bytes, createFolders = true)
        val result = DesktopSmoke.scanForSentinel(paths, userData)
        assert(result.left.exists(message => message.contains("<redacted>") && !message.contains(sentinel)))
      }
    }

    test("scan accepts sentinel-free artifacts and nested user data") {
      val root     = validatedRunRoot("desktop-smoke-scan-safe-")
      val paths    = DesktopSmoke.artifacts(root)
      val userData = root.path / "user-data" / "nested"
      os.makeDir.all(paths.screenshot / os.up)
      os.makeDir.all(userData)
      writeSuccessfulArtifacts(paths)
      os.write(userData / "safe.bin", Array[Byte](0, 1, 2, 3))
      assert(DesktopSmoke.scanForSentinel(paths, root.path / "user-data") == Right(()))
    }

    test("scan streams large files and detects a sentinel split across chunk boundaries") {
      val root      = validatedRunRoot("desktop-smoke-scan-boundary-")
      val paths     = DesktopSmoke.artifacts(root)
      val userData  = root.path / "user-data"
      val sentinel  = DesktopSmoke.launchEnvironment("MORPHIR_DESKTOP_SMOKE_SENTINEL").getBytes(StandardCharsets.UTF_8)
      val chunkSize = DesktopSmoke.scanChunkSize
      os.makeDir.all(paths.screenshot / os.up)
      os.makeDir.all(userData)
      writeSuccessfulArtifacts(paths)

      val largeSafe = Array.fill[Byte](chunkSize * 5 + 17)(7)
      os.write.over(paths.processLog, largeSafe)
      assert(DesktopSmoke.scanForSentinel(paths, userData) == Right(()))

      val split = Array.fill[Byte](chunkSize - 5)(3) ++ sentinel ++ Array.fill[Byte](chunkSize * 3)(4)
      os.write.over(paths.processLog, split)
      assert(DesktopSmoke.scanForSentinel(paths, userData).isLeft)
    }

    test("log merge streams large files and diagnostic tail is capped redacted and marked") {
      val root       = os.temp.dir(prefix = "desktop-smoke-large-logs-", deleteOnExit = true)
      val stdout     = root / "stdout.log"
      val stderr     = root / "stderr.log"
      val merged     = root / "merged.log"
      val sentinel   = DesktopSmoke.launchEnvironment("MORPHIR_DESKTOP_SMOKE_SENTINEL")
      val stdoutSize = 120000
      os.write(stdout, Array.fill[Byte](stdoutSize)('a'.toByte))
      os.write(stderr, Array.fill[Byte](80000)('b'.toByte) ++ s"tail-$sentinel".getBytes(StandardCharsets.UTF_8))

      DesktopSmoke.mergeLogs(Seq(stdout, stderr), merged)
      assert(os.size(merged) == os.size(stdout) + os.size(stderr))

      val diagnostic = DesktopSmoke.cappedDiagnosticTail(Seq(stdout, stderr), 65536)
      assert(diagnostic.truncated)
      assert(diagnostic.text.startsWith("[truncated]\n"))
      assert(diagnostic.text.endsWith("tail-<redacted>"))
      assert(!diagnostic.text.contains(sentinel))
      assert(diagnostic.text.getBytes(StandardCharsets.UTF_8).length <= 65536 + 64)
    }

    test("scan checks remaining artifacts and user data when other artifacts are missing") {
      val root     = validatedRunRoot("desktop-smoke-scan-partial-")
      val paths    = DesktopSmoke.artifacts(root)
      val userData = root.path / "user-data"
      val sentinel = DesktopSmoke.launchEnvironment("MORPHIR_DESKTOP_SMOKE_SENTINEL")
      os.makeDir.all(paths.processLog / os.up)
      os.makeDir.all(userData)
      os.write(paths.processLog, sentinel)

      assert(DesktopSmoke.scanForSentinel(paths, userData).left.exists(!_.contains(sentinel)))
    }

    test("process runner retires a surviving descendant after a successful parent exit") {
      if scala.util.Properties.isWin then ()
      else {
        val root   = os.temp.dir(prefix = "desktop-smoke-process-success-", deleteOnExit = true)
        val stdout = root / "stdout.log"
        val stderr = root / "stderr.log"
        val result = DesktopSmoke.runProcess(
          Seq("/bin/sh", "-c", "sleep 30 & echo $! > child.pid"),
          root,
          Map.empty,
          Set.empty,
          5.seconds,
          stdout,
          stderr,
          DesktopSmoke.platformToken(System.getProperty("os.name"), System.getProperty("os.arch"))
        )
        val childPid = os.read(root / "child.pid").trim.toLong
        awaitDead(childPid)

        assert(result.status == DesktopSmoke.ProcessStatus.Completed)
        assert(result.exitCode.contains(0))
        assert(result.treeStopped)
        assert(ProcessHandle.of(childPid).isEmpty || !ProcessHandle.of(childPid).get().isAlive)
      }
    }

    test("process runner times out and retires its retained root and descendant handles") {
      if scala.util.Properties.isWin then ()
      else {
        val root   = os.temp.dir(prefix = "desktop-smoke-process-timeout-", deleteOnExit = true)
        val stdout = root / "stdout.log"
        val stderr = root / "stderr.log"
        val result = DesktopSmoke.runProcess(
          Seq("/bin/sh", "-c", "echo $$ > root.pid; sleep 30 & echo $! > child.pid; wait"),
          root,
          Map.empty,
          Set.empty,
          250.millis,
          stdout,
          stderr,
          DesktopSmoke.platformToken(System.getProperty("os.name"), System.getProperty("os.arch"))
        )
        val rootPid  = os.read(root / "root.pid").trim.toLong
        val childPid = os.read(root / "child.pid").trim.toLong
        awaitDead(rootPid)
        awaitDead(childPid)

        assert(result.status == DesktopSmoke.ProcessStatus.TimedOut)
        assert(result.treeStopped)
        assert(ProcessHandle.of(rootPid).isEmpty || !ProcessHandle.of(rootPid).get().isAlive)
        assert(ProcessHandle.of(childPid).isEmpty || !ProcessHandle.of(childPid).get().isAlive)
      }
    }

    test("safeRunRoot accepts an existing ordinary descendant") {
      val base      = os.temp.dir(prefix = "desktop-smoke-safe-base-", deleteOnExit = true)
      val candidate = base / "task-42" / "run"
      os.makeDir.all(candidate)

      val physicalCandidate = os.Path(candidate.toNIO.toRealPath())
      assert(DesktopSmoke.safeRunRoot(base, candidate).map(_.path) == Right(physicalCandidate))

      DesktopSmoke.safeRunRoot(base, candidate).foreach(DesktopSmoke.cleanup)
      assert(!os.exists(candidate))
    }

    test("cleanup removes only a validated run root") {
      val root = validatedRunRoot("desktop-smoke-cleanup-")
      val base = root.path / os.up
      os.write(root.path / "nested" / "file", "owned", createFolders = true)
      os.write(base / "keep", "safe")

      DesktopSmoke.cleanup(root)
      assert(!os.exists(root.path))
      assert(os.exists(base / "keep"))
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
