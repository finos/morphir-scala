package millbuild

import java.util.concurrent.{CountDownLatch, TimeUnit}

import scala.concurrent.duration.*

import upickle.default.*
import utest.*

object LinkerBenchmarkTests extends TestSuite {
  import LinkerBenchmark.*

  private val defaultResolvedConfiguration = hostedPreset("js-strategies").toOption.get

  private def strategyTrial(
      trial: Int,
      wallMillis: Long,
      outcome: Outcome = Outcome.Succeeded,
      peakAggregateRssKiB: Long = 4096L,
      strategy: Strategy = Strategy.Recycled,
      profile: Profile = ciProfile
  ): StrategyTrialRecord =
    StrategyTrialRecord(
      "run-1",
      trial,
      0,
      Platform.ScalaJs,
      strategy,
      profile,
      wallMillis,
      peakAggregateRssKiB,
      4L,
      outcome
    )

  private def worker(
      trial: Int,
      lane: Int = 0,
      startupMillis: Long = 100L,
      peakRssKiB: Long = 2048L,
      peakHeapBytes: Long = 512L,
      gcMillis: Long = 20L,
      outcome: Outcome = Outcome.Succeeded,
      strategy: Strategy = Strategy.Recycled,
      detail: Option[String] = None,
      profile: Profile = ciProfile
  ): WorkerRecord =
    WorkerRecord(
      "run-1",
      trial,
      0,
      defaultEvaluationSettings,
      Platform.ScalaJs,
      strategy,
      profile,
      lane,
      0,
      Seq(s"target-$lane"),
      startupMillis,
      peakRssKiB,
      Seq(PhaseMetrics("cold-link", 700L, 256L, peakHeapBytes, 2L, gcMillis, 6L, 1L)),
      outcome,
      detail
    )

  val tests = Tests {
    test("CLI optional strings trim values and reject control characters without echoing them") {
      assert(parseOptionalTrimmedString("", "target filter") == Right(None))
      assert(parseOptionalTrimmedString("   ", "target filter") == Right(None))
      assert(parseOptionalTrimmedString("  langkit  ", "target filter") == Right(Some("langkit")))

      val payload = "secret\ninjected"
      val message = parseOptionalTrimmedString(payload, "target filter").left.toOption.get
      assert(message.contains("target filter"))
      assert(!message.contains(payload))
      assert(!message.exists(Character.isISOControl))
    }

    test("CLI numeric and continuation inputs preserve unset values and reject unsafe input") {
      assert(parseZeroAsUnsetPositiveInt("0", "trials") == Right(None))
      assert(parseZeroAsUnsetPositiveInt(" 3 ", "trials") == Right(Some(3)))
      assert(parseZeroAsUnsetPositiveInt("-1", "trials").isLeft)
      assert(parsePositiveInt("1", "artifact run attempt") == Right(1))
      assert(parsePositiveInt("0", "artifact run attempt").isLeft)
      assert(parseBoolean(" true ", "smoke") == Right(true))
      assert(parseBoolean("false", "smoke") == Right(false))
      assert(parseBoolean("TRUE", "smoke") == Right(true))
      assert(parseBoolean(" False ", "smoke") == Right(false))
      assert(parseOptionalLong("", "order seed") == Right(None))
      assert(parseOptionalLong(" 42 ", "order seed") == Right(Some(42L)))
      assert(parseContinuationChoice(" preset ") == Right(None))
      assert(parseContinuationChoice("TRUE") == Right(Some(true)))
      assert(parseContinuationChoice("False") == Right(Some(false)))

      Seq("not-a-long", "secret\ninjected").foreach { payload =>
        val message = parseOptionalLong(payload, "order seed").left.toOption.get
        assert(message.contains("order seed"))
        assert(!message.contains(payload))
        assert(!message.exists(Character.isISOControl))
      }
      Seq("sometimes", "true\ninjected").foreach { payload =>
        val message = parseContinuationChoice(payload).left.toOption.get
        assert(message.contains("continue on failure"))
        assert(!message.contains(payload))
        assert(!message.exists(Character.isISOControl))
      }
      Seq("not-an-int", "123\ninjected", Int.MaxValue.toString + "0").foreach { payload =>
        val message = parseZeroAsUnsetPositiveInt(payload, "trials").left.toOption.get
        assert(message.contains("trials"))
        assert(!message.contains(payload))
        assert(!message.exists(Character.isISOControl))
      }
      Seq("sometimes", "true\ninjected").foreach { payload =>
        val message = parseBoolean(payload, "smoke").left.toOption.get
        assert(message.contains("smoke"))
        assert(!message.contains(payload))
        assert(!message.exists(Character.isISOControl))
      }
    }

    test("direct CLI defaults and explicit legacy selections resolve identically") {
      val local    = Profile("local", 32, 8, 8, 2, 2, 4, 30)
      val defaults = resolveBenchmarkConfiguration("", "", local, BenchmarkOverrides()).toOption.get
      assert(defaults == ResolvedBenchmarkConfiguration(
        "direct",
        local,
        EvaluationSettings(3, 0L),
        Platform.values.toSeq,
        Strategy.values.toSeq,
        None,
        None,
        continueOnFailure = false
      ))

      val explicit = resolveBenchmarkConfiguration(
        "",
        "local",
        local,
        BenchmarkOverrides(platforms = Some("wasm"), trials = Some(1))
      ).toOption.get
      assert(explicit.platforms == Seq(Platform.Wasm))
      assert(explicit.settings == EvaluationSettings(1, 0L))
      assert(explicit.profile == local)
    }

    test("hosted configuration rejects the direct-only profile input") {
      val local   = Profile("local", 32, 8, 8, 2, 2, 4, 30)
      val payload = "secret\ninjected"
      val result  = resolveBenchmarkConfiguration("quick-smoke", payload, local, BenchmarkOverrides())
      val message = result.left.toOption.get
      assert(message.contains("profile"))
      assert(!message.contains(payload))
      assert(!message.exists(Character.isISOControl))
      assert(resolveBenchmarkConfiguration("quick-smoke", "ci", local, BenchmarkOverrides()).isLeft)
    }

    test("hosted artifact names normalize unsafe separators and preserve unique run suffixes") {
      assert(
        hostedArtifactName("quick-smoke", "Feature/Ünicode Branch", "123", 2) ==
          Right("linker-benchmark-quick-smoke-feature-nicode-branch-123-2")
      )
      assert(
        hostedArtifactName("js-strategies", "release...candidate", "run.42", 1) ==
          Right("linker-benchmark-js-strategies-release...candidate-run.42-1")
      )

      val longRef = "feature/" + ("long-name-" * 40)
      val first   = hostedArtifactName("quick-smoke", longRef, "987654321", 3).toOption.get
      val second  = hostedArtifactName("quick-smoke", longRef + "different", "987654322", 3).toOption.get
      assert(first.length <= 120)
      assert(second.length <= 120)
      assert(first != second)
      assert(first.endsWith("-987654321-3"))
      assert(second.endsWith("-987654322-3"))
      assert(first.matches("[a-z0-9](?:[a-z0-9.-]*[a-z0-9])?"))
    }

    test("hosted artifact names reject missing, invalid, and secret-bearing inputs without echo") {
      assert(hostedArtifactName("", "main", "1", 1).isLeft)
      assert(hostedArtifactName("unknown", "main", "1", 1).isLeft)
      assert(hostedArtifactName("quick-smoke", "", "1", 1).isLeft)
      assert(hostedArtifactName("quick-smoke", "main", "", 1).isLeft)
      assert(hostedArtifactName("quick-smoke", "main", "1", 0).isLeft)
      assert(hostedArtifactName("quick-smoke", "main", "1", -1).isLeft)
      assert(hostedArtifactName(" quick-smoke ", "main", "1", 1).isRight)
      assert(hostedArtifactName("quick/smoke", "main", "1", 1).isLeft)
      assert(hostedArtifactName("Quick-Smoke", "main", "1", 1).isLeft)
      assert(hostedArtifactName("quick‐smoke", "main", "1", 1).isLeft)

      val payload = "secret\ninjected"
      val message = hostedArtifactName("quick-smoke", payload, "1", 1).left.toOption.get
      assert(message.contains("artifact ref"))
      assert(!message.contains(payload))
      assert(!message.exists(Character.isISOControl))
    }

    test("hosted resource bounds accept their limits and reject larger values") {
      val atLimits = BenchmarkOverrides(
        trials = Some(maxHostedTrials),
        targetLimit = Some(maxHostedTargetLimit),
        memoryGiB = Some(maxHostedMemoryGiB),
        reserveGiB = Some(maxHostedReserveGiB),
        millJobs = Some(maxHostedMillJobs),
        maxChildren = Some(maxHostedChildren),
        batchSize = Some(maxHostedBatchSize),
        timeoutMinutes = Some(maxHostedTimeoutMinutes)
      )
      assert(validateHostedOverrideBounds(atLimits).isRight)

      val aboveLimits = Seq(
        BenchmarkOverrides(trials = Some(maxHostedTrials + 1)),
        BenchmarkOverrides(targetLimit = Some(maxHostedTargetLimit + 1)),
        BenchmarkOverrides(memoryGiB = Some(maxHostedMemoryGiB + 1)),
        BenchmarkOverrides(reserveGiB = Some(maxHostedReserveGiB + 1)),
        BenchmarkOverrides(millJobs = Some(maxHostedMillJobs + 1)),
        BenchmarkOverrides(maxChildren = Some(maxHostedChildren + 1)),
        BenchmarkOverrides(batchSize = Some(maxHostedBatchSize + 1)),
        BenchmarkOverrides(timeoutMinutes = Some(maxHostedTimeoutMinutes + 1))
      )
      val maximumValues = Seq(
        BenchmarkOverrides(trials = Some(Int.MaxValue)),
        BenchmarkOverrides(targetLimit = Some(Int.MaxValue)),
        BenchmarkOverrides(memoryGiB = Some(Int.MaxValue)),
        BenchmarkOverrides(reserveGiB = Some(Int.MaxValue)),
        BenchmarkOverrides(millJobs = Some(Int.MaxValue)),
        BenchmarkOverrides(maxChildren = Some(Int.MaxValue)),
        BenchmarkOverrides(batchSize = Some(Int.MaxValue)),
        BenchmarkOverrides(timeoutMinutes = Some(Int.MaxValue))
      )
      assert((aboveLimits ++ maximumValues).forall(validateHostedOverrideBounds(_).isLeft))
    }

    test("configuration JSON is sanitized and matches the configuration embedded in final results") {
      val secret        = "super-secret-filter"
      val configuration = defaultResolvedConfiguration.copy(targetFilter = Some(secret))
      val environment   = Map("FILTER_SECRET" -> secret)
      val standalone    = ujson.read(renderConfigurationJson(configuration, environment))
      val result        = ujson.read(renderJson(BenchmarkResult(configuration, 0L, Seq.empty, Seq.empty), environment))

      assert(standalone == result("configuration"))
      assert(!standalone.render().contains(secret))
    }

    test("hosted presets resolve to their exact configurations") {
      val expected = Map(
        "quick-smoke" -> ResolvedBenchmarkConfiguration(
          "quick-smoke",
          ciProfile,
          EvaluationSettings(1, 0L),
          Platform.values.toSeq,
          Strategy.values.toSeq,
          None,
          Some(1),
          continueOnFailure = true
        ),
        "js-strategies" -> ResolvedBenchmarkConfiguration(
          "js-strategies",
          ciProfile,
          EvaluationSettings(3, 0L),
          Seq(Platform.ScalaJs),
          Strategy.values.toSeq,
          None,
          None,
          continueOnFailure = true
        ),
        "wasm-strategies" -> ResolvedBenchmarkConfiguration(
          "wasm-strategies",
          ciProfile,
          EvaluationSettings(3, 0L),
          Seq(Platform.Wasm),
          Strategy.values.toSeq,
          None,
          None,
          continueOnFailure = true
        ),
        "native-long-lived" -> ResolvedBenchmarkConfiguration(
          "native-long-lived",
          ciProfile.copy(timeoutMinutes = 40),
          EvaluationSettings(1, 0L),
          Seq(Platform.Native),
          Seq(Strategy.LongLived),
          None,
          None,
          continueOnFailure = true
        ),
        "native-fresh-recycled" -> ResolvedBenchmarkConfiguration(
          "native-fresh-recycled",
          ciProfile,
          EvaluationSettings(3, 0L),
          Seq(Platform.Native),
          Seq(Strategy.Fresh, Strategy.Recycled),
          None,
          None,
          continueOnFailure = true
        )
      )

      assert(expected.map { case (token, value) => token -> hostedPreset(token) } ==
        expected.view.mapValues(Right(_)).toMap)
      assert(hostedPreset("unknown").isLeft)
    }

    test("hosted configuration applies concrete overrides and retains unset preset values") {
      val overrides = BenchmarkOverrides(
        platforms = Some("native,wasm"),
        strategies = Some("fresh,recycled"),
        trials = Some(2),
        orderSeed = Some(7L),
        targetFilter = Some("langkit"),
        targetLimit = Some(3),
        memoryGiB = Some(32),
        reserveGiB = Some(4),
        millJobs = Some(3),
        maxChildren = Some(3),
        batchSize = Some(2),
        timeoutMinutes = Some(45),
        continueOnFailure = Some(false)
      )
      val resolved = resolveHostedConfiguration("quick-smoke", overrides).toOption.get

      assert(resolved.platforms == Seq(Platform.Native, Platform.Wasm))
      assert(resolved.strategies == Seq(Strategy.Fresh, Strategy.Recycled))
      assert(resolved.settings == EvaluationSettings(2, 7L))
      assert(resolved.targetFilter.contains("langkit"))
      assert(resolved.targetLimit.contains(3))
      assert(resolved.profile == Profile("ci", 32, 4, 8, 3, 3, 2, 45))
      assert(!resolved.continueOnFailure)
      assert(resolveHostedConfiguration("native-long-lived", BenchmarkOverrides()).toOption.get ==
        hostedPreset("native-long-lived").toOption.get)
    }

    test("hosted configuration rejects invalid overrides") {
      val invalid = Seq(
        BenchmarkOverrides(platforms = Some("native,native")),
        BenchmarkOverrides(platforms = Some("native,,wasm")),
        BenchmarkOverrides(platforms = Some("unknown")),
        BenchmarkOverrides(strategies = Some("fresh,fresh")),
        BenchmarkOverrides(strategies = Some("unknown")),
        BenchmarkOverrides(trials = Some(0)),
        BenchmarkOverrides(orderSeed = Some(-1L)),
        BenchmarkOverrides(targetFilter = Some("   ")),
        BenchmarkOverrides(targetLimit = Some(-1)),
        BenchmarkOverrides(memoryGiB = Some(-1)),
        BenchmarkOverrides(reserveGiB = Some(-1)),
        BenchmarkOverrides(millJobs = Some(-1)),
        BenchmarkOverrides(maxChildren = Some(-1)),
        BenchmarkOverrides(batchSize = Some(-1)),
        BenchmarkOverrides(timeoutMinutes = Some(-1)),
        BenchmarkOverrides(memoryGiB = Some(8), reserveGiB = Some(4))
      )

      assert(invalid.forall(value => resolveHostedConfiguration("quick-smoke", value).isLeft))
      assert(resolveHostedConfiguration("quick-smoke", BenchmarkOverrides(platforms = Some(""))).isLeft)
      assert(read[BenchmarkOverrides](write(BenchmarkOverrides())) == BenchmarkOverrides())
    }

    test("hosted trials have a bounded reproducible range") {
      val maximum = resolveHostedConfiguration(
        "quick-smoke",
        BenchmarkOverrides(trials = Some(maxHostedTrials))
      )

      assert(maximum.toOption.exists(_.settings.trials == maxHostedTrials))
      assert(resolveHostedConfiguration(
        "quick-smoke",
        BenchmarkOverrides(trials = Some(maxHostedTrials + 1))
      ).isLeft)
      assert(resolveHostedConfiguration(
        "quick-smoke",
        BenchmarkOverrides(trials = Some(Int.MaxValue))
      ).isLeft)
    }

    test("hosted selection errors do not echo untrusted override text") {
      val cases = Seq(
        ("platforms", "native\ninjected", BenchmarkOverrides(platforms = Some("native\ninjected"))),
        ("platforms", "native\u001binjected", BenchmarkOverrides(platforms = Some("native\u001binjected"))),
        ("strategies", "fresh\ninjected", BenchmarkOverrides(strategies = Some("fresh\ninjected"))),
        ("strategies", "fresh\u001binjected", BenchmarkOverrides(strategies = Some("fresh\u001binjected"))),
        ("platforms", "secret-platform-value", BenchmarkOverrides(platforms = Some("secret-platform-value"))),
        ("strategies", "secret-strategy-value", BenchmarkOverrides(strategies = Some("secret-strategy-value"))),
        ("platforms", "native,native", BenchmarkOverrides(platforms = Some("native,native"))),
        ("strategies", "fresh,fresh", BenchmarkOverrides(strategies = Some("fresh,fresh")))
      )

      cases.foreach { case (field, payload, overrides) =>
        val message = resolveHostedConfiguration("quick-smoke", overrides).left.toOption.get
        assert(message.contains(field))
        assert(!message.contains(payload))
        assert(!message.exists(Character.isISOControl))
      }
    }

    test("hosted preset errors do not echo untrusted tokens") {
      val tokens = Seq("quick-smoke\ninjected", "quick-smoke\u001binjected", "secret-preset-value")

      tokens.foreach { token =>
        val message = hostedPreset(token).left.toOption.get
        assert(message.contains("preset"))
        assert(!message.contains(token))
        assert(!message.exists(Character.isISOControl))
      }
    }

    test("inventory target selection is stable and validates filters and limits") {
      val targets = Seq("zeta", "alpha-two", "alpha-one", "zeta")

      assert(selectInventoryTargets(targets, None, None, Platform.Native) ==
        Right(Seq("alpha-one", "alpha-two", "zeta")))
      assert(selectInventoryTargets(targets.reverse, Some("alpha"), Some(1), Platform.Native) ==
        Right(Seq("alpha-one")))
      assert(selectInventoryTargets(targets, Some("missing"), None, Platform.Native).isLeft)
      assert(selectInventoryTargets(targets, None, Some(0), Platform.Native).isLeft)
      assert(selectInventoryTargets(targets, Some("alpha\n"), None, Platform.Native).isLeft)
      assert(selectInventoryTargets(Seq.empty, None, None, Platform.Native).isLeft)
    }

    test("smoke child scrubs inherited JVM and Mill sandbox options") {
      assert(
        smokeEnvironmentRemovals == Set(
          "JAVA_OPTS",
          "JDK_JAVA_OPTIONS",
          "JAVA_TOOL_OPTIONS",
          "_JAVA_OPTIONS",
          "MILL_WORKSPACE_ROOT"
        )
      )
    }

    test("smoke workspace resolution accepts the Mill root or an aliased nested cwd") {
      val root       = os.temp.dir(prefix = "linker-benchmark-workspace-")
      val repository = root / "repository"
      val alias      = root / "repository-alias"
      try {
        os.makeDir.all(repository / "scripts" / "ci")
        os.write(repository / "mill", "launcher")
        os.write(repository / ".mill-version", "1.2.0")
        os.write(repository / "build.mill", "")
        java.nio.file.Files.createSymbolicLink(alias.toNIO, repository.toNIO)

        val physicalRepository = canonicalPhysicalPath(repository).map(os.Path(_))
        assert(resolveSmokeWorkspace(Map("MILL_WORKSPACE_ROOT" -> repository.toString), root) == physicalRepository)
        assert(
          resolveSmokeWorkspace(
            Map("MILL_WORKSPACE_ROOT" -> (root / "not-a-repository").toString),
            alias / "scripts" / "ci"
          ) == physicalRepository
        )
        assert(resolveSmokeWorkspace(Map.empty, root).isLeft)
      } finally os.remove.all(root)
    }

    test("smoke marker and proof records round trip through uPickle") {
      val record = SmokeRecord(
        childPid = 42L,
        javaVersion = "21",
        pinnedMillVersion = "1.2.0",
        requestedHeapBytes = 1024L,
        effectiveMaxHeapBytes = 8192L,
        requestedHeapHonored = false,
        outputDirectoryIdentity = "lane-identity",
        proofFilename = "worker-proof-42.json"
      )
      val proof = SmokeProof(record.childPid, record.outputDirectoryIdentity)

      assert(read[SmokeRecord](write(record)) == record)
      assert(read[SmokeProof](write(proof)) == proof)
    }

    test("recovery records preserve timeout and replacement evidence") {
      val replacementWorker = SmokeRecord(
        childPid = 43L,
        javaVersion = "secret-value",
        pinnedMillVersion = "1.2.0",
        requestedHeapBytes = 1024L,
        effectiveMaxHeapBytes = 8192L,
        requestedHeapHonored = false,
        outputDirectoryIdentity = "recovery-lane",
        proofFilename = "worker-proof-43.json"
      )
      val record = RecoveryRecord(
        first = RecoveryAttemptRecord(Outcome.TimedOut, 1L, None, None),
        replacement = RecoveryAttemptRecord(Outcome.Succeeded, 2L, Some(0), Some(replacementWorker))
      )

      assert(read[RecoveryRecord](write(record)) == record)
      assert(validateRecovery(record).isRight)
      assert(validateRecovery(record.copy(first = record.first.copy(outcome = Outcome.Succeeded))).isLeft)
      assert(validateRecovery(record.copy(replacement = record.replacement.copy(worker = None))).isLeft)
      assert(!renderRecoveryJson(record, Map("API_TOKEN" -> "secret-value")).contains("secret-value"))
    }

    test("benchmark output roots fingerprint the complete configuration without exposing values") {
      val base       = os.Path("/benchmark-output")
      val first      = defaultResolvedConfiguration
      val overridden = first.copy(settings = first.settings.copy(orderSeed = 1L), targetFilter = Some("secret/value"))
      val firstId    = benchmarkOutputIdentity(first).toOption.get

      assert(firstId == benchmarkOutputIdentity(first).toOption.get)
      assert(firstId.matches("js-strategies-[a-f0-9]{16}"))
      assert(firstId != benchmarkOutputIdentity(overridden).toOption.get)
      assert(!benchmarkOutputIdentity(overridden).toOption.get.contains("secret"))
      assert(
        benchmarkOutputRoot(base, first, recoverySmoke = false, planOnly = false) ==
          Right(base / firstId / "ci" / "scala-js")
      )
      assert(
        benchmarkOutputRoot(base, first, recoverySmoke = true, planOnly = false) ==
          Right(base / firstId / "ci" / "recovery-smoke")
      )
      assert(
        benchmarkOutputRoot(base, first, recoverySmoke = false, planOnly = true) ==
          Right(base / firstId / "ci" / "plan-only" / "scala-js")
      )
      assert(benchmarkOutputRoot(base, first.copy(platforms = Seq.empty), false, false).isLeft)
      assert(benchmarkOutputIdentity(first.copy(preset = "../../escaped")).isLeft)
    }

    test("recovery proof validation binds identity and rejects symlink escapes") {
      val root             = os.temp.dir(prefix = "linker-recovery-proof-")
      val workspace        = root / "workspace"
      val rootOutput       = workspace / "out"
      val lane             = root / "external" / "lane"
      val proofPath        = lane / "worker-proof-43.json"
      val expectedIdentity = "expected-lane-identity"
      val worker           = SmokeRecord(
        childPid = 43L,
        javaVersion = "21",
        pinnedMillVersion = "1.2.0",
        requestedHeapBytes = 1024L,
        effectiveMaxHeapBytes = 8192L,
        requestedHeapHonored = false,
        outputDirectoryIdentity = expectedIdentity,
        proofFilename = proofPath.last
      )
      val proof    = SmokeProof(worker.childPid, worker.outputDirectoryIdentity)
      val recovery = RecoveryRecord(
        RecoveryAttemptRecord(Outcome.TimedOut, 1L, None, None),
        RecoveryAttemptRecord(Outcome.Succeeded, 2L, Some(0), Some(worker))
      )
      try {
        os.makeDir.all(rootOutput)
        os.makeDir.all(lane)
        os.write(proofPath, write(proof))

        val valid = validateRecoveryProof(
          workspace,
          rootOutput,
          lane,
          proofPath,
          expectedIdentity,
          worker,
          proof
        )
        assert(valid.isRight)
        assert(renderValidatedRecoveryJson(recovery, valid, Map.empty).isRight)

        val wrongWorker = worker.copy(outputDirectoryIdentity = "self-consistent-wrong-identity")
        val wrongProof  = SmokeProof(wrongWorker.childPid, wrongWorker.outputDirectoryIdentity)
        assert(
          validateRecoveryProof(
            workspace,
            rootOutput,
            lane,
            proofPath,
            expectedIdentity,
            wrongWorker,
            wrongProof
          ).isLeft
        )

        val escapedTarget = root / "escaped-proof.json"
        val escapedPath   = lane / "escaped-proof.json"
        val escapedWorker = worker.copy(proofFilename = escapedPath.last)
        val escapedProof  = SmokeProof(escapedWorker.childPid, escapedWorker.outputDirectoryIdentity)
        os.write(escapedTarget, write(escapedProof))
        java.nio.file.Files.createSymbolicLink(escapedPath.toNIO, escapedTarget.toNIO)
        val escaped = validateRecoveryProof(
          workspace,
          rootOutput,
          lane,
          escapedPath,
          expectedIdentity,
          escapedWorker,
          escapedProof
        )
        assert(escaped.isLeft)
        assert(renderValidatedRecoveryJson(recovery, escaped, Map.empty).isLeft)
      } finally os.remove.all(root)
    }

    test("smoke temporary base preserves its lexical symlink spelling") {
      val root         = os.temp.dir(prefix = "linker-benchmark-lexical-tmp-")
      val physicalBase = root / "physical-tmp"
      val lexicalBase  = root / "lexical-tmp"
      try {
        os.makeDir.all(physicalBase)
        java.nio.file.Files.createSymbolicLink(lexicalBase.toNIO, physicalBase.toNIO)

        assert(resolveSmokeTemporaryBase(lexicalBase.toString, root) == Right(lexicalBase))
        assert(resolveSmokeTemporaryBase("", root).isLeft)
        assert(resolveSmokeTemporaryBase((root / "missing").toString, root).isLeft)
      } finally os.remove.all(root)
    }

    test("smoke temporary base prefers a physically equivalent TMPDIR spelling") {
      val root         = os.temp.dir(prefix = "linker-benchmark-tmpdir-")
      val physicalBase = root / "physical-tmp"
      val lexicalBase  = root / "lexical-tmp"
      val otherBase    = root / "other-tmp"
      try {
        os.makeDir.all(physicalBase)
        os.makeDir.all(otherBase)
        java.nio.file.Files.createSymbolicLink(lexicalBase.toNIO, physicalBase.toNIO)

        assert(
          resolveSmokeTemporaryBase(physicalBase.toString, Some(lexicalBase.toString), root) == Right(lexicalBase)
        )
        assert(resolveSmokeTemporaryBase(physicalBase.toString, None, root) == Right(physicalBase))
        assert(resolveSmokeTemporaryBase(physicalBase.toString, Some(""), root).isLeft)
        assert(resolveSmokeTemporaryBase(physicalBase.toString, Some((root / "missing").toString), root).isLeft)
        assert(resolveSmokeTemporaryBase(physicalBase.toString, Some(otherBase.toString), root).isLeft)
      } finally os.remove.all(root)
    }

    test("smoke child output value preserves its lexical lane spelling") {
      val root         = os.temp.dir(prefix = "linker-benchmark-lexical-lane-")
      val physicalBase = root / "physical-tmp"
      val lexicalBase  = root / "lexical-tmp"
      try {
        os.makeDir.all(physicalBase / "morphir-linker-smoke-run" / "lane-0")
        java.nio.file.Files.createSymbolicLink(lexicalBase.toNIO, physicalBase.toNIO)
        val lane = lexicalBase / "morphir-linker-smoke-run" / "lane-0"

        assert(smokeChildOutputDirectoryValue(lane) == lane.wrapped.toAbsolutePath.normalize().toString)
        assert(!smokeChildOutputDirectoryValue(lane).contains("physical-tmp"))
      } finally os.remove.all(root)
    }

    test("smoke run allocation ignores a canonicalizing path serializer") {
      val root         = os.temp.dir(prefix = "linker-benchmark-serializer-")
      val physicalBase = root / "physical-tmp"
      val lexicalBase  = root / "lexical-tmp"
      object CanonicalizingSerializer extends os.Path.Serializer {
        override def serializeString(path: os.Path): String               = serializePath(path).toString
        override def serializeFile(path: os.Path): java.io.File           = serializePath(path).toFile
        override def serializePath(path: os.Path): java.nio.file.Path     = path.wrapped.toRealPath()
        override def deserialize(value: String): java.nio.file.Path       = java.nio.file.Paths.get(value).toRealPath()
        override def deserialize(value: java.io.File): java.nio.file.Path = value.toPath.toRealPath()
        override def deserialize(value: java.nio.file.Path): java.nio.file.Path = value.toRealPath()
        override def deserialize(value: java.net.URI): java.nio.file.Path       =
          java.nio.file.Paths.get(value).toRealPath()
      }
      try {
        os.makeDir.all(physicalBase)
        java.nio.file.Files.createSymbolicLink(lexicalBase.toNIO, physicalBase.toNIO)

        val allocated = os.Path.pathSerializer.withValue(CanonicalizingSerializer) {
          createSmokeTemporaryRunRoot(lexicalBase)
        }

        assert(allocated.wrapped.getParent == lexicalBase.wrapped)
        assert(allocated.last.startsWith("morphir-linker-smoke-"))
        assert(java.nio.file.Files.isDirectory(allocated.wrapped))
      } finally os.remove.all(root)
    }

    test("pinned eight GiB heap must win the one GiB probe") {
      val gib = 1024L * 1024L * 1024L

      assert(validatePinnedHeapProbe(gib, 8L * gib, requestedHeapHonored = false).isRight)
      assert(validatePinnedHeapProbe(gib, 7L * gib, requestedHeapHonored = false).isRight)
      assert(validatePinnedHeapProbe(gib, 9L * gib, requestedHeapHonored = false).isRight)
      assert(validatePinnedHeapProbe(gib, gib, requestedHeapHonored = true).isLeft)
      assert(validatePinnedHeapProbe(gib, gib, requestedHeapHonored = false).isLeft)
      assert(validatePinnedHeapProbe(2L * gib, 8L * gib, requestedHeapHonored = false).isLeft)
    }

    test("smoke heap probe must also pass profile admission") {
      val gib    = 1024L * 1024L * 1024L
      val record = SmokeRecord(
        childPid = 42L,
        javaVersion = "21",
        pinnedMillVersion = "1.2.0",
        requestedHeapBytes = gib,
        effectiveMaxHeapBytes = 8L * gib,
        requestedHeapHonored = false,
        outputDirectoryIdentity = "lane-identity",
        proofFilename = "worker-proof-42.json"
      )
      val overcommitted = ciProfile.copy(memoryGiB = 11)

      assert(validateSmokeHeapProbe(record, ciProfile) == Right(8))
      assert(validateSmokeHeapProbe(record, overcommitted).isLeft)
      assert(validateSmokeHeapProbe(record.copy(requestedHeapHonored = true), ciProfile).isLeft)
    }

    test("effective heap validation conservatively enforces the profile memory budget") {
      val gib        = 1024L * 1024L * 1024L
      val overcommit = ciProfile.copy(heapGiB = 4, maxChildren = 2)

      assert(validateEffectiveHeap(ciProfile, 8L * gib) == Right(8))
      assert(validateEffectiveHeap(ciProfile, 7L * gib + 1L) == Right(8))
      assert(validateEffectiveHeap(overcommit, 8L * gib).isLeft)
      assert(validateEffectiveHeap(ciProfile, 0L).isLeft)
      assert(validateEffectiveHeap(ciProfile, -1L).isLeft)
      assert(validateEffectiveHeap(ciProfile, Long.MaxValue).isLeft)
    }

    test("configured heap validation accepts inclusive tolerance boundaries") {
      val gib       = 1024L * 1024L * 1024L
      val tolerance = 256L * 1024L * 1024L

      assert(validateConfiguredHeap(ciProfile, 8L * gib - tolerance) == Right(8))
      assert(validateConfiguredHeap(ciProfile, 8L * gib + tolerance) == Right(9))
      assert(validateConfiguredHeap(ciProfile, 8L * gib - tolerance - 1L).isLeft)
      assert(validateConfiguredHeap(ciProfile, 8L * gib + tolerance + 1L).isLeft)
      assert(validateConfiguredHeap(ciProfile.copy(heapGiB = 0), 8L * gib).isLeft)
      assert(validateConfiguredHeap(ciProfile.copy(heapGiB = Int.MaxValue), Long.MaxValue).isLeft)
    }

    test("configured heap admission uses the observed rounded-up heap") {
      val gib       = 1024L * 1024L * 1024L
      val requested = Profile("ci", memoryGiB = 10, reserveGiB = 4, heapGiB = 6, 1, 1, 4, 30)

      assert(validate(requested).isRight)
      assert(validateConfiguredHeap(requested, 6L * gib) == Right(6))
      assert(validateConfiguredHeap(requested, 6L * gib + 1L).isLeft)
    }

    test("benchmark child output directory must be isolated from the orchestrator") {
      val root         = os.Path("/tmp/linker-benchmark-output-validation")
      val orchestrator = root / "orchestrator"
      val alias        = os.Path(s"$orchestrator/nested/..")
      val child        = root / "lane-0"
      val message      = "benchmark child output directory must differ from the orchestrator output directory"

      assert(validateChildOutputDirectory(orchestrator, orchestrator) == Left(message))
      assert(validateChildOutputDirectory(orchestrator, alias) == Left(message))
      assert(validateChildOutputDirectory(orchestrator, child) == Right(child))
    }

    test("benchmark child output directory must be physically outside the workspace") {
      val root          = os.temp.dir(prefix = "linker-benchmark-external-output-")
      val workspace     = root / "workspace"
      val workspaceLink = root / "workspace-link"
      val externalLane  = root / "external-run" / "lane-0"
      try {
        os.makeDir.all(workspace / "inside")
        os.makeDir.all(externalLane)
        java.nio.file.Files.createSymbolicLink(workspaceLink.toNIO, workspace.toNIO)

        assert(validateExternalChildOutputDirectory(workspace, workspace / "inside").isLeft)
        assert(validateExternalChildOutputDirectory(workspace, workspaceLink / "inside").isLeft)
        assert(validateExternalChildOutputDirectory(workspace, externalLane) == Right(externalLane))
      } finally os.remove.all(root)
    }

    test("physical path validation rejects symlink aliases and containment escapes") {
      val root     = os.temp.dir(prefix = "linker-benchmark-paths-")
      val real     = root / "real"
      val alias    = root / "alias"
      val lane     = root / "lane"
      val external = root / "external"
      try {
        os.makeDir.all(real / "out")
        os.makeDir.all(lane)
        os.makeDir.all(external)
        java.nio.file.Files.createSymbolicLink(alias.toNIO, real.toNIO)
        java.nio.file.Files.createSymbolicLink((lane / "escaped").toNIO, external.toNIO)

        val collision = "benchmark child output directory must differ from the orchestrator output directory"
        assert(validateChildOutputDirectory(real / "out", alias / "out") == Left(collision))
        assert(validatePhysicalDescendant(lane, lane / "escaped" / "proof", "artifact escaped") ==
          Left("artifact escaped"))
      } finally os.remove.all(root)
    }

    test("smoke run validation rejects symlink deletion escapes and preserves external files") {
      val root      = os.temp.dir(prefix = "linker-benchmark-delete-")
      val workspace = root / "workspace"
      val smokeBase = workspace / ".dev" / ".sdlc" / "mill-jvm-worker-pool" / "out" / "smoke"
      val external  = root / "external"
      val sentinel  = external / "sentinel"
      try {
        os.makeDir.all(smokeBase)
        os.makeDir.all(external)
        os.write(sentinel, "keep")
        java.nio.file.Files.createSymbolicLink((smokeBase / "run").toNIO, external.toNIO)

        val validated = validateSmokeRunDirectory(workspace, smokeBase, smokeBase / "run")
        validated.foreach(path => os.remove.all(os.Path(path)))

        assert(validated.isLeft)
        assert(os.read(sentinel) == "keep")
      } finally os.remove.all(root)
    }

    test("external smoke cleanup is constrained to its uniquely-created temp run root") {
      val root          = os.temp.dir(prefix = "linker-benchmark-external-cleanup-")
      val workspace     = root / "workspace"
      val temporaryBase = root / "tmp"
      val external      = root / "external"
      val sentinel      = external / "sentinel"
      try {
        os.makeDir.all(workspace)
        os.makeDir.all(temporaryBase)
        os.makeDir.all(external)
        os.write(sentinel, "keep")
        val allocated = os.Path(
          java.nio.file.Files.createTempDirectory(temporaryBase.toNIO, "morphir-linker-smoke-")
        )
        val escape = temporaryBase / "morphir-linker-smoke-escape"
        java.nio.file.Files.createSymbolicLink(escape.toNIO, external.toNIO)

        assert(validateExternalSmokeCleanupRoot(workspace, temporaryBase, allocated).isRight)
        assert(validateExternalSmokeCleanupRoot(workspace, temporaryBase, allocated / "lane-0").isLeft)
        val rejected = validateExternalSmokeCleanupRoot(workspace, temporaryBase, escape)
        rejected.foreach(path => os.remove.all(os.Path(path)))

        assert(rejected.isLeft)
        assert(os.read(sentinel) == "keep")
      } finally os.remove.all(root)
    }

    test("platforms expose stable tokens and exact linker selectors") {
      assert(Platform.ScalaJs.token == "scala-js")
      assert(Platform.ScalaJs.selector == "morphir.__.js.__.fastLinkJSTest")
      assert(Platform.Wasm.token == "wasm")
      assert(Platform.Wasm.selector == "morphir.__.wasm.fullLinkJS")
      assert(Platform.Native.token == "native")
      assert(Platform.Native.selector == "morphir.__.native.__.test.nativeLink")
    }

    test("strategies expose stable tokens") {
      assert(Strategy.LongLived.token == "long-lived")
      assert(Strategy.Fresh.token == "fresh")
      assert(Strategy.Recycled.token == "recycled")
    }

    test("profile validation accepts the CI profile") {
      assert(ciProfile == Profile("ci", 16, 4, 8, 2, 1, 4, 30))
      assert(validate(ciProfile) == Right(ciProfile))
      assert(validate(Profile("ci", 16, 4, 8, 4, 1, 2, 30)).isRight)
    }

    test("profile validation rejects invalid fields and excess memory") {
      assert(validate(Profile("ci", 0, 4, 8, 2, 1, 4, 30)).isLeft)
      assert(validate(Profile("ci", 16, -1, 8, 2, 1, 4, 30)).isLeft)
      assert(validate(Profile("ci", 16, 4, 0, 2, 1, 4, 30)).isLeft)
      assert(validate(Profile("ci", 16, 4, 8, 0, 1, 4, 30)).isLeft)
      assert(validate(Profile("ci", 16, 4, 8, 2, 0, 4, 30)).isLeft)
      assert(validate(Profile("ci", 16, 4, 8, 2, 1, 0, 30)).isLeft)
      assert(validate(Profile("ci", 16, 4, 8, 2, 1, 4, 0)).isLeft)
      assert(validate(Profile("ci", 16, 4, 8, 2, 1, 4, -1)).isLeft)
      assert(validate(Profile("ci", 16, 9, 8, 4, 1, 2, 30)).isLeft)
    }

    test("local profile derives admitted children from host memory and processors") {
      assert(localProfile(memoryGiB = 32, availableProcessors = 8) ==
        Right(Profile("local", 32, 8, 8, 2, 2, 4, 30)))
      assert(localProfile(memoryGiB = 16, availableProcessors = 8) ==
        Right(Profile("local", 16, 4, 8, 1, 1, 4, 30)))
      assert(localProfile(memoryGiB = 8, availableProcessors = 8).isLeft)
      assert(localProfile(memoryGiB = 32, availableProcessors = 0).isLeft)
    }

    test("evaluation settings require at least one trial") {
      assert(defaultEvaluationSettings == EvaluationSettings(3, 0L))
      assert(validate(defaultEvaluationSettings) == Right(defaultEvaluationSettings))
      assert(validate(EvaluationSettings(0, 0L)).isLeft)
      assert(validate(EvaluationSettings(-1, 0L)).isLeft)
    }

    test("strategy orders rotate canonically and reproducibly") {
      val input    = Seq(Strategy.Recycled, Strategy.LongLived, Strategy.Fresh, Strategy.LongLived)
      val settings = EvaluationSettings(3, 0L)
      val expected = Seq(
        Seq(Strategy.LongLived, Strategy.Fresh, Strategy.Recycled),
        Seq(Strategy.Fresh, Strategy.Recycled, Strategy.LongLived),
        Seq(Strategy.Recycled, Strategy.LongLived, Strategy.Fresh)
      )

      assert(strategyOrders(input, settings) == expected)
      assert(strategyOrders(input.reverse, settings) == expected)
      assert(strategyOrders(input, settings) == strategyOrders(input, settings))
      assert(strategyOrders(Seq.empty, settings).isEmpty)
      assert(strategyOrders(input, EvaluationSettings(0, 0L)).isEmpty)
    }

    test("strategy order rotation supports negative seeds") {
      val expected = Seq(Seq(Strategy.Recycled, Strategy.LongLived, Strategy.Fresh))
      assert(strategyOrders(Strategy.values.toSeq, EvaluationSettings(1, -1L)) == expected)
    }

    test("strategy order rotation does not overflow at the maximum seed") {
      val orders = strategyOrders(Strategy.values.toSeq, EvaluationSettings(3, Long.MaxValue))
      assert(orders.map(_.head) == Seq(Strategy.Fresh, Strategy.Recycled, Strategy.LongLived))
      assert(orders.map(_.head).distinct.size == 3)
    }

    test("plan normalizes and deterministically batches targets") {
      val targets  = Seq("e", "c", "a", "b", "a", "d")
      val expected = WorkPlan(
        lanes = 2,
        batches = Seq(
          Batch(lane = 0, index = 0, targets = Seq("a")),
          Batch(lane = 0, index = 2, targets = Seq("c")),
          Batch(lane = 0, index = 4, targets = Seq("e")),
          Batch(lane = 1, index = 1, targets = Seq("b")),
          Batch(lane = 1, index = 3, targets = Seq("d"))
        )
      )

      assert(plan(targets, lanes = 2, batchSize = 1) == Right(expected))
      assert(plan(targets.reverse, lanes = 2, batchSize = 1) == Right(expected))
      assert(expected.batches.flatMap(_.targets) == Seq("a", "c", "e", "b", "d"))
    }

    test("plan deduplicates and sorts targets deterministically") {
      val targets = Seq("c", "a", "b", "a", "d")
      val forward = plan(targets, lanes = 2, batchSize = 2)
      val reverse = plan(targets.reverse, lanes = 2, batchSize = 2)

      assert(forward == reverse)
      val flattened = forward.toOption.toSeq.flatMap(_.batches).flatMap(_.targets)
      assert(flattened == Seq("a", "b", "c", "d"))
      assert(flattened.distinct.size == 4)
    }

    test("plan rejects invalid inputs") {
      assert(plan(Seq.empty, lanes = 1, batchSize = 1).isLeft)
      assert(plan(Seq("a"), lanes = 0, batchSize = 1).isLeft)
      assert(plan(Seq("a"), lanes = 1, batchSize = 0).isLeft)
    }

    test("strategy plans use the required worker lifetimes") {
      val targets = Seq("a", "b", "c", "d", "e")
      val profile = ciProfile

      val longLived = strategyPlan(Strategy.LongLived, targets, profile).toOption.get
      val fresh     = strategyPlan(Strategy.Fresh, targets, profile).toOption.get
      val recycled  = strategyPlan(Strategy.Recycled, targets, profile).toOption.get

      assert(longLived == WorkPlan(1, Seq(Batch(0, 0, targets))))
      assert(fresh.batches.forall(_.targets.size == 1))
      assert(fresh.batches.map(_.lane).distinct.size == profile.maxChildren)
      assert(recycled.batches.forall(_.targets.size <= profile.batchSize))
      assert(recycled.batches.flatMap(_.targets).sorted == targets)
    }

    test("preparation lanes preserve strategy worker lifetimes and lane order") {
      val targets = Seq("a", "b", "c", "d", "e")

      val longLived = preparationLanes(strategyPlan(Strategy.LongLived, targets, ciProfile).toOption.get)
      val fresh     = preparationLanes(strategyPlan(Strategy.Fresh, targets, ciProfile).toOption.get)
      val recycled  = preparationLanes(strategyPlan(Strategy.Recycled, targets, ciProfile).toOption.get)

      assert(longLived == Seq(PreparationLane(0, Seq(Batch(0, 0, targets)))))
      assert(fresh == Seq(PreparationLane(
        0,
        targets.zipWithIndex.map { case (target, index) =>
          Batch(0, index, Seq(target))
        }
      )))
      assert(recycled == Seq(PreparationLane(0, Seq(Batch(0, 0, targets.take(4)), Batch(0, 1, targets.drop(4))))))

      val twoLaneFresh = preparationLanes(plan(targets, lanes = 2, batchSize = 1).toOption.get)
      assert(twoLaneFresh.map(_.lane) == Seq(0, 1))
      assert(twoLaneFresh.flatMap(_.batches).map(_.index) == Seq(0, 2, 4, 1, 3))
    }

    test("child Mill arguments apply the profile job limit") {
      assert(
        millChildArguments(ciProfile, Seq("ci.linkerBenchmarkInventory")) ==
          Seq("--ticker", "false", "--no-daemon", "-j", "2", "ci.linkerBenchmarkInventory")
      )
    }

    test("preparation decisions skip unsafe measurements and honor continuation") {
      assert(
        preparationDecision(Seq(Outcome.Succeeded, Outcome.Succeeded), continueOnFailure = false) ==
          PreparationDecision(measure = true, Outcome.Succeeded, failRun = false)
      )
      assert(
        preparationDecision(Seq(Outcome.Succeeded, Outcome.Failed), continueOnFailure = false) ==
          PreparationDecision(measure = false, Outcome.Failed, failRun = true)
      )
      assert(
        preparationDecision(Seq(Outcome.Succeeded, Outcome.TimedOut), continueOnFailure = true) ==
          PreparationDecision(measure = false, Outcome.TimedOut, failRun = false)
      )
      assert(preparationDecision(Seq.empty, continueOnFailure = true).measure == false)
    }

    test("concurrent lane failure interrupts and retires sibling work") {
      val siblingStarted = CountDownLatch(1)
      val siblingRetired = CountDownLatch(1)
      val siblingBlock   = CountDownLatch(1)
      val failure        = assertThrows[IllegalStateException] {
        runConcurrentLanes(
          parallelism = 2,
          tasks = Seq(
            () => {
              assert(siblingStarted.await(5L, TimeUnit.SECONDS))
              throw IllegalStateException("lane failed")
            },
            () => {
              siblingStarted.countDown()
              try siblingBlock.await()
              finally siblingRetired.countDown()
              1
            }
          ),
          cleanupTimeout = 5.seconds
        )
      }

      assert(failure.getMessage == "lane failed")
      assert(siblingRetired.await(1L, TimeUnit.SECONDS))
    }

    test("atomic text failure removes its temporary file") {
      val directory = os.temp.dir(prefix = "linker-benchmark-atomic-")
      val target    = directory / "result.json"
      val failingMove: (java.nio.file.Path, java.nio.file.Path) => Unit =
        (_: java.nio.file.Path, _: java.nio.file.Path) => throw IllegalStateException("move failed")
      try {
        val failure = assertThrows[IllegalStateException] {
          writeTextAtomically(target, "payload", failingMove)
        }

        assert(failure.getMessage == "move failed")
        assert(!os.exists(target))
        assert(os.list(directory).isEmpty)
      } finally os.remove.all(directory)
    }

    test("model values have uPickle read writers") {
      def roundTrip[A: ReadWriter](value: A): A = read[A](write(value))

      assert(roundTrip(Platform.ScalaJs) == Platform.ScalaJs)
      assert(roundTrip(Strategy.Recycled) == Strategy.Recycled)
      assert(roundTrip(ciProfile) == ciProfile)
      assert(roundTrip(Batch(1, 2, Seq("target"))) == Batch(1, 2, Seq("target")))
      val workPlan = WorkPlan(2, Seq(Batch(0, 0, Seq("target"))))
      assert(roundTrip(workPlan) == workPlan)
      assert(roundTrip(defaultEvaluationSettings) == defaultEvaluationSettings)
    }

    test("outcomes round trip through uPickle") {
      Outcome.values.foreach(outcome => assert(read[Outcome](write(outcome)) == outcome))
    }

    test("JVM snapshot deltas use phase completion values and nonnegative GC changes") {
      val before = JvmSnapshot(heapUsedBytes = 100L, peakHeapBytes = 200L, gcCount = 12L, gcMillis = 40L)
      val after  = JvmSnapshot(heapUsedBytes = 125L, peakHeapBytes = 260L, gcCount = 10L, gcMillis = 55L)

      val metrics = JvmSnapshot.phaseDelta("cold-link", 900L, before, after, evaluated = 8L, cached = 2L)

      assert(
        metrics == PhaseMetrics(
          name = "cold-link",
          wallMillis = 900L,
          heapUsedBytes = 125L,
          peakHeapBytes = 260L,
          gcCount = 0L,
          gcMillis = 15L,
          evaluated = 8L,
          cached = 2L
        )
      )
    }

    test("JVM snapshots capture live nonnegative metrics") {
      JvmSnapshot.resetPeakUsage()
      val snapshot = JvmSnapshot.capture()
      assert(snapshot.heapUsedBytes >= 0L)
      assert(snapshot.peakHeapBytes >= 0L)
      assert(snapshot.gcCount >= 0L)
      assert(snapshot.gcMillis >= 0L)
    }

    test("JVM metric folding sums pools, ignores unsupported values, and saturates") {
      assert(JvmSnapshot.sumNonnegative(Seq(10L, 20L, -1L)) == 30L)
      assert(JvmSnapshot.sumNonnegative(Seq(Long.MaxValue - 5L, 10L)) == Long.MaxValue)
      assert(JvmSnapshot.sumNonnegative(Seq(-1L, -20L)) == 0L)
    }

    test("worker and benchmark records round trip through uPickle") {
      val phase  = PhaseMetrics("cold-link", 700L, 256L, 512L, 2L, 20L, 6L, 1L)
      val worker = WorkerRecord(
        runId = "run-1",
        trial = 1,
        strategyPosition = 2,
        settings = defaultEvaluationSettings,
        platform = Platform.ScalaJs,
        strategy = Strategy.Recycled,
        profile = ciProfile,
        lane = 0,
        batch = 3,
        targets = Seq("morphir.foo.js.test.fastLinkJS"),
        startupMillis = 100L,
        peakRssKiB = 2048L,
        phases = Seq(phase),
        outcome = Outcome.Succeeded,
        detail = Some("complete")
      )
      val strategyTrial = StrategyTrialRecord(
        "run-1",
        1,
        2,
        Platform.ScalaJs,
        Strategy.Recycled,
        ciProfile,
        900L,
        4096L,
        1L,
        Outcome.Succeeded
      )
      val result = BenchmarkResult(defaultResolvedConfiguration, 8192L, Seq(strategyTrial), Seq(worker))

      assert(read[WorkerRecord](write(worker)) == worker)
      assert(read[BenchmarkResult](write(result)) == result)
      assert(write(worker).contains("512"))
      val resultFields = ujson.read(write(result)).obj.keySet
      assert(resultFields.contains("configuration"))
      assert(!resultFields.contains("settings"))
    }

    test("aggregation summarizes successful strategy elapsed time") {
      val cases   = Seq(strategyTrial(1, 1400L), strategyTrial(2, 900L), strategyTrial(3, 1000L))
      val summary = aggregate(cases, cases.map(value => worker(value.trial))).head

      assert(summary.wallMillis.contains(LongSummary(1000L, 900L, 1400L)))
      assert(summary.peakAggregateRssKiB.contains(LongSummary(4096L, 4096L, 4096L)))
      assert(summary.succeeded == 3)
      assert(summary.failed == 0)
    }

    test("aggregation keeps distinct profiles that share a name") {
      val otherProfile = ciProfile.copy(heapGiB = 4, maxChildren = 2)
      val cases        = Seq(
        strategyTrial(1, 900L),
        strategyTrial(2, 1000L, profile = otherProfile)
      )
      val records = Seq(
        worker(1),
        worker(2, profile = otherProfile)
      )
      val summaries = aggregate(cases, records)

      assert(summaries.size == 2)
      assert(summaries.map(_.profile).toSet == Set(ciProfile, otherProfile))
    }

    test("aggregation collapses workers per trial before summarizing") {
      val cases   = Seq(strategyTrial(1, 1000L), strategyTrial(2, 2000L))
      val records = Seq(
        worker(1, lane = 0, startupMillis = 100L, peakRssKiB = 1000L, peakHeapBytes = 300L, gcMillis = 10L),
        worker(1, lane = 1, startupMillis = 200L, peakRssKiB = 1500L, peakHeapBytes = 400L, gcMillis = 20L),
        worker(2, lane = 0, startupMillis = 500L, peakRssKiB = 2500L, peakHeapBytes = 800L, gcMillis = 100L)
      )
      val summary = aggregate(cases, records).head

      assert(summary.startupShare.exists(value => math.abs(value.min - 3.0 / 17.0) < 0.0000001))
      assert(summary.startupShare.exists(value => math.abs(value.max - 5.0 / 12.0) < 0.0000001))
      assert(summary.throughputTargetsPerMinute.exists(value => value.min == 120.0 && value.max == 240.0))
      assert(summary.peakChildRssKiB.contains(LongSummary(2000L, 1500L, 2500L)))
      assert(summary.peakHeapBytes.contains(LongSummary(600L, 400L, 800L)))
      assert(summary.gcShare.exists(value => math.abs(value.min - 3.0 / 140.0) < 0.0000001))
      assert(summary.gcShare.exists(value => math.abs(value.max - 1.0 / 7.0) < 0.0000001))
    }

    test("aggregation excludes workers from another strategy position") {
      val caseRecord    = strategyTrial(1, 1000L).copy(strategyPosition = 1)
      val wrongPosition = worker(1, startupMillis = 500L).copy(strategyPosition = 2)
      val summary       = aggregate(Seq(caseRecord), Seq(wrongPosition)).head

      assert(summary.wallMillis.nonEmpty)
      assert(summary.startupShare.isEmpty)
      assert(summary.peakChildRssKiB.isEmpty)
    }

    test("aggregation and rendering collapse identical duplicate identities") {
      val caseRecord   = strategyTrial(1, 1000L)
      val workerRecord = worker(1)
      val result       = BenchmarkResult(
        defaultResolvedConfiguration,
        8192L,
        Seq(caseRecord, caseRecord),
        Seq(workerRecord, workerRecord)
      )
      val summary = aggregate(result.cases, result.records).head
      val report  = renderMarkdown(result)

      assert(summary.succeeded == 1)
      assert(report.linesIterator.count(_.startsWith("| 1 |")) == 2)
    }

    test("aggregation rejects conflicting duplicate case identities") {
      val caseRecord = strategyTrial(1, 1000L)
      val error      = assertThrows[IllegalArgumentException] {
        aggregate(Seq(caseRecord, caseRecord.copy(wallMillis = 1200L)), Seq(worker(1)))
      }

      assert(error.getMessage.contains("strategy trial identity"))
    }

    test("aggregation rejects conflicting duplicate worker identities") {
      val workerRecord = worker(1)
      val error        = assertThrows[IllegalArgumentException] {
        aggregate(Seq(strategyTrial(1, 1000L)), Seq(workerRecord, workerRecord.copy(startupMillis = 200L)))
      }

      assert(error.getMessage.contains("worker identity"))
    }

    test("even medians do not overflow") {
      val cases = Seq(
        strategyTrial(1, Long.MaxValue - 1L, peakAggregateRssKiB = Long.MaxValue - 1L),
        strategyTrial(2, Long.MaxValue, peakAggregateRssKiB = Long.MaxValue)
      )
      val summary = aggregate(cases, Seq(worker(1), worker(2))).head

      assert(summary.wallMillis.exists(_.median == Long.MaxValue - 1L))
      assert(summary.peakAggregateRssKiB.exists(_.median == Long.MaxValue - 1L))
    }

    test("failed-only groups retain outcomes without numeric summaries") {
      val summary = aggregate(
        Seq(strategyTrial(1, 900L, Outcome.Failed), strategyTrial(2, 1000L, Outcome.TimedOut)),
        Seq(worker(1, outcome = Outcome.Failed), worker(2, outcome = Outcome.TimedOut))
      ).head

      assert(summary.failed == 1)
      assert(summary.timedOut == 1)
      assert(summary.wallMillis.isEmpty)
      assert(summary.peakAggregateRssKiB.isEmpty)
      assert(summary.startupShare.isEmpty)
    }

    test("worker-time shares are unavailable for nonpositive denominators") {
      val zeroWorker = worker(1, startupMillis = 0L).copy(
        phases = Seq(PhaseMetrics("zero", 0L, 0L, 0L, 0L, 10L, 0L, 0L))
      )
      val summary = aggregate(Seq(strategyTrial(1, 1000L)), Seq(zeroWorker)).head

      assert(summary.startupShare.isEmpty)
      assert(summary.gcShare.isEmpty)
      assert(summary.peakChildRssKiB.nonEmpty)
    }

    test("double summaries ignore non-finite values and average without overflow") {
      assert(summarizeDoubles(Seq(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity)).isEmpty)
      val summary = summarizeDoubles(Seq(Double.MaxValue, Double.MaxValue)).get
      assert(summary.median == Double.MaxValue)
      assert(summary.min == Double.MaxValue)
      assert(summary.max == Double.MaxValue)
    }

    test("redaction removes home identities and secret assignment values") {
      val redacted = redact(
        "mac=/Users/example/project linux=/home/example/project SECRET_TOKEN=needle PASSWORD=hunter2 api_token=lower Mixed_Secret=mixed"
      )

      assert(!redacted.contains("example"))
      assert(!redacted.contains("needle"))
      assert(!redacted.contains("hunter2"))
      assert(!redacted.contains("lower"))
      assert(!redacted.contains("mixed"))
      assert(redacted.contains("SECRET_TOKEN"))
    }

    test("redaction removes bare environment secrets from an explicit map") {
      val redacted = redact(
        "long=abcdef short=abc ordinary=visible",
        Map(
          "DB_SECRET"      -> "abcdef",
          "api_token"      -> "abc",
          "ordinary"       -> "visible",
          "EMPTY_PASSWORD" -> ""
        )
      )

      assert(!redacted.contains("abcdef"))
      assert(!redacted.contains("abc"))
      assert(redacted == "long=<redacted> short=<redacted> ordinary=visible")
    }

    test("markdown reports are redacted and contain raw and aggregate metrics") {
      val unsafeProfile = ciProfile.copy(name = "/Users/report-user SECRET_TOKEN=profile-secret")
      val caseRecord    = strategyTrial(1, 900L, profile = unsafeProfile)
      val record        = worker(
        1,
        detail = Some("SECRET_TOKEN=report-secret /Users/report-user/private"),
        profile = unsafeProfile
      ).copy(targets = Seq("/home/report-user/SECRET_TOKEN=target-secret"))
      val preparation = PreparationRecord(
        "run-1",
        1,
        Platform.ScalaJs,
        Strategy.Recycled,
        unsafeProfile,
        0,
        450L,
        Seq("target-0"),
        Outcome.Succeeded
      )
      val report = renderMarkdown(
        BenchmarkResult(defaultResolvedConfiguration, 8192L, Seq(caseRecord), Seq(record), Seq(preparation))
      )

      assert(report.contains("Preset: js-strategies"))
      assert(report.contains("Profile: ci; memory: 16 GiB; reserve: 4 GiB; heap: 8 GiB"))
      assert(report.contains("Platforms: scala-js"))
      assert(report.contains("Strategies: long-lived, fresh, recycled"))
      assert(report.contains("Trials: 3; order seed: 0"))
      assert(report.contains("Target filter: none; target limit: none"))
      assert(report.contains("Continue on failure: true"))
      assert(report.contains("recycled"))
      assert(report.contains("Whole-run peak aggregate RSS: 8192 KiB"))
      assert(report.contains("## Strategy trials"))
      assert(report.contains("## Preparation"))
      assert(report.contains("Worker phase wall"))
      assert(report.contains("Successful wall ms"))
      assert(report.contains("Targets/min"))
      assert(report.contains("Worker-time startup share"))
      assert(report.contains("Worker-time GC share"))
      assert(!report.contains("profile-secret"))
      assert(!report.contains("report-user"))
      assert(report.endsWith("\n"))
    }

    test("worker runtime metadata round trips") {
      val runtime = RuntimeMetadata("21", "1.2.0", "Mac OS X", "aarch64", 8, 8589934592L, "lane-id")
      val record  = worker(1).copy(runtime = runtime)

      assert(read[WorkerRecord](write(record)).runtime == runtime)
    }

    test("sanitized JSON redacts every free-text field") {
      val environment   = Map("API_TOKEN" -> "bare-environment-secret")
      val unsafeProfile = ciProfile.copy(
        name = "profile /Users/profile-user SECRET_TOKEN=profile-secret"
      )
      val caseRecord = strategyTrial(1, 900L, profile = unsafeProfile).copy(
        runId = "/home/run-user/bare-environment-secret"
      )
      val record = worker(
        1,
        detail = Some("Mixed_Password=detail-secret"),
        profile = unsafeProfile
      ).copy(
        runId = caseRecord.runId,
        targets = Seq("/home/target-user API_KEY=target-secret"),
        phases = Seq(PhaseMetrics("/Users/phase-user api_token=phase-secret", 1L, 2L, 3L, 4L, 5L, 6L, 7L))
      )
      val json = renderJson(
        BenchmarkResult(defaultResolvedConfiguration, 8192L, Seq(caseRecord), Seq(record)),
        environment
      )

      Seq(
        "profile-user",
        "profile-secret",
        "run-user",
        "bare-environment-secret",
        "target-user",
        "target-secret",
        "phase-user",
        "phase-secret",
        "detail-secret"
      ).foreach(secret => assert(!json.contains(secret)))
      assert(read[BenchmarkResult](json).records.head.detail.exists(_.contains("<redacted>")))
    }

    test("markdown escapes profile table delimiters and line breaks") {
      val unsafeProfile = ciProfile.copy(name = "profile|row\nbreak\\slash /Users/table-user")
      val caseRecord    = strategyTrial(1, 900L, profile = unsafeProfile)
      val record        = worker(1, profile = unsafeProfile)
      val report  = renderMarkdown(BenchmarkResult(defaultResolvedConfiguration, 8192L, Seq(caseRecord), Seq(record)))
      val rawRows = report.linesIterator.filter(_.startsWith("| 1 |")).toSeq

      assert(rawRows.size == 2)
      assert(rawRows.forall(_.contains("profile\\|row break\\\\slash")))
      assert(!report.contains("table-user"))
    }

    test("markdown raw rows use deterministic benchmark order") {
      val first  = worker(2, lane = 1, strategy = Strategy.Fresh).copy(strategyPosition = 1, batch = 2)
      val second = worker(1, lane = 1, strategy = Strategy.Recycled).copy(strategyPosition = 2, batch = 1)
      val third  = worker(1, lane = 0, strategy = Strategy.LongLived).copy(strategyPosition = 0, batch = 3)
      val result = BenchmarkResult(
        defaultResolvedConfiguration,
        8192L,
        Seq(
          strategyTrial(2, 900L, strategy = Strategy.Fresh).copy(strategyPosition = 1),
          strategyTrial(1, 900L, strategy = Strategy.Recycled).copy(strategyPosition = 2),
          strategyTrial(1, 900L, strategy = Strategy.LongLived).copy(strategyPosition = 0)
        ),
        Seq(first, second, third)
      )
      val report = renderMarkdown(result)

      assert(report.indexOf("| 1 | 0 |") < report.indexOf("| 1 | 2 |"))
      assert(report.indexOf("| 1 | 2 |") < report.indexOf("| 2 | 1 |"))
      assert(renderMarkdown(result.copy(records = result.records.reverse)) == report)
    }
  }
}
