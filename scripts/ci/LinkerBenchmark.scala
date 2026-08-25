//| moduleDeps: [//millbuildHelpers]

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.concurrent.ConcurrentHashMap

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import millbuild.{LinkerBenchmark, LinkerBenchmarkProcess}
import millbuild.LinkerBenchmark.*
import upickle.default.{read, write}

def main(
    preset: String = "",
    profile: String = "",
    strategies: String = "",
    platforms: String = "",
    trials: String = "0",
    orderSeed: String = "",
    continueOnFailure: String = "preset",
    targetFilter: String = "",
    targetLimit: String = "0",
    memoryGib: String = "0",
    reserveGib: String = "0",
    millJobs: String = "0",
    maxChildren: String = "0",
    batchSize: String = "0",
    timeoutMinutes: String = "0",
    output: String = ".dev/.sdlc/mill-jvm-worker-pool/out/run",
    smoke: String = "false",
    recoverySmoke: String = "false",
    planOnly: String = "false",
    artifactNameOnly: String = "false",
    artifactRef: String = "",
    artifactRunId: String = "",
    artifactRunAttempt: String = "0"
): Unit = {
  val smokeEnabled         = requiredBoolean(smoke, "smoke")
  val recoverySmokeEnabled = requiredBoolean(recoverySmoke, "recovery smoke")
  val planOnlyEnabled      = requiredBoolean(planOnly, "plan only")
  val artifactNameEnabled  = requiredBoolean(artifactNameOnly, "artifact name only")
  val parsedTrials         = requiredPositive(trials, "trials")
  val parsedTargetLimit    = requiredPositive(targetLimit, "target limit")
  val parsedMemoryGiB      = requiredPositive(memoryGib, "memory GiB")
  val parsedReserveGiB     = requiredPositive(reserveGib, "reserve GiB")
  val parsedMillJobs       = requiredPositive(millJobs, "Mill jobs")
  val parsedMaxChildren    = requiredPositive(maxChildren, "max children")
  val parsedBatchSize      = requiredPositive(batchSize, "batch size")
  val parsedTimeout        = requiredPositive(timeoutMinutes, "timeout minutes")
  val parsedRunAttempt     = requiredPositive(artifactRunAttempt, "artifact run attempt")
  val specialModes = Seq(smokeEnabled, recoverySmokeEnabled, planOnlyEnabled, artifactNameEnabled).count(identity)
  if specialModes > 1 then
    throw new IllegalArgumentException("smoke, recovery-smoke, plan-only, and artifact-name-only are exclusive")
  if artifactNameEnabled then
    println(
      LinkerBenchmark
        .hostedArtifactName(
          preset,
          artifactRef,
          artifactRunId,
          parsedRunAttempt.getOrElse(
            throw new IllegalArgumentException("artifact run attempt must be greater than zero")
          )
        )
        .fold(message => throw new IllegalArgumentException(message), identity)
    )
  else if smokeEnabled then LinkerBenchmarkSmoke.run()
  else {
    val localProfile =
      if preset.trim.isEmpty then
        LinkerBenchmark.detectedLocalProfile().fold(message => throw new IllegalArgumentException(message), identity)
      else LinkerBenchmark.ciProfile
    val overrides = BenchmarkOverrides(
      platforms = requiredOptional(platforms, "platforms"),
      strategies = requiredOptional(strategies, "strategies"),
      trials = parsedTrials,
      orderSeed = LinkerBenchmark
        .parseOptionalLong(orderSeed, "order seed")
        .fold(message => throw new IllegalArgumentException(message), identity),
      targetFilter = requiredOptional(targetFilter, "target filter"),
      targetLimit = parsedTargetLimit,
      memoryGiB = parsedMemoryGiB,
      reserveGiB = parsedReserveGiB,
      millJobs = parsedMillJobs,
      maxChildren = parsedMaxChildren,
      batchSize = parsedBatchSize,
      timeoutMinutes = parsedTimeout,
      continueOnFailure = LinkerBenchmark
        .parseContinuationChoice(continueOnFailure)
        .fold(message => throw new IllegalArgumentException(message), identity)
    )
    val configuration = LinkerBenchmark
      .resolveBenchmarkConfiguration(preset, profile, localProfile, overrides)
      .fold(message => throw new IllegalArgumentException(message), identity)
    LinkerBenchmarkOrchestrator.run(
      configuration,
      output,
      recoverySmokeEnabled,
      planOnlyEnabled
    )
  }
}

private def requiredOptional(value: String, field: String): Option[String] =
  LinkerBenchmark
    .parseOptionalTrimmedString(value, field)
    .fold(message => throw new IllegalArgumentException(message), identity)

private def requiredPositive(value: String, field: String): Option[Int] =
  LinkerBenchmark
    .parseZeroAsUnsetPositiveInt(value, field)
    .fold(message => throw new IllegalArgumentException(message), identity)

private def requiredBoolean(value: String, field: String): Boolean =
  LinkerBenchmark
    .parseBoolean(value, field)
    .fold(message => throw new IllegalArgumentException(message), identity)

private object LinkerBenchmarkOrchestrator {
  private val OutputIdentityEnv = "MORPHIR_LINKER_BENCHMARK_OUTPUT_IDENTITY"

  def run(
      configuration: ResolvedBenchmarkConfiguration,
      output: String,
      recoverySmoke: Boolean,
      planOnly: Boolean
  ): Unit = {
    val workspace = LinkerBenchmark
      .resolveSmokeWorkspace(sys.env, os.pwd)
      .fold(message => throw new Exception(message), identity)
    val profile    = configuration.profile
    val settings   = configuration.settings
    val strategies = configuration.strategies
    val platforms  = configuration.platforms
    val orders     = LinkerBenchmark.strategyOrders(strategies, settings)
    val outputRoot = LinkerBenchmark
      .benchmarkOutputRoot(os.Path(output, workspace), configuration, recoverySmoke, planOnly)
      .fold(message => throw new Exception(message), identity)
    os.makeDir.all(outputRoot)
    val renderedConfiguration = LinkerBenchmark.renderConfigurationJson(configuration)
    writeAtomically(outputRoot / "configuration.json", renderedConfiguration)
    println(renderedConfiguration)
    val temporaryBase = LinkerBenchmark
      .resolveSmokeTemporaryBase(System.getProperty("java.io.tmpdir"), sys.env.get("TMPDIR"), workspace)
      .fold(message => throw new Exception(message), identity)
    val externalRunRoot           = LinkerBenchmark.createSmokeTemporaryRunRoot(temporaryBase)
    var primaryFailure: Throwable = null
    try {
      LinkerBenchmark
        .validateExternalSmokeCleanupRoot(workspace, temporaryBase, externalRunRoot)
        .fold(message => throw new Exception(message), _ => ())
      val inventory = discoverInventory(workspace, externalRunRoot, outputRoot, profile, platforms)
      LinkerBenchmark
        .validateConfiguredHeap(profile, inventory.effectiveMaxHeapBytes)
        .fold(message => throw new Exception(s"linker benchmark admission failed: $message"), _ => ())
      val selected = inventory.inventories.map { entry =>
        val targets = LinkerBenchmark
          .selectInventoryTargets(
            entry.targets,
            configuration.targetFilter,
            configuration.targetLimit,
            entry.platform
          )
          .fold(message => throw new Exception(message), identity)
        entry.copy(targets = targets)
      }
      val plans = selected.flatMap { entry =>
        strategies.map(strategy =>
          (entry.platform, strategy) ->
            LinkerBenchmark
              .strategyPlan(strategy, entry.targets, profile)
              .fold(message => throw new Exception(message), identity)
        )
      }.toMap

      if planOnly then println(renderPlans(configuration, orders, selected, plans))
      else if recoverySmoke then runRecoverySmoke(workspace, externalRunRoot, outputRoot, profile)
      else {
        runEvaluation(
          workspace,
          externalRunRoot,
          outputRoot,
          configuration,
          orders,
          selected,
          plans
        )
      }
    } catch {
      case error: Throwable =>
        primaryFailure = error
        throw error
    } finally
      try clearExternalRunDirectory(workspace, temporaryBase, externalRunRoot)
      catch {
        case NonFatal(cleanupError) if primaryFailure != null => primaryFailure.addSuppressed(cleanupError)
      }
  }

  private def discoverInventory(
      workspace: os.Path,
      externalRoot: os.Path,
      outputRoot: os.Path,
      profile: Profile,
      platforms: Seq[Platform]
  ): InventoryRecord = {
    val directory = outputRoot / "inventory"
    val record    = directory / "inventory.json"
    val started   = directory / "started"
    os.makeDir.all(directory)
    val lane = externalRoot / "inventory"
    os.makeDir.all(lane)
    val result = runMill(
      profile,
      workspace,
      lane,
      started,
      directory / "stdout.log",
      directory / "stderr.log",
      10.minutes,
      Seq(
        "ci.linkerBenchmarkInventory",
        "--record",
        physicalPath(record).toString,
        "--platforms",
        platforms.map(platformArgument).mkString(",")
      )
    )
    if result.outcome != Outcome.Succeeded then
      throw new Exception(s"linker inventory failed: ${result.detail}; see ${directory / "stderr.log"}")
    read[InventoryRecord](os.read(record))
  }

  private def runEvaluation(
      workspace: os.Path,
      externalRoot: os.Path,
      outputRoot: os.Path,
      configuration: ResolvedBenchmarkConfiguration,
      orders: Seq[Seq[Strategy]],
      inventory: Seq[PlatformInventory],
      plans: Map[(Platform, Strategy), WorkPlan]
  ): Unit = {
    val profile      = configuration.profile
    val settings     = configuration.settings
    val runId        = s"run-${System.currentTimeMillis()}-${ProcessHandle.current().pid()}"
    val preparations = prepareLanes(workspace, externalRoot, outputRoot, runId, profile, settings, inventory, plans)

    def preparationDecisionFor(trial: Int, platform: Platform, strategy: Strategy): PreparationDecision =
      LinkerBenchmark.preparationDecision(
        preparations
          .filter(record => record.trial == trial && record.platform == platform && record.strategy == strategy)
          .map(_.outcome),
        configuration.continueOnFailure
      )

    def skippedCase(
        trial: Int,
        position: Int,
        platform: Platform,
        strategy: Strategy,
        outcome: Outcome
    ): StrategyTrialRecord =
      StrategyTrialRecord(runId, trial, position, platform, strategy, profile, 0L, 0L, 0L, outcome)

    val skippedCases = orders.zipWithIndex.flatMap { case (order, trial) =>
      inventory.flatMap { entry =>
        order.zipWithIndex.flatMap { case (strategy, position) =>
          val decision = preparationDecisionFor(trial, entry.platform, strategy)
          Option.when(!decision.measure)(skippedCase(trial, position, entry.platform, strategy, decision.outcome))
        }
      }
    }
    val mustFailForPreparation = orders.zipWithIndex.exists { case (order, trial) =>
      inventory.exists(entry =>
        order.exists(strategy => preparationDecisionFor(trial, entry.platform, strategy).failRun)
      )
    }
    if mustFailForPreparation then {
      val result = BenchmarkResult(configuration, 0L, skippedCases, Seq.empty, preparations)
      writeAtomically(outputRoot / "results.json", LinkerBenchmark.renderJson(result))
      writeAtomically(outputRoot / "summary.md", LinkerBenchmark.renderMarkdown(result))
      throw new Exception("one or more linker benchmark preparations failed; reports were written")
    }
    val activeRoots = ConcurrentHashMap.newKeySet[ProcessHandle]()
    val sampler     = LinkerBenchmarkProcess.AggregateRssSampler.start(
      ProcessHandle.current().pid(),
      () => activeRoots.asScala
    )
    val records             = Seq.newBuilder[WorkerRecord]
    val cases               = Seq.newBuilder[StrategyTrialRecord]
    val inventoryByPlatform = inventory.map(entry => entry.platform -> entry).toMap
    val schedule            = LinkerBenchmark.evaluationSchedule(orders, inventory.map(_.platform))
    try
      LinkerBenchmark.runEvaluationSchedule(schedule, configuration.continueOnFailure) { scheduled =>
        val entry       = inventoryByPlatform(scheduled.platform)
        val preparation = preparationDecisionFor(scheduled.trial, entry.platform, scheduled.strategy)
        val outcome     =
          if !preparation.measure then {
            cases += skippedCase(
              scheduled.trial,
              scheduled.strategyPosition,
              entry.platform,
              scheduled.strategy,
              preparation.outcome
            )
            preparation.outcome
          } else {
            sampler.resetStrategyWindow()
            val startedAt     = System.nanoTime()
            val plan          = plans(entry.platform -> scheduled.strategy)
            val windowRecords = runPlan(
              workspace,
              externalRoot,
              outputRoot,
              runId,
              settings,
              profile,
              entry.platform,
              scheduled.strategy,
              scheduled.trial,
              scheduled.strategyPosition,
              plan
            )
            records ++= windowRecords
            val windowMillis = math.max(0L, (System.nanoTime() - startedAt) / 1000000L)
            val snapshot     = sampler.snapshot()
            val caseOutcome  = combineOutcomes(windowRecords.map(_.outcome))
            val completed    = windowRecords.filter(_.outcome == Outcome.Succeeded).flatMap(_.targets).distinct.size
            cases += StrategyTrialRecord(
              runId,
              scheduled.trial,
              scheduled.strategyPosition,
              entry.platform,
              scheduled.strategy,
              profile,
              windowMillis,
              snapshot.strategyWindowPeakKiB,
              completed,
              caseOutcome
            )
            caseOutcome
          }
        outcome
      }
    finally sampler.stop()
    val result = BenchmarkResult(
      configuration,
      sampler.snapshot().wholeRunPeakKiB,
      cases.result(),
      records.result(),
      preparations
    )
    writeAtomically(outputRoot / "results.json", LinkerBenchmark.renderJson(result))
    writeAtomically(outputRoot / "summary.md", LinkerBenchmark.renderMarkdown(result))
    if LinkerBenchmark.evaluationFailed(result.cases.map(_.outcome)) then
      throw new Exception("one or more linker benchmark cases failed; reports were written")
  }

  private def prepareLanes(
      workspace: os.Path,
      externalRoot: os.Path,
      outputRoot: os.Path,
      runId: String,
      profile: Profile,
      settings: EvaluationSettings,
      inventory: Seq[PlatformInventory],
      plans: Map[(Platform, Strategy), WorkPlan]
  ): Seq[PreparationRecord] = {
    val records = Seq.newBuilder[PreparationRecord]
    (0 until settings.trials).foreach { trial =>
      inventory.foreach { entry =>
        Strategy.values.foreach { strategy =>
          plans.get(entry.platform -> strategy).foreach { plan =>
            val tasks = LinkerBenchmark.preparationLanes(plan).map { preparationLane => () =>
              val laneIndex = preparationLane.lane
              val lane      = externalRoot / profile.name / s"trial-$trial" / strategy.token / entry.platform.token /
                s"lane-$laneIndex"
              val directory = outputRoot / profile.name / s"trial-$trial" / strategy.token /
                entry.platform.token / s"lane-$laneIndex" / "prepare"
              os.makeDir.all(directory)
              val results = preparationLane.batches.map { batch =>
                val batchDirectory = directory / s"batch-${batch.index}"
                os.makeDir.all(batchDirectory)
                runMill(
                  profile,
                  workspace,
                  lane,
                  batchDirectory / "started",
                  batchDirectory / "stdout.log",
                  batchDirectory / "stderr.log",
                  profile.timeoutMinutes.minutes,
                  Seq(
                    "ci.linkerBenchmarkPrepare",
                    "--platform",
                    entry.platform.token,
                    "--targets",
                    batch.targets.mkString(",")
                  )
                )
              }
              PreparationRecord(
                runId,
                trial,
                entry.platform,
                strategy,
                profile,
                laneIndex,
                results.map(_.wallMillis).sum,
                preparationLane.batches.flatMap(_.targets).distinct.sorted,
                combineOutcomes(results.map(_.outcome))
              )
            }
            records ++= LinkerBenchmark.runConcurrentLanes(plan.lanes, tasks, cleanupTimeout = 15.seconds)
          }
        }
      }
    }
    records.result()
  }

  private def runPlan(
      workspace: os.Path,
      externalRoot: os.Path,
      outputRoot: os.Path,
      runId: String,
      settings: EvaluationSettings,
      profile: Profile,
      platform: Platform,
      strategy: Strategy,
      trial: Int,
      position: Int,
      plan: WorkPlan
  ): Seq[WorkerRecord] = {
    val lanes = plan.batches.groupBy(_.lane).toSeq.sortBy(_._1)
    val tasks = lanes.map { case (laneIndex, batches) =>
      () =>
        val lane = externalRoot / profile.name / s"trial-$trial" / strategy.token / platform.token /
          s"lane-$laneIndex"
        os.makeDir.all(lane)
        LinkerBenchmark.runBatchesUntilInterrupted(batches.sortBy(_.index)) { batch =>
          runWorker(
            workspace,
            lane,
            outputRoot,
            runId,
            settings,
            profile,
            platform,
            strategy,
            trial,
            position,
            batch
          )
        }
    }
    LinkerBenchmark
      .runConcurrentLanes(plan.lanes, tasks, cleanupTimeout = 15.seconds)
      .flatten
      .sortBy(record => (record.lane, record.batch))
  }

  private def runWorker(
      workspace: os.Path,
      lane: os.Path,
      outputRoot: os.Path,
      runId: String,
      settings: EvaluationSettings,
      profile: Profile,
      platform: Platform,
      strategy: Strategy,
      trial: Int,
      position: Int,
      batch: Batch
  ): WorkerRecord = {
    def failedWorkerRecord(detail: String, outcome: Outcome = Outcome.Failed): WorkerRecord =
      WorkerRecord(
        runId,
        trial,
        position,
        settings,
        platform,
        strategy,
        profile,
        batch.lane,
        batch.index,
        batch.targets,
        0L,
        0L,
        Seq.empty,
        outcome,
        Some(detail)
      )

    val requestedDirectory = outputRoot / profile.name / s"trial-$trial" / strategy.token / platform.token /
      s"lane-${batch.lane}" / s"batch-${batch.index}"
    val preparedDirectory = LinkerBenchmark.prepareWorkerOutputDirectory(outputRoot, requestedDirectory)

    preparedDirectory.fold(
      detail => failedWorkerRecord(detail),
      directory => {
        val record           = directory / "record.json"
        val started          = directory / "started"
        val gcLog            = directory / "gc.log"
        val expectedIdentity = WorkerIdentity(
          runId,
          settings,
          platform,
          strategy,
          profile,
          trial,
          position,
          batch.lane,
          batch.index,
          batch.targets
        )
        val result = runMill(
          profile,
          workspace,
          lane,
          started,
          directory / "stdout.log",
          directory / "stderr.log",
          profile.timeoutMinutes.minutes,
          LinkerBenchmark.workerCommandArguments(
            physicalPath(record).toString,
            physicalPath(started).toString,
            expectedIdentity
          ),
          Map("JDK_JAVA_OPTIONS" -> s"-Xlog:gc:file=${physicalPath(gcLog)}")
        )
        val loadedRecord =
          if !Files.isRegularFile(record.toNIO, java.nio.file.LinkOption.NOFOLLOW_LINKS) then
            Left("worker record is missing or is not a regular file")
          else
            try LinkerBenchmark.validateWorkerRecordIdentity(read[WorkerRecord](os.read(record)), expectedIdentity)
            catch {
              case NonFatal(_) => Left("worker record is invalid")
            }

        loadedRecord.fold(
          detail =>
            failedWorkerRecord(
              detail,
              if result.outcome == Outcome.Succeeded then Outcome.Failed else result.outcome
            ).copy(
              startupMillis = result.startupMillis.getOrElse(0L),
              peakRssKiB = result.peakRssKiB
            ),
          childRecord =>
            childRecord.copy(
              startupMillis = result.startupMillis.getOrElse(childRecord.startupMillis),
              peakRssKiB = math.max(childRecord.peakRssKiB, result.peakRssKiB),
              outcome = if result.outcome == Outcome.Succeeded then childRecord.outcome else result.outcome,
              detail = childRecord.detail.orElse(Some(result.detail))
            )
        )
      }
    )
  }

  private def runMill(
      profile: Profile,
      workspace: os.Path,
      lane: os.Path,
      started: os.Path,
      stdout: os.Path,
      stderr: os.Path,
      timeout: FiniteDuration,
      arguments: Seq[String],
      additionalEnvironment: Map[String, String] = Map.empty
  ): LinkerBenchmarkProcess.ProcessResult = {
    os.makeDir.all(lane)
    val environment = Map(
      "MILL_OUTPUT_DIR"              -> LinkerBenchmark.smokeChildOutputDirectoryValue(lane),
      "OS_LIB_PATH_RELATIVIZER_BASE" -> "",
      OutputIdentityEnv              -> sha256(physicalPath(lane).toString)
    ) ++ additionalEnvironment
    LinkerBenchmark.validateExternalChildOutputDirectory(workspace, lane).fold(
      message => throw new Exception(message),
      _ => ()
    )
    os.Path.pathSerializer.withValue(PhysicalPathSerializer) {
      LinkerBenchmarkProcess.run(
        physicalPath(workspace / "mill").toString +: LinkerBenchmark.millChildArguments(profile, arguments),
        workspace,
        environment,
        timeout,
        started,
        stdout,
        stderr,
        LinkerBenchmark.smokeEnvironmentRemovals
      )
    }
  }

  private def runRecoverySmoke(
      workspace: os.Path,
      externalRoot: os.Path,
      outputRoot: os.Path,
      profile: Profile
  ): Unit = {
    val directory = outputRoot
    os.makeDir.all(directory)
    val recoveryRecord = directory / "recovery.json"
    if os.exists(recoveryRecord) then os.remove(recoveryRecord)
    val lane  = externalRoot / "recovery"
    val first = runMill(
      profile,
      workspace,
      lane,
      directory / "first.started",
      directory / "first.out",
      directory / "first.err",
      1.millis,
      Seq("ci.linkerBenchmarkWorkerSmoke", "--marker", physicalPath(directory / "first.started").toString)
    )
    val replacement = runMill(
      profile,
      workspace,
      lane,
      directory / "replacement.started",
      directory / "replacement.out",
      directory / "replacement.err",
      5.minutes,
      Seq("ci.linkerBenchmarkWorkerSmoke", "--marker", physicalPath(directory / "replacement.started").toString)
    )
    val record = RecoveryRecord(
      first = RecoveryAttemptRecord(
        first.outcome,
        first.wallMillis,
        first.exitCode,
        readSmokeRecord(directory / "first.started")
      ),
      replacement = RecoveryAttemptRecord(
        replacement.outcome,
        replacement.wallMillis,
        replacement.exitCode,
        readSmokeRecord(directory / "replacement.started")
      )
    )
    LinkerBenchmark.validateRecovery(record).fold(
      message => throw new Exception(s"recovery smoke failed: $message"),
      _ => ()
    )
    val worker          = record.replacement.worker.get
    val proofValidation = verifyRecoveryProof(
      workspace,
      workspace / "out",
      lane,
      sha256(physicalPath(lane).toString),
      worker
    )
    val json = LinkerBenchmark
      .renderValidatedRecoveryJson(record, proofValidation, sys.env)
      .fold(message => throw new Exception(s"recovery smoke failed: $message"), identity)
    writeAtomically(recoveryRecord, json)
  }

  private def readSmokeRecord(marker: os.Path): Option[SmokeRecord] =
    Option.when(os.isFile(marker))(read[SmokeRecord](os.read(marker)))

  private def verifyRecoveryProof(
      workspace: os.Path,
      rootOutput: os.Path,
      lane: os.Path,
      expectedOutputIdentity: String,
      worker: SmokeRecord
  ): Either[String, Path] = {
    val matchingProofs = os.walk(lane).filter(_.last == worker.proofFilename)
    if matchingProofs.size != 1 then Left("replacement recovery worker proof is not unique")
    else
      LinkerBenchmark
        .validateRecoveryProofPath(
          workspace,
          rootOutput,
          lane,
          matchingProofs.head,
          expectedOutputIdentity,
          worker
        )
        .flatMap { canonicalProof =>
          val proof =
            try Right(read[SmokeProof](Files.readString(canonicalProof, StandardCharsets.UTF_8)))
            catch {
              case error: Exception =>
                Left(s"replacement recovery worker proof is invalid: ${error.getClass.getSimpleName}")
            }
          proof.flatMap(value =>
            LinkerBenchmark.validateRecoveryProof(
              workspace,
              rootOutput,
              lane,
              matchingProofs.head,
              expectedOutputIdentity,
              worker,
              value
            )
          )
        }
  }

  private def renderPlans(
      configuration: ResolvedBenchmarkConfiguration,
      orders: Seq[Seq[Strategy]],
      inventory: Seq[PlatformInventory],
      plans: Map[(Platform, Strategy), WorkPlan]
  ): String = {
    val json = ujson.Obj(
      "configuration" -> ujson.read(LinkerBenchmark.renderConfigurationJson(configuration)),
      "trialOrders"   -> ujson.Arr.from(orders.map(order => ujson.Arr.from(order.map(_.token)))),
      "inventories"   -> ujson.Arr.from(inventory.map(entry => ujson.read(write(entry)))),
      "plans"         -> ujson.Arr.from(
        plans.toSeq.sortBy { case ((platform, strategy), _) => (platform.token, strategy.token) }.map {
          case ((platform, strategy), plan) =>
            ujson.Obj(
              "platform" -> platform.token,
              "strategy" -> strategy.token,
              "plan"     -> ujson.read(write(plan))
            )
        }
      )
    )
    json.render(indent = 2)
  }

  private def platformArgument(platform: Platform): String =
    if platform == Platform.ScalaJs then "js" else platform.token

  private def combineOutcomes(outcomes: Seq[Outcome]): Outcome =
    if outcomes.contains(Outcome.Cancelled) then Outcome.Cancelled
    else if outcomes.contains(Outcome.TimedOut) then Outcome.TimedOut
    else if outcomes.contains(Outcome.Failed) then Outcome.Failed
    else Outcome.Succeeded

  private def clearExternalRunDirectory(workspace: os.Path, temporaryBase: os.Path, externalRoot: os.Path): Unit = {
    val validated = LinkerBenchmark
      .validateExternalSmokeCleanupRoot(workspace, temporaryBase, externalRoot)
      .fold(message => throw new Exception(message), identity)
    os.remove.all(os.Path(validated))
  }

  private def physicalPath(path: os.Path): Path =
    LinkerBenchmark.canonicalPhysicalPath(path).fold(message => throw new Exception(message), identity)

  private def sha256(value: String): String =
    MessageDigest.getInstance("SHA-256").digest(value.getBytes(StandardCharsets.UTF_8)).map(byte =>
      f"${byte & 0xff}%02x"
    ).mkString

  private def writeAtomically(path: os.Path, value: String): Unit =
    LinkerBenchmark.writeTextAtomically(path, value)

  private object PhysicalPathSerializer extends os.Path.Serializer {
    override def serializeString(path: os.Path): String     = path.wrapped.toString
    override def serializeFile(path: os.Path): java.io.File = path.wrapped.toFile
    override def serializePath(path: os.Path): Path         = path.wrapped
    override def deserialize(value: String): Path           = java.nio.file.Paths.get(value)
    override def deserialize(value: java.io.File): Path     = value.toPath
    override def deserialize(value: Path): Path             = value
    override def deserialize(value: java.net.URI): Path     = java.nio.file.Paths.get(value)
  }
}

private object LinkerBenchmarkSmoke {
  private val RequestedHeapBytes = 1024L * 1024L * 1024L
  private val OutputIdentityEnv  = "MORPHIR_LINKER_BENCHMARK_OUTPUT_IDENTITY"
  private val CollisionMessage   =
    "benchmark child output directory must differ from the orchestrator output directory"

  def run(): Unit = {
    val workspace = LinkerBenchmark
      .resolveSmokeWorkspace(sys.env, os.pwd)
      .fold(message => throw new Exception(message), identity)
    val mill          = physicalPath(workspace / "mill")
    val smokeBase     = workspace / ".dev" / ".sdlc" / "mill-jvm-worker-pool" / "out" / "smoke"
    val smokeRun      = smokeBase / "run"
    val rootOutput    = workspace / "out"
    val temporaryBase = LinkerBenchmark
      .resolveSmokeTemporaryBase(System.getProperty("java.io.tmpdir"), sys.env.get("TMPDIR"), workspace)
      .fold(message => throw new Exception(message), identity)

    verifyPinnedLauncher(mill, workspace)
    verifyMissingOutputDirectoryProbe(rootOutput)
    clearSmokeRunDirectory(workspace, smokeBase, smokeRun)
    val normalTaskOutputBefore    = fileState(rootOutput / "ci" / "linkerBenchmarkWorkerSmoke.json")
    val externalRunRoot           = LinkerBenchmark.createSmokeTemporaryRunRoot(temporaryBase)
    var primaryFailure: Throwable = null
    try {
      LinkerBenchmark
        .validateExternalSmokeCleanupRoot(workspace, temporaryBase, externalRunRoot)
        .fold(message => throw new Exception(message), _ => ())
      val lane = LinkerBenchmark
        .validateChildOutputDirectory(rootOutput, externalRunRoot / "lane-0")
        .flatMap(_ => LinkerBenchmark.validateExternalChildOutputDirectory(workspace, externalRunRoot / "lane-0"))
        .fold(message => throw new Exception(message), identity)
      os.makeDir.all(lane)
      val outputIdentity = sha256(physicalPath(lane).toString)
      val records        = (0 until 2).map { index =>
        runChild(index, mill, workspace, smokeRun, lane, outputIdentity, rootOutput)
      }

      if records.map(_.childPid).distinct.size != records.size then
        throw new Exception("linker benchmark successor children must have distinct PIDs")
      if records.map(_.proofFilename).distinct.size != records.size then
        throw new Exception("linker benchmark successor children must have distinct proofs")
      if records.map(_.effectiveMaxHeapBytes).distinct.size != 1 then
        throw new Exception("linker benchmark successor children reported different effective heaps")
      if fileState(rootOutput / "ci" / "linkerBenchmarkWorkerSmoke.json") != normalTaskOutputBefore then
        throw new Exception("linker benchmark smoke wrote worker task output below the normal repository out root")

      records.zipWithIndex.foreach { case (record, index) =>
        println(
          s"linker smoke child ${index + 1}/2: pid=${record.childPid} effectiveHeapBytes=${record.effectiveMaxHeapBytes} proof=${record.proofFilename}"
        )
      }
      println("linker smoke: 2/2 sequential workers succeeded with one external lane")
    } catch {
      case error: Throwable =>
        primaryFailure = error
        throw error
    } finally
      try clearExternalRunDirectory(workspace, temporaryBase, externalRunRoot)
      catch {
        case NonFatal(cleanupError) if primaryFailure != null => primaryFailure.addSuppressed(cleanupError)
      }
  }

  private def runChild(
      index: Int,
      mill: Path,
      workspace: os.Path,
      controlDirectory: os.Path,
      lane: os.Path,
      outputIdentity: String,
      rootOutput: os.Path
  ): SmokeRecord = {
    val marker      = controlDirectory / s"child-$index-marker.json"
    val stdout      = controlDirectory / s"child-$index-stdout.log"
    val stderr      = controlDirectory / s"child-$index-stderr.log"
    val environment = Map(
      "MILL_OUTPUT_DIR"              -> LinkerBenchmark.smokeChildOutputDirectoryValue(lane),
      "JAVA_OPTS"                    -> "-Xmx1g",
      "OS_LIB_PATH_RELATIVIZER_BASE" -> "",
      OutputIdentityEnv              -> outputIdentity
    )
    if !environment.get("MILL_OUTPUT_DIR").exists(_.nonEmpty) then
      throw new Exception("linker benchmark child MILL_OUTPUT_DIR is missing")
    val command = Seq(
      mill.toString,
      "--ticker",
      "false",
      "--no-daemon",
      "ci.linkerBenchmarkWorkerSmoke",
      "--marker",
      physicalPath(marker).toString
    )
    val result = os.Path.pathSerializer.withValue(PhysicalPathSerializer) {
      LinkerBenchmarkProcess.run(
        command = command,
        workingDirectory = workspace,
        environment = environment,
        timeout = 5.minutes,
        startupMarker = marker,
        stdout = stdout,
        stderr = stderr,
        environmentRemovals = LinkerBenchmark.smokeEnvironmentRemovals
      )
    }
    if result.outcome != Outcome.Succeeded then
      throw new Exception(
        s"linker benchmark smoke child $index failed: ${LinkerBenchmark.redact(result.detail)}; durable logs: child-$index-stdout.log, child-$index-stderr.log"
      )
    if !os.isFile(marker) then throw new Exception(s"linker benchmark smoke child $index did not write its marker")
    val record =
      try read[SmokeRecord](os.read(marker))
      catch {
        case error: Exception =>
          throw new Exception(s"linker benchmark smoke child $index marker is invalid: ${error.getClass.getSimpleName}")
      }
    verifyRecord(record, outputIdentity)
    verifyArtifacts(record, controlDirectory, lane, marker, stdout, stderr, rootOutput)
    record
  }

  private def verifyRecord(record: SmokeRecord, outputIdentity: String): Unit = {
    if record.outputDirectoryIdentity != outputIdentity then
      throw new Exception("linker benchmark smoke child reported the wrong output directory identity")
    LinkerBenchmark
      .validateSmokeHeapProbe(record, LinkerBenchmark.ciProfile)
      .fold(
        message =>
          throw new Exception(s"linker benchmark smoke heap validation failed: ${LinkerBenchmark.redact(message)}"),
        _ => ()
      )
  }

  private def verifyArtifacts(
      record: SmokeRecord,
      controlDirectory: os.Path,
      lane: os.Path,
      marker: os.Path,
      stdout: os.Path,
      stderr: os.Path,
      rootOutput: os.Path
  ): Unit = {
    val canonicalControl    = physicalPath(controlDirectory)
    val canonicalLane       = physicalPath(lane)
    val canonicalRootOutput = physicalPath(rootOutput)
    Seq(marker, stdout, stderr).foreach { artifact =>
      val canonicalArtifact = physicalPath(artifact)
      if !canonicalArtifact.startsWith(canonicalControl) then
        throw new Exception("linker benchmark smoke control artifact escaped the durable smoke run directory")
      if canonicalArtifact.startsWith(canonicalLane) || canonicalArtifact.startsWith(canonicalRootOutput) then
        throw new Exception("linker benchmark smoke control artifact used a Mill output directory")
      if !Files.isRegularFile(canonicalArtifact) then
        throw new Exception(s"linker benchmark smoke control artifact is missing: ${artifact.last}")
    }

    val taskOutput = physicalPath(lane / "ci" / "linkerBenchmarkWorkerSmoke.json")
    if !Files.isRegularFile(taskOutput) || Files.size(taskOutput) == 0L then
      throw new Exception("linker benchmark smoke task output is missing or incomplete in the external lane")
    if !taskOutput.startsWith(canonicalLane) || taskOutput.startsWith(canonicalRootOutput) then
      throw new Exception("linker benchmark smoke task output escaped the external lane")
    if !Files.isDirectory(physicalPath(lane / "mill-no-daemon")) then
      throw new Exception("linker benchmark smoke did not create daemonless Mill artifacts in the external lane")

    val matchingProofs = os.walk(lane).filter(_.last == record.proofFilename)
    if matchingProofs.size != 1 then
      throw new Exception(s"linker benchmark smoke child ${record.childPid} proof was not uniquely discoverable")
    val proofPath = physicalPath(matchingProofs.head)
    if !proofPath.startsWith(canonicalLane) || proofPath.startsWith(canonicalRootOutput) then
      throw new Exception(s"linker benchmark smoke child ${record.childPid} proof escaped the external lane")
    val proof =
      try read[SmokeProof](Files.readString(proofPath, StandardCharsets.UTF_8))
      catch {
        case error: Exception =>
          throw new Exception(
            s"linker benchmark smoke child ${record.childPid} proof is invalid: ${error.getClass.getSimpleName}"
          )
      }
    if proof != SmokeProof(record.childPid, record.outputDirectoryIdentity) then
      throw new Exception(s"linker benchmark smoke child ${record.childPid} proof does not match its marker")
  }

  private def clearSmokeRunDirectory(workspace: os.Path, smokeBase: os.Path, smokeRun: os.Path): Unit = {
    LinkerBenchmark
      .validateSmokeRunDirectory(workspace, smokeBase, smokeRun)
      .fold(message => throw new Exception(message), _ => ())
    os.remove.all(smokeRun)
    os.makeDir.all(smokeRun)
  }

  private def clearExternalRunDirectory(
      workspace: os.Path,
      temporaryBase: os.Path,
      externalRunRoot: os.Path
  ): Unit = {
    val validated = LinkerBenchmark
      .validateExternalSmokeCleanupRoot(workspace, temporaryBase, externalRunRoot)
      .fold(message => throw new Exception(message), identity)
    os.remove.all(os.Path(validated))
  }

  private def verifyPinnedLauncher(mill: Path, workspace: os.Path): Unit = {
    if !Files.isRegularFile(mill) then throw new Exception("the pinned Mill launcher is not a file")
    if !Files.isExecutable(mill) then throw new Exception("the pinned Mill launcher is not executable")
    if os.read(workspace / ".mill-version").trim.isEmpty then throw new Exception(".mill-version is empty")
    if !Files.readString(mill, StandardCharsets.UTF_8).contains("MILL_OUTPUT_DIR") then
      throw new Exception("the pinned Mill launcher does not honor MILL_OUTPUT_DIR")
  }

  private def verifyMissingOutputDirectoryProbe(orchestratorOutput: os.Path): Unit = {
    val rejection = LinkerBenchmark.validateChildOutputDirectory(orchestratorOutput, orchestratorOutput)
    if rejection != Left(CollisionMessage) then
      throw new Exception("benchmark child without MILL_OUTPUT_DIR was not rejected before launch")
  }

  private def physicalPath(path: os.Path): Path =
    LinkerBenchmark.canonicalPhysicalPath(path).fold(message => throw new Exception(message), identity)

  private def fileState(path: os.Path): Option[(Long, Long)] =
    Option.when(os.isFile(path))((os.size(path), Files.getLastModifiedTime(physicalPath(path)).toMillis))

  private def sha256(value: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(value.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

  private object PhysicalPathSerializer extends os.Path.Serializer {
    override def serializeString(path: os.Path): String     = path.wrapped.toString
    override def serializeFile(path: os.Path): java.io.File = path.wrapped.toFile
    override def serializePath(path: os.Path): Path         = path.wrapped
    override def deserialize(value: String): Path           = java.nio.file.Paths.get(value)
    override def deserialize(value: java.io.File): Path     = value.toPath
    override def deserialize(value: Path): Path             = value
    override def deserialize(value: java.net.URI): Path     = java.nio.file.Paths.get(value)
  }
}
