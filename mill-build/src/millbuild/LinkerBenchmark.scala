package millbuild

import java.lang.management.ManagementFactory
import java.lang.management.MemoryType
import java.nio.charset.StandardCharsets
import java.nio.file.{AtomicMoveNotSupportedException, Files, LinkOption, Path, StandardCopyOption}
import java.security.MessageDigest
import java.util.Locale
import java.util.concurrent.{Callable, ExecutionException, ExecutorCompletionService, Executors, TimeUnit}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal
import scala.util.matching.Regex

import upickle.default.{ReadWriter, write}

object LinkerBenchmark {
  enum Platform(val token: String, val selector: String) derives ReadWriter {
    case ScalaJs extends Platform("scala-js", "morphir.__.js.__.fastLinkJSTest")
    case Wasm    extends Platform("wasm", "morphir.__.wasm.fullLinkJS")
    case Native  extends Platform("native", "morphir.__.native.__.test.nativeLink")
  }

  enum Strategy(val token: String) derives ReadWriter {
    case LongLived extends Strategy("long-lived")
    case Fresh     extends Strategy("fresh")
    case Recycled  extends Strategy("recycled")
  }

  enum Outcome derives ReadWriter {
    case Succeeded, Failed, TimedOut, Cancelled
  }

  final case class PhaseMetrics(
      name: String,
      wallMillis: Long,
      heapUsedBytes: Long,
      peakHeapBytes: Long,
      gcCount: Long,
      gcMillis: Long,
      evaluated: Long,
      cached: Long
  ) derives ReadWriter

  final case class JvmSnapshot(heapUsedBytes: Long, peakHeapBytes: Long, gcCount: Long, gcMillis: Long)

  object JvmSnapshot {
    private def heapPools = ManagementFactory.getMemoryPoolMXBeans.asScala.filter(_.getType == MemoryType.HEAP)

    def sumNonnegative(values: IterableOnce[Long]): Long = {
      val iterator                         = values.iterator
      @tailrec def loop(total: Long): Long =
        if !iterator.hasNext then total
        else {
          val value = iterator.next()
          if value < 0L then loop(total)
          else if total > Long.MaxValue - value then Long.MaxValue
          else loop(total + value)
        }
      loop(0L)
    }

    def resetPeakUsage(): Unit = heapPools.foreach(_.resetPeakUsage())

    def capture(): JvmSnapshot = {
      val peakHeapBytes = sumNonnegative(heapPools.iterator.flatMap(pool => Option(pool.getPeakUsage)).map(_.getUsed))
      val garbageCollectors = ManagementFactory.getGarbageCollectorMXBeans.asScala
      JvmSnapshot(
        heapUsedBytes = math.max(0L, ManagementFactory.getMemoryMXBean.getHeapMemoryUsage.getUsed),
        peakHeapBytes = peakHeapBytes,
        gcCount = sumNonnegative(garbageCollectors.iterator.map(_.getCollectionCount)),
        gcMillis = sumNonnegative(garbageCollectors.iterator.map(_.getCollectionTime))
      )
    }

    def phaseDelta(
        name: String,
        wallMillis: Long,
        before: JvmSnapshot,
        after: JvmSnapshot,
        evaluated: Long,
        cached: Long
    ): PhaseMetrics =
      PhaseMetrics(
        name = name,
        wallMillis = wallMillis,
        heapUsedBytes = after.heapUsedBytes,
        peakHeapBytes = after.peakHeapBytes,
        gcCount = math.max(0L, after.gcCount - before.gcCount),
        gcMillis = math.max(0L, after.gcMillis - before.gcMillis),
        evaluated = evaluated,
        cached = cached
      )
  }

  final case class Profile(
      name: String,
      memoryGiB: Int,
      reserveGiB: Int,
      heapGiB: Int,
      millJobs: Int,
      maxChildren: Int,
      batchSize: Int,
      timeoutMinutes: Int
  ) derives ReadWriter

  val ciProfile: Profile = Profile("ci", 16, 4, 8, 2, 1, 4, 30)

  def workerProfileArguments(profile: Profile): Seq[String] =
    Seq(
      "--profile",
      profile.name,
      "--memoryGib",
      profile.memoryGiB.toString,
      "--reserveGib",
      profile.reserveGiB.toString,
      "--heapGib",
      profile.heapGiB.toString,
      "--millJobs",
      profile.millJobs.toString,
      "--maxChildren",
      profile.maxChildren.toString,
      "--batchSize",
      profile.batchSize.toString,
      "--timeoutMinutes",
      profile.timeoutMinutes.toString
    )

  def reconstructWorkerProfile(
      base: Profile,
      memoryGiB: Int,
      reserveGiB: Int,
      heapGiB: Int,
      millJobs: Int,
      maxChildren: Int,
      batchSize: Int,
      timeoutMinutes: Int
  ): Either[String, Profile] =
    validate(
      base.copy(
        memoryGiB = memoryGiB,
        reserveGiB = reserveGiB,
        heapGiB = heapGiB,
        millJobs = millJobs,
        maxChildren = maxChildren,
        batchSize = batchSize,
        timeoutMinutes = timeoutMinutes
      )
    )

  def localProfile(memoryGiB: Int, availableProcessors: Int): Either[String, Profile] =
    if memoryGiB <= 0 then Left("local memory budget must be greater than zero")
    else if availableProcessors <= 0 then Left("local available processors must be greater than zero")
    else {
      val reserveGiB       = math.max(4, memoryGiB / 4)
      val admittedByMemory = (memoryGiB - reserveGiB) / ciProfile.heapGiB
      if admittedByMemory <= 0 then Left("local memory budget cannot admit one child heap and the safety reserve")
      else {
        val maxChildren = math.min(2, math.min(availableProcessors, admittedByMemory))
        validate(
          Profile(
            name = "local",
            memoryGiB = memoryGiB,
            reserveGiB = reserveGiB,
            heapGiB = ciProfile.heapGiB,
            millJobs = maxChildren,
            maxChildren = maxChildren,
            batchSize = ciProfile.batchSize,
            timeoutMinutes = ciProfile.timeoutMinutes
          )
        )
      }
    }

  def detectedLocalProfile(): Either[String, Profile] = {
    val totalBytes = ManagementFactory.getOperatingSystemMXBean match {
      case operatingSystem: com.sun.management.OperatingSystemMXBean => operatingSystem.getTotalMemorySize
      case _ => ciProfile.memoryGiB.toLong * 1024L * 1024L * 1024L
    }
    val memoryGiB = math.max(1L, totalBytes / (1024L * 1024L * 1024L)).min(Int.MaxValue.toLong).toInt
    localProfile(memoryGiB, Runtime.getRuntime.availableProcessors())
  }

  final case class Batch(lane: Int, index: Int, targets: Seq[String]) derives ReadWriter

  final case class WorkPlan(lanes: Int, batches: Seq[Batch]) derives ReadWriter

  final case class PreparationLane(lane: Int, batches: Seq[Batch])

  final case class PlatformInventory(platform: Platform, targets: Seq[String]) derives ReadWriter

  final case class InventoryRecord(effectiveMaxHeapBytes: Long, inventories: Seq[PlatformInventory]) derives ReadWriter

  final case class EvaluationSettings(trials: Int, orderSeed: Long) derives ReadWriter

  final case class ResolvedBenchmarkConfiguration(
      preset: String,
      profile: Profile,
      settings: EvaluationSettings,
      platforms: Seq[Platform],
      strategies: Seq[Strategy],
      targetFilter: Option[String],
      targetLimit: Option[Int],
      continueOnFailure: Boolean
  ) derives ReadWriter

  final case class BenchmarkOverrides(
      platforms: Option[String] = None,
      strategies: Option[String] = None,
      trials: Option[Int] = None,
      orderSeed: Option[Long] = None,
      targetFilter: Option[String] = None,
      targetLimit: Option[Int] = None,
      memoryGiB: Option[Int] = None,
      reserveGiB: Option[Int] = None,
      millJobs: Option[Int] = None,
      maxChildren: Option[Int] = None,
      batchSize: Option[Int] = None,
      timeoutMinutes: Option[Int] = None,
      continueOnFailure: Option[Boolean] = None
  ) derives ReadWriter

  private val hostedPresets: Map[String, ResolvedBenchmarkConfiguration] = Seq(
    ResolvedBenchmarkConfiguration(
      preset = "quick-smoke",
      profile = ciProfile,
      settings = EvaluationSettings(1, 0L),
      platforms = Platform.values.toSeq,
      strategies = Strategy.values.toSeq,
      targetFilter = None,
      targetLimit = Some(1),
      continueOnFailure = true
    ),
    ResolvedBenchmarkConfiguration(
      preset = "js-strategies",
      profile = ciProfile,
      settings = EvaluationSettings(3, 0L),
      platforms = Seq(Platform.ScalaJs),
      strategies = Strategy.values.toSeq,
      targetFilter = None,
      targetLimit = None,
      continueOnFailure = true
    ),
    ResolvedBenchmarkConfiguration(
      preset = "wasm-strategies",
      profile = ciProfile,
      settings = EvaluationSettings(3, 0L),
      platforms = Seq(Platform.Wasm),
      strategies = Strategy.values.toSeq,
      targetFilter = None,
      targetLimit = None,
      continueOnFailure = true
    ),
    ResolvedBenchmarkConfiguration(
      preset = "native-long-lived",
      profile = ciProfile.copy(timeoutMinutes = 40),
      settings = EvaluationSettings(1, 0L),
      platforms = Seq(Platform.Native),
      strategies = Seq(Strategy.LongLived),
      targetFilter = None,
      targetLimit = None,
      continueOnFailure = true
    ),
    ResolvedBenchmarkConfiguration(
      preset = "native-fresh-recycled",
      profile = ciProfile,
      settings = EvaluationSettings(3, 0L),
      platforms = Seq(Platform.Native),
      strategies = Seq(Strategy.Fresh, Strategy.Recycled),
      targetFilter = None,
      targetLimit = None,
      continueOnFailure = true
    )
  ).map(configuration => configuration.preset -> configuration).toMap

  /**
   * Bounds manual hosted dispatches to an ubuntu-latest-sized machine and GitHub's six-hour job envelope. The
   * per-operation timeout leaves 30 minutes for teardown, report writing, and artifact publication. Profile admission
   * applies the stricter cross-field memory constraint after these individual limits.
   */
  val maxHostedTrials: Int         = 100
  val maxHostedMemoryGiB: Int      = 16
  val maxHostedReserveGiB: Int     = 15
  val maxHostedMillJobs: Int       = 64
  val maxHostedChildren: Int       = 16
  val maxHostedBatchSize: Int      = 256
  val maxHostedTimeoutMinutes: Int = 330
  val maxHostedTargetLimit: Int    = 10_000

  def parseOptionalTrimmedString(value: String, field: String): Either[String, Option[String]] =
    if value.exists(Character.isISOControl) then Left(s"$field contains invalid characters")
    else Right(Option.when(value.trim.nonEmpty)(value.trim))

  private def parseInt(value: String, field: String): Either[String, Int] =
    parseOptionalTrimmedString(value, field).flatMap {
      case Some(token) => token.toIntOption.toRight(s"$field must be a whole number")
      case None        => Left(s"$field must be a whole number")
    }

  def parseZeroAsUnsetPositiveInt(value: String, field: String): Either[String, Option[Int]] =
    parseInt(value, field).flatMap(parsed =>
      if parsed < 0 then Left(s"$field must be nonnegative")
      else Right(Option.when(parsed > 0)(parsed))
    )

  def parsePositiveInt(value: String, field: String): Either[String, Int] =
    parseInt(value, field).flatMap(parsed => Either.cond(parsed > 0, parsed, s"$field must be greater than zero"))

  /** Boolean CLI tokens are trimmed and compared case-insensitively using the root locale. */
  def parseBoolean(value: String, field: String): Either[String, Boolean] =
    parseOptionalTrimmedString(value, field).flatMap {
      case Some(token) =>
        token.toLowerCase(Locale.ROOT) match {
          case "true"  => Right(true)
          case "false" => Right(false)
          case _       => Left(s"$field must be true or false")
        }
      case None => Left(s"$field must be true or false")
    }

  def parseOptionalLong(value: String, field: String): Either[String, Option[Long]] =
    parseOptionalTrimmedString(value, field).flatMap {
      case None        => Right(None)
      case Some(token) => token.toLongOption.map(Some(_)).toRight(s"$field must be a whole number")
    }

  /** Continuation tokens are trimmed and compared case-insensitively using the root locale. */
  def parseContinuationChoice(value: String): Either[String, Option[Boolean]] =
    parseOptionalTrimmedString(value, "continue on failure").flatMap {
      case Some(token) =>
        token.toLowerCase(Locale.ROOT) match {
          case "preset" => Right(None)
          case "true"   => Right(Some(true))
          case "false"  => Right(Some(false))
          case _        => Left("continue on failure must be preset, true, or false")
        }
      case None => Left("continue on failure must be preset, true, or false")
    }

  def hostedPreset(token: String): Either[String, ResolvedBenchmarkConfiguration] =
    if token.exists(Character.isISOControl) then Left("preset contains invalid characters")
    else hostedPresets.get(token).toRight("preset selection is invalid")

  def hostedArtifactName(
      preset: String,
      refName: String,
      runId: String,
      runAttempt: Int
  ): Either[String, String] = {
    val prefix = "linker-benchmark-"

    def normalize(value: String, field: String): Either[String, String] =
      if value.exists(Character.isISOControl) then Left(s"$field contains invalid characters")
      else {
        val normalized = value.trim.iterator
          .map {
            case character if character >= 'A' && character <= 'Z' => character.toLower
            case character
                if (character >= 'a' && character <= 'z') ||
                  (character >= '0' && character <= '9') || character == '.' || character == '-' =>
              character
            case _ => '-'
          }
          .mkString
          .replaceAll("-+", "-")
          .dropWhile(character => character == '-' || character == '.')
          .reverse
          .dropWhile(character => character == '-' || character == '.')
          .reverse
        Either.cond(normalized.nonEmpty, normalized, s"$field must not be empty")
      }

    def shorten(value: String, original: String, maximum: Int): String =
      if value.length <= maximum then value
      else {
        val hash   = sha256Hex(original).take(16)
        val prefix = value.take(maximum - hash.length - 1).reverse.dropWhile(character =>
          character == '-' || character == '.'
        ).reverse
        s"$prefix-$hash"
      }

    for {
      rawPreset   <- parseOptionalTrimmedString(preset, "preset").flatMap(_.toRight("preset must not be empty"))
      _           <- hostedPreset(rawPreset)
      presetToken <- normalize(rawPreset, "preset")
      refToken    <- normalize(refName, "artifact ref")
      runToken    <- normalize(runId, "artifact run id")
      _           <- Either.cond(runAttempt > 0, (), "artifact run attempt must be greater than zero")
      safeRun     = shorten(runToken, runId, 40)
      fixedLength = prefix.length + presetToken.length + safeRun.length + runAttempt.toString.length + 3
      refMaximum  = 120 - fixedLength
      _ <- Either.cond(refMaximum >= 18, (), "artifact name inputs are too long")
      safeRef = shorten(refToken, refName, refMaximum)
      result  = s"$prefix$presetToken-$safeRef-$safeRun-$runAttempt"
    } yield result
  }

  private def sha256Hex(value: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(value.getBytes(StandardCharsets.UTF_8))
      .iterator
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

  def validateHostedOverrideBounds(overrides: BenchmarkOverrides): Either[String, Unit] = {
    def bounded(value: Option[Int], field: String, maximum: Int): Either[String, Unit] =
      value match {
        case Some(number) if number <= 0      => Left(s"$field must be greater than zero")
        case Some(number) if number > maximum => Left(s"$field exceeds the hosted maximum")
        case _                                => Right(())
      }

    for {
      _ <- bounded(overrides.trials, "trials", maxHostedTrials)
      _ <- bounded(overrides.targetLimit, "target limit", maxHostedTargetLimit)
      _ <- bounded(overrides.memoryGiB, "memory GiB", maxHostedMemoryGiB)
      _ <- bounded(overrides.reserveGiB, "reserve GiB", maxHostedReserveGiB)
      _ <- bounded(overrides.millJobs, "Mill jobs", maxHostedMillJobs)
      _ <- bounded(overrides.maxChildren, "max children", maxHostedChildren)
      _ <- bounded(overrides.batchSize, "batch size", maxHostedBatchSize)
      _ <- bounded(overrides.timeoutMinutes, "timeout minutes", maxHostedTimeoutMinutes)
    } yield ()
  }

  private def parseOverrideTokens[A](
      value: String,
      label: String,
      parse: String => Option[A]
  ): Either[String, Seq[A]] =
    if value.exists(Character.isISOControl) then Left(s"$label selection contains invalid characters")
    else {
      val tokens = value.split(",", -1).toSeq.map(_.trim)
      if tokens.exists(_.isEmpty) then Left(s"$label selection must be nonempty")
      else {
        val parsed = tokens.map(parse)
        if parsed.exists(_.isEmpty) then Left(s"$label selection is invalid")
        else {
          val values = parsed.flatten
          if values.distinct.size != values.size then Left(s"$label selection contains duplicates")
          else Right(values)
        }
      }
    }

  private def validateTargetFilter(value: String): Either[String, String] =
    if value.trim.isEmpty then Left("target filter must not be empty")
    else if value.exists(Character.isISOControl) then Left("target filter must not contain control characters")
    else Right(value)

  def resolveHostedConfiguration(
      preset: String,
      overrides: BenchmarkOverrides
  ): Either[String, ResolvedBenchmarkConfiguration] =
    for {
      _         <- validateHostedOverrideBounds(overrides)
      base      <- hostedPreset(preset)
      platforms <- overrides.platforms.fold[Either[String, Seq[Platform]]](Right(base.platforms))(value =>
        parseOverrideTokens(
          value,
          "platforms",
          {
            case "js" | "scala-js" => Some(Platform.ScalaJs)
            case token             => Platform.values.find(_.token == token)
          }
        )
      )
      strategies <- overrides.strategies.fold[Either[String, Seq[Strategy]]](Right(base.strategies))(value =>
        parseOverrideTokens(value, "strategies", token => Strategy.values.find(_.token == token))
      )
      targetFilter <- overrides.targetFilter match {
        case None        => Right(base.targetFilter)
        case Some(value) => validateTargetFilter(value).map(Some(_))
      }
      targetLimit = overrides.targetLimit.orElse(base.targetLimit)
      _ <- Either.cond(overrides.orderSeed.forall(_ >= 0L), (), "order seed must be nonnegative")
      trials = overrides.trials.getOrElse(base.settings.trials)
      settings <- validate(
        EvaluationSettings(
          trials = trials,
          orderSeed = overrides.orderSeed.getOrElse(base.settings.orderSeed)
        )
      )
      profile <- validate(
        base.profile.copy(
          memoryGiB = overrides.memoryGiB.getOrElse(base.profile.memoryGiB),
          reserveGiB = overrides.reserveGiB.getOrElse(base.profile.reserveGiB),
          millJobs = overrides.millJobs.getOrElse(base.profile.millJobs),
          maxChildren = overrides.maxChildren.getOrElse(base.profile.maxChildren),
          batchSize = overrides.batchSize.getOrElse(base.profile.batchSize),
          timeoutMinutes = overrides.timeoutMinutes.getOrElse(base.profile.timeoutMinutes)
        )
      )
    } yield base.copy(
      profile = profile,
      settings = settings,
      platforms = platforms,
      strategies = strategies,
      targetFilter = targetFilter,
      targetLimit = targetLimit,
      continueOnFailure = overrides.continueOnFailure.getOrElse(base.continueOnFailure)
    )

  def resolveBenchmarkConfiguration(
      preset: String,
      profileToken: String,
      local: Profile,
      overrides: BenchmarkOverrides
  ): Either[String, ResolvedBenchmarkConfiguration] =
    parseOptionalTrimmedString(preset, "preset").flatMap {
      case Some(hosted) =>
        parseOptionalTrimmedString(profileToken, "profile").flatMap {
          case Some(_) => Left("profile is available only without a hosted preset")
          case None    => resolveHostedConfiguration(hosted, overrides)
        }
      case None =>
        for {
          profileChoice <- parseOptionalTrimmedString(profileToken, "profile")
          baseProfile   <- profileChoice.getOrElse("local") match {
            case "local" => validate(local)
            case "ci"    => Right(ciProfile)
            case _       => Left("profile selection is invalid")
          }
          platforms <- overrides.platforms.fold[Either[String, Seq[Platform]]](Right(Platform.values.toSeq))(value =>
            parseOverrideTokens(
              value,
              "platforms",
              {
                case "js" | "scala-js" => Some(Platform.ScalaJs)
                case token             => Platform.values.find(_.token == token)
              }
            )
          )
          strategies <- overrides.strategies.fold[Either[String, Seq[Strategy]]](Right(Strategy.values.toSeq))(value =>
            parseOverrideTokens(value, "strategies", token => Strategy.values.find(_.token == token))
          )
          targetFilter <- overrides.targetFilter match {
            case None        => Right(None)
            case Some(value) => validateTargetFilter(value).map(Some(_))
          }
          _        <- Either.cond(overrides.targetLimit.forall(_ > 0), (), "target limit must be greater than zero")
          settings <- validate(
            EvaluationSettings(overrides.trials.getOrElse(3), overrides.orderSeed.getOrElse(0L))
          )
          profile <- validate(
            baseProfile.copy(
              memoryGiB = overrides.memoryGiB.getOrElse(baseProfile.memoryGiB),
              reserveGiB = overrides.reserveGiB.getOrElse(baseProfile.reserveGiB),
              millJobs = overrides.millJobs.getOrElse(baseProfile.millJobs),
              maxChildren = overrides.maxChildren.getOrElse(baseProfile.maxChildren),
              batchSize = overrides.batchSize.getOrElse(baseProfile.batchSize),
              timeoutMinutes = overrides.timeoutMinutes.getOrElse(baseProfile.timeoutMinutes)
            )
          )
        } yield ResolvedBenchmarkConfiguration(
          preset = "direct",
          profile = profile,
          settings = settings,
          platforms = platforms,
          strategies = strategies,
          targetFilter = targetFilter,
          targetLimit = overrides.targetLimit,
          continueOnFailure = overrides.continueOnFailure.getOrElse(false)
        )
    }

  def selectInventoryTargets(
      targets: Seq[String],
      filter: Option[String],
      limit: Option[Int],
      platform: Platform
  ): Either[String, Seq[String]] =
    for {
      validatedFilter <- filter match {
        case None        => Right(None)
        case Some(value) => validateTargetFilter(value).map(Some(_))
      }
      _ <- Either.cond(limit.forall(_ > 0), (), "target limit must be greater than zero")
      selected = {
        val ordered  = targets.distinct.sorted
        val filtered = validatedFilter.fold(ordered)(value => ordered.filter(_.contains(value)))
        limit.fold(filtered)(filtered.take)
      }
      _ <- Either.cond(selected.nonEmpty, (), s"no ${platform.token} inventory targets matched the selection")
    } yield selected

  final case class SmokeRecord(
      childPid: Long,
      javaVersion: String,
      pinnedMillVersion: String,
      requestedHeapBytes: Long,
      effectiveMaxHeapBytes: Long,
      requestedHeapHonored: Boolean,
      outputDirectoryIdentity: String,
      proofFilename: String
  ) derives ReadWriter

  final case class SmokeProof(childPid: Long, outputDirectoryIdentity: String) derives ReadWriter

  final case class RecoveryAttemptRecord(
      outcome: Outcome,
      wallMillis: Long,
      exitCode: Option[Int],
      worker: Option[SmokeRecord]
  ) derives ReadWriter

  final case class RecoveryRecord(first: RecoveryAttemptRecord, replacement: RecoveryAttemptRecord)
      derives ReadWriter

  def validateRecovery(record: RecoveryRecord): Either[String, Unit] =
    if record.first.outcome != Outcome.TimedOut then Left("first recovery worker must time out")
    else if record.first.wallMillis < 0L || record.replacement.wallMillis < 0L then
      Left("recovery wall times must be nonnegative")
    else if record.replacement.outcome != Outcome.Succeeded then Left("replacement recovery worker must succeed")
    else if !record.replacement.exitCode.contains(0) then Left("replacement recovery worker must exit zero")
    else if record.replacement.worker.isEmpty then Left("replacement recovery worker marker is required")
    else Right(())

  def renderRecoveryJson(record: RecoveryRecord): String = renderRecoveryJson(record, sys.env)

  def renderRecoveryJson(record: RecoveryRecord, environment: Map[String, String]): String = {
    def cleanWorker(worker: SmokeRecord): SmokeRecord =
      worker.copy(
        javaVersion = redact(worker.javaVersion, environment),
        pinnedMillVersion = redact(worker.pinnedMillVersion, environment),
        outputDirectoryIdentity = redact(worker.outputDirectoryIdentity, environment),
        proofFilename = redact(worker.proofFilename, environment)
      )
    write(
      record.copy(
        first = record.first.copy(worker = record.first.worker.map(cleanWorker)),
        replacement = record.replacement.copy(worker = record.replacement.worker.map(cleanWorker))
      )
    )
  }

  def validateRecoveryProof(
      workspace: os.Path,
      rootOutput: os.Path,
      lane: os.Path,
      proofPath: os.Path,
      expectedOutputIdentity: String,
      worker: SmokeRecord,
      proof: SmokeProof
  ): Either[String, Path] =
    validateRecoveryProofPath(
      workspace,
      rootOutput,
      lane,
      proofPath,
      expectedOutputIdentity,
      worker
    ).flatMap(canonicalProof =>
      Either.cond(
        proof == SmokeProof(worker.childPid, worker.outputDirectoryIdentity),
        canonicalProof,
        "replacement recovery worker proof does not match its marker"
      )
    )

  def validateRecoveryProofPath(
      workspace: os.Path,
      rootOutput: os.Path,
      lane: os.Path,
      proofPath: os.Path,
      expectedOutputIdentity: String,
      worker: SmokeRecord
  ): Either[String, Path] = {
    val lexicalLane  = lane.wrapped.toAbsolutePath.normalize()
    val lexicalProof = proofPath.wrapped.toAbsolutePath.normalize()

    def containsSymlink: Boolean =
      if !lexicalProof.startsWith(lexicalLane) then true
      else
        lexicalLane
          .relativize(lexicalProof)
          .iterator()
          .asScala
          .scanLeft(lexicalLane)((current, segment) => current.resolve(segment))
          .exists(Files.isSymbolicLink)

    if worker.outputDirectoryIdentity != expectedOutputIdentity then
      Left("replacement recovery worker reported the wrong output directory identity")
    else if proofPath.last != worker.proofFilename then Left("replacement recovery worker proof has the wrong name")
    else if containsSymlink then Left("replacement recovery worker proof must not use symbolic links")
    else if !Files.isRegularFile(lexicalProof, LinkOption.NOFOLLOW_LINKS) then
      Left("replacement recovery worker proof must be a regular file")
    else
      for {
        canonicalWorkspace  <- canonicalPhysicalPath(workspace)
        canonicalRootOutput <- canonicalPhysicalPath(rootOutput)
        canonicalLane       <- canonicalPhysicalPath(lane)
        canonicalProof      <- canonicalPhysicalPath(proofPath)
        result              <- Either.cond(
          canonicalProof != canonicalLane &&
            canonicalProof.startsWith(canonicalLane) &&
            !canonicalProof.startsWith(canonicalWorkspace) &&
            !canonicalProof.startsWith(canonicalRootOutput) &&
            Files.isRegularFile(canonicalProof, LinkOption.NOFOLLOW_LINKS),
          canonicalProof,
          "replacement recovery worker proof escaped its external lane"
        )
      } yield result
  }

  def renderValidatedRecoveryJson(
      record: RecoveryRecord,
      proofValidation: Either[String, Path],
      environment: Map[String, String]
  ): Either[String, String] =
    validateRecovery(record).flatMap(_ => proofValidation.map(_ => renderRecoveryJson(record, environment)))

  def benchmarkOutputRoot(
      base: os.Path,
      configuration: ResolvedBenchmarkConfiguration,
      recoverySmoke: Boolean,
      planOnly: Boolean
  ): Either[String, os.Path] = {
    val safeToken = "[a-z0-9](?:[a-z0-9-]*[a-z0-9])?"
    if recoverySmoke && planOnly then Left("recovery-smoke and plan-only are exclusive")
    else
      for {
        identity <- benchmarkOutputIdentity(configuration)
        _ <- Either.cond(configuration.profile.name.matches(safeToken), (), "profile name is not a safe output token")
        _ <- Either.cond(configuration.platforms.nonEmpty, (), "platforms must not be empty")
        platformDirectory = configuration.platforms.distinct.map(_.token).sorted.mkString("+")
      } yield {
        val root = base / identity / configuration.profile.name
        if recoverySmoke then root / "recovery-smoke"
        else if planOnly then root / "plan-only" / platformDirectory
        else root / platformDirectory
      }
  }

  def benchmarkOutputIdentity(configuration: ResolvedBenchmarkConfiguration): Either[String, String] = {
    val safeToken = "[a-z0-9](?:[a-z0-9-]*[a-z0-9])?"
    Either.cond(
      configuration.preset.matches(safeToken),
      s"${configuration.preset}-${sha256Hex(write(configuration)).take(16)}",
      "preset is not a safe output token"
    )
  }

  final case class RuntimeMetadata(
      javaVersion: String,
      pinnedMillVersion: String,
      operatingSystem: String,
      architecture: String,
      availableProcessors: Int,
      effectiveMaxHeapBytes: Long,
      outputDirectoryIdentity: String
  ) derives ReadWriter

  object RuntimeMetadata {
    val Unknown: RuntimeMetadata = RuntimeMetadata("unknown", "unknown", "unknown", "unknown", 0, 0L, "unknown")
  }

  final case class WorkerRecord(
      runId: String,
      trial: Int,
      strategyPosition: Int,
      settings: EvaluationSettings,
      platform: Platform,
      strategy: Strategy,
      profile: Profile,
      lane: Int,
      batch: Int,
      targets: Seq[String],
      startupMillis: Long,
      peakRssKiB: Long,
      phases: Seq[PhaseMetrics],
      outcome: Outcome,
      detail: Option[String],
      runtime: RuntimeMetadata = RuntimeMetadata.Unknown
  ) derives ReadWriter

  final case class WorkerIdentity(
      runId: String,
      settings: EvaluationSettings,
      platform: Platform,
      strategy: Strategy,
      profile: Profile,
      trial: Int,
      strategyPosition: Int,
      lane: Int,
      batch: Int,
      targets: Seq[String]
  )

  object WorkerIdentity {
    def from(record: WorkerRecord): WorkerIdentity =
      WorkerIdentity(
        record.runId,
        record.settings,
        record.platform,
        record.strategy,
        record.profile,
        record.trial,
        record.strategyPosition,
        record.lane,
        record.batch,
        record.targets
      )
  }

  def workerCommandArguments(record: String, started: String, identity: WorkerIdentity): Seq[String] =
    Seq(
      "ci.linkerBenchmarkWorker",
      "--record",
      record,
      "--started",
      started,
      "--runId",
      identity.runId,
      "--platform",
      identity.platform.token,
      "--strategy",
      identity.strategy.token
    ) ++ workerProfileArguments(identity.profile) ++ Seq(
      "--trials",
      identity.settings.trials.toString,
      "--orderSeed",
      identity.settings.orderSeed.toString,
      "--trial",
      identity.trial.toString,
      "--strategyPosition",
      identity.strategyPosition.toString,
      "--lane",
      identity.lane.toString,
      "--batch",
      identity.batch.toString,
      "--targets",
      identity.targets.mkString(",")
    )

  def validateWorkerRecordIdentity(
      record: WorkerRecord,
      expected: WorkerIdentity
  ): Either[String, WorkerRecord] =
    Either.cond(
      WorkerIdentity.from(record) == expected,
      record,
      "worker record identity does not match the scheduled worker"
    )

  def clearWorkerLaunchMarkers(directory: os.Path): Either[String, Unit] = {
    val lexicalDirectory = directory.toNIO.toAbsolutePath.normalize()
    val markers          = Seq("record.json", "started").map(lexicalDirectory.resolve)

    def validMarker(path: Path): Boolean =
      path.getParent == lexicalDirectory &&
        (!Files.exists(path, LinkOption.NOFOLLOW_LINKS) ||
          Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS))

    if !Files.isDirectory(lexicalDirectory, LinkOption.NOFOLLOW_LINKS) then
      Left("worker output directory must be a regular directory")
    else if !markers.forall(validMarker) then Left("worker launch markers must be regular files")
    else
      try {
        markers.foreach(Files.deleteIfExists)
        Right(())
      } catch {
        case NonFatal(error) => Left(s"cannot clear worker launch markers: ${error.getClass.getSimpleName}")
      }
  }

  /**
   * Validates the existing ancestor chain before creating a worker directory. This assumes a trusted local filesystem
   * where another process does not replace a validated parent between validation and creation.
   */
  def prepareWorkerOutputDirectory(outputRoot: os.Path, requested: os.Path): Either[String, os.Path] = {
    val message          = "worker output directory escaped benchmark output"
    val lexicalRoot      = outputRoot.toNIO.toAbsolutePath.normalize()
    val lexicalRequested = requested.toNIO.toAbsolutePath.normalize()
    val containsSymlink  =
      lexicalRequested.startsWith(lexicalRoot) &&
        lexicalRoot
          .relativize(lexicalRequested)
          .iterator()
          .asScala
          .scanLeft(lexicalRoot)((current, segment) => current.resolve(segment))
          .exists(Files.isSymbolicLink)

    if lexicalRequested == lexicalRoot || !lexicalRequested.startsWith(lexicalRoot) then Left(message)
    else if containsSymlink then Left("worker output directory must not contain symbolic links")
    else
      validatePhysicalDescendant(outputRoot, requested, message).flatMap { physicalRequested =>
        val directory = os.Path(physicalRequested)
        try {
          os.makeDir.all(directory)
          validatePhysicalDescendant(outputRoot, directory, message)
            .map(os.Path(_))
            .flatMap(validated => clearWorkerLaunchMarkers(validated).map(_ => validated))
        } catch {
          case NonFatal(error) => Left(s"cannot prepare worker output directory: ${error.getClass.getSimpleName}")
        }
      }
  }

  final case class StrategyTrialRecord(
      runId: String,
      trial: Int,
      strategyPosition: Int,
      platform: Platform,
      strategy: Strategy,
      profile: Profile,
      wallMillis: Long,
      peakAggregateRssKiB: Long,
      targetsCompleted: Long,
      outcome: Outcome
  ) derives ReadWriter

  final case class PreparationRecord(
      runId: String,
      trial: Int,
      platform: Platform,
      strategy: Strategy,
      profile: Profile,
      lane: Int,
      wallMillis: Long,
      targets: Seq[String],
      outcome: Outcome
  ) derives ReadWriter

  final case class PreparationDecision(measure: Boolean, outcome: Outcome, failRun: Boolean)

  final case class EvaluationCase(
      trial: Int,
      strategyPosition: Int,
      platform: Platform,
      strategy: Strategy
  )

  def evaluationSchedule(orders: Seq[Seq[Strategy]], platforms: Seq[Platform]): Seq[EvaluationCase] =
    orders.zipWithIndex.flatMap { case (order, trial) =>
      order.zipWithIndex.flatMap { case (strategy, position) =>
        platforms.map(platform => EvaluationCase(trial, position, platform, strategy))
      }
    }

  def runEvaluationSchedule[A](
      scheduled: Seq[A],
      continueOnFailure: Boolean
  )(run: A => Outcome): Seq[(A, Outcome)] = {
    @tailrec def loop(remaining: List[A], completed: Vector[(A, Outcome)]): Vector[(A, Outcome)] =
      remaining match {
        case Nil          => completed
        case next :: tail =>
          val outcome = run(next)
          val updated = completed :+ (next -> outcome)
          if outcome == Outcome.Succeeded || continueOnFailure then loop(tail, updated)
          else updated
      }

    loop(scheduled.toList, Vector.empty)
  }

  def evaluationFailed(outcomes: Seq[Outcome]): Boolean =
    outcomes.exists(_ != Outcome.Succeeded)

  def runBatchesUntilInterrupted[A, B](batches: Seq[A])(run: A => B): Seq[B] = {
    @tailrec def loop(remaining: List[A], completed: Vector[B]): Vector[B] =
      if Thread.currentThread().isInterrupted then completed
      else
        remaining match {
          case Nil          => completed
          case head :: tail => loop(tail, completed :+ run(head))
        }

    loop(batches.toList, Vector.empty)
  }

  def preparationDecision(outcomes: Seq[Outcome], continueOnFailure: Boolean): PreparationDecision = {
    val outcome =
      if outcomes.isEmpty then Outcome.Failed
      else if outcomes.contains(Outcome.Cancelled) then Outcome.Cancelled
      else if outcomes.contains(Outcome.TimedOut) then Outcome.TimedOut
      else if outcomes.contains(Outcome.Failed) then Outcome.Failed
      else Outcome.Succeeded
    val measure = outcome == Outcome.Succeeded
    PreparationDecision(measure, outcome, failRun = !measure && !continueOnFailure)
  }

  final case class BenchmarkResult(
      configuration: ResolvedBenchmarkConfiguration,
      peakAggregateRssKiB: Long,
      cases: Seq[StrategyTrialRecord],
      records: Seq[WorkerRecord],
      preparations: Seq[PreparationRecord] = Seq.empty
  ) derives ReadWriter

  final case class LongSummary(median: Long, min: Long, max: Long) derives ReadWriter

  final case class DoubleSummary(median: Double, min: Double, max: Double) derives ReadWriter

  final case class AggregateMetrics(
      profile: Profile,
      platform: Platform,
      strategy: Strategy,
      succeeded: Int,
      failed: Int,
      timedOut: Int,
      cancelled: Int,
      wallMillis: Option[LongSummary],
      peakAggregateRssKiB: Option[LongSummary],
      startupShare: Option[DoubleSummary],
      throughputTargetsPerMinute: Option[DoubleSummary],
      peakChildRssKiB: Option[LongSummary],
      peakHeapBytes: Option[LongSummary],
      gcShare: Option[DoubleSummary]
  ) derives ReadWriter

  private final case class TrialWorkerMetrics(
      startupShare: Option[Double],
      throughputTargetsPerMinute: Option[Double],
      peakChildRssKiB: Long,
      peakHeapBytes: Long,
      gcShare: Option[Double]
  )

  private def deduplicate[A, Identity](values: Seq[A], label: String)(identity: A => Identity): Seq[A] = {
    val unique = mutable.LinkedHashMap.empty[Identity, A]
    values.foreach { value =>
      val key = identity(value)
      unique.get(key) match {
        case None                                => unique.addOne(key -> value)
        case Some(existing) if existing == value => ()
        case Some(_) => throw new IllegalArgumentException(s"conflicting duplicate $label identity: $key")
      }
    }
    unique.values.toSeq
  }

  private def deduplicateCases(cases: Seq[StrategyTrialRecord]): Seq[StrategyTrialRecord] =
    deduplicate(cases, "strategy trial")(value =>
      (value.runId, value.profile, value.platform, value.strategy, value.trial, value.strategyPosition)
    )

  private def deduplicateRecords(records: Seq[WorkerRecord]): Seq[WorkerRecord] =
    deduplicate(records, "worker")(value =>
      (
        value.runId,
        value.profile,
        value.platform,
        value.strategy,
        value.trial,
        value.strategyPosition,
        value.lane,
        value.batch
      )
    )

  private def deduplicatePreparations(records: Seq[PreparationRecord]): Seq[PreparationRecord] =
    deduplicate(records, "preparation")(value =>
      (value.runId, value.profile, value.platform, value.strategy, value.trial, value.lane)
    )

  private def summarizeLong(values: Seq[Long]): Option[LongSummary] =
    if values.isEmpty then None
    else {
      val sorted = values.sorted
      val middle = sorted.size / 2
      val median =
        if sorted.size % 2 == 1 then sorted(middle)
        else ((BigInt(sorted(middle - 1)) + BigInt(sorted(middle))) / 2).toLong
      Some(LongSummary(median, sorted.head, sorted.last))
    }

  private[millbuild] def summarizeDoubles(values: Seq[Double]): Option[DoubleSummary] = {
    val finite = values.filter(_.isFinite)
    if finite.isEmpty then None
    else {
      val sorted = finite.sorted
      val middle = sorted.size / 2
      val median =
        if sorted.size % 2 == 1 then sorted(middle)
        else ((BigDecimal(sorted(middle - 1)) + BigDecimal(sorted(middle))) / BigDecimal(2)).toDouble
      Some(DoubleSummary(median, sorted.head, sorted.last))
    }
  }

  def aggregate(cases: Seq[StrategyTrialRecord], records: Seq[WorkerRecord]): Seq[AggregateMetrics] = {
    val uniqueCases   = deduplicateCases(cases)
    val uniqueRecords = deduplicateRecords(records)
    uniqueCases
      .groupBy(value => (value.profile, value.platform, value.strategy))
      .toSeq
      .sortBy { case ((profile, platform, strategy), _) =>
        (
          profile.name,
          (
            profile.memoryGiB,
            profile.reserveGiB,
            profile.heapGiB,
            profile.millJobs,
            profile.maxChildren,
            profile.batchSize,
            profile.timeoutMinutes
          ),
          platform.token,
          strategy.token
        )
      }
      .map { case ((profile, platform, strategy), groupedCases) =>
        val successfulCases = groupedCases.filter(_.outcome == Outcome.Succeeded)
        val workerMetrics   = successfulCases.flatMap { strategyTrial =>
          val matching = uniqueRecords.filter(record =>
            record.runId == strategyTrial.runId &&
              record.profile == strategyTrial.profile &&
              record.platform == strategyTrial.platform &&
              record.strategy == strategyTrial.strategy &&
              record.trial == strategyTrial.trial &&
              record.strategyPosition == strategyTrial.strategyPosition
          )
          Option.when(matching.nonEmpty) {
            val startupMillis   = matching.iterator.map(_.startupMillis.toDouble).sum
            val phaseWallMillis = matching.iterator.flatMap(_.phases).map(_.wallMillis.toDouble).sum
            val gcMillis        = matching.iterator.flatMap(_.phases).map(_.gcMillis.toDouble).sum
            TrialWorkerMetrics(
              startupShare = Option.when(startupMillis + phaseWallMillis > 0.0)(
                startupMillis / (startupMillis + phaseWallMillis)
              ),
              throughputTargetsPerMinute = Option.when(strategyTrial.wallMillis > 0L)(
                strategyTrial.targetsCompleted.toDouble * 60000.0 / strategyTrial.wallMillis
              ),
              peakChildRssKiB = matching.iterator.map(_.peakRssKiB).max,
              peakHeapBytes = matching.iterator.flatMap(_.phases).map(_.peakHeapBytes).maxOption.getOrElse(0L),
              gcShare = Option.when(phaseWallMillis > 0.0)(gcMillis / phaseWallMillis)
            )
          }
        }
        AggregateMetrics(
          profile = profile,
          platform = platform,
          strategy = strategy,
          succeeded = groupedCases.count(_.outcome == Outcome.Succeeded),
          failed = groupedCases.count(_.outcome == Outcome.Failed),
          timedOut = groupedCases.count(_.outcome == Outcome.TimedOut),
          cancelled = groupedCases.count(_.outcome == Outcome.Cancelled),
          wallMillis = summarizeLong(successfulCases.map(_.wallMillis)),
          peakAggregateRssKiB = summarizeLong(successfulCases.map(_.peakAggregateRssKiB)),
          startupShare = summarizeDoubles(workerMetrics.flatMap(_.startupShare)),
          throughputTargetsPerMinute = summarizeDoubles(workerMetrics.flatMap(_.throughputTargetsPerMinute)),
          peakChildRssKiB = summarizeLong(workerMetrics.map(_.peakChildRssKiB)),
          peakHeapBytes = summarizeLong(workerMetrics.map(_.peakHeapBytes)),
          gcShare = summarizeDoubles(workerMetrics.flatMap(_.gcShare))
        )
      }
  }

  private val HomePath: Regex         = raw"/(?:Users|home)/[^/\s]+".r
  private val SecretAssignment: Regex =
    raw"(?i)\b([A-Z][A-Z0-9_]*)(\s*=\s*)([^\s]+)".r
  private val SecretSuffixes = Seq("TOKEN", "PASSWORD", "SECRET", "KEY")

  def redact(value: String): String = redact(value, sys.env)

  def redact(value: String, environment: Map[String, String]): String = {
    val withoutAssignments = SecretAssignment.replaceAllIn(
      value,
      matched =>
        if SecretSuffixes.exists(matched.group(1).toUpperCase(Locale.ROOT).endsWith) then
          s"${matched.group(1)}${matched.group(2)}<redacted>"
        else matched.matched
    )
    val withoutHomes = HomePath.replaceAllIn(
      withoutAssignments,
      matched =>
        if matched.matched.startsWith("/Users/") then "/Users/<redacted>" else "/home/<redacted>"
    )
    environment.iterator
      .filter { case (name, secret) =>
        secret.nonEmpty && SecretSuffixes.exists(name.toUpperCase(Locale.ROOT).endsWith)
      }
      .map(_._2)
      .toSeq
      .distinct
      .sortBy(secret => -secret.length)
      .foldLeft(withoutHomes)((text, secret) => text.replace(secret, "<redacted>"))
  }

  def sanitize(result: BenchmarkResult): BenchmarkResult = sanitize(result, sys.env)

  def sanitize(result: BenchmarkResult, environment: Map[String, String]): BenchmarkResult = {
    def clean(value: String): String            = redact(value, environment)
    def cleanProfile(profile: Profile): Profile = profile.copy(name = clean(profile.name))
    result.copy(
      configuration = result.configuration.copy(
        preset = clean(result.configuration.preset),
        profile = cleanProfile(result.configuration.profile),
        targetFilter = result.configuration.targetFilter.map(clean)
      ),
      cases =
        result.cases.map(record => record.copy(runId = clean(record.runId), profile = cleanProfile(record.profile))),
      records = result.records.map(record =>
        record.copy(
          runId = clean(record.runId),
          profile = cleanProfile(record.profile),
          targets = record.targets.map(clean),
          phases = record.phases.map(phase => phase.copy(name = clean(phase.name))),
          detail = record.detail.map(clean),
          runtime = record.runtime.copy(
            javaVersion = clean(record.runtime.javaVersion),
            pinnedMillVersion = clean(record.runtime.pinnedMillVersion),
            operatingSystem = clean(record.runtime.operatingSystem),
            architecture = clean(record.runtime.architecture),
            outputDirectoryIdentity = clean(record.runtime.outputDirectoryIdentity)
          )
        )
      ),
      preparations = result.preparations.map(record =>
        record.copy(
          runId = clean(record.runId),
          profile = cleanProfile(record.profile),
          targets = record.targets.map(clean)
        )
      )
    )
  }

  private def normalized(result: BenchmarkResult): BenchmarkResult =
    result.copy(
      cases = deduplicateCases(result.cases),
      records = deduplicateRecords(result.records),
      preparations = deduplicatePreparations(result.preparations)
    )

  def renderJson(result: BenchmarkResult): String = renderJson(result, sys.env)

  def renderJson(result: BenchmarkResult, environment: Map[String, String]): String =
    write(sanitize(normalized(result), environment))

  def renderConfigurationJson(configuration: ResolvedBenchmarkConfiguration): String =
    renderConfigurationJson(configuration, sys.env)

  def renderConfigurationJson(
      configuration: ResolvedBenchmarkConfiguration,
      environment: Map[String, String]
  ): String =
    write(sanitize(BenchmarkResult(configuration, 0L, Seq.empty, Seq.empty), environment).configuration, indent = 2)

  private def renderLong(value: Option[LongSummary]): String =
    value.fold("n/a")(summary => s"${summary.median} [${summary.min}-${summary.max}]")

  private def renderDouble(value: Option[DoubleSummary]): String =
    value.fold("n/a")(summary =>
      String.format(
        Locale.ROOT,
        "%.3f [%.3f-%.3f]",
        Double.box(summary.median),
        Double.box(summary.min),
        Double.box(summary.max)
      )
    )

  private def tableCell(value: String): String =
    value.replace("\\", "\\\\").replace("|", "\\|").replace('\r', ' ').replace('\n', ' ')

  def renderMarkdown(result: BenchmarkResult): String = renderMarkdown(result, sys.env)

  def renderMarkdown(result: BenchmarkResult, environment: Map[String, String]): String = {
    val uniqueResult = normalized(result)
    val safeResult   = sanitize(uniqueResult, environment)
    val rawRows      = safeResult.records
      .sortBy(record =>
        (
          (
            record.trial,
            record.strategyPosition,
            record.platform.token,
            record.strategy.token,
            record.lane,
            record.batch
          ),
          record.runId,
          (
            record.profile.name,
            record.profile.memoryGiB,
            record.profile.reserveGiB,
            record.profile.heapGiB,
            record.profile.millJobs,
            record.profile.maxChildren,
            record.profile.batchSize,
            record.profile.timeoutMinutes
          )
        )
      )
      .map { record =>
        val phaseWallMillis = record.phases.iterator.map(_.wallMillis).sum
        s"| ${record.trial} | ${record.strategyPosition} | ${tableCell(record.platform.token)} | ${tableCell(record.strategy.token)} | ${tableCell(record.profile.name)} | ${tableCell(record.outcome.toString.toLowerCase(Locale.ROOT))} | $phaseWallMillis | ${record.peakRssKiB} |"
      }
    val strategyRows = safeResult.cases
      .sortBy(record => (record.trial, record.strategyPosition, record.platform.token, record.strategy.token))
      .map { record =>
        s"| ${record.trial} | ${record.strategyPosition} | ${tableCell(record.platform.token)} | ${tableCell(record.strategy.token)} | ${tableCell(record.profile.name)} | ${tableCell(record.outcome.toString.toLowerCase(Locale.ROOT))} | ${record.wallMillis} | ${record.peakAggregateRssKiB} | ${record.targetsCompleted} |"
      }
    val preparationRows = safeResult.preparations
      .sortBy(record => (record.trial, record.platform.token, record.strategy.token, record.lane))
      .map { record =>
        s"| ${record.trial} | ${tableCell(record.platform.token)} | ${tableCell(record.strategy.token)} | ${tableCell(record.profile.name)} | ${record.lane} | ${tableCell(record.outcome.toString.toLowerCase(Locale.ROOT))} | ${record.wallMillis} | ${record.targets.size} |"
      }
    val aggregateRows = aggregate(uniqueResult.cases, uniqueResult.records).map { metrics =>
      val profileName = redact(metrics.profile.name, environment)
      s"| ${tableCell(profileName)} | ${tableCell(metrics.platform.token)} | ${tableCell(metrics.strategy.token)} | ${metrics.succeeded} | ${metrics.failed} | ${metrics.timedOut} | ${metrics.cancelled} | ${renderLong(metrics.wallMillis)} | ${renderLong(metrics.peakAggregateRssKiB)} | ${renderDouble(metrics.startupShare)} | ${renderDouble(metrics.throughputTargetsPerMinute)} | ${renderLong(metrics.peakChildRssKiB)} | ${renderLong(metrics.peakHeapBytes)} | ${renderDouble(metrics.gcShare)} |"
    }
    val markdown = Seq(
      "# Linker benchmark report",
      "",
      s"Preset: ${tableCell(safeResult.configuration.preset)}",
      s"Profile: ${tableCell(safeResult.configuration.profile.name)}; memory: ${safeResult.configuration.profile.memoryGiB} GiB; reserve: ${safeResult.configuration.profile.reserveGiB} GiB; heap: ${safeResult.configuration.profile.heapGiB} GiB; Mill jobs: ${safeResult.configuration.profile.millJobs}; max children: ${safeResult.configuration.profile.maxChildren}; batch size: ${safeResult.configuration.profile.batchSize}; timeout: ${safeResult.configuration.profile.timeoutMinutes} minutes",
      s"Platforms: ${safeResult.configuration.platforms.map(_.token).mkString(", ")}",
      s"Strategies: ${safeResult.configuration.strategies.map(_.token).mkString(", ")}",
      s"Trials: ${safeResult.configuration.settings.trials}; order seed: ${safeResult.configuration.settings.orderSeed}",
      s"Target filter: ${safeResult.configuration.targetFilter.fold("none")(tableCell)}; target limit: ${safeResult.configuration.targetLimit.fold("none")(_.toString)}",
      s"Continue on failure: ${safeResult.configuration.continueOnFailure}",
      "",
      s"Whole-run peak aggregate RSS: ${safeResult.peakAggregateRssKiB} KiB",
      "",
      "## Strategy trials",
      "",
      "| Trial | Position | Platform | Strategy | Profile | Outcome | Wall ms | Peak aggregate RSS KiB | Targets completed |",
      "| ---: | ---: | --- | --- | --- | --- | ---: | ---: | ---: |"
    ) ++ strategyRows ++ Seq(
      "",
      "## Preparation",
      "",
      "| Trial | Platform | Strategy | Profile | Lane | Outcome | Wall ms | Targets |",
      "| ---: | --- | --- | --- | ---: | --- | ---: | ---: |"
    ) ++ preparationRows ++ Seq(
      "",
      "## Worker records",
      "",
      "| Trial | Position | Platform | Strategy | Profile | Outcome | Worker phase wall ms | Peak child RSS KiB |",
      "| ---: | ---: | --- | --- | --- | --- | ---: | ---: |"
    ) ++ rawRows ++ Seq(
      "",
      "## Aggregates",
      "",
      "| Profile | Platform | Strategy | Succeeded | Failed | Timed out | Cancelled | Successful wall ms median [min-max] | Aggregate RSS KiB median [min-max] | Worker-time startup share median [min-max] | Targets/min median [min-max] | Child RSS KiB median [min-max] | Peak heap bytes median [min-max] | Worker-time GC share median [min-max] |",
      "| --- | --- | --- | ---: | ---: | ---: | ---: | --- | --- | --- | --- | --- | --- | --- |"
    ) ++ aggregateRows
    redact(markdown.mkString("\n") + "\n", environment)
  }

  val defaultEvaluationSettings: EvaluationSettings = EvaluationSettings(3, 0L)

  val smokeEnvironmentRemovals: Set[String] = Set(
    "JAVA_OPTS",
    "JDK_JAVA_OPTIONS",
    "JAVA_TOOL_OPTIONS",
    "_JAVA_OPTIONS",
    // Mill propagates this with its os-lib relativizer base. In a nested invocation it names the outer task's
    // sandbox alias; keeping it makes the physical MILL_OUTPUT_DIR recursively resolve through mill-workspace.
    "MILL_WORKSPACE_ROOT"
  )

  private val OneGiBBytes   = 1024L * 1024L * 1024L
  private val HeapTolerance = 256L * 1024L * 1024L
  private val SevenGiBBytes = 7L * OneGiBBytes
  private val NineGiBBytes  = 9L * OneGiBBytes

  def validatePinnedHeapProbe(
      requestedHeapBytes: Long,
      effectiveHeapBytes: Long,
      requestedHeapHonored: Boolean
  ): Either[String, Long] =
    if requestedHeapBytes != OneGiBBytes then Left("heap probe must request one GiB")
    else if requestedHeapHonored then Left("pinned eight GiB heap must override the one GiB request")
    else if effectiveHeapBytes < SevenGiBBytes || effectiveHeapBytes > NineGiBBytes then
      Left("effective heap must be within the seven-to-nine GiB tolerance")
    else Right(effectiveHeapBytes)

  def validateSmokeHeapProbe(record: SmokeRecord, profile: Profile): Either[String, Int] =
    validatePinnedHeapProbe(
      record.requestedHeapBytes,
      record.effectiveMaxHeapBytes,
      record.requestedHeapHonored
    ).flatMap(_ => validateEffectiveHeap(profile, record.effectiveMaxHeapBytes))

  def validateEffectiveHeap(profile: Profile, effectiveMaxHeapBytes: Long): Either[String, Int] =
    validate(profile).flatMap { _ =>
      if effectiveMaxHeapBytes <= 0L then Left("effective max heap bytes must be positive")
      else {
        val effectiveHeapGiB = (BigInt(effectiveMaxHeapBytes) + BigInt(OneGiBBytes - 1L)) / BigInt(OneGiBBytes)
        if effectiveHeapGiB > BigInt(Int.MaxValue) then Left("effective max heap is too large")
        else {
          val requiredGiB = effectiveHeapGiB * BigInt(profile.maxChildren) + BigInt(profile.reserveGiB)
          if requiredGiB > BigInt(profile.memoryGiB) then
            Left("effective child heaps and reserve exceed available memory")
          else Right(effectiveHeapGiB.toInt)
        }
      }
    }

  def validateConfiguredHeap(profile: Profile, effectiveMaxHeapBytes: Long): Either[String, Int] = {
    val requestedBytes = BigInt(profile.heapGiB) * BigInt(OneGiBBytes)
    val effectiveBytes = BigInt(effectiveMaxHeapBytes)
    if profile.heapGiB <= 0 then Left("configured heap GiB must be greater than zero")
    else if effectiveMaxHeapBytes <= 0L then Left("effective max heap bytes must be positive")
    else if effectiveBytes < requestedBytes - BigInt(HeapTolerance) ||
      effectiveBytes > requestedBytes + BigInt(HeapTolerance)
    then Left("effective max heap differs from the configured heap by more than 256 MiB")
    else validateEffectiveHeap(profile, effectiveMaxHeapBytes)
  }

  def canonicalPhysicalPath(path: os.Path): Either[String, Path] = {
    val absolute = path.wrapped.toAbsolutePath.normalize()

    @tailrec def loop(candidate: Path, missing: List[String]): Either[String, Path] =
      if Files.exists(candidate, LinkOption.NOFOLLOW_LINKS) then
        try Right(missing.foldLeft(candidate.toRealPath())((current, segment) => current.resolve(segment)).normalize())
        catch {
          case NonFatal(error) => Left(s"cannot resolve physical path: ${error.getClass.getSimpleName}")
        }
      else {
        val parent = candidate.getParent
        if parent == null then Left("cannot resolve physical path: no existing ancestor")
        else loop(parent, candidate.getFileName.toString :: missing)
      }

    loop(absolute, Nil)
  }

  def resolveSmokeWorkspace(
      environment: Map[String, String],
      currentWorkingDirectory: os.Path
  ): Either[String, os.Path] = {
    def repositoryRoot(candidate: os.Path): Option[os.Path] =
      canonicalPhysicalPath(candidate).toOption.flatMap { physicalCandidate =>
        @tailrec def loop(path: Path): Option[Path] =
          if Files.isRegularFile(path.resolve("mill")) &&
            Files.isRegularFile(path.resolve(".mill-version")) &&
            Files.isRegularFile(path.resolve("build.mill"))
          then Some(path)
          else
            Option(path.getParent) match {
              case Some(parent) => loop(parent)
              case None         => None
            }
        loop(physicalCandidate)
      }.map(os.Path(_))

    val candidates =
      environment.get("MILL_WORKSPACE_ROOT").filter(_.nonEmpty).map(os.Path(_, currentWorkingDirectory)).toSeq :+
        currentWorkingDirectory
    candidates.iterator.flatMap(repositoryRoot).nextOption().toRight(
      "cannot resolve the physical repository from MILL_WORKSPACE_ROOT or the current working directory"
    )
  }

  def resolveSmokeTemporaryBase(value: String, workspace: os.Path): Either[String, os.Path] = {
    val trimmed = value.trim
    if trimmed.isEmpty then Left("java.io.tmpdir must not be empty")
    else {
      val lexical = os.Path(os.Path(trimmed, workspace).wrapped.toAbsolutePath.normalize())
      Either.cond(
        Files.isDirectory(lexical.wrapped),
        lexical,
        "java.io.tmpdir must name an existing directory"
      )
    }
  }

  def resolveSmokeTemporaryBase(
      javaTemporaryDirectory: String,
      environmentTemporaryDirectory: Option[String],
      workspace: os.Path
  ): Either[String, os.Path] =
    for {
      javaBase <- resolveSmokeTemporaryBase(javaTemporaryDirectory, workspace)
      result   <- environmentTemporaryDirectory match {
        case None        => Right(javaBase)
        case Some(value) =>
          for {
            environmentBase      <- resolveSmokeTemporaryBase(value, workspace)
            canonicalJava        <- canonicalPhysicalPath(javaBase)
            canonicalEnvironment <- canonicalPhysicalPath(environmentBase)
            selected             <- Either.cond(
              canonicalEnvironment == canonicalJava,
              environmentBase,
              "TMPDIR and java.io.tmpdir must resolve to the same physical directory"
            )
          } yield selected
      }
    } yield result

  def smokeChildOutputDirectoryValue(lane: os.Path): String =
    lane.wrapped.toAbsolutePath.normalize().toString

  def createSmokeTemporaryRunRoot(temporaryBase: os.Path): os.Path = {
    val allocated = Files.createTempDirectory(temporaryBase.wrapped, "morphir-linker-smoke-")
    os.Path.pathSerializer.withValue(LexicalPathSerializer)(os.Path(allocated))
  }

  private object LexicalPathSerializer extends os.Path.Serializer {
    override def serializeString(path: os.Path): String     = path.wrapped.toString
    override def serializeFile(path: os.Path): java.io.File = path.wrapped.toFile
    override def serializePath(path: os.Path): Path         = path.wrapped
    override def deserialize(value: String): Path           = java.nio.file.Paths.get(value)
    override def deserialize(value: java.io.File): Path     = value.toPath
    override def deserialize(value: Path): Path             = value
    override def deserialize(value: java.net.URI): Path     = java.nio.file.Paths.get(value)
  }

  def validatePhysicalDescendant(base: os.Path, target: os.Path, message: String): Either[String, Path] =
    for {
      canonicalBase   <- canonicalPhysicalPath(base)
      canonicalTarget <- canonicalPhysicalPath(target)
      result          <- Either.cond(
        canonicalTarget != canonicalBase && canonicalTarget.startsWith(canonicalBase),
        canonicalTarget,
        message
      )
    } yield result

  def validateSmokeRunDirectory(workspace: os.Path, smokeBase: os.Path, smokeRun: os.Path): Either[String, Path] = {
    val fixedRelativeBase = java.nio.file.Paths.get(".dev", ".sdlc", "mill-jvm-worker-pool", "out", "smoke")
    val workspaceLexical  = workspace.wrapped.toAbsolutePath.normalize()
    val baseLexical       = smokeBase.wrapped.toAbsolutePath.normalize()
    val runLexical        = smokeRun.wrapped.toAbsolutePath.normalize()
    val expectedBase      = workspaceLexical.resolve(fixedRelativeBase).normalize()

    def containsSymlink: Boolean =
      if !runLexical.startsWith(workspaceLexical) then true
      else {
        val relative = workspaceLexical.relativize(runLexical)
        relative.iterator().asScala
          .scanLeft(workspaceLexical)((current, segment) => current.resolve(segment))
          .exists(Files.isSymbolicLink)
      }

    if baseLexical != expectedBase then Left("smoke base must be the generated workspace smoke directory")
    else if containsSymlink then Left("smoke run directory must not contain symbolic links")
    else
      for {
        canonicalWorkspace <- canonicalPhysicalPath(workspace)
        canonicalBase      <- canonicalPhysicalPath(smokeBase)
        canonicalRun       <- validatePhysicalDescendant(
          smokeBase,
          smokeRun,
          "smoke run directory must be below the generated smoke directory"
        )
        expectedPhysicalBase = canonicalWorkspace.resolve(fixedRelativeBase).normalize()
        result <- Either.cond(
          canonicalBase == expectedPhysicalBase,
          canonicalRun,
          "smoke base resolves outside the generated workspace smoke directory"
        )
      } yield result
  }

  private val ChildOutputDirectoryCollision =
    "benchmark child output directory must differ from the orchestrator output directory"

  def validateChildOutputDirectory(orchestrator: os.Path, child: os.Path): Either[String, os.Path] =
    for {
      canonicalOrchestrator <- canonicalPhysicalPath(orchestrator)
      canonicalChild        <- canonicalPhysicalPath(child)
      result                <- Either.cond(
        canonicalOrchestrator != canonicalChild,
        child,
        ChildOutputDirectoryCollision
      )
    } yield result

  def validateExternalChildOutputDirectory(workspace: os.Path, child: os.Path): Either[String, os.Path] =
    for {
      canonicalWorkspace <- canonicalPhysicalPath(workspace)
      canonicalChild     <- canonicalPhysicalPath(child)
      result             <- Either.cond(
        !canonicalChild.startsWith(canonicalWorkspace),
        child,
        "benchmark child output directory must be outside the workspace"
      )
    } yield result

  def validateExternalSmokeCleanupRoot(
      workspace: os.Path,
      temporaryBase: os.Path,
      runRoot: os.Path
  ): Either[String, Path] = {
    val runLexical = runRoot.wrapped.toAbsolutePath.normalize()
    val name       = Option(runLexical.getFileName).fold("")(_.toString)
    if Files.isSymbolicLink(runLexical) then Left("external smoke cleanup root must not be a symbolic link")
    else if !Files.isDirectory(runLexical, LinkOption.NOFOLLOW_LINKS) then
      Left("external smoke cleanup root must be an existing directory")
    else if !name.startsWith("morphir-linker-smoke-") || name == "morphir-linker-smoke-" then
      Left("external smoke cleanup root must have the generated smoke prefix")
    else
      for {
        canonicalWorkspace <- canonicalPhysicalPath(workspace)
        canonicalBase      <- canonicalPhysicalPath(temporaryBase)
        canonicalRun       <- canonicalPhysicalPath(runRoot)
        result             <- Either.cond(
          canonicalRun.getParent == canonicalBase && !canonicalRun.startsWith(canonicalWorkspace),
          canonicalRun,
          "external smoke cleanup root must be a direct temporary child outside the workspace"
        )
      } yield result
  }

  def validate(profile: Profile): Either[String, Profile] =
    if profile.memoryGiB <= 0 then Left("memoryGiB must be greater than zero")
    else if profile.reserveGiB < 0 then Left("reserveGiB must be nonnegative")
    else if profile.heapGiB <= 0 then Left("heapGiB must be greater than zero")
    else if profile.millJobs <= 0 then Left("millJobs must be greater than zero")
    else if profile.maxChildren <= 0 then Left("maxChildren must be greater than zero")
    else if profile.batchSize <= 0 then Left("batchSize must be greater than zero")
    else if profile.timeoutMinutes <= 0 then Left("timeoutMinutes must be greater than zero")
    else if profile.maxChildren.toLong * profile.heapGiB + profile.reserveGiB > profile.memoryGiB then
      Left("child heaps and reserve exceed available memory")
    else Right(profile)

  def validate(settings: EvaluationSettings): Either[String, EvaluationSettings] =
    if settings.trials <= 0 then Left("trials must be greater than zero")
    else Right(settings)

  def strategyOrders(strategies: Seq[Strategy], settings: EvaluationSettings): Seq[Seq[Strategy]] = {
    val canonical = strategies.distinct.sortBy(_.ordinal)
    if canonical.isEmpty || validate(settings).isLeft then Seq.empty
    else {
      val seedOffset = Math.floorMod(settings.orderSeed, canonical.size.toLong).toInt
      (0 until settings.trials).map { trial =>
        val offset = (seedOffset + trial % canonical.size) % canonical.size
        canonical.drop(offset) ++ canonical.take(offset)
      }
    }
  }

  def plan(targets: Seq[String], lanes: Int, batchSize: Int): Either[String, WorkPlan] =
    if targets.isEmpty then Left("targets must not be empty")
    else if lanes <= 0 then Left("lanes must be greater than zero")
    else if batchSize <= 0 then Left("batchSize must be greater than zero")
    else {
      val batches = targets.distinct.sorted.grouped(batchSize).zipWithIndex.map { case (batchTargets, index) =>
        Batch(lane = index % lanes, index = index, targets = batchTargets)
      }.toSeq.sortBy(batch => (batch.lane, batch.index))
      Right(WorkPlan(lanes, batches))
    }

  def strategyPlan(strategy: Strategy, targets: Seq[String], profile: Profile): Either[String, WorkPlan] =
    validate(profile).flatMap { admitted =>
      strategy match {
        case Strategy.LongLived => plan(targets, lanes = 1, batchSize = targets.distinct.size)
        case Strategy.Fresh     => plan(targets, lanes = admitted.maxChildren, batchSize = 1)
        case Strategy.Recycled  => plan(targets, lanes = admitted.maxChildren, batchSize = admitted.batchSize)
      }
    }

  def preparationLanes(plan: WorkPlan): Seq[PreparationLane] =
    plan.batches
      .groupBy(_.lane)
      .toSeq
      .sortBy(_._1)
      .map { case (lane, batches) => PreparationLane(lane, batches.sortBy(_.index)) }

  def millChildArguments(profile: Profile, arguments: Seq[String]): Seq[String] =
    Seq("--ticker", "false", "--no-daemon", "-j", profile.millJobs.toString) ++ arguments

  def runConcurrentLanes[A](
      parallelism: Int,
      tasks: Seq[() => A],
      cleanupTimeout: FiniteDuration
  ): Seq[A] = {
    require(parallelism > 0, "parallelism must be positive")
    require(cleanupTimeout > Duration.Zero, "cleanup timeout must be positive")
    if tasks.isEmpty then Seq.empty
    else {
      val executor   = Executors.newFixedThreadPool(math.min(parallelism, tasks.size))
      val completion = new ExecutorCompletionService[(Int, A)](executor)
      val futures    = tasks.zipWithIndex.map { case (task, index) =>
        completion.submit(new Callable[(Int, A)] {
          override def call(): (Int, A) = index -> task()
        })
      }
      def takeCompleted(): Either[(Throwable, Boolean), (Int, A)] =
        try Right(completion.take().get())
        catch {
          case error: InterruptedException => Left(error -> true)
          case error: ExecutionException   => Left(Option(error.getCause).getOrElse(error) -> false)
          case NonFatal(error)             => Left(error -> false)
        }

      @tailrec def loop(
          remaining: Int,
          results: Vector[Option[A]]
      ): (Vector[Option[A]], Option[Throwable], Boolean) =
        if remaining == 0 then (results, None, false)
        else
          takeCompleted() match {
            case Right((index, value))         => loop(remaining - 1, results.updated(index, Some(value)))
            case Left((error, wasInterrupted)) => (results, Some(error), wasInterrupted)
          }

      val (results, initialPrimary, initiallyInterrupted) =
        loop(tasks.size, Vector.fill(tasks.size)(None))
      var primary     = initialPrimary.orNull
      var interrupted = initiallyInterrupted
      if primary != null then {
        futures.foreach(_.cancel(true))
        executor.shutdownNow()
      } else executor.shutdown()

      def awaitTermination(timeout: FiniteDuration): Boolean = {
        val deadline                 = System.nanoTime() + timeout.toNanos
        @tailrec def loop(): Boolean = {
          val remainingNanos = deadline - System.nanoTime()
          if remainingNanos <= 0L then executor.isTerminated
          else
            try executor.awaitTermination(remainingNanos, TimeUnit.NANOSECONDS)
            catch {
              case error: InterruptedException =>
                interrupted = true
                if primary == null then primary = error else primary.addSuppressed(error)
                executor.shutdownNow()
                loop()
            }
        }
        loop()
      }

      val terminated        = awaitTermination(cleanupTimeout)
      val finallyTerminated =
        if terminated then true
        else {
          futures.foreach(_.cancel(true))
          executor.shutdownNow()
          awaitTermination(cleanupTimeout)
        }
      if !finallyTerminated then {
        val cleanupFailure = IllegalStateException("concurrent lane executor did not terminate after cancellation")
        if primary == null then primary = cleanupFailure else primary.addSuppressed(cleanupFailure)
      }
      if interrupted then Thread.currentThread().interrupt()
      if primary != null then throw primary
      results.iterator.map(_.getOrElse(throw IllegalStateException("concurrent lane result is missing"))).toSeq
    }
  }

  def writeTextAtomically(path: os.Path, value: String): Unit =
    writeTextAtomically(
      path,
      value,
      (temporary, target) =>
        try
          Files.move(temporary, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
        catch {
          case error: AtomicMoveNotSupportedException =>
            throw new Exception(s"atomic move is not supported for ${path.last}", error)
        }
    )

  private[millbuild] def writeTextAtomically(
      path: os.Path,
      value: String,
      move: (Path, Path) => Unit
  ): Unit = {
    val parent = path.toNIO.toAbsolutePath.normalize().getParent
    Files.createDirectories(parent)
    val temporary = Files.createTempFile(parent, s".${path.last}.tmp-", ".part")
    try {
      Files.writeString(temporary, value, StandardCharsets.UTF_8)
      move(temporary, path.toNIO)
    } catch {
      case NonFatal(error) =>
        Files.deleteIfExists(temporary)
        throw error
    }
  }
}
