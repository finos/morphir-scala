//| scalaVersion: 3.8.4
//| moduleDeps: [SquireModel.scala]

import java.net.{InetAddress, InetSocketAddress, ServerSocket, Socket, SocketException}
import java.nio.file.{Files, Path as JavaPath}
import java.time.Instant
import kyo.*

object SquireEnv:
  enum CheckKind:
    case JvmNetwork, VarFolders

  enum DaemonProbe:
    case Open
    case Sandbox(detail: String)
    case Refused(detail: String)

  final case class CheckResult(ok: Maybe[Boolean], detail: String, durationS: Double) derives Schema

  final case class ClaudeCodeInfo(
      detected: Boolean,
      entrypoint: Maybe[String],
      sessionId: Maybe[String],
      childSession: Boolean
  ) derives Schema

  final case class SettingsSource(path: Maybe[String], present: Boolean) derives Schema

  final case class ClaudeSettings(
      sources: Map[String, SettingsSource],
      sandboxEnabled: Map[String, Maybe[Boolean]],
      networkAllowedDomains: Chunk[String],
      networkDeniedDomains: Chunk[String]
  ) derives Schema

  final case class EnvReport(
      generatedAt: String,
      claudeCode: ClaudeCodeInfo,
      ci: Boolean,
      checks: Map[String, CheckResult],
      sandboxed: Boolean,
      claudeSettings: ClaudeSettings,
      recommendation: Map[String, String]
  ) derives Schema

  trait Platform:
    def environment: Map[String, String]
    def home: Path
    def managedSettingsCandidates: Chunk[Path]
    def varFolders: Path
    def now: Instant
    def probeJvmNetwork(timeout: Duration): CheckResult
    def probeDaemon(port: Int): DaemonProbe
    def writeProbe(path: Path): Unit = Files.writeString(path.toJava, "squire probe")
    def deleteProbe(path: Path): Unit = Files.deleteIfExists(path.toJava)

  object LivePlatform extends Platform:
    def environment: Map[String, String] = java.lang.System.getenv().entrySet().toArray.foldLeft(Map.empty[String, String]) {
      case (values, entry: java.util.Map.Entry[String @unchecked, String @unchecked]) => values.updated(entry.getKey, entry.getValue)
      case (values, _)                                                                  => values
    }

    def home: Path = Path(java.lang.System.getProperty("user.home"))

    def managedSettingsCandidates: Chunk[Path] =
      Chunk(
        Path("/Library/Application Support/ClaudeCode/managed-settings.json"),
        Path("/etc/claude-code/managed-settings.json")
      )

    def varFolders: Path = Path("/var/folders")

    def now: Instant = Instant.now()

    def probeJvmNetwork(timeout: Duration): CheckResult = liveJvmNetwork(timeout)

    def probeDaemon(port: Int): DaemonProbe =
      try
        val socket = new Socket()
        try
          socket.connect(new InetSocketAddress("127.0.0.1", port), timeoutMillis(1.seconds))
          DaemonProbe.Open
        finally socket.close()
      catch
        case error: SecurityException => DaemonProbe.Sandbox(error.getMessage)
        case error: SocketException if Option(error.getMessage).exists(_.toLowerCase.contains("operation not permitted")) =>
          DaemonProbe.Sandbox(error.getMessage)
        case error: java.io.IOException => DaemonProbe.Refused(s"${error.getClass.getSimpleName}: ${error.getMessage}")

  def report(timeout: Duration, platform: Platform = LivePlatform, root: Path = Path(".")): EnvReport < Sync =
    for
      jvmNetwork <- checkResult(CheckKind.JvmNetwork, timeout, platform)
      varFolders <- checkResult(CheckKind.VarFolders, timeout, platform)
      settings   <- readClaudeSettings(platform, root)
    yield
      val sandboxed = jvmNetwork.ok == Present(false)
      EnvReport(
        generatedAt = platform.now.toString,
        claudeCode = ClaudeCodeInfo(
          detected = claudeEnvironmentDetected(platform.environment),
          entrypoint = environmentValue(platform.environment, "CLAUDE_CODE_ENTRYPOINT"),
          sessionId = environmentValue(platform.environment, "CLAUDE_CODE_SESSION_ID"),
          childSession = platform.environment.get("CLAUDE_CODE_CHILD_SESSION").contains("1")
        ),
        ci = platform.environment.get("CI").exists(_.nonEmpty) || platform.environment.get("GITHUB_ACTIONS").exists(_.nonEmpty),
        checks = Map("jvm_network" -> jvmNetwork, "var_folders_writable" -> varFolders),
        sandboxed = sandboxed,
        claudeSettings = settings,
        recommendation = Map("mill_daemon" -> (if sandboxed then "use_no_server" else "ok"))
      )

  def check(kind: CheckKind, timeout: Duration, platform: Platform = LivePlatform): Boolean < Sync =
    checkResult(kind, timeout, platform).map {
      _.ok match
        case Present(false) => false
        case _              => true
    }

  def renderLegacyReport(report: EnvReport): String =
    def nullable(value: Maybe[String]): Structure.Value = value match
      case Present(text) => Structure.Value.Str(text)
      case Absent        => Structure.Value.Null
    def nullableBoolean(value: Maybe[Boolean]): Structure.Value = value match
      case Present(boolean) => Structure.Value.Bool(boolean)
      case Absent           => Structure.Value.Null
    def checkValue(value: CheckResult): Structure.Value = Structure.Value.Record(
      Chunk("ok" -> nullableBoolean(value.ok), "detail" -> Structure.Value.Str(value.detail), "duration_s" -> Structure.Value.Decimal(value.durationS))
    )
    def sourceValue(value: SettingsSource): Structure.Value = Structure.Value.Record(
      Chunk("path" -> nullable(value.path), "present" -> Structure.Value.Bool(value.present))
    )
    def stringArray(values: Chunk[String]): Structure.Value = Structure.Value.Sequence(values.map(Structure.Value.Str(_)))
    def settingSources: Structure.Value = Structure.Value.Record(
      Chunk("managed", "user", "project", "project_local").map(name => name -> sourceValue(report.claudeSettings.sources(name)))
    )
    def sandboxEnabled: Structure.Value = Structure.Value.Record(
      Chunk("managed", "user", "project", "project_local").map(name => name -> nullableBoolean(report.claudeSettings.sandboxEnabled(name)))
    )
    SquireJson.pretty(
      Structure.Value.Record(
        Chunk(
          "generated_at" -> Structure.Value.Str(report.generatedAt),
          "claude_code" -> Structure.Value.Record(Chunk(
            "detected" -> Structure.Value.Bool(report.claudeCode.detected),
            "entrypoint" -> nullable(report.claudeCode.entrypoint),
            "session_id" -> nullable(report.claudeCode.sessionId),
            "child_session" -> Structure.Value.Bool(report.claudeCode.childSession)
          )),
          "ci" -> Structure.Value.Bool(report.ci),
          "checks" -> Structure.Value.Record(Chunk("python_network" -> Structure.Value.Null, "jvm_network" -> checkValue(report.checks("jvm_network")), "var_folders_writable" -> checkValue(report.checks("var_folders_writable")))),
          "sandboxed" -> Structure.Value.Bool(report.sandboxed),
          "claude_settings" -> Structure.Value.Record(Chunk(
            "sources" -> settingSources,
            "sandbox_enabled" -> sandboxEnabled,
            "network_allowed_domains" -> stringArray(report.claudeSettings.networkAllowedDomains),
            "network_denied_domains" -> stringArray(report.claudeSettings.networkDeniedDomains)
          )),
          "recommendation" -> Structure.Value.Record(Chunk("mill_daemon" -> Structure.Value.Str(report.recommendation("mill_daemon"))))
        )
      )
    )

  private def checkResult(kind: CheckKind, timeout: Duration, platform: Platform): CheckResult < Sync =
    kind match
      case CheckKind.JvmNetwork => Sync.defer(platform.probeJvmNetwork(timeout))
      case CheckKind.VarFolders => checkVarFolders(platform)

  private def checkVarFolders(platform: Platform): CheckResult < Sync =
    Sync.defer {
      if !Files.exists(platform.varFolders.toJava) then
        CheckResult(Absent, "/var/folders does not exist on this platform — check skipped", 0.0)
      else
        val probe = platform.varFolders / ".squire-env-probe"
        var result: CheckResult = CheckResult(Present(false), "write probe did not complete", 0.0)
        var cleanupFailure: Maybe[String] = Absent
        try
          platform.writeProbe(probe)
          result = CheckResult(Present(true), "write probe succeeded", 0.0)
        catch
          case error: java.io.IOException => result = CheckResult(Present(false), s"${error.getClass.getSimpleName}: ${error.getMessage}", 0.0)
          case error: SecurityException    => result = CheckResult(Present(false), s"${error.getClass.getSimpleName}: ${error.getMessage}", 0.0)
        finally if Files.exists(probe.toJava) then
          try platform.deleteProbe(probe)
          catch
            case error: java.io.IOException => cleanupFailure = Present(s"could not clean probe file: ${error.getClass.getSimpleName}: ${error.getMessage}")
            case error: SecurityException    => cleanupFailure = Present(s"could not clean probe file: ${error.getClass.getSimpleName}: ${error.getMessage}")
        cleanupFailure match
          case Present(detail) => CheckResult(Present(false), detail, 0.0)
          case Absent          => result
    }

  private def claudeEnvironmentDetected(environment: Map[String, String]): Boolean =
    Chunk("CLAUDECODE", "CLAUDE_CODE_ENTRYPOINT", "CLAUDE_CODE_SESSION_ID", "CLAUDE_CODE_CHILD_SESSION")
      .exists(key => environment.get(key).exists(_.nonEmpty))

  private def environmentValue(environment: Map[String, String], key: String): Maybe[String] =
    environment.get(key).filter(_.nonEmpty) match
      case Some(value) => Present(value)
      case None        => Absent

  private final case class SettingsFile(sandbox: Maybe[SandboxSettings] = Absent) derives Schema
  private final case class SandboxSettings(
      enabled: Maybe[Boolean] = Absent,
      network: Maybe[NetworkSettings] = Absent
  ) derives Schema
  private final case class NetworkSettings(
      allowedDomains: Chunk[String] = Chunk.empty,
      deniedDomains: Chunk[String] = Chunk.empty
  ) derives Schema

  private final case class LoadedSettings(path: Maybe[Path], settings: Maybe[SettingsFile], present: Boolean)

  private def readClaudeSettings(platform: Platform, root: Path): ClaudeSettings < Sync =
    Sync.defer {
      val managedPath = platform.managedSettingsCandidates.find(path => Files.exists(path.toJava))
      val levels = Chunk(
        "managed" -> loadSettings(managedPath match
          case Some(path) => Present(path)
          case None       => Absent
        ),
        "user" -> loadSettings(Present(platform.home / ".claude" / "settings.json")),
        "project" -> loadSettings(Present(root / ".claude" / "settings.json")),
        "project_local" -> loadSettings(Present(root / ".claude" / "settings.local.json"))
      )
      val sources = levels.map { case (name, loaded) =>
        name -> SettingsSource(loaded.path.map(_.toString), loaded.present)
      }.toMap
      val enabled = levels.map { case (name, loaded) => name -> sandbox(loaded).flatMap(_.enabled) }.toMap
      val allowed = levels.flatMap { case (_, loaded) => sandbox(loaded).flatMap(_.network).map(_.allowedDomains).getOrElse(Chunk.empty) }
      val denied  = levels.flatMap { case (_, loaded) => sandbox(loaded).flatMap(_.network).map(_.deniedDomains).getOrElse(Chunk.empty) }
      ClaudeSettings(sources, enabled, Chunk.from(allowed.toSeq.distinct.sorted), Chunk.from(denied.toSeq.distinct.sorted))
    }

  private def sandbox(loaded: LoadedSettings): Maybe[SandboxSettings] =
    loaded.settings.flatMap(_.sandbox)

  private def loadSettings(path: Maybe[Path]): LoadedSettings =
    path match
      case Absent => LoadedSettings(Absent, Absent, false)
      case Present(value) if !Files.exists(value.toJava) => LoadedSettings(Present(value), Absent, false)
      case Present(value) =>
        val content = Files.readString(value.toJava)
        val settings =
          try
            SquireJson.decode[SettingsFile](content) match
              case Result.Success(decoded) => Present(decoded)
              case Result.Failure(_)       => Absent
          catch case _: java.io.IOException => Absent
        LoadedSettings(Present(value), settings, settings.isDefined && content.filterNot(_.isWhitespace) != "{}")

  private def timeoutMillis(timeout: Duration): Int =
    math.max(1L, math.min(Int.MaxValue.toLong, timeout.toJava.toMillis)).toInt

  private def liveJvmNetwork(timeout: Duration): CheckResult =
    val started = java.lang.System.nanoTime()
    try
      val server = new ServerSocket()
      try
        server.bind(new InetSocketAddress(InetAddress.getByName("127.0.0.1"), 0), 1)
        val client = new Socket()
        try
          client.connect(new InetSocketAddress("127.0.0.1", server.getLocalPort), remainingMillis(started, timeout))
          server.setSoTimeout(remainingMillis(started, timeout))
          val accepted = server.accept()
          try CheckResult(Present(true), "loopback bind+accept+connect succeeded", elapsedSeconds(started))
          finally accepted.close()
        finally client.close()
      finally server.close()
    catch
      case _: java.net.SocketTimeoutException => CheckResult(Present(false), s"JVM loopback probe hung past ${timeout.toJava.toMillis}ms timeout — JVM sockets likely blocked by sandbox", elapsedSeconds(started))
      case error: java.io.IOException => CheckResult(Present(false), s"${error.getClass.getSimpleName}: ${error.getMessage}", elapsedSeconds(started))
      case error: SecurityException    => CheckResult(Present(false), s"${error.getClass.getSimpleName}: ${error.getMessage}", elapsedSeconds(started))

  private def elapsedSeconds(started: Long): Double =
    math.round((java.lang.System.nanoTime() - started).toDouble / 1000000.0) / 1000.0

  private def remainingMillis(started: Long, timeout: Duration): Int =
    val elapsed = java.lang.System.nanoTime() - started
    val remaining = timeout.toJava.toMillis - elapsed / 1000000L
    math.max(1L, math.min(Int.MaxValue.toLong, remaining)).toInt
