//| scalaVersion: 3.8.4
//| moduleDeps: [SquireEnv.scala, SquireProcess.scala]

import java.nio.file.{Files, LinkOption}
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*
import kyo.*
import scala.util.matching.Regex

object SquireDoctor:
  final case class Finding(area: String, code: String, message: String, blocked: Boolean)
  final case class DoctorReport(findings: Chunk[Finding]):
    def finding(area: String): Maybe[Finding] =
      findings.find(_.area == area) match
        case Some(finding) => Present(finding)
        case None          => Absent

  def run(root: Path, runner: ProcessRunner, platform: SquireEnv.Platform = SquireEnv.LivePlatform): DoctorReport < (Async & Sync & Abort[SquireError]) =
    SquireEnv.check(SquireEnv.CheckKind.VarFolders, 1.seconds, platform).map { varFoldersWritable =>
      DoctorReport(
        Chunk(
          daemonFinding(root, platform),
          setupFinding(root),
          mainClassFinding(root),
          millMorphirFinding(root),
          acquisitionCacheFinding(platform),
          metabuildFinding(root),
          jvmTempFinding(platform, varFoldersWritable)
        )
      )
    }

  private def daemonFinding(root: Path, platform: SquireEnv.Platform): Finding =
    daemonPort(root) match
      case Absent =>
        Finding(
          "mill_daemon",
          "NO_DAEMON",
          "could not determine mill daemon port (not running or no log yet); plain ./mill will start one, or use ./mill --no-server",
          false
        )
      case Present(port) =>
        platform.probeDaemon(port) match
          case SquireEnv.DaemonProbe.Open =>
            Finding("mill_daemon", "PORT_OPEN", s"mill daemon on port $port responds to a JVM socket", false)
          case SquireEnv.DaemonProbe.Sandbox(detail) =>
            Finding("mill_daemon", "SANDBOX", s"JVM socket is blocked on mill daemon port $port: $detail; use ./mill --no-server or ./morphir-local", true)
          case SquireEnv.DaemonProbe.Refused(detail) =>
            Finding("mill_daemon", "REFUSED", s"mill daemon port $port is not accepting connections: $detail; plain ./mill will restart it", false)

  private def setupFinding(root: Path): Finding =
    val setup = root / ".config" / "mise" / "tasks" / "setup"
    val packageJson = root / "package.json"
    if Files.exists(setup.toJava) && Files.readString(setup.toJava).contains("bun install --ignore-scripts") &&
        (!Files.exists(packageJson.toJava) || !rootMorphirElmDependency(Files.readString(packageJson.toJava))) then
      Finding("setup", "OK", "setup leaves Morphir Elm provisioning to Mill", false)
    else if Files.exists(setup.toJava) then
      Finding("setup", "MISSING", "setup must use bun install --ignore-scripts and leave Morphir Elm provisioning to Mill", true)
    else Finding("setup", "NOT_FOUND", ".config/mise/tasks/setup does not exist", true)

  private def rootMorphirElmDependency(json: String): Boolean =
    """(?s)"devDependencies"\s*:\s*\{.*?"morphir-elm"\s*:""".r.findFirstIn(json).nonEmpty

  private def mainClassFinding(root: Path): Finding =
    val packageYaml = root / "morphir" / "package.mill.yaml"
    val packageMill = root / "morphir" / "package.mill"
    val taskWrapper = "(?s)Task\\s*\\{\\s*Some\\s*\\(".r
    if Files.exists(packageYaml.toJava) && "(?m)^\\s*mainClass:\\s*\\S+\\s*$".r
        .findFirstIn(Files.readString(packageYaml.toJava)).nonEmpty then
      Finding("main_class_task", "OK", "mainClass is configured in morphir/package.mill.yaml", false)
    else if Files.exists(packageMill.toJava) && taskWrapper.findFirstIn(Files.readString(packageMill.toJava)).nonEmpty then
      Finding("main_class_task", "OK", "mainClass is wrapped as Task in morphir/package.mill", false)
    else if Files.exists(packageMill.toJava) then
      Finding("main_class_task", "MISSING", "Task wrapper for mainClass is missing from morphir/package.mill — assembly will warn", true)
    else Finding("main_class_task", "NOT_FOUND", "mainClass is missing from morphir/package.mill.yaml", true)

  private val PluginModules = List("toolchain", "javascript", "elm-tooling", "core", "elm", "integration")

  private def millMorphirFinding(root: Path): Finding =
    val pluginRoot = root / "mill-plugins" / "morphir"
    val packageMill = pluginRoot / "package.mill"
    val packageText = if Files.isRegularFile(packageMill.toJava) then Files.readString(packageMill.toJava) else ""
    val missing = PluginModules.filter { name =>
      val declaration = if name.contains('-') then s"`$name`" else name
      !Files.isDirectory((pluginRoot / name).toJava) ||
      s"(?m)^object\\s+${java.util.regex.Pattern.quote(declaration)}\\s+extends\\b".r.findFirstIn(packageText).isEmpty
    }
    val wiring = List(
      packageMill -> List("publishLocalTestRepo", "publishedPluginRepositories"),
      (pluginRoot / "integration" / "test" / "src" / "org" / "finos" / "morphir" / "mill" / "PublishedPluginIntegrationTests.scala") -> List("COURSIER_REPOSITORIES", "millExecutable"),
      (pluginRoot / "integration" / "resources" / "published-consumer" / "build.mill") -> List("MORPHIR_PUBLISHED_TEST_REPOSITORIES")
    )
    val wiringOk = wiring.forall { case (path, markers) =>
      Files.isRegularFile(path.toJava) && markers.forall(Files.readString(path.toJava).contains)
    }
    if missing.nonEmpty then
      Finding("mill_morphir", "MISSING", s"Mill Morphir plugin modules are missing: ${missing.mkString(", ")}; verify ./mill resolve 'mill-plugins.morphir.__'", true)
    else if !wiringOk then
      Finding("mill_morphir", "MISSING", "task-local plugin repository resolution is not wired; verify ./mill mill-plugins.morphir.integration.test", true)
    else Finding("mill_morphir", "OK", "Mill Morphir plugin modules and task-local repository wiring are present", false)

  private def acquisitionCacheFinding(platform: SquireEnv.Platform): Finding =
    val configured = platform.environment.get("MORPHIR_NODE_CACHE").filter(_.nonEmpty)
    val path = configured.map(Path(_)).getOrElse(defaultAcquisitionCacheRoot(platform))
    if !path.toJava.isAbsolute then
      Finding("acquisition_cache", "INVALID", s"MORPHIR_NODE_CACHE must be absolute: $path", true)
    else if environmentEnabled(platform.environment.get("MORPHIR_NODE_DISABLE_MACHINE_CACHE")) then
      Finding("acquisition_cache", "DISABLED", "Morphir machine acquisition cache is disabled; verified downloads remain task-local", false)
    else
      val inspection = inspectCache(path / "sha256")
      inspection.corrupt match
        case Some(entry) =>
          Finding("acquisition_cache", "CORRUPT", s"corrupt acquisition cache entry: $entry; reacquire online with MORPHIR_NODE_OFFLINE=false", true)
        case None if inspection.bounded.nonEmpty =>
          Finding("acquisition_cache", "NOTICE", s"acquisition cache diagnostic was bounded: ${inspection.bounded.mkString("; ")}", false)
        case None => Finding("acquisition_cache", "OK", s"acquisition cache has no corrupt content: $path", false)

  private def absoluteEnvironmentPath(platform: SquireEnv.Platform, name: String): Option[Path] =
    platform.environment.get(name).filter(_.nonEmpty).map(Path(_)).filter(_.toJava.isAbsolute)

  private def defaultAcquisitionCacheRoot(platform: SquireEnv.Platform): Path =
    if platform.isMacOS then platform.home / "Library" / "Caches" / "morphir-scala"
    else if platform.isWindows then
      absoluteEnvironmentPath(platform, "LOCALAPPDATA")
        .getOrElse(platform.home / "AppData" / "Local") / "morphir-scala" / "Cache"
    else
      absoluteEnvironmentPath(platform, "XDG_CACHE_HOME")
        .getOrElse(platform.home / ".cache") / "morphir-scala"

  private def environmentEnabled(value: Option[String]): Boolean =
    value.exists(rendered => Set("1", "true", "yes", "on").contains(rendered.toLowerCase))

  private final case class CacheInspection(corrupt: Option[Path], bounded: List[String])

  private val CacheDiagnosticMaxEntries     = 256
  private val CacheDiagnosticMaxEntryBytes  = 64L * 1024 * 1024
  private val CacheDiagnosticMaxHashedBytes = 256L * 1024 * 1024

  private def inspectCache(digestRoot: Path): CacheInspection =
    if !Files.isDirectory(digestRoot.toJava, LinkOption.NOFOLLOW_LINKS) then CacheInspection(None, Nil)
    else
      try
        val stream = Files.list(digestRoot.toJava)
        val entries = try stream.iterator.asScala.take(CacheDiagnosticMaxEntries + 1).toList
        finally stream.close()
        val bounded = scala.collection.mutable.ListBuffer.empty[String]
        if entries.size > CacheDiagnosticMaxEntries then
          bounded += s"directory entry limit reached ($CacheDiagnosticMaxEntries)"
        var hashedBytes = 0L
        var corrupt     = Option.empty[Path]
        entries.take(CacheDiagnosticMaxEntries).iterator.takeWhile(_ => corrupt.isEmpty).foreach { entry =>
          if entry.getFileName.toString.matches("[0-9a-f]{64}") then
            try
              if !Files.isRegularFile(entry, LinkOption.NOFOLLOW_LINKS) then corrupt = Some(Path(entry.toString))
              else
                val size = Files.size(entry)
                if size > CacheDiagnosticMaxEntryBytes then bounded += s"oversized entry: ${Path(entry.toString)}"
                else if size > CacheDiagnosticMaxHashedBytes - hashedBytes then
                  bounded += s"total hash budget reached ($CacheDiagnosticMaxHashedBytes bytes)"
                else
                  sha256(entry, CacheDiagnosticMaxHashedBytes - hashedBytes) match
                    case Some((actual, bytesRead)) =>
                      hashedBytes += bytesRead
                      if actual != entry.getFileName.toString then corrupt = Some(Path(entry.toString))
                    case None => bounded += s"unreadable or changed during inspection: ${Path(entry.toString)}"
            catch
              case scala.util.control.NonFatal(_) =>
                bounded += s"unreadable or changed during inspection: ${Path(entry.toString)}"
        }
        CacheInspection(corrupt, bounded.toList.take(8))
      catch
        case scala.util.control.NonFatal(_) =>
          CacheInspection(None, List(s"unreadable or changed during inspection: $digestRoot"))

  private def sha256(path: java.nio.file.Path, maxBytes: Long): Option[(String, Long)] =
    try
      val digest = MessageDigest.getInstance("SHA-256")
      val stream = Files.newInputStream(path, java.nio.file.StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS)
      try
        val buffer = Array.ofDim[Byte](1024 * 1024)
        var total  = 0L
        var read   = stream.read(buffer)
        while read >= 0 do
          if read > 0 then
            total += read
            if total > maxBytes then return None
            digest.update(buffer, 0, read)
          read = stream.read(buffer)
        Some(digest.digest().map(byte => f"${byte & 0xff}%02x").mkString -> total)
      finally stream.close()
    catch case scala.util.control.NonFatal(_) => None

  private def metabuildFinding(root: Path): Finding =
    val sourceCandidates = List(root / "build.mill", root / ".mill-version").filter(path => Files.isRegularFile(path.toJava)) ++
      walkFiles(root / "mill-build" / "src") ++ walkNamed(root, Set("package.mill", "package.mill.yaml"))
    val compiled = walkFiles(root / "out" / "mill-build" / "compile.dest")
    if compiled.isEmpty then Finding("metabuild", "NOTICE", "Mill metabuild has no compiled output yet", false)
    else if sourceCandidates.nonEmpty && sourceCandidates.map(lastModified).max > compiled.map(lastModified).max then
      Finding("metabuild", "STALE", "Mill metabuild compilation is stale; verify ./mill resolve 'mill-plugins.morphir.__'", true)
    else Finding("metabuild", "OK", "Mill metabuild compilation is current", false)

  private def walkFiles(root: Path): List[Path] =
    if !Files.isDirectory(root.toJava) then Nil
    else
      val stream = Files.walk(root.toJava)
      try stream.iterator.asScala.filter(Files.isRegularFile(_)).map(path => Path(path.toString)).toList
      finally stream.close()

  private def walkNamed(root: Path, names: Set[String]): List[Path] =
    val stream = Files.walk(root.toJava)
    try stream.iterator.asScala.filter(path => Files.isRegularFile(path) && names.contains(path.getFileName.toString))
      .filterNot(path => path.iterator.asScala.exists(part => Set(".git", ".dev", "out").contains(part.toString)))
      .map(path => Path(path.toString)).toList
    finally stream.close()

  private def lastModified(path: Path): Long = Files.getLastModifiedTime(path.toJava).toMillis

  private def jvmTempFinding(platform: SquireEnv.Platform, writable: Boolean): Finding =
    val rendered = platform.jvmTempDirectory.map(_.toString).getOrElse("unavailable")
    if writable then Finding("jvm_temp", "OK", s"JVM temp directory is writable: $rendered", false)
    else
      Finding(
        "jvm_temp",
        "BLOCKED",
        s"JVM temp directory is not writable: $rendered; recheck with JAVA_TOOL_OPTIONS=-Djava.io.tmpdir=<writable-temp> squire ai env info --check var-folders and retry Cellar with --temp-directory <writable-temp>",
        true
      )

  private def daemonPort(root: Path): Maybe[Int] =
    socketPort(root).orElse(serverLogPort(root)) match
      case Some(port) => Present(port)
      case None       => Absent

  private def socketPort(root: Path): Option[Int] =
    val portFile = root / "out" / "mill-daemon" / "socketPort"
    if Files.exists(portFile.toJava) then Files.readString(portFile.toJava).trim.toIntOption else None

  private def serverLogPort(root: Path): Option[Int] =
    val log = root / "out" / "mill-daemon" / "server.log"
    if Files.exists(log.toJava) then
      "listening on port (\\d+)".r.findAllMatchIn(Files.readString(log.toJava)).map(_.group(1).toInt).toList.lastOption
    else None
