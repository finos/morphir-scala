//| scalaVersion: 3.8.4
//| moduleDeps: [SquireEnv.scala, SquireProcess.scala]

import java.nio.file.Files
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
          elmToolingFinding(root),
          mainClassFinding(root),
          varFoldersFinding(varFoldersWritable)
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

  private def elmToolingFinding(root: Path): Finding =
    val setup = root / ".config" / "mise" / "tasks" / "setup"
    if Files.exists(setup.toJava) && Files.readString(setup.toJava).contains("ELM_TOOLING_INSTALL") then
      Finding("elm_tooling_guard", "OK", "elm-tooling skip guard present in mise setup task", false)
    else if Files.exists(setup.toJava) then
      Finding("elm_tooling_guard", "MISSING", "elm-tooling guard is missing from .config/mise/tasks/setup", true)
    else Finding("elm_tooling_guard", "NOT_FOUND", ".config/mise/tasks/setup does not exist", true)

  private def mainClassFinding(root: Path): Finding =
    val packageMill = root / "morphir" / "package.mill"
    val taskWrapper = "(?s)Task\\s*\\{\\s*Some\\s*\\(".r
    if Files.exists(packageMill.toJava) && taskWrapper.findFirstIn(Files.readString(packageMill.toJava)).nonEmpty then
      Finding("main_class_task", "OK", "mainClass is wrapped as Task in morphir/package.mill", false)
    else if Files.exists(packageMill.toJava) then
      Finding("main_class_task", "MISSING", "Task wrapper for mainClass is missing from morphir/package.mill — assembly will warn", true)
    else Finding("main_class_task", "NOT_FOUND", "morphir/package.mill does not exist", true)

  private def varFoldersFinding(writable: Boolean): Finding =
    if writable then Finding("var_folders", "OK", "/var/folders is writable; cellar can write temp .tasty files", false)
    else
      Finding(
        "var_folders",
        "BLOCKED",
        "/var/folders is not writable; add /var/folders to sandbox.filesystem.allowWrite in ~/.claude/settings.json and restart Claude Code",
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
