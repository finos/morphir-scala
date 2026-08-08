//| scalaVersion: 3.8.4
//| moduleDeps: [SquireModel.scala, SquireProcess.scala]

import java.nio.file.Files
import java.time.Instant
import kyo.*

final case class CellarSettings(
    repositories: List[String] = Nil,
    binary: Option[String] = None
) derives Schema

final case class SquireSettings(cellar: CellarSettings = CellarSettings()) derives Schema

enum CellarAction derives CanEqual:
  case Get(
      coordinate: String,
      symbol: String,
      hideInherited: Boolean = false,
      groupInherited: Boolean = false,
      limit: Option[Int] = None
  )
  case Search(coordinate: String, query: String, limit: Option[Int] = None)
  case Deps(coordinate: String)

trait SquirePlatform:
  def findExecutable(name: String): Maybe[String]
  def now: Instant

object SquireExecutableLookup:
  def find(
      name: String,
      pathEntries: List[Path],
      windows: Boolean,
      pathExtensions: List[String],
      isExecutable: Path => Boolean
  ): Maybe[String] =
    val names =
      if windows && !pathExtensions.exists(extension => name.toLowerCase.endsWith(extension.toLowerCase)) then
        pathExtensions.map(extension => name + extension)
      else List(name)
    pathEntries.iterator
      .flatMap(directory => names.iterator.map(directory / _))
      .find(isExecutable) match
      case Some(candidate) => Present(candidate.toString)
      case None            => Absent

object LiveSquirePlatform extends SquirePlatform:
  def findExecutable(name: String): Maybe[String] =
    val windows     = java.lang.System.getProperty("os.name", "").toLowerCase.startsWith("windows")
    val pathEntries = Option(java.lang.System.getenv("PATH")).getOrElse("")
      .split(java.io.File.pathSeparator)
      .iterator
      .filter(_.nonEmpty)
      .map(Path(_))
      .toList
    val pathExtensions =
      if windows then
        Option(java.lang.System.getenv("PATHEXT")).getOrElse(".COM;.EXE;.BAT;.CMD")
          .split(';')
          .iterator
          .filter(_.nonEmpty)
          .toList
      else Nil
    SquireExecutableLookup.find(
      name,
      pathEntries,
      windows,
      pathExtensions,
      candidate => Files.isRegularFile(candidate.toJava) && (windows || Files.isExecutable(candidate.toJava))
    )

  def now: Instant = Instant.now()

object SquireCellar:
  private val aliases = Map(
    "case-app:2.1.0" -> "com.github.alexarchambault:case-app_3:2.1.0",
    "kyo-case-app"   -> "io.getkyo:kyo-case-app_3:1.0.0-RC5",
    "kyo-schema"     -> "io.getkyo:kyo-schema_3:1.0.0-RC5",
    "kyo-zio"        -> "io.getkyo:kyo-zio_3:1.0.0-RC5",
    "zio:2.1.26"     -> "dev.zio:zio_3:2.1.26",
    "zio-cli"        -> "dev.zio:zio-cli_3:0.8.1",
    "mill-scalalib"  -> "com.lihaoyi:mill-scalalib_3:0.12.0",
    "scala3-library" -> "org.scala-lang:scala3-library_3:3.8.4"
  )

  def loadSettings(root: Path): Result[SquireError, CellarSettings] =
    val path = root / ".config" / "squire" / "settings.local.yaml"
    if !Files.exists(path.toJava) then Result.Success(CellarSettings())
    else
      try
        SquireYaml.decode[SquireSettings](Files.readString(path.toJava)) match
          case Result.Success(settings) => Result.Success(settings.cellar)
          case Result.Failure(error)    =>
            Result.Failure(SquireError.Failure("cellar", "could not decode local settings", Present(error.getMessage)))
      catch
        case error: java.io.IOException =>
          Result.Failure(SquireError.Failure("cellar", "could not read local settings", Present(error.getMessage)))

  def repositoryFlags(settings: CellarSettings): Chunk[String] =
    Chunk.from(settings.repositories.flatMap(repository => List("--repository", repository)))

  def resolveCoordinate(coordinate: String): String = aliases.getOrElse(coordinate, coordinate)

  def executable(settings: CellarSettings, platform: SquirePlatform): Result[SquireError, String] =
    settings.binary.filter(_.nonEmpty) match
      case Some(value) => Result.Success(value)
      case None        =>
        platform.findExecutable("cellar") match
          case Present(value) => Result.Success(value)
          case Absent         =>
            Result.Failure(SquireError.Failure("cellar", "cellar not found on PATH"))

  def command(action: CellarAction, settings: CellarSettings, executable: String): ProcessRequest =
    val repositories = repositoryFlags(settings)
    val argv         = action match
      case CellarAction.Get(coordinate, symbol, hideInherited, groupInherited, limit) =>
        Chunk(executable, "get-external") ++ repositories ++ Chunk(resolveCoordinate(coordinate), symbol) ++
          optionFlag(hideInherited, "--hide-inherited") ++
          optionFlag(groupInherited, "--group-inherited") ++
          valueFlag(limit, "--limit")
      case CellarAction.Search(coordinate, query, limit) =>
        Chunk(executable, "search-external") ++ repositories ++ Chunk(resolveCoordinate(coordinate), query) ++
          valueFlag(limit, "--limit")
      case CellarAction.Deps(coordinate) =>
        Chunk(executable, "deps") ++ repositories ++ Chunk(resolveCoordinate(coordinate))
    ProcessRequest(argv)

  def run(
      action: CellarAction,
      root: Path,
      runner: ProcessRunner,
      platform: SquirePlatform
  ): ProcessResult < (Async & Abort[SquireError]) =
    loadSettings(root) match
      case Result.Failure(error)    => Abort.fail(error)
      case Result.Success(settings) =>
        executable(settings, platform) match
          case Result.Failure(error)  => Abort.fail(error)
          case Result.Success(binary) => runner.run(command(action, settings, binary))

  private def optionFlag(enabled: Boolean, flag: String): Chunk[String] =
    if enabled then Chunk(flag) else Chunk.empty

  private def valueFlag(value: Option[Int], flag: String): Chunk[String] =
    value match
      case Some(number) if number != 0 => Chunk(flag, number.toString)
      case _                           => Chunk.empty
