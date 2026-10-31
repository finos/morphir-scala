package millbuild

import java.io.{BufferedInputStream, InputStream}
import java.net.URI
import java.nio.file.{Files, Paths, StandardCopyOption}
import java.nio.file.attribute.PosixFilePermissions
import java.util.Locale
import java.util.zip.ZipFile

import scala.annotation.tailrec
import scala.util.Using

import mill.*
import mill.api.BuildCtx
import mill.scalalib.JavaModule
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import org.finos.morphir.mill.toolchain.{AcquisitionCache, AcquisitionSettings, ArchiveFormat}
import upickle.default.{ReadWriter, macroRW, readwriter}

/**
 * The finos/morphir CLI release this build runs the Morphir Compatibility Kit with, pinned by version and by the
 * SHA-256 of each platform's archive as published on the GitHub release. The kit is embedded in the CLI, so this pin is
 * also the kit version. Moving it means updating `Version` and every checksum together.
 */
final case class MorphirCliDistribution(
    version: String,
    target: String,
    archiveName: String,
    sha256: String,
    format: ArchiveFormat,
    executableName: String
) {
  def url: String = s"https://github.com/finos/morphir/releases/download/v$version/$archiveName"
}

object MorphirCliDistribution {
  private given ReadWriter[ArchiveFormat] =
    readwriter[String].bimap(_.toString, ArchiveFormat.valueOf)
  given ReadWriter[MorphirCliDistribution] = macroRW

  val Version = "0.4.0-beta.10"

  /** The largest executable accepted from an archive; the published binaries are under 100 MiB. */
  private val MaxExecutableBytes = 512L * 1024L * 1024L

  /** Rust target triple to the SHA-256 of `morphir-<Version>-<target>.(tgz|zip)`. */
  private val Checksums = Map(
    "aarch64-apple-darwin"      -> "98635ec74fbdc3c2cef1d4ae184bb1c5ed9e2960575f3d8abb4b97e0e4da363f",
    "x86_64-apple-darwin"       -> "4080dcfb04f6e1c902bce2780756c6af44320ac0dd6ff6bd953ca74a0da8e128",
    "aarch64-unknown-linux-gnu" -> "76a92a11030cd254d2283e98363cf203f86aaecd583fa480bdf6f508c86710f6",
    "x86_64-unknown-linux-gnu"  -> "3f6dc0559e7570d9cdb9c95761333552593da6bc8c788b9da3bb4a1d93101923",
    "aarch64-pc-windows-msvc"   -> "f3634261a8ccf55d5a8b862cc7d7ecf683b5237bf4f3a47119b46d1e5b93210f",
    "x86_64-pc-windows-msvc"    -> "c178002d570f4cb8c9ceed4bf7b51f04d4ebec2458765935839c90ac1cc01176"
  )

  def resolve(osName: String, osArch: String): Either[String, MorphirCliDistribution] =
    for {
      operatingSystem <- operatingSystemOf(osName)
      architecture    <- architectureOf(osArch)
      target = s"$architecture-$operatingSystem"
      checksum <- Checksums.get(target).toRight(s"morphir $Version is not published for $target")
    } yield {
      val windows = operatingSystem.endsWith("windows-msvc")
      MorphirCliDistribution(
        version = Version,
        target = target,
        archiveName = s"morphir-$Version-$target.${if (windows) "zip" else "tgz"}",
        sha256 = checksum,
        format = if (windows) ArchiveFormat.Zip else ArchiveFormat.TarGz,
        executableName = if (windows) "morphir.exe" else "morphir"
      )
    }

  private def operatingSystemOf(value: String): Either[String, String] =
    value.toLowerCase(Locale.ROOT) match {
      case name if name == "darwin" || name.startsWith("mac") => Right("apple-darwin")
      case name if name.startsWith("linux")                   => Right("unknown-linux-gnu")
      case name if name.startsWith("windows")                 => Right("pc-windows-msvc")
      case _ => Left(s"Unsupported operating system for morphir $Version: '$value'")
    }

  private def architectureOf(value: String): Either[String, String] =
    value.toLowerCase(Locale.ROOT) match {
      case "amd64" | "x86_64"  => Right("x86_64")
      case "aarch64" | "arm64" => Right("aarch64")
      case _                   => Left(s"Unsupported architecture for morphir $Version: '$value'")
    }

  /**
   * Downloads the archive through the verified acquisition cache, which checks it against the pinned SHA-256, and
   * copies its one executable into `taskRoot`.
   *
   * The release archive holds the executable at its root, with no enclosing directory, which `VerifiedArchive.extract`
   * does not accept: it expects one root directory and strips it. Only the named regular file is copied; every other
   * entry is ignored, so no archive path is ever written to disk.
   */
  def provision(distribution: MorphirCliDistribution, settings: AcquisitionSettings, taskRoot: os.Path): os.Path = {
    val url     = URI.create(distribution.url).toURL
    val content = AcquisitionCache(settings, taskRoot / "acquisition").acquire(distribution.sha256, url.toString) {
      val connection = url.openConnection()
      connection.setConnectTimeout(30000)
      connection.setReadTimeout(60000)
      new BufferedInputStream(connection.getInputStream)
    }
    val executable = taskRoot / "bin" / distribution.executableName
    os.makeDir.all(executable / os.up)
    distribution.format match {
      case ArchiveFormat.TarGz => copyTarEntry(content.path, distribution.executableName, executable)
      case ArchiveFormat.Zip   => copyZipEntry(content.path, distribution.executableName, executable)
    }
    if (!scala.util.Properties.isWin)
      Files.setPosixFilePermissions(executable.toNIO, PosixFilePermissions.fromString("rwxr-xr-x"))
    executable
  }

  private def copyTarEntry(archive: os.Path, name: String, target: os.Path): Unit =
    Using.resource(
      new TarArchiveInputStream(
        new GzipCompressorInputStream(new BufferedInputStream(Files.newInputStream(archive.toNIO)))
      )
    ) { input =>
      @tailrec def loop(): Unit =
        Option(input.getNextEntry) match {
          case None                                                                   => missing(name, archive)
          case Some(entry) if entry.isFile && entry.getName.stripPrefix("./") == name => copyBounded(input, target)
          case Some(_)                                                                => loop()
        }
      loop()
    }

  private def copyZipEntry(archive: os.Path, name: String, target: os.Path): Unit =
    Using.resource(new ZipFile(archive.toIO)) { zip =>
      Option(zip.getEntry(name)).filterNot(_.isDirectory) match {
        case Some(entry) => Using.resource(zip.getInputStream(entry))(copyBounded(_, target))
        case None        => missing(name, archive)
      }
    }

  private def missing(name: String, archive: os.Path): Nothing =
    throw new IllegalStateException(s"$archive has no '$name' at its root")

  private def copyBounded(input: InputStream, target: os.Path): Unit = {
    val copied = Files.copy(
      new BoundedInputStream(input, MaxExecutableBytes),
      target.toNIO,
      StandardCopyOption.REPLACE_EXISTING
    )
    if (copied > MaxExecutableBytes) throw new IllegalStateException(s"$target exceeds $MaxExecutableBytes bytes")
  }

  /** Reads at most one byte past `limit`, so an oversized entry is detected without being read in full. */
  private final class BoundedInputStream(underlying: InputStream, limit: Long) extends InputStream {
    private var remaining    = limit + 1
    override def read(): Int =
      if (remaining <= 0) -1
      else {
        val byte = underlying.read()
        if (byte >= 0) remaining -= 1
        byte
      }
    override def read(buffer: Array[Byte], offset: Int, length: Int): Int =
      if (remaining <= 0) -1
      else {
        val count = underlying.read(buffer, offset, math.min(length.toLong, remaining).toInt)
        if (count > 0) remaining -= count
        count
      }
  }
}

/**
 * Runs the IR suite of the Morphir Compatibility Kit against this module's `launcher` with the pinned morphir CLI.
 *
 * The CLI is acquired the way the build acquires Node: downloaded once into the machine cache (`MORPHIR_TOOLS_CACHE`,
 * or the platform default) and verified against its pinned SHA-256 before it is used. `MORPHIR_TOOLS_OFFLINE` or Mill's
 * `--offline` refuses the download.
 */
trait MorphirMckRunner extends JavaModule {
  def morphirCliCacheRoot: T[String] = Task.Input(Task.env.getOrElse("MORPHIR_TOOLS_CACHE", ""))

  def morphirCliOffline: T[Boolean] = Task.Input {
    Task.offline || Task.env.get("MORPHIR_TOOLS_OFFLINE").exists(Set("1", "true", "yes", "on"))
  }

  def morphirCliDistribution: T[MorphirCliDistribution] = Task {
    MorphirCliDistribution
      .resolve(System.getProperty("os.name"), System.getProperty("os.arch"))
      .fold(message => throw new IllegalArgumentException(message), identity)
  }

  /** The verified `morphir` executable. */
  def morphirCli: T[PathRef] = Task {
    val cacheRoot = Option(morphirCliCacheRoot()).filter(_.nonEmpty).map { value =>
      val path = Paths.get(value)
      if (!path.isAbsolute)
        throw new IllegalArgumentException(s"MORPHIR_TOOLS_CACHE must be an absolute path: '$value'")
      os.Path(path)
    }
    PathRef(
      MorphirCliDistribution.provision(
        morphirCliDistribution(),
        AcquisitionSettings(cacheRoot, useMachineCache = true, offline = morphirCliOffline()),
        Task.dest
      )
    )
  }

  /** The binding's allowed-failing baseline for `morphir mck report check`: `{"cases": [<case id>, …]}`. */
  def mckAllowedFailing: T[PathRef] = Task.Source(moduleDir / "allowed-failing.json")

  /**
   * Runs `morphir mck run --suite ir` against [[launcher]] with the kit embedded in the pinned CLI, writes
   * `report.json` and `report.html` under `reportDir` (relative to the workspace), then gates on `morphir mck report
   * check` against [[mckAllowedFailing]].
   *
   * The run is not `--strict`: a v3-only adapter has every v4 case skipped for its capabilities, and `--strict` fails
   * on any skip. `report check` is the gate. It verifies the record inventory against the kit, that every skip has a
   * capability or `pending` reason, that the failing cases are exactly the baseline, and that at least one case passed.
   */
  def mckRun(reportDir: String = ".dev/mck") = Task.Command {
    val cli      = morphirCli().path
    val adapter  = launcher().path
    val baseline = mckAllowedFailing().path
    val reports  = os.Path(reportDir, BuildCtx.workspaceRoot)
    val report   = reports / "report.json"
    val html     = reports / "report.html"
    val outDir   = Task.dest / "morphir-out"
    BuildCtx.withFilesystemCheckerDisabled {
      os.makeDir.all(reports)
      os.remove(report)
      os.remove(html)
      // Inside a task an os.Path renders relative to Mill's daemon directory; a child process needs it absolute.
      def host(path: os.Path): String      = path.toNIO.toAbsolutePath.normalize.toString
      def morphir(arguments: String*): Int =
        os.proc(host(cli) +: arguments)
          .call(cwd = Task.dest, check = false, stdout = os.Inherit, stderr = os.Inherit)
          .exitCode
      val runExit = morphir(
        "mck",
        "run",
        "--no-banner",
        "--suite",
        "ir",
        "--adapter",
        host(adapter),
        "--report",
        host(report),
        "--html",
        host(html),
        "--out-dir",
        host(outDir)
      )
      Task.log.info(s"morphir mck run exited $runExit; reports in $reportDir")
      if (runExit == 2 || !os.exists(report))
        throw new Exception(s"morphir mck run did not produce a report (exit $runExit)")
      val checkExit =
        morphir("mck", "report", "check", "--no-banner", "--out-dir", host(outDir), host(report), host(baseline))
      if (checkExit != 0)
        throw new Exception(s"morphir mck report check failed (exit $checkExit); see $reportDir/report.html")
      s"$reportDir/report.json"
    }
  }
}
