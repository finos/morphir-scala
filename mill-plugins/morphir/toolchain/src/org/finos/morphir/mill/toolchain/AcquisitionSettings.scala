package org.finos.morphir.mill.toolchain

import java.util.Locale
import scala.util.Try

enum ArchiveFormat {
  case TarGz, Zip
}

final case class AcquisitionSettings(
    cacheRoot: Option[os.Path] = None,
    useMachineCache: Boolean = true,
    offline: Boolean = false
)

object AcquisitionSettings {
  def defaultCacheRoot: os.Path = {
    val userHome = os.Path(System.getProperty("user.home"))
    val osName   = System.getProperty("os.name", "").toLowerCase(Locale.ROOT)
    if (osName.startsWith("mac") || osName == "darwin") userHome / "Library" / "Caches" / "morphir-scala"
    else if (osName.startsWith("windows")) {
      val localAppData = absoluteEnvironmentPath("LOCALAPPDATA")
      localAppData.getOrElse(userHome / "AppData" / "Local") / "morphir-scala" / "Cache"
    } else {
      val xdgCacheHome = absoluteEnvironmentPath("XDG_CACHE_HOME")
      xdgCacheHome.getOrElse(userHome / ".cache") / "morphir-scala"
    }
  }

  private def absoluteEnvironmentPath(name: String): Option[os.Path] =
    Option(System.getenv(name))
      .filter(_.nonEmpty)
      .flatMap(value => Try(os.Path(value)).toOption)
      .filter(_.toNIO.isAbsolute)
}

final case class VerifiedContent(path: os.Path, sha256: String)
