package org.finos.millmorphir.toolchain

import upickle.default.{ReadWriter, macroRW, readwriter}

enum ArchiveFormat derives ReadWriter {
  case TarGz, Zip
}

final case class NodeDistribution(
    version: String,
    archiveName: String,
    sha256: String,
    format: ArchiveFormat,
    nodeRelativePath: os.RelPath,
    npmCliRelativePath: os.RelPath
)

object NodeDistribution {
  private given ReadWriter[os.RelPath] = readwriter[String].bimap(_.toString, os.RelPath(_))
  given ReadWriter[NodeDistribution]   = macroRW

  val Version = "24.19.0"

  private val Checksums = Map(
    ("darwin", "arm64") -> "8294b7aa9b03997481c06babf1e8b270c859358f27da57a11509afe537ac381d",
    ("darwin", "x64")   -> "d1b5e999db158c62fe8f7267a4476b035d8bd93b1a605bac24a3f0dd166e3316",
    ("linux", "arm64")  -> "d28c8a5bf0a808f0ed434a1dce8c54ae98f0371c0bd86ac58abc613f73e6643f",
    ("linux", "x64")    -> "f625d97cd707df4ff96254916fbc5ff014f09c09effe5a1e0ca8f6d41a8789d4",
    ("win", "arm64")    -> "8502f4a50b458d4cc38ed8f2001556c2cd239d464920f74017926ccb1e1c157f",
    ("win", "x64")      -> "57f71ab3652e797d84acddc79c81cc9ff1c6ddb2a1974cdb83f00fee9bff4c73"
  )

  def resolve(osName: String, osArch: String): Either[String, NodeDistribution] =
    for {
      operatingSystem <- normalizeOperatingSystem(osName)
      architecture    <- normalizeArchitecture(osArch)
      checksum        <- Checksums.get((operatingSystem, architecture)).toRight(
        s"Node $Version is not available for operating system '$osName' and architecture '$osArch'"
      )
    } yield {
      val isWindows = operatingSystem == "win"
      val extension = if (isWindows) "zip" else "tar.gz"
      NodeDistribution(
        version = Version,
        archiveName = s"node-v$Version-$operatingSystem-$architecture.$extension",
        sha256 = checksum,
        format = if (isWindows) ArchiveFormat.Zip else ArchiveFormat.TarGz,
        nodeRelativePath = if (isWindows) os.rel / "node.exe" else os.rel / "bin" / "node",
        npmCliRelativePath =
          if (isWindows) os.rel / "node_modules" / "npm" / "bin" / "npm-cli.js"
          else os.rel / "lib" / "node_modules" / "npm" / "bin" / "npm-cli.js"
      )
    }

  private def normalizeOperatingSystem(value: String): Either[String, String] =
    value.toLowerCase(java.util.Locale.ROOT) match {
      case name if name == "darwin" || name == "macos" || name.startsWith("mac os") => Right("darwin")
      case name if name.startsWith("linux")                                         => Right("linux")
      case name if name.startsWith("windows") || name == "win"                      => Right("win")
      case _ => Left(s"Unsupported operating system for Node $Version: '$value'")
    }

  private def normalizeArchitecture(value: String): Either[String, String] =
    value.toLowerCase(java.util.Locale.ROOT) match {
      case "amd64" | "x86_64"  => Right("x64")
      case "aarch64" | "arm64" => Right("arm64")
      case _                   => Left(s"Unsupported architecture for Node $Version: '$value'")
    }
}
