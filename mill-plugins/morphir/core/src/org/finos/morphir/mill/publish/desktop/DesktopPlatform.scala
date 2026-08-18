package org.finos.morphir.mill.publish.desktop

/** Archive format a desktop distribution ships in. Both are accepted by the toolchain plugin's extractor. */
enum ArchiveKind(val ext: String) {
  case Zip   extends ArchiveKind("zip")
  case TarGz extends ArchiveKind("tar.gz")
}

/**
 * One publishable desktop target.
 *
 * Tokens follow Mill's own native-launcher naming (`mill-dist-native-mac-aarch64`), so a downloader can build the same
 * filename tail whether it fetches from Maven Central or from a GitHub Release. `builderOs` and `builderArch` are
 * electron-builder's spelling, which differs from the canonical token.
 */
enum DesktopPlatform(
    val token: String,
    val builderOs: String,
    val builderArch: String,
    val archive: ArchiveKind,
    val installerExts: Seq[String]
) {
  case MacAarch64 extends DesktopPlatform("mac-aarch64", "mac", "arm64", ArchiveKind.Zip, Seq("dmg"))
  case MacAmd64   extends DesktopPlatform("mac-amd64", "mac", "x64", ArchiveKind.Zip, Seq("dmg"))
  case LinuxAmd64
      extends DesktopPlatform("linux-amd64", "linux", "x64", ArchiveKind.TarGz, Seq("AppImage", "deb"))
  case LinuxAarch64
      extends DesktopPlatform("linux-aarch64", "linux", "arm64", ArchiveKind.TarGz, Seq("AppImage", "deb"))
  case WinAmd64 extends DesktopPlatform("win-amd64", "win", "x64", ArchiveKind.Zip, Seq("exe"))

  /** Maven artifactId, and the leading part of every asset name for this platform. */
  def artifactName: String = s"${DesktopPlatform.ProductName}-$token"

  /** Archive extension first, then installer extensions. */
  def allExts: Seq[String] = archive.ext +: installerExts
}

object DesktopPlatform {
  val ProductName = "morphir-desktop"

  def fromToken(token: String): Option[DesktopPlatform] = values.find(_.token == token)

  def fromBuilder(os: String, arch: String): Option[DesktopPlatform] =
    values.find(platform => platform.builderOs == os && platform.builderArch == arch)

  /** Every token, comma-joined in declaration order — the default for a `--platforms` CLI option. */
  val AllTokens: String = values.toSeq.map(_.token).mkString(",")

  /**
   * Parses a comma-separated token list (e.g. `linux-amd64,win-amd64`) into the platforms it names, in the
   * order given. Every token is trimmed before matching. An empty overall string, a blank token (a stray or
   * doubled comma), or a token that names no platform is rejected, naming every valid token so the caller
   * does not have to look them up elsewhere.
   */
  def parseTokens(csv: String): Either[String, Seq[DesktopPlatform]] = {
    val tokens = csv.split(",", -1).toSeq.map(_.trim)
    if (tokens.exists(_.isEmpty))
      Left(
        s"desktop platform token list must not contain empty tokens: '$csv' (valid tokens: ${values.map(_.token).mkString(", ")})"
      )
    else {
      val unknown = tokens.filter(fromToken(_).isEmpty)
      if (unknown.nonEmpty)
        Left(
          s"unknown desktop platform token(s): ${unknown.mkString(", ")} (valid tokens: ${values.map(_.token).mkString(", ")})"
        )
      else Right(tokens.map(token => fromToken(token).get))
    }
  }
}
