package org.finos.morphir.mill.publish.desktop

/**
 * The filename contract shared by Maven Central and GitHub Releases.
 *
 * Maven lays a release out as `<artifactId>-<version>.<ext>`. Release assets use the identical tail, so a downloader
 * can swap only the base URL between hosts — the property Mill's bootstrap script relies on for
 * `mill-dist-native-<os>-<arch>`.
 */
object DesktopReleaseLayout {
  val ChecksumsFileName = "checksums.txt"

  def assetName(platform: DesktopPlatform, version: String, ext: String): String = {
    require(version.nonEmpty, "version must not be empty")
    require(ext.nonEmpty, "ext must not be empty")
    s"${platform.artifactName}-$version.$ext"
  }

  def archiveName(platform: DesktopPlatform, version: String): String =
    assetName(platform, version, platform.archive.ext)

  def sidecarName(assetName: String): String = s"$assetName.sha256"

  /** `sha256sum` output format, so `sha256sum -c <file>.sha256` verifies without transformation. */
  def sidecarContent(sha256: String, assetName: String): String = s"$sha256  $assetName\n"

  /** Every asset in `sha256sum` format, sorted by name so the file is stable across runs. */
  def checksumsContent(entries: Seq[(String, String)]): String =
    entries.sortBy((name, _) => name).map((name, sha) => s"$sha  $name\n").mkString
}
