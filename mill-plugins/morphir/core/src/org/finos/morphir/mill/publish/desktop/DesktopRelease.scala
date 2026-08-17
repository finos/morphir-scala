package org.finos.morphir.mill.publish.desktop

import org.finos.morphir.mill.publish.Sha256Digest

/** One canonically named release asset, with the digest published beside it. */
final case class ManifestEntry(name: String, sha256: String, path: os.Path, mavenArtifact: Boolean)

/** Every asset in a canonicalized release, ordered by name. */
final case class ReleaseManifest(version: String, entries: Seq[ManifestEntry])

/**
 * Turns a staging tree of raw per-platform electron-builder output into a canonically named release directory:
 * `<stagingRoot>/<token>/<whatever electron-builder called it>` becomes
 * `<outputDir>/morphir-desktop-<token>-<version>.<ext>` with a `.sha256` sidecar, plus one `checksums.txt` covering
 * every asset.
 *
 * Only archives are flagged as Maven artifacts; installers are GitHub-Releases-only.
 */
object DesktopRelease {
  def canonicalize(
      stagingRoot: os.Path,
      outputDir: os.Path,
      version: String,
      platforms: Seq[DesktopPlatform] = DesktopPlatform.values.toSeq
  ): Either[String, ReleaseManifest] =
    if (version.isEmpty) Left("desktop release version must not be empty")
    else if (platforms.isEmpty) Left("no desktop platforms requested")
    else
      collect(stagingRoot, platforms).map { staged =>
        os.makeDir.all(outputDir)
        val entries = staged.flatMap { case (assets, directory) =>
          val archiveEntry = copyAsset(
            directory / assets.archive,
            outputDir / DesktopReleaseLayout.archiveName(assets.platform, version),
            mavenArtifact = true
          )
          val installerEntries = assets.installers.map { installer =>
            val ext = assets.platform.installerExts
              .find(candidate => installer.endsWith(s".$candidate"))
              .getOrElse(throw new IllegalStateException(s"unclassified installer $installer"))
            copyAsset(
              directory / installer,
              outputDir / DesktopReleaseLayout.assetName(assets.platform, version, ext),
              mavenArtifact = false
            )
          }
          archiveEntry +: installerEntries
        }.sortBy(_.name)

        entries.foreach { entry =>
          os.write.over(
            outputDir / DesktopReleaseLayout.sidecarName(entry.name),
            DesktopReleaseLayout.sidecarContent(entry.sha256, entry.name)
          )
        }
        os.write.over(
          outputDir / DesktopReleaseLayout.ChecksumsFileName,
          DesktopReleaseLayout.checksumsContent(entries.map(entry => entry.name -> entry.sha256))
        )

        ReleaseManifest(version, entries)
      }

  private def collect(
      stagingRoot: os.Path,
      platforms: Seq[DesktopPlatform]
  ): Either[String, Seq[(PlatformAssets, os.Path)]] =
    platforms.foldLeft[Either[String, Seq[(PlatformAssets, os.Path)]]](Right(Seq.empty)) {
      (accumulated, platform) =>
        for {
          already <- accumulated
          directory = stagingRoot / platform.token
          _ <- Either.cond(
            os.isDir(directory),
            (),
            s"${platform.token}: no staged output at $directory"
          )
          names = os.list(directory).filter(os.isFile).map(_.last)
          assets <- DesktopReleaseInventory.classify(platform, names)
        } yield already :+ (assets, directory)
    }

  private def copyAsset(source: os.Path, destination: os.Path, mavenArtifact: Boolean): ManifestEntry = {
    os.copy.over(source, destination, createFolders = true)
    ManifestEntry(destination.last, Sha256Digest.ofFile(destination), destination, mavenArtifact)
  }
}
