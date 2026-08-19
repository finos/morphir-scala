package org.finos.morphir.mill.publish.desktop

/**
 * The day-one pre-publish checks for a canonicalized desktop release directory.
 *
 * Each check is a pure `ReleaseSnapshot => Seq[String]` wrapped in a [[ReleaseCheck]]; [[all]] is the list
 * [[ReleaseVerifier]] runs. Adding a new check is one entry in [[all]] and one test — nothing about how
 * checks run needs to change.
 */
object DesktopReleaseChecks {

  private val ZipMagic: IndexedSeq[Byte]   = IndexedSeq(0x50, 0x4b, 0x03, 0x04)
  private val TarGzMagic: IndexedSeq[Byte] = IndexedSeq(0x1f, 0x8b.toByte)

  /** Magic number expected for each archive extension this package canonicalizes assets into. */
  private val ArchiveMagic: Map[String, IndexedSeq[Byte]] = Map(
    ArchiveKind.Zip.ext   -> ZipMagic,
    ArchiveKind.TarGz.ext -> TarGzMagic
  )

  private val PgpSignatureHeader = "-----BEGIN PGP SIGNATURE-----"

  private def signatureAssetName = s"${DesktopReleaseLayout.ChecksumsFileName}.asc"

  /** Every asset name a [[ReleaseSnapshot]]'s platforms and version are expected to produce. */
  private def expectedAssetNames(snapshot: ReleaseSnapshot): Seq[String] =
    snapshot.platforms.flatMap { platform =>
      (platform.archive.ext +: platform.installerExts).map(ext =>
        DesktopReleaseLayout.assetName(platform, snapshot.version, ext)
      )
    }

  /** True if `fileName` looks like a canonical asset for `platform` — its prefix and one of its extensions. */
  private def isAssetLike(platform: DesktopPlatform, fileName: String): Boolean =
    fileName.startsWith(s"${platform.artifactName}-") && platform.allExts.exists(ext => fileName.endsWith(s".$ext"))

  private def parseChecksumsLine(line: String): Option[(String, String)] = {
    val trimmed = line.trim
    if (trimmed.isEmpty) None
    else
      trimmed.split("  ", 2) match {
        case Array(sha, name) => Some(name -> sha)
        case _                => None
      }
  }

  /**
   * Every asset's `.sha256` sidecar exists, its digest matches the asset's freshly recomputed sha256, and the
   * filename recorded inside it matches the asset's own name.
   *
   * This is the check that closes the job-boundary gap: digests are computed once during canonicalization on
   * one runner, and the assets then cross to another (uploaded as workflow artifacts, downloaded again) before
   * anything publishes them. Recomputing the digest here, from the bytes on disk after that crossing, is what
   * catches corruption in transit — trusting a value carried alongside the file would not.
   */
  val sidecarDigestsMatch: ReleaseCheck = ReleaseCheck(
    "sidecar-digests-match",
    snapshot =>
      expectedAssetNames(snapshot).flatMap { name =>
        snapshot.file(name) match {
          case None => Seq.empty // reported by expectedAssetsPresent
          case Some(asset) =>
            val sidecarName = DesktopReleaseLayout.sidecarName(name)
            snapshot.file(sidecarName) match {
              case None => Seq(s"$sidecarName: missing sidecar for $name")
              case Some(sidecar) =>
                sidecar.text match {
                  case None => Seq(s"$sidecarName: could not be read as text")
                  case Some(text) =>
                    val expected = DesktopReleaseLayout.sidecarContent(asset.sha256, name)
                    if (text == expected) Seq.empty
                    else Seq(s"$sidecarName: does not match $name (recomputed sha256 is ${asset.sha256})")
                }
            }
        }
      }
  )

  /** `checksums.txt` lists every asset once, and nothing that is not an asset. */
  val checksumsCoversAssets: ReleaseCheck = ReleaseCheck(
    "checksums-covers-assets",
    snapshot =>
      snapshot.file(DesktopReleaseLayout.ChecksumsFileName) match {
        case None => Seq(s"${DesktopReleaseLayout.ChecksumsFileName}: missing")
        case Some(checksums) =>
          checksums.text match {
            case None => Seq(s"${DesktopReleaseLayout.ChecksumsFileName}: could not be read as text")
            case Some(text) =>
              val expected    = expectedAssetNames(snapshot).toSet
              val listed      = text.linesIterator.flatMap(parseChecksumsLine).toSeq
              val listedNames = listed.map(_._1)
              val missing     = (expected -- listedNames.toSet).toSeq.sorted
              val extra       = (listedNames.toSet -- expected).toSeq.sorted
              val duplicates =
                listedNames.groupBy(identity).collect { case (name, occurrences) if occurrences.size > 1 => name }.toSeq.sorted
              missing.map(name => s"${DesktopReleaseLayout.ChecksumsFileName}: missing entry for $name") ++
                extra.map(name => s"${DesktopReleaseLayout.ChecksumsFileName}: unexpected entry for $name") ++
                duplicates.map(name => s"${DesktopReleaseLayout.ChecksumsFileName}: duplicate entry for $name")
          }
      }
  )

  /**
   * `checksums.txt.asc` exists, is non-empty, and begins with the PGP signature header.
   *
   * The signature is produced by a workflow step after `canonicalize` runs, not by `canonicalize` itself —
   * see [[defaults]] for how a local run opts out of this one check.
   */
  val signaturePresent: ReleaseCheck = ReleaseCheck(
    "signature-present",
    snapshot =>
      snapshot.file(signatureAssetName) match {
        case None => Seq(s"$signatureAssetName: missing")
        case Some(file) =>
          if (file.size == 0L) Seq(s"$signatureAssetName: empty")
          else
            file.text match {
              case None => Seq(s"$signatureAssetName: could not be read as text")
              case Some(text) =>
                if (text.startsWith(PgpSignatureHeader)) Seq.empty
                else Seq(s"$signatureAssetName: does not begin with '$PgpSignatureHeader'")
            }
      }
  )

  /** No asset is a zero-byte file. */
  val noEmptyAssets: ReleaseCheck = ReleaseCheck(
    "no-empty-assets",
    snapshot =>
      expectedAssetNames(snapshot).flatMap { name =>
        snapshot.file(name).toSeq.collect { case file if file.size == 0L => s"$name: empty file" }
      }
  )

  /**
   * Archive assets start with their format's magic number: `PK\x03\x04` for zip, `\x1f\x8b` for tar.gz.
   *
   * Scoped to archive extensions only — installer formats (dmg, exe, AppImage, deb) vary and are not worth
   * encoding here. This is the check that catches a truncated download or an HTML error page standing in for
   * the real archive.
   */
  val archiveMagicNumbers: ReleaseCheck = ReleaseCheck(
    "archive-magic-numbers",
    snapshot =>
      snapshot.platforms.flatMap { platform =>
        val name  = DesktopReleaseLayout.archiveName(platform, snapshot.version)
        val magic = ArchiveMagic(platform.archive.ext)
        snapshot.file(name).toSeq.flatMap { file =>
          if (file.headBytes.take(magic.length) == magic) Seq.empty
          else Seq(s"$name: does not start with the ${platform.archive.ext} magic number")
        }
      }
  )

  /** Every asset-like file carries the version being published, so a stale release directory cannot ship. */
  val versionInAssetNames: ReleaseCheck = ReleaseCheck(
    "version-in-asset-names",
    snapshot =>
      snapshot.files.flatMap { file =>
        val belongsToARequestedPlatform = snapshot.platforms.exists(platform => isAssetLike(platform, file.name))
        if (belongsToARequestedPlatform && !file.name.contains(snapshot.version))
          Seq(s"${file.name}: does not contain version ${snapshot.version}")
        else Seq.empty
      }
  )

  /**
   * Every platform's archive and each of its declared installers are present under their canonical names.
   *
   * Reads the expected set from [[DesktopPlatform]] rather than a second, hand-maintained list, so it cannot
   * drift from what `canonicalize` actually produces.
   */
  val expectedAssetsPresent: ReleaseCheck = ReleaseCheck(
    "expected-assets-present",
    snapshot =>
      expectedAssetNames(snapshot).flatMap { name =>
        if (snapshot.file(name).isDefined) Seq.empty else Seq(s"$name: expected asset is missing")
      }
  )

  /** Every day-one check, in the order they read best when logged. */
  val all: Seq[ReleaseCheck] = Seq(
    expectedAssetsPresent,
    noEmptyAssets,
    archiveMagicNumbers,
    versionInAssetNames,
    sidecarDigestsMatch,
    checksumsCoversAssets,
    signaturePresent
  )

  /**
   * The checks to run for a given call site.
   *
   * `requireSignature = false` drops [[signaturePresent]]: `canonicalize` never writes `checksums.txt.asc` —
   * a later workflow step signs it — so a local `ci.desktop.all` run (canonicalize immediately followed by
   * verify, with no signing step in between) has no signature to check yet.
   */
  def defaults(requireSignature: Boolean): Seq[ReleaseCheck] =
    if (requireSignature) all else all.filterNot(_.name == signaturePresent.name)
}
