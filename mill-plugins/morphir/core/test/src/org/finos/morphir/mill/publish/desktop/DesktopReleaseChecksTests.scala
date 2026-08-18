package org.finos.morphir.mill.publish.desktop

import org.finos.morphir.mill.publish.Sha256Digest
import utest.*

import java.nio.charset.StandardCharsets

/**
 * Unit tests for [[DesktopReleaseChecks]] and [[ReleaseVerifier]]. Every [[ReleaseSnapshot]] here is built
 * in memory — the checks are pure, so no filesystem access belongs in this suite. [[ReleaseSnapshot.read]]
 * (the one IO boundary) is covered separately in `ReleaseSnapshotTests`.
 */
object DesktopReleaseChecksTests extends TestSuite {
  private val Version   = "0.4.2"
  private val Platforms = Seq(DesktopPlatform.MacAarch64, DesktopPlatform.LinuxAmd64)

  private val ZipMagic   = Array[Byte](0x50, 0x4b, 0x03, 0x04)
  private val TarGzMagic = Array[Byte](0x1f.toByte, 0x8b.toByte)

  private val MacZip     = "morphir-desktop-mac-aarch64-0.4.2.zip"
  private val MacDmg     = "morphir-desktop-mac-aarch64-0.4.2.dmg"
  private val LinuxTarGz = "morphir-desktop-linux-amd64-0.4.2.tar.gz"
  private val LinuxAppImage = "morphir-desktop-linux-amd64-0.4.2.AppImage"
  private val LinuxDeb   = "morphir-desktop-linux-amd64-0.4.2.deb"

  private def bytesOf(content: String): Array[Byte] = content.getBytes(StandardCharsets.UTF_8)

  private def binaryFile(name: String, magic: Array[Byte], filler: String = "payload"): ReleaseFile = {
    val bytes = magic ++ bytesOf(filler)
    ReleaseFile(name, bytes.length.toLong, Sha256Digest.hex(bytes), bytes.take(8).toIndexedSeq, text = None)
  }

  private def textFile(name: String, content: String): ReleaseFile = {
    val bytes = bytesOf(content)
    ReleaseFile(name, bytes.length.toLong, Sha256Digest.hex(bytes), bytes.take(8).toIndexedSeq, text = Some(content))
  }

  private def sidecarFor(asset: ReleaseFile): ReleaseFile =
    textFile(DesktopReleaseLayout.sidecarName(asset.name), DesktopReleaseLayout.sidecarContent(asset.sha256, asset.name))

  private def goodAssets: Seq[ReleaseFile] = Seq(
    binaryFile(MacZip, ZipMagic),
    textFile(MacDmg, "dmg-installer-bytes"),
    binaryFile(LinuxTarGz, TarGzMagic),
    textFile(LinuxAppImage, "appimage-installer-bytes"),
    textFile(LinuxDeb, "deb-installer-bytes")
  )

  private def goodChecksums(assets: Seq[ReleaseFile]): ReleaseFile =
    textFile(DesktopReleaseLayout.ChecksumsFileName, DesktopReleaseLayout.checksumsContent(assets.map(a => a.name -> a.sha256)))

  private def goodSignature: ReleaseFile =
    textFile(
      s"${DesktopReleaseLayout.ChecksumsFileName}.asc",
      "-----BEGIN PGP SIGNATURE-----\n\nabcdef\n-----END PGP SIGNATURE-----\n"
    )

  private def goodSnapshot(): ReleaseSnapshot = {
    val assets    = goodAssets
    val sidecars  = assets.map(sidecarFor)
    val checksums = goodChecksums(assets)
    ReleaseSnapshot(Version, Platforms, assets ++ sidecars ++ Seq(checksums, goodSignature))
  }

  private def withFile(snapshot: ReleaseSnapshot, replacement: ReleaseFile): ReleaseSnapshot =
    snapshot.copy(files = snapshot.files.filterNot(_.name == replacement.name) :+ replacement)

  private def withoutFile(snapshot: ReleaseSnapshot, name: String): ReleaseSnapshot =
    snapshot.copy(files = snapshot.files.filterNot(_.name == name))

  private def withExtraFile(snapshot: ReleaseSnapshot, file: ReleaseFile): ReleaseSnapshot =
    snapshot.copy(files = snapshot.files :+ file)

  val tests = Tests {
    test("a well-formed release passes every check, signature included") {
      val outcomes = ReleaseVerifier.run(DesktopReleaseChecks.all, goodSnapshot())
      assert(outcomes.forall(_.passed))
    }

    test("sidecarDigestsMatch") {
      test("passes when every sidecar's digest matches the asset's recomputed sha256") {
        assert(DesktopReleaseChecks.sidecarDigestsMatch.find(goodSnapshot()).isEmpty)
      }

      test("catches an asset that no longer matches its sidecar's digest") {
        // Simulates the job-boundary gap: the asset's bytes changed (upload/download, disk corruption)
        // after the sidecar was written, so the sidecar's digest is now stale.
        val corrupted = binaryFile(MacZip, ZipMagic, filler = "corrupted-payload")
        val tampered  = withFile(goodSnapshot(), corrupted)
        val problems  = DesktopReleaseChecks.sidecarDigestsMatch.find(tampered)
        assert(problems.exists(_.contains(s"$MacZip.sha256")))
      }

      test("catches a sidecar that names the wrong asset") {
        val original  = goodSnapshot()
        val realAsset = original.file(MacZip).get
        val misnamed  = textFile(s"$MacZip.sha256", DesktopReleaseLayout.sidecarContent(realAsset.sha256, "some-other-file.zip"))
        val tampered  = withFile(original, misnamed)
        val problems  = DesktopReleaseChecks.sidecarDigestsMatch.find(tampered)
        assert(problems.exists(_.contains(s"$MacZip.sha256")))
      }

      test("catches a missing sidecar") {
        val tampered = withoutFile(goodSnapshot(), s"$MacZip.sha256")
        val problems = DesktopReleaseChecks.sidecarDigestsMatch.find(tampered)
        assert(problems.exists(_.contains(s"$MacZip.sha256")))
      }
    }

    test("checksumsCoversAssets") {
      test("passes when checksums.txt lists exactly the asset set") {
        assert(DesktopReleaseChecks.checksumsCoversAssets.find(goodSnapshot()).isEmpty)
      }

      test("catches an asset missing from checksums.txt") {
        val assets    = goodAssets.filterNot(_.name == LinuxDeb)
        val checksums = goodChecksums(assets)
        val tampered  = withFile(goodSnapshot(), checksums)
        val problems  = DesktopReleaseChecks.checksumsCoversAssets.find(tampered)
        assert(problems.exists(_.contains(LinuxDeb)))
      }

      test("catches an extra entry in checksums.txt") {
        val assets    = goodAssets
        val checksums = goodChecksums(assets :+ binaryFile("bogus-extra.zip", ZipMagic))
        val tampered  = withFile(goodSnapshot(), checksums)
        val problems  = DesktopReleaseChecks.checksumsCoversAssets.find(tampered)
        assert(problems.exists(_.contains("bogus-extra.zip")))
      }
    }

    test("signaturePresent") {
      test("passes when checksums.txt.asc exists, is non-empty, and starts with the PGP header") {
        assert(DesktopReleaseChecks.signaturePresent.find(goodSnapshot()).isEmpty)
      }

      test("catches a missing signature") {
        val tampered = withoutFile(goodSnapshot(), s"${DesktopReleaseLayout.ChecksumsFileName}.asc")
        val problems = DesktopReleaseChecks.signaturePresent.find(tampered)
        assert(problems.nonEmpty)
      }

      test("catches an empty signature file") {
        val empty    = ReleaseFile(s"${DesktopReleaseLayout.ChecksumsFileName}.asc", 0L, Sha256Digest.hex(Array.emptyByteArray), IndexedSeq.empty, Some(""))
        val tampered = withFile(goodSnapshot(), empty)
        val problems = DesktopReleaseChecks.signaturePresent.find(tampered)
        assert(problems.nonEmpty)
      }

      test("catches a signature file missing the PGP header") {
        val bogus    = textFile(s"${DesktopReleaseLayout.ChecksumsFileName}.asc", "not a signature\n")
        val tampered = withFile(goodSnapshot(), bogus)
        val problems = DesktopReleaseChecks.signaturePresent.find(tampered)
        assert(problems.nonEmpty)
      }
    }

    test("noEmptyAssets") {
      test("passes when every asset is larger than zero bytes") {
        assert(DesktopReleaseChecks.noEmptyAssets.find(goodSnapshot()).isEmpty)
      }

      test("catches a zero-byte asset") {
        val empty    = ReleaseFile(MacDmg, 0L, Sha256Digest.hex(Array.emptyByteArray), IndexedSeq.empty, Some(""))
        val tampered = withFile(goodSnapshot(), empty)
        val problems = DesktopReleaseChecks.noEmptyAssets.find(tampered)
        assert(problems.exists(_.contains(MacDmg)))
      }
    }

    test("archiveMagicNumbers") {
      test("passes when zip and tar.gz archives start with their magic numbers") {
        assert(DesktopReleaseChecks.archiveMagicNumbers.find(goodSnapshot()).isEmpty)
      }

      test("catches a zip archive that is really an HTML error page") {
        val htmlPage = textFile(MacZip, "<html><body>502 Bad Gateway</body></html>")
        val tampered = withFile(goodSnapshot(), htmlPage)
        val problems = DesktopReleaseChecks.archiveMagicNumbers.find(tampered)
        assert(problems.exists(_.contains(MacZip)))
      }

      test("catches a truncated tar.gz archive") {
        val truncated = textFile(LinuxTarGz, "")
        val tampered  = withFile(goodSnapshot(), truncated)
        val problems  = DesktopReleaseChecks.archiveMagicNumbers.find(tampered)
        assert(problems.exists(_.contains(LinuxTarGz)))
      }
    }

    test("versionInAssetNames") {
      test("passes when every asset-like file carries the release version") {
        assert(DesktopReleaseChecks.versionInAssetNames.find(goodSnapshot()).isEmpty)
      }

      test("catches a stale asset left over from a previous version") {
        val stale     = binaryFile("morphir-desktop-mac-aarch64-0.4.1.zip", ZipMagic)
        val tampered  = withExtraFile(goodSnapshot(), stale)
        val problems  = DesktopReleaseChecks.versionInAssetNames.find(tampered)
        assert(problems.exists(_.contains("morphir-desktop-mac-aarch64-0.4.1.zip")))
      }
    }

    test("expectedAssetsPresent") {
      test("passes when every platform's archive and installers are present") {
        assert(DesktopReleaseChecks.expectedAssetsPresent.find(goodSnapshot()).isEmpty)
      }

      test("catches a missing installer") {
        val tampered = withoutFile(goodSnapshot(), LinuxDeb)
        val problems = DesktopReleaseChecks.expectedAssetsPresent.find(tampered)
        assert(problems.exists(_.contains(LinuxDeb)))
      }

      test("catches a missing archive") {
        val tampered = withoutFile(goodSnapshot(), LinuxTarGz)
        val problems = DesktopReleaseChecks.expectedAssetsPresent.find(tampered)
        assert(problems.exists(_.contains(LinuxTarGz)))
      }
    }

    test("defaults excludes the signature check when a signature is not required") {
      val names = DesktopReleaseChecks.defaults(requireSignature = false).map(_.name)
      assert(!names.contains(DesktopReleaseChecks.signaturePresent.name))
      assert(DesktopReleaseChecks.defaults(requireSignature = true).map(_.name).contains(DesktopReleaseChecks.signaturePresent.name))
    }

    test("ReleaseVerifier collects every problem across every failing check, not just the first") {
      val brokenAsset  = binaryFile(MacZip, ZipMagic, filler = "corrupted")
      val brokenSignature = withoutFile(withFile(goodSnapshot(), brokenAsset), s"${DesktopReleaseLayout.ChecksumsFileName}.asc")
      val outcomes = ReleaseVerifier.run(DesktopReleaseChecks.all, brokenSignature)
      val failed   = outcomes.filterNot(_.passed).map(_.name)
      assert(failed.contains(DesktopReleaseChecks.sidecarDigestsMatch.name))
      assert(failed.contains(DesktopReleaseChecks.signaturePresent.name))
    }
  }
}
