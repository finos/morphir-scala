package org.finos.morphir.mill.publish.desktop

import org.finos.morphir.mill.publish.Sha256Digest
import utest.*

object DesktopReleaseLayoutTests extends TestSuite {
  private val version = "0.4.2"

  val tests = Tests {
    test("archive name equals the Maven filename tail") {
      // Maven lays out <artifactId>-<version>.<ext>; the release asset must match byte for byte
      // so one download script can target either host.
      val platform  = DesktopPlatform.MacAarch64
      val mavenTail = s"${platform.artifactName}-$version.${platform.archive.ext}"
      assert(DesktopReleaseLayout.archiveName(platform, version) == mavenTail)
      assert(DesktopReleaseLayout.archiveName(platform, version) == "morphir-desktop-mac-aarch64-0.4.2.zip")
    }

    test("linux archives carry the compound tar.gz extension") {
      assert(
        DesktopReleaseLayout.archiveName(DesktopPlatform.LinuxAmd64, version) ==
          "morphir-desktop-linux-amd64-0.4.2.tar.gz"
      )
    }

    test("installer names use the same stem as the archive") {
      assert(
        DesktopReleaseLayout.assetName(DesktopPlatform.MacAmd64, version, "dmg") ==
          "morphir-desktop-mac-amd64-0.4.2.dmg"
      )
      assert(
        DesktopReleaseLayout.assetName(DesktopPlatform.WinAmd64, version, "exe") ==
          "morphir-desktop-win-amd64-0.4.2.exe"
      )
    }

    test("assetName rejects an empty version or extension") {
      assertThrows[IllegalArgumentException](DesktopReleaseLayout.assetName(DesktopPlatform.WinAmd64, "", "exe"))
      assertThrows[IllegalArgumentException](DesktopReleaseLayout.assetName(DesktopPlatform.WinAmd64, version, ""))
    }

    test("sidecar is byte-for-byte sha256sum compatible") {
      val asset = "morphir-desktop-win-amd64-0.4.2.zip"
      val sha   = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
      assert(DesktopReleaseLayout.sidecarName(asset) == s"$asset.sha256")
      assert(DesktopReleaseLayout.sidecarContent(sha, asset) == s"$sha  $asset\n")
    }

    test("checksums.txt is sorted by asset name") {
      val content = DesktopReleaseLayout.checksumsContent(
        Seq("b.zip" -> "bbb", "a.zip" -> "aaa", "c.dmg" -> "ccc")
      )
      assert(content == "aaa  a.zip\nbbb  b.zip\nccc  c.dmg\n")
      assert(DesktopReleaseLayout.ChecksumsFileName == "checksums.txt")
    }

    test("sha256 of empty input is the known digest") {
      assert(
        Sha256Digest.hex(Array.emptyByteArray) ==
          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
      )
    }

    test("sha256 of a file matches the digest of its bytes") {
      val dir  = os.temp.dir()
      val file = dir / "payload.bin"
      os.write(file, "morphir")
      assert(Sha256Digest.ofFile(file) == Sha256Digest.hex("morphir".getBytes("UTF-8")))
    }
  }
}
