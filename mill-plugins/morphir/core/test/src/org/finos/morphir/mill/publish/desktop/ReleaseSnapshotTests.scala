package org.finos.morphir.mill.publish.desktop

import org.finos.morphir.mill.publish.Sha256Digest
import utest.*

/** Tests for [[ReleaseSnapshot.read]], the one IO boundary that turns a release directory into data. */
object ReleaseSnapshotTests extends TestSuite {
  private val version   = "0.4.2"
  private val platforms = Seq(DesktopPlatform.MacAarch64, DesktopPlatform.WinAmd64)

  // 0xFF and 0xFE are never valid UTF-8 bytes, in any position — trailing them onto the zip magic
  // guarantees the archive is not mistaken for decodable text, unlike a run of plain ASCII bytes would.
  private val invalidUtf8Tail = Array[Byte](0xff.toByte, 0xfe.toByte, 0x00)

  private def stageReleaseDir(dir: os.Path): Unit = {
    os.write.over(dir / "morphir-desktop-mac-aarch64-0.4.2.zip", Array[Byte](0x50, 0x4b, 0x03, 0x04) ++ invalidUtf8Tail)
    os.write.over(dir / "morphir-desktop-mac-aarch64-0.4.2.dmg", "dmg-installer-bytes")
    os.write.over(dir / "morphir-desktop-win-amd64-0.4.2.zip", Array[Byte](0x50, 0x4b, 0x03, 0x04) ++ invalidUtf8Tail)
    os.write.over(dir / "morphir-desktop-win-amd64-0.4.2.exe", "exe-installer-bytes")
    Seq(
      "morphir-desktop-mac-aarch64-0.4.2.zip",
      "morphir-desktop-mac-aarch64-0.4.2.dmg",
      "morphir-desktop-win-amd64-0.4.2.zip",
      "morphir-desktop-win-amd64-0.4.2.exe"
    ).foreach { name =>
      val sha = Sha256Digest.ofFile(dir / name)
      os.write.over(dir / DesktopReleaseLayout.sidecarName(name), DesktopReleaseLayout.sidecarContent(sha, name))
    }
    os.write.over(
      dir / DesktopReleaseLayout.ChecksumsFileName,
      DesktopReleaseLayout.checksumsContent(
        Seq(
          "morphir-desktop-mac-aarch64-0.4.2.zip",
          "morphir-desktop-mac-aarch64-0.4.2.dmg",
          "morphir-desktop-win-amd64-0.4.2.zip",
          "morphir-desktop-win-amd64-0.4.2.exe"
        ).map(name => name -> Sha256Digest.ofFile(dir / name))
      )
    )
  }

  val tests = Tests {
    test("reads version and platforms straight through") {
      val dir = os.temp.dir()
      stageReleaseDir(dir)
      val snapshot = ReleaseSnapshot.read(dir, version, platforms)
      assert(snapshot.version == version)
      assert(snapshot.platforms == platforms)
    }

    test("recomputes size and sha256 from the bytes actually on disk") {
      val dir = os.temp.dir()
      stageReleaseDir(dir)
      val snapshot = ReleaseSnapshot.read(dir, version, platforms)
      val asset    = snapshot.file("morphir-desktop-mac-aarch64-0.4.2.zip").get
      assert(asset.size == os.size(dir / "morphir-desktop-mac-aarch64-0.4.2.zip"))
      assert(asset.sha256 == Sha256Digest.ofFile(dir / "morphir-desktop-mac-aarch64-0.4.2.zip"))
    }

    test("captures the leading bytes for magic-number checks") {
      val dir = os.temp.dir()
      stageReleaseDir(dir)
      val snapshot = ReleaseSnapshot.read(dir, version, platforms)
      val asset    = snapshot.file("morphir-desktop-mac-aarch64-0.4.2.zip").get
      assert(asset.headBytes.take(4) == IndexedSeq[Byte](0x50, 0x4b, 0x03, 0x04))
    }

    test("decodes small text files but leaves binary archives undecoded") {
      val dir = os.temp.dir()
      stageReleaseDir(dir)
      val snapshot  = ReleaseSnapshot.read(dir, version, platforms)
      val checksums = snapshot.file(DesktopReleaseLayout.ChecksumsFileName).get
      assert(checksums.text.contains(os.read(dir / DesktopReleaseLayout.ChecksumsFileName)))

      val sidecar = snapshot.file("morphir-desktop-mac-aarch64-0.4.2.zip.sha256").get
      assert(sidecar.text.contains(os.read(dir / "morphir-desktop-mac-aarch64-0.4.2.zip.sha256")))

      // The zip's tail bytes are not valid UTF-8, so the archive isn't reported as decodable text.
      val archive = snapshot.file("morphir-desktop-win-amd64-0.4.2.zip").get
      assert(archive.text.isEmpty)
    }

    test("every file on disk shows up in the snapshot, sorted by name") {
      val dir = os.temp.dir()
      stageReleaseDir(dir)
      val snapshot = ReleaseSnapshot.read(dir, version, platforms)
      assert(snapshot.files.map(_.name) == snapshot.files.map(_.name).sorted)
      assert(snapshot.files.size == 9) // 4 assets + 4 sidecars + checksums.txt
    }
  }
}
