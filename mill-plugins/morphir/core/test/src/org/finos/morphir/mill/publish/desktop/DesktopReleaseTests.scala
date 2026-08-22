package org.finos.morphir.mill.publish.desktop

import org.finos.morphir.mill.publish.Sha256Digest
import utest.*

object DesktopReleaseTests extends TestSuite {
  private val version   = "0.4.2"
  private val platforms = Seq(DesktopPlatform.MacAarch64, DesktopPlatform.WinAmd64)

  private def stage(root: os.Path): Unit = {
    os.write.over(root / "mac-aarch64" / "raw-mac.zip", "mac-archive", createFolders = true)
    os.write.over(root / "mac-aarch64" / "raw-mac.dmg", "mac-installer", createFolders = true)
    os.write.over(root / "win-amd64" / "raw-win.zip", "win-archive", createFolders = true)
    os.write.over(root / "win-amd64" / "raw-win.exe", "win-installer", createFolders = true)
  }

  val tests = Tests {
    test("canonicalizes every staged asset and writes sidecars") {
      val staging = os.temp.dir()
      val output  = os.temp.dir()
      stage(staging)

      val manifest = DesktopRelease.canonicalize(staging, output, version, platforms)
        .fold(message => throw new java.lang.AssertionError(message), identity)

      assert(manifest.version == version)
      assert(manifest.entries.map(_.name) == Seq(
        "morphir-desktop-mac-aarch64-0.4.2.dmg",
        "morphir-desktop-mac-aarch64-0.4.2.zip",
        "morphir-desktop-win-amd64-0.4.2.exe",
        "morphir-desktop-win-amd64-0.4.2.zip"
      ))
      assert(os.exists(output / "morphir-desktop-mac-aarch64-0.4.2.zip"))
      assert(os.read(output / "morphir-desktop-mac-aarch64-0.4.2.zip") == "mac-archive")
    }

    test("only archives are marked as Maven artifacts") {
      val staging = os.temp.dir()
      val output  = os.temp.dir()
      stage(staging)

      val manifest = DesktopRelease.canonicalize(staging, output, version, platforms).toOption.get
      val maven    = manifest.entries.filter(_.mavenArtifact).map(_.name)
      assert(maven == Seq("morphir-desktop-mac-aarch64-0.4.2.zip", "morphir-desktop-win-amd64-0.4.2.zip"))
    }

    test("sidecar content matches the file digest") {
      val staging = os.temp.dir()
      val output  = os.temp.dir()
      stage(staging)

      DesktopRelease.canonicalize(staging, output, version, platforms).toOption.get
      val asset   = "morphir-desktop-win-amd64-0.4.2.exe"
      val sidecar = os.read(output / s"$asset.sha256")
      assert(sidecar == s"${Sha256Digest.ofFile(output / asset)}  $asset\n")
    }

    test("checksums.txt lists every asset sorted by name") {
      val staging = os.temp.dir()
      val output  = os.temp.dir()
      stage(staging)

      val manifest = DesktopRelease.canonicalize(staging, output, version, platforms).toOption.get
      val lines    = os.read.lines(output / DesktopReleaseLayout.ChecksumsFileName)
      assert(lines.size == manifest.entries.size)
      assert(lines.map(_.split("  ").last) == manifest.entries.map(_.name).sorted)
    }

    test("a missing platform directory is an error naming the platform") {
      val staging = os.temp.dir()
      val output  = os.temp.dir()
      os.write.over(staging / "mac-aarch64" / "raw-mac.zip", "mac", createFolders = true)
      os.write.over(staging / "mac-aarch64" / "raw-mac.dmg", "mac", createFolders = true)

      val result = DesktopRelease.canonicalize(staging, output, version, platforms)
      assert(result.isLeft)
      assert(result.left.toOption.exists(_.contains("win-amd64")))
    }

    test("an empty version is rejected before any file is written") {
      val staging = os.temp.dir()
      val output  = os.temp.dir()
      stage(staging)

      val result = DesktopRelease.canonicalize(staging, output, "", platforms)
      assert(result.isLeft)
      assert(os.list(output).isEmpty)
    }

    test("canonicalizing at a new version removes stale assets and sidecars from a previous version") {
      val staging = os.temp.dir()
      val output  = os.temp.dir()
      stage(staging)

      DesktopRelease.canonicalize(staging, output, "0.4.1", platforms).toOption.get
      assert(os.exists(output / "morphir-desktop-mac-aarch64-0.4.1.zip"))
      assert(os.exists(output / "morphir-desktop-mac-aarch64-0.4.1.zip.sha256"))
      assert(os.exists(output / "morphir-desktop-mac-aarch64-0.4.1.dmg"))
      assert(os.exists(output / "morphir-desktop-win-amd64-0.4.1.zip"))

      DesktopRelease.canonicalize(staging, output, "0.4.2", platforms).toOption.get

      assert(!os.exists(output / "morphir-desktop-mac-aarch64-0.4.1.zip"))
      assert(!os.exists(output / "morphir-desktop-mac-aarch64-0.4.1.zip.sha256"))
      assert(!os.exists(output / "morphir-desktop-mac-aarch64-0.4.1.dmg"))
      assert(!os.exists(output / "morphir-desktop-win-amd64-0.4.1.zip"))
      assert(os.exists(output / "morphir-desktop-mac-aarch64-0.4.2.zip"))
      val remaining = os.list(output).map(_.last)
      assert(remaining.filterNot(_ == DesktopReleaseLayout.ChecksumsFileName).forall(_.contains("0.4.2")))
    }

    test("a failed canonicalize leaves a pre-existing output directory untouched") {
      val staging = os.temp.dir()
      val output  = os.temp.dir()
      stage(staging)
      DesktopRelease.canonicalize(staging, output, version, platforms).toOption.get

      val before         = os.list(output).map(_.last).toSet
      val beforeContents = before.map(name => name -> os.read(output / name)).toMap
      assert(before.nonEmpty)

      // Drop one platform's staged directory so `collect` fails validation before any write.
      os.remove.all(staging / "win-amd64")
      val result = DesktopRelease.canonicalize(staging, output, "0.4.9", platforms)

      assert(result.isLeft)
      val after = os.list(output).map(_.last).toSet
      assert(after == before)
      after.foreach(name => assert(os.read(output / name) == beforeContents(name)))
    }
  }
}
