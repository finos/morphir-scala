package org.finos.morphir.mill.publish.desktop

import utest.*

object DesktopPlatformTests extends TestSuite {
  val tests = Tests {
    test("tokens follow the Mill native launcher naming") {
      val tokens = DesktopPlatform.values.toSeq.map(_.token)
      assert(
        tokens == Seq("mac-aarch64", "mac-amd64", "linux-amd64", "linux-aarch64", "win-amd64")
      )
    }

    test("artifactName prefixes the product name") {
      assert(DesktopPlatform.MacAarch64.artifactName == "morphir-desktop-mac-aarch64")
      assert(DesktopPlatform.LinuxAmd64.artifactName == "morphir-desktop-linux-amd64")
    }

    test("mac and windows ship zip archives, linux ships tar.gz") {
      assert(DesktopPlatform.MacAmd64.archive.ext == "zip")
      assert(DesktopPlatform.WinAmd64.archive.ext == "zip")
      assert(DesktopPlatform.LinuxAarch64.archive.ext == "tar.gz")
    }

    test("installer extensions are declared per platform") {
      assert(DesktopPlatform.MacAarch64.installerExts == Seq("dmg"))
      assert(DesktopPlatform.WinAmd64.installerExts == Seq("exe"))
      assert(DesktopPlatform.LinuxAmd64.installerExts == Seq("AppImage", "deb"))
    }

    test("allExts lists the archive first, then installers") {
      assert(DesktopPlatform.LinuxAmd64.allExts == Seq("tar.gz", "AppImage", "deb"))
    }

    test("fromToken round-trips every platform") {
      DesktopPlatform.values.foreach { platform =>
        assert(DesktopPlatform.fromToken(platform.token) == Some(platform))
      }
      assert(DesktopPlatform.fromToken("solaris-sparc") == None)
    }

    test("fromBuilder translates electron-builder os and arch spelling") {
      assert(DesktopPlatform.fromBuilder("mac", "arm64") == Some(DesktopPlatform.MacAarch64))
      assert(DesktopPlatform.fromBuilder("linux", "x64") == Some(DesktopPlatform.LinuxAmd64))
      assert(DesktopPlatform.fromBuilder("win", "arm64") == None)
    }
  }
}
