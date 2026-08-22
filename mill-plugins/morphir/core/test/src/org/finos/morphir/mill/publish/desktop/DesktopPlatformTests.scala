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
      // arm64 ships no deb: electron-builder builds one through fpm, and the only fpm build it
      // publishes is linux-x86, which cannot execute on an arm64 runner.
      assert(DesktopPlatform.LinuxAarch64.installerExts == Seq("AppImage"))
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

    test("AllTokens joins every token in declaration order") {
      assert(DesktopPlatform.AllTokens == "mac-aarch64,mac-amd64,linux-amd64,linux-aarch64,win-amd64")
    }

    test("parseTokens accepts a single known token") {
      assert(DesktopPlatform.parseTokens("linux-amd64") == Right(Seq(DesktopPlatform.LinuxAmd64)))
    }

    test("parseTokens accepts a comma-separated subset, preserving order") {
      assert(
        DesktopPlatform.parseTokens("win-amd64,mac-aarch64") ==
          Right(Seq(DesktopPlatform.WinAmd64, DesktopPlatform.MacAarch64))
      )
    }

    test("parseTokens trims whitespace around tokens") {
      assert(DesktopPlatform.parseTokens(" linux-amd64 , win-amd64 ") ==
        Right(Seq(DesktopPlatform.LinuxAmd64, DesktopPlatform.WinAmd64)))
    }

    test("parseTokens on AllTokens round-trips every platform in declaration order") {
      assert(DesktopPlatform.parseTokens(DesktopPlatform.AllTokens) == Right(DesktopPlatform.values.toSeq))
    }

    test("parseTokens rejects an unknown token, naming the valid tokens") {
      val result = DesktopPlatform.parseTokens("solaris-sparc")
      assert(result.isLeft)
      assert(result.left.toOption.exists(_.contains("solaris-sparc")))
      assert(result.left.toOption.exists(_.contains("mac-aarch64")))
      assert(result.left.toOption.exists(_.contains("win-amd64")))
    }

    test("parseTokens rejects one unknown token among otherwise-known ones") {
      val result = DesktopPlatform.parseTokens("linux-amd64,bogus")
      assert(result.isLeft)
      assert(result.left.toOption.exists(_.contains("bogus")))
    }

    test("parseTokens rejects an empty string, naming the valid tokens") {
      val result = DesktopPlatform.parseTokens("")
      assert(result.isLeft)
      assert(result.left.toOption.exists(_.contains("mac-aarch64")))
    }

    test("parseTokens rejects a blank token from a stray comma") {
      val trailing = DesktopPlatform.parseTokens("linux-amd64,")
      assert(trailing.isLeft)
      val leading = DesktopPlatform.parseTokens(",linux-amd64")
      assert(leading.isLeft)
      val doubled = DesktopPlatform.parseTokens("linux-amd64,,win-amd64")
      assert(doubled.isLeft)
    }

    test("parseTokens rejects a whitespace-only token") {
      val result = DesktopPlatform.parseTokens("linux-amd64,   ,win-amd64")
      assert(result.isLeft)
    }
  }
}
