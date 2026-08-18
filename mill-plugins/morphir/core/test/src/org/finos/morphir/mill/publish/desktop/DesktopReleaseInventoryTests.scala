package org.finos.morphir.mill.publish.desktop

import utest.*

object DesktopReleaseInventoryTests extends TestSuite {
  val tests = Tests {
    test("classifies one archive and one installer for mac") {
      val result = DesktopReleaseInventory.classify(
        DesktopPlatform.MacAarch64,
        Seq("morphir-desktop-0.4.2-mac-arm64.zip", "morphir-desktop-0.4.2-mac-arm64.dmg")
      )
      assert(result == Right(PlatformAssets(
        DesktopPlatform.MacAarch64,
        "morphir-desktop-0.4.2-mac-arm64.zip",
        Seq("morphir-desktop-0.4.2-mac-arm64.dmg"),
        Seq.empty
      )))
    }

    test("requires every declared installer extension") {
      val result = DesktopReleaseInventory.classify(
        DesktopPlatform.LinuxAmd64,
        Seq("app.tar.gz", "app.AppImage")
      )
      assert(result.isLeft)
      assert(result.left.toOption.exists(_.contains("deb")))
    }

    test("ignores electron-builder side files rather than failing") {
      val result = DesktopReleaseInventory.classify(
        DesktopPlatform.WinAmd64,
        Seq("app.zip", "app.exe", "app.exe.blockmap", "latest.yml")
      )
      assert(result.map(_.ignored) == Right(Seq("app.exe.blockmap", "latest.yml")))
    }

    test("a missing archive names the expected extension") {
      val result = DesktopReleaseInventory.classify(DesktopPlatform.MacAmd64, Seq("app.dmg"))
      assert(result.isLeft)
      assert(result.left.toOption.exists(_.contains(".zip")))
      assert(result.left.toOption.exists(_.contains("mac-amd64")))
    }

    test("two candidates for one extension is an error") {
      val result = DesktopReleaseInventory.classify(
        DesktopPlatform.MacAmd64,
        Seq("a.zip", "b.zip", "app.dmg")
      )
      assert(result.isLeft)
      assert(result.left.toOption.exists(_.contains("a.zip")))
      assert(result.left.toOption.exists(_.contains("b.zip")))
    }

    test("a tar.gz is not mistaken for another gz-suffixed file") {
      // linux-amd64 rather than linux-aarch64: only the x64 leg declares a deb, because
      // electron-builder's fpm build cannot execute on arm64.
      val result = DesktopReleaseInventory.classify(
        DesktopPlatform.LinuxAmd64,
        Seq("app.tar.gz", "app.AppImage", "app.deb", "notes.txt.gz")
      )
      assert(result.map(_.archive) == Right("app.tar.gz"))
      assert(result.map(_.ignored) == Right(Seq("notes.txt.gz")))
    }
  }
}
