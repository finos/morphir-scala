package millbuild

import java.nio.file.Files
import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveInputStream, TarArchiveOutputStream}
import utest.*

object ExtensionReleaseTests extends TestSuite:
  private val version = "0.6.0-M01"
  private val linux   = ExtensionRelease.Platform.LinuxAmd64
  private val windows = ExtensionRelease.Platform.WinAmd64

  private def fixture(): (os.Path, os.Path) =
    val root       = os.temp.dir(prefix = "morphir-extension-release-", deleteOnExit = true)
    val executable = root / "native-executable"
    os.write(executable, Array[Byte](1, 2, 3))
    (root, executable)

  val tests = Tests:
    test("verifies an MEP-only release"):
      val (root, executable) = fixture()
      val asset              = ExtensionRelease.packageMepNative(windows, version, executable, root / "release")

      val result = ExtensionRelease.verifyAndWriteChecksums(root / "release", version, Seq(windows))

      assert(result == Right(Seq(asset.last)))
      assert(os.read.lines(root / "release" / "checksums.txt").size == 1)

    test("maps supported native hosts and rejects unsupported inputs"):
      assert(ExtensionRelease.Platform.fromHost("Windows 11", "amd64").map(_.token) == Right("win-amd64"))
      assert(ExtensionRelease.Platform.fromHost("Linux", "aarch64").map(_.token) == Right("linux-aarch64"))
      assert(ExtensionRelease.Platform.fromHost("Mac OS X", "x86_64").map(_.token) == Right("mac-amd64"))
      val arm64 = ExtensionRelease.Platform.fromHost("Windows 11", "aarch64")
      assert(arm64.left.exists(_.contains("Windows ARM64")))
      assert(!arm64.left.exists(_.contains("JVM CLI")))
      assert(ExtensionRelease.Platform.fromHost("FreeBSD", "amd64").isLeft)
      assert(ExtensionRelease.Platform.fromToken("linux-x64").isLeft)
      ExtensionRelease.Platform.values.foreach { platform =>
        assert(ExtensionRelease.Platform.fromToken(platform.token) == Right(platform))
      }

    test("uses stable release asset names"):
      val mepAssets = ExtensionRelease.Platform.values.map(platform =>
        platform.token -> ExtensionRelease.mepAssetName(platform, version)
      ).toMap
      assert(
        mepAssets == Map(
          "mac-aarch64"   -> "morphir-scala-elm-mac-aarch64-0.6.0-M01",
          "mac-amd64"     -> "morphir-scala-elm-mac-amd64-0.6.0-M01",
          "linux-amd64"   -> "morphir-scala-elm-linux-amd64-0.6.0-M01",
          "linux-aarch64" -> "morphir-scala-elm-linux-aarch64-0.6.0-M01",
          "win-amd64"     -> "morphir-scala-elm-win-amd64-0.6.0-M01.exe"
        )
      )
      assert(
        ExtensionRelease.nativeTransportName(ExtensionRelease.Platform.MacAmd64, version) ==
          "morphir-native-transport-mac-amd64-0.6.0-M01.tar"
      )

    test("rejects unsafe release versions"):
      Seq("", "../escape", "bad/version", "bad\\version", "bad version").foreach { unsafe =>
        assertThrows[IllegalArgumentException](ExtensionRelease.mepAssetName(linux, unsafe))
        assertThrows[IllegalArgumentException](ExtensionRelease.nativeTransportName(linux, unsafe))
      }

    test("copies the MEP executable out of the native-image output"):
      val (root, executable) = fixture()
      os.write(root / "unrelated-library", "not a release asset")
      val asset = ExtensionRelease.packageMepNative(linux, version, executable, root / "release")

      assert(asset.last == "morphir-scala-elm-linux-amd64-0.6.0-M01")
      assert(os.read.bytes(asset).toSeq == Seq[Byte](1, 2, 3))
      assert(os.list(root / "release").map(_.last).toSet == Set(asset.last, s"${asset.last}.sha256"))
      assert(Files.isExecutable(asset.toNIO))

    test("native transport contains only the MEP executable and checksum and restores executable mode"):
      val (root, executable) = fixture()
      val mepAsset           = ExtensionRelease.packageMepNative(linux, version, executable, root / "release")
      val transport = ExtensionRelease.packageNativeTransport(linux, version, root / "release", root / "transport")
      assert(transport.last == "morphir-native-transport-linux-amd64-0.6.0-M01.tar")
      assert(!transport.startsWith(root / "release"))

      val input = TarArchiveInputStream(Files.newInputStream(transport.toNIO))
      try
        val binary = input.getNextEntry
        assert(binary.getName == mepAsset.last)
        assert(binary.getMode == 0x1ed)
        val checksum = input.getNextEntry
        assert(checksum.getName == s"${mepAsset.last}.sha256")
        assert(input.getNextEntry == null)
      finally input.close()

      val extracted = root / "extracted"
      os.write(extracted / mepAsset.last, "old", createFolders = true)
      assert((extracted / mepAsset.last).toIO.setExecutable(false, false))
      val result = ExtensionRelease.extractNativeTransport(linux, version, transport, extracted)
      assert(result == Right(Seq(mepAsset.last, s"${mepAsset.last}.sha256")))
      assert(os.read.bytes(extracted / mepAsset.last).toSeq == Seq[Byte](1, 2, 3))
      assert(Files.isExecutable((extracted / mepAsset.last).toNIO))
      assert(ExtensionRelease.verifyAndWriteChecksums(extracted, version, Seq(linux), requireExecutable = true).isRight)

    test("native transport rejects missing files and malformed entries"):
      val (root, _) = fixture()
      assert(ExtensionRelease.extractNativeTransport(linux, version, root / "absent.tar", root / "release").isLeft)
      assertThrows[IllegalArgumentException] {
        ExtensionRelease.packageNativeTransport(linux, version, root / "release", root / "transport")
      }
      val transport  = root / "invalid.tar"
      val mepName    = ExtensionRelease.mepAssetName(linux, version)
      val output     = TarArchiveOutputStream(Files.newOutputStream(transport.toNIO))
      val retiredCli = "morphir-cli-linux-amd64-0.6.0-M01.tar.gz"
      try Seq("../escape", "unexpected", retiredCli, mepName, mepName).foreach { name =>
          val entry = TarArchiveEntry(name)
          entry.setSize(1)
          output.putArchiveEntry(entry)
          output.write(Array[Byte](1))
          output.closeArchiveEntry()
        }
      finally output.close()

      val result = ExtensionRelease.extractNativeTransport(linux, version, transport, root / "release")
      assert(result.left.exists(_.contains("unexpected native transport entry: ../escape")))
      assert(result.left.exists(_.contains("unexpected native transport entry: unexpected")))
      assert(result.left.exists(_.contains(s"unexpected native transport entry: $retiredCli")))
      assert(result.left.exists(_.contains(s"duplicate native transport entry: $mepName")))
      assert(result.left.exists(_.contains(s"missing native transport entry: $mepName.sha256")))
      assert(!os.exists(root / "escape"))

    test("verification rejects a Unix MEP asset whose executable mode was lost"):
      val (root, executable) = fixture()
      val mepAsset           = ExtensionRelease.packageMepNative(linux, version, executable, root / "release")
      assert(mepAsset.toIO.setExecutable(false, false))

      val result =
        ExtensionRelease.verifyAndWriteChecksums(root / "release", version, Seq(linux), requireExecutable = true)

      assert(result.left.exists(_.contains(s"non-executable release asset: ${mepAsset.last}")))

    test("checksums assets that span multiple digest buffer reads"):
      val (root, executable) = fixture()
      os.write.over(executable, Array.fill[Byte](131073)('a'.toByte))
      val asset   = ExtensionRelease.packageMepNative(windows, version, executable, root / "release")
      val sidecar = os.read(os.Path(asset.toString + ".sha256"))
      assert(sidecar == s"7e009ea4ef882e385b3c0bcbbfa8d009bb0a633bdd764415c09182ee0e75da73  ${asset.last}\n")

    test("verifies all supported platforms and writes sorted combined checksums"):
      val (root, executable) = fixture()
      val platforms          = ExtensionRelease.Platform.values.toSeq
      val assets = platforms.map(ExtensionRelease.packageMepNative(_, version, executable, root / "release"))

      val result = ExtensionRelease.verifyAndWriteChecksums(root / "release", version, platforms)

      assert(result == Right(assets.map(_.last)))
      assert(os.read.lines(root / "release" / "checksums.txt").toSeq ==
        assets.sortBy(_.last).map(asset => os.read(os.Path(asset.toString + ".sha256")).trim))

    test("verification rejects an empty platform selection"):
      val (root, _) = fixture()
      assert(ExtensionRelease.verifyAndWriteChecksums(root / "release", version, Seq.empty).isLeft)

    test("verification rejects an omitted or misnamed MEP executable"):
      val (root, _) = fixture()
      val omitted   = ExtensionRelease.verifyAndWriteChecksums(root / "release", version, Seq(linux))
      assert(omitted.left.exists(_.contains("missing release asset: morphir-scala-elm-linux-amd64-0.6.0-M01")))

      os.write(root / "release" / "morphir-scala-elm-linux-x64-0.6.0-M01", Array[Byte](4, 5, 6), createFolders = true)
      val misnamed = ExtensionRelease.verifyAndWriteChecksums(root / "release", version, Seq(linux))
      assert(misnamed.left.exists(_.exists(_.startsWith("unexpected release file:"))))

    test("verification rejects retired CLI assets"):
      val (root, executable) = fixture()
      ExtensionRelease.packageMepNative(windows, version, executable, root / "release")
      Seq("morphir-cli-jvm-0.6.0-M01.jar", "morphir-cli-win-amd64-0.6.0-M01.zip").foreach { name =>
        os.write(root / "release" / name, "retired")
      }
      val result = ExtensionRelease.verifyAndWriteChecksums(root / "release", version, Seq(windows))
      assert(result.left.exists(_.size == 2))
      assert(result.left.exists(_.forall(_.startsWith("unexpected release file:"))))

    test("verification rejects empty assets and missing sidecars"):
      val (root, executable) = fixture()
      val asset              = ExtensionRelease.packageMepNative(windows, version, executable, root / "release")
      os.remove(os.Path(asset.toString + ".sha256"))
      val missing = ExtensionRelease.verifyAndWriteChecksums(root / "release", version, Seq(windows))
      assert(missing.left.exists(_.contains(s"missing checksum sidecar: ${asset.last}.sha256")))
      os.write.over(asset, "")
      val empty = ExtensionRelease.verifyAndWriteChecksums(root / "release", version, Seq(windows))
      assert(empty.left.exists(_.contains(s"empty release asset: ${asset.last}")))

    test("verification rejects empty and corrupt sidecars without replacing them"):
      val (root, executable) = fixture()
      val asset              = ExtensionRelease.packageMepNative(windows, version, executable, root / "release")
      val sidecar            = os.Path(asset.toString + ".sha256")
      Seq("", s"${"0" * 64}  ${asset.last}\n").foreach { contents =>
        os.write.over(sidecar, contents)
        val result = ExtensionRelease.verifyAndWriteChecksums(root / "release", version, Seq(windows))
        assert(result.left.exists(_.contains(s"digest mismatch: ${asset.last}")))
        assert(os.read(sidecar) == contents)
        assert(!os.exists(root / "release" / "checksums.txt"))
      }

    test("verification reports a corrupt asset without replacing its sidecar"):
      val (root, executable) = fixture()
      val asset              = ExtensionRelease.packageMepNative(windows, version, executable, root / "release")
      val originalSidecar    = os.read(os.Path(asset.toString + ".sha256"))
      os.write.append(asset, Array[Byte](9))

      val result = ExtensionRelease.verifyAndWriteChecksums(root / "release", version, Seq(windows))

      assert(result.left.exists(_.exists(_.contains("digest mismatch"))))
      assert(os.read(os.Path(asset.toString + ".sha256")) == originalSidecar)
