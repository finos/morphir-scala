package millbuild

import java.util.zip.ZipFile
import scala.jdk.CollectionConverters.*
import utest.*

object CliReleaseTests extends TestSuite:
  val tests = Tests:
    test("maps supported native hosts and rejects Windows ARM64"):
      assert(CliRelease.Platform.fromHost("Windows 11", "amd64").map(_.token) == Right("win-amd64"))
      assert(CliRelease.Platform.fromHost("Linux", "aarch64").map(_.token) == Right("linux-aarch64"))
      assert(CliRelease.Platform.fromHost("Mac OS X", "x86_64").map(_.token) == Right("mac-amd64"))
      assert(CliRelease.Platform.fromHost("Windows 11", "aarch64").left.exists(_.contains("Windows ARM64")))

    test("uses stable release asset names"):
      assert(
        CliRelease.nativeArchiveName(CliRelease.Platform.WinAmd64, "0.6.0-M01") ==
          "morphir-cli-win-amd64-0.6.0-M01.zip"
      )
      assert(
        CliRelease.nativeArchiveName(CliRelease.Platform.LinuxAarch64, "0.6.0-M01") ==
          "morphir-cli-linux-aarch64-0.6.0-M01.tar.gz"
      )
      assert(CliRelease.jvmAssetName("0.6.0-M01") == "morphir-cli-jvm-0.6.0-M01.jar")

    test("reads the final nonblank line from command output"):
      val output =
        "-XX:InitialHeapSize=134217728 -XX:+PrintCommandLineFlags\n\n0.6.0-M01\n"

      assert(CliRelease.lastNonBlankLine(output).contains("0.6.0-M01"))
      assert(CliRelease.lastNonBlankLine("\n  \n").isEmpty)

    test("packages a Windows image with its runtime libraries"):
      val root       = os.temp.dir(prefix = "morphir-cli-release-zip-", deleteOnExit = true)
      val imageDir   = root / "image"
      val executable = imageDir / "native-executable.exe"
      os.write(executable, Array[Byte](1, 2, 3), createFolders = true)
      os.write(imageDir / "awt.dll", Array[Byte](4, 5, 6))

      val asset = CliRelease.packageNative(
        CliRelease.Platform.WinAmd64,
        "0.6.0-M01",
        executable,
        root / "release"
      )
      val zip     = ZipFile(asset.toIO)
      val entries = try zip.entries().asScala.map(_.getName).toSet
      finally zip.close()

      assert(entries == Set("morphir.exe", "awt.dll"))
      assert(os.isFile(os.Path(asset.toString + ".sha256")))

    test("packages a Unix image as an executable tar archive"):
      val root       = os.temp.dir(prefix = "morphir-cli-release-tar-", deleteOnExit = true)
      val imageDir   = root / "image"
      val executable = imageDir / "native-executable"
      os.write(executable, Array[Byte](1, 2, 3), createFolders = true)

      val asset = CliRelease.packageNative(
        CliRelease.Platform.LinuxAmd64,
        "0.6.0-M01",
        executable,
        root / "release"
      )

      assert(asset.last == "morphir-cli-linux-amd64-0.6.0-M01.tar.gz")
      assert(os.size(asset) > 0)
      assert(os.isFile(os.Path(asset.toString + ".sha256")))

    test("copies the executable assembly as the JVM fallback"):
      val root     = os.temp.dir(prefix = "morphir-cli-release-jvm-", deleteOnExit = true)
      val assembly = root / "out.jar"
      os.write(assembly, "assembly")

      val asset = CliRelease.packageJvm("0.6.0-M01", assembly, root / "release")

      assert(asset.last == "morphir-cli-jvm-0.6.0-M01.jar")
      assert(os.read(asset) == "assembly")
      assert(os.read(os.Path(asset.toString + ".sha256")).endsWith(s"  ${asset.last}\n"))

    test("verifies the complete release and writes a combined checksum file"):
      val root       = os.temp.dir(prefix = "morphir-cli-release-verify-", deleteOnExit = true)
      val imageDir   = root / "image"
      val executable = imageDir / "native-executable.exe"
      val assembly   = root / "out.jar"
      os.write(executable, Array[Byte](1, 2, 3), createFolders = true)
      os.write(assembly, "assembly")
      CliRelease.packageNative(CliRelease.Platform.WinAmd64, "0.6.0-M01", executable, root / "release")
      CliRelease.packageJvm("0.6.0-M01", assembly, root / "release")

      val result = CliRelease.verifyAndWriteChecksums(
        root / "release",
        "0.6.0-M01",
        Seq(CliRelease.Platform.WinAmd64),
        includeJvm = true
      )

      assert(result.isRight)
      assert(os.read.lines(root / "release" / "checksums.txt").size == 2)

    test("verification reports a corrupt asset without replacing its sidecar"):
      val root       = os.temp.dir(prefix = "morphir-cli-release-corrupt-", deleteOnExit = true)
      val imageDir   = root / "image"
      val executable = imageDir / "native-executable.exe"
      os.write(executable, Array[Byte](1, 2, 3), createFolders = true)
      val asset = CliRelease.packageNative(
        CliRelease.Platform.WinAmd64,
        "0.6.0-M01",
        executable,
        root / "release"
      )
      val originalSidecar = os.read(os.Path(asset.toString + ".sha256"))
      os.write.append(asset, Array[Byte](9))

      val result = CliRelease.verifyAndWriteChecksums(
        root / "release",
        "0.6.0-M01",
        Seq(CliRelease.Platform.WinAmd64),
        includeJvm = false
      )

      assert(result.left.exists(_.exists(_.contains("digest mismatch"))))
      assert(os.read(os.Path(asset.toString + ".sha256")) == originalSidecar)
