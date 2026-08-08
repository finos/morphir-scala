//| moduleDeps: ["//mill-build/src/org/finos/millmorphir/toolchain/NodeDistribution.scala", "//mill-plugins/morphir/toolchain/src/org/finos/morphir/mill/toolchain/AcquisitionSettings.scala"]

import org.finos.millmorphir.toolchain.*
import org.finos.morphir.mill.toolchain.ArchiveFormat

def assertEquals[A](actual: A, expected: A): Unit =
  assert(actual == expected, s"Expected $expected, got $actual")

def distribution(osName: String, osArch: String): NodeDistribution =
  NodeDistribution.resolve(osName, osArch).fold(message => throw new AssertionError(message), identity)

@main def runNodeToolchainTests(): Unit = {
  assertEquals(NodeDistribution.Version, "24.19.0")

  val expected = Seq(
    (
      "Mac OS X",
      "aarch64",
      "node-v24.19.0-darwin-arm64.tar.gz",
      "8294b7aa9b03997481c06babf1e8b270c859358f27da57a11509afe537ac381d"
    ),
    (
      "darwin",
      "arm64",
      "node-v24.19.0-darwin-arm64.tar.gz",
      "8294b7aa9b03997481c06babf1e8b270c859358f27da57a11509afe537ac381d"
    ),
    (
      "macos",
      "amd64",
      "node-v24.19.0-darwin-x64.tar.gz",
      "d1b5e999db158c62fe8f7267a4476b035d8bd93b1a605bac24a3f0dd166e3316"
    ),
    (
      "linux",
      "aarch64",
      "node-v24.19.0-linux-arm64.tar.gz",
      "d28c8a5bf0a808f0ed434a1dce8c54ae98f0371c0bd86ac58abc613f73e6643f"
    ),
    (
      "Linux",
      "x86_64",
      "node-v24.19.0-linux-x64.tar.gz",
      "f625d97cd707df4ff96254916fbc5ff014f09c09effe5a1e0ca8f6d41a8789d4"
    ),
    (
      "windows",
      "arm64",
      "node-v24.19.0-win-arm64.zip",
      "8502f4a50b458d4cc38ed8f2001556c2cd239d464920f74017926ccb1e1c157f"
    ),
    (
      "Windows 11",
      "amd64",
      "node-v24.19.0-win-x64.zip",
      "57f71ab3652e797d84acddc79c81cc9ff1c6ddb2a1974cdb83f00fee9bff4c73"
    )
  )

  expected.foreach { case (osName, osArch, archiveName, sha256) =>
    val resolved = distribution(osName, osArch)
    assertEquals(resolved.version, NodeDistribution.Version)
    assertEquals(resolved.archiveName, archiveName)
    assertEquals(resolved.sha256, sha256)
    assertEquals(resolved.format, if (archiveName.endsWith(".zip")) ArchiveFormat.Zip else ArchiveFormat.TarGz)
  }

  val unix = distribution("linux", "amd64")
  assertEquals(unix.nodeRelativePath, os.rel / "bin" / "node")
  assertEquals(unix.npmCliRelativePath, os.rel / "lib" / "node_modules" / "npm" / "bin" / "npm-cli.js")

  val windows = distribution("windows", "x86_64")
  assertEquals(windows.nodeRelativePath, os.rel / "node.exe")
  assertEquals(windows.npmCliRelativePath, os.rel / "node_modules" / "npm" / "bin" / "npm-cli.js")

  assert(NodeDistribution.resolve("freebsd", "x86_64").isLeft)
  assert(NodeDistribution.resolve("linux", "riscv64").isLeft)
}
