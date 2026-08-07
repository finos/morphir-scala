//| moduleDeps: ["//mill-build/src/org/finos/millmorphir/toolchain/NodeDistribution.scala", "//mill-build/src/org/finos/millmorphir/toolchain/VerifiedArchive.scala"]
//| mvnDeps: ["org.apache.commons:commons-compress:1.28.0"]

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveOutputStream, TarConstants}
import org.finos.millmorphir.toolchain.*

def assertEquals[A](actual: A, expected: A): Unit =
  assert(actual == expected, s"Expected $expected, got $actual")

def distribution(osName: String, osArch: String): NodeDistribution =
  NodeDistribution.resolve(osName, osArch).fold(message => throw new AssertionError(message), identity)

def withTempDir[A](f: os.Path => A): A = {
  val directory = os.Path(Files.createTempDirectory("node-toolchain-test"))
  try f(directory)
  finally os.remove.all(directory)
}

def writeTarGz(path: os.Path, entries: Seq[(String, Array[Byte], Option[String])]): Unit = {
  val output = new TarArchiveOutputStream(
    new java.util.zip.GZIPOutputStream(Files.newOutputStream(path.toNIO))
  )
  try {
    entries.foreach { case (name, contents, linkTarget) =>
      val entry = linkTarget match {
        case Some(target) =>
          val link = new TarArchiveEntry(name, TarConstants.LF_SYMLINK)
          link.setLinkName(target)
          link
        case None =>
          val file = new TarArchiveEntry(name)
          file.setSize(contents.length.toLong)
          file.setMode(0x1ed)
          file
      }
      output.putArchiveEntry(entry)
      if (linkTarget.isEmpty) output.write(contents)
      output.closeArchiveEntry()
    }
    output.finish()
  } finally output.close()
}

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

  val abc = "abc".getBytes(StandardCharsets.UTF_8)
  assertEquals(VerifiedArchive.sha256(abc), "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")

  withTempDir { directory =>
    val file = directory / "abc"
    os.write(file, abc)
    VerifiedArchive.verifySha256(file, VerifiedArchive.sha256(abc))
    assert(scala.util.Try(VerifiedArchive.verifySha256(file, "0" * 64)).isFailure)

    assertEquals(VerifiedArchive.safeTarget(directory, "safe/node"), Right(directory / "safe" / "node"))
    Seq("../escape", "/absolute", "C:\\escape", "C:/escape").foreach { unsafe =>
      assert(VerifiedArchive.safeTarget(directory, unsafe).isLeft, s"Expected unsafe path to be rejected: $unsafe")
    }
  }

  withTempDir { directory =>
    val archive = directory / "node.tar.gz"
    writeTarGz(
      archive,
      Seq(
        ("node-v24.19.0-test/bin/node", "node".getBytes(StandardCharsets.UTF_8), None),
        ("node-v24.19.0-test/link", Array.emptyByteArray, Some("../../escape"))
      )
    )
    val destination = directory / "extracted"
    val result      = scala.util.Try(
      VerifiedArchive.downloadAndExtract(
        archive.toNIO.toUri.toURL,
        VerifiedArchive.sha256(archive),
        ArchiveFormat.TarGz,
        destination
      )
    )
    assert(result.isFailure, "Expected an escaping symbolic link to be rejected")
    assert(!os.exists(directory / "escape"))
  }

  withTempDir { directory =>
    val archive = directory / "node.tar.gz"
    writeTarGz(
      archive,
      Seq(("node-v24.19.0-test/bin/node", "node".getBytes(StandardCharsets.UTF_8), None))
    )
    val rejected = directory / "rejected"
    assert(
      scala.util
        .Try(VerifiedArchive.downloadAndExtract(archive.toNIO.toUri.toURL, "0" * 64, ArchiveFormat.TarGz, rejected))
        .isFailure
    )
    assert(!os.exists(rejected / "bin"), "A checksum mismatch must be rejected before extraction")

    val destination = directory / "extracted"
    VerifiedArchive.downloadAndExtract(
      archive.toNIO.toUri.toURL,
      VerifiedArchive.sha256(archive),
      ArchiveFormat.TarGz,
      destination
    )
    assertEquals(os.read(destination / "bin" / "node"), "node")
    assert(Files.isExecutable((destination / "bin" / "node").toNIO))
  }
}
