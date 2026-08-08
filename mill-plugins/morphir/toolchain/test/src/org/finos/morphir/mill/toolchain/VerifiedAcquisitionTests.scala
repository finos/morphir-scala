package org.finos.morphir.mill.toolchain

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.channels.FileChannel
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardOpenOption}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}

import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveOutputStream, TarConstants}
import org.apache.commons.compress.archivers.zip.{ZipArchiveEntry, ZipArchiveOutputStream}
import utest.*

object VerifiedAcquisitionTests extends TestSuite {
  private final class TrackingInputStream(bytes: Array[Byte]) extends ByteArrayInputStream(bytes) {
    var wasClosed              = false
    override def close(): Unit = {
      wasClosed = true
      super.close()
    }
  }

  private def withTempDir[A](f: os.Path => A): A = {
    val directory = os.Path(Files.createTempDirectory("verified-acquisition-test"))
    try f(directory)
    finally os.remove.all(directory)
  }

  private def writeTarGz(path: os.Path, entries: Seq[(String, Array[Byte], Option[String])]): Unit = {
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

  private def writeZip(path: os.Path, entries: Seq[(String, Array[Byte], Int)]): Unit = {
    val output = new ZipArchiveOutputStream(path.toNIO)
    try {
      entries.foreach { case (name, contents, unixMode) =>
        val entry = new ZipArchiveEntry(name)
        entry.setUnixMode(unixMode)
        output.putArchiveEntry(entry)
        output.write(contents)
        output.closeArchiveEntry()
      }
      output.finish()
    } finally output.close()
  }

  private def overwriteZipLocalExtraLength(path: os.Path, length: Int): Unit = {
    val bytes = ByteBuffer.allocate(2).order(ByteOrder.LITTLE_ENDIAN).putShort(length.toShort)
    bytes.flip()
    val channel = FileChannel.open(path.toNIO, StandardOpenOption.WRITE)
    try {
      var position = 28L
      while (bytes.hasRemaining) position += channel.write(bytes, position)
    } finally channel.close()
  }

  private def writeTarGzSpecial(path: os.Path, name: String, linkFlag: Byte): Unit = {
    val output = new TarArchiveOutputStream(
      new java.util.zip.GZIPOutputStream(Files.newOutputStream(path.toNIO))
    )
    try {
      val entry = new TarArchiveEntry(name, linkFlag)
      entry.setSize(0L)
      output.putArchiveEntry(entry)
      output.closeArchiveEntry()
      output.finish()
    } finally output.close()
  }

  private def writeSyntheticZip64(path: os.Path, entries: Long): Unit = {
    val bytes = ByteBuffer.allocate(56 + 20 + 22).order(ByteOrder.LITTLE_ENDIAN)
    bytes.putInt(0x06064b50)
    bytes.putLong(44L)
    bytes.putShort(45.toShort)
    bytes.putShort(45.toShort)
    bytes.putInt(0)
    bytes.putInt(0)
    bytes.putLong(entries)
    bytes.putLong(entries)
    bytes.putLong(0L)
    bytes.putLong(0L)
    bytes.putInt(0x07064b50)
    bytes.putInt(0)
    bytes.putLong(0L)
    bytes.putInt(1)
    bytes.putInt(0x06054b50)
    bytes.putShort(0.toShort)
    bytes.putShort(0.toShort)
    bytes.putShort(0xffff.toShort)
    bytes.putShort(0xffff.toShort)
    bytes.putInt(0xffffffffL.toInt)
    bytes.putInt(0xffffffffL.toInt)
    bytes.putShort(0.toShort)
    os.write(path, bytes.array())
  }

  private def writeSyntheticZip64CentralDirectory(path: os.Path, actualEntries: Int, declaredEntries: Long): Unit = {
    val centralBytes = actualEntries * 46
    val bytes        = ByteBuffer.allocate(centralBytes + 56 + 20 + 22).order(ByteOrder.LITTLE_ENDIAN)
    (0 until actualEntries).foreach { _ =>
      bytes.putInt(0x02014b50)
      bytes.position(bytes.position() + 42)
    }
    bytes.putInt(0x06064b50)
    bytes.putLong(44L)
    bytes.putShort(45.toShort)
    bytes.putShort(45.toShort)
    bytes.putInt(0)
    bytes.putInt(0)
    bytes.putLong(declaredEntries)
    bytes.putLong(declaredEntries)
    bytes.putLong(centralBytes.toLong)
    bytes.putLong(0L)
    bytes.putInt(0x07064b50)
    bytes.putInt(0)
    bytes.putLong(centralBytes.toLong)
    bytes.putInt(1)
    bytes.putInt(0x06054b50)
    bytes.putShort(0.toShort)
    bytes.putShort(0.toShort)
    bytes.putShort(0xffff.toShort)
    bytes.putShort(0xffff.toShort)
    bytes.putInt(0xffffffffL.toInt)
    bytes.putInt(0xffffffffL.toInt)
    bytes.putShort(0.toShort)
    os.write(path, bytes.array())
  }

  private def writeSyntheticZipEnd(path: os.Path, disk: Int, declaredCommentBytes: Int): Unit = {
    val bytes = ByteBuffer.allocate(22).order(ByteOrder.LITTLE_ENDIAN)
    bytes.putInt(0x06054b50)
    bytes.putShort(disk.toShort)
    bytes.putShort(disk.toShort)
    bytes.putShort(0.toShort)
    bytes.putShort(0.toShort)
    bytes.putInt(0)
    bytes.putInt(0)
    bytes.putShort(declaredCommentBytes.toShort)
    os.write(path, bytes.array())
  }

  private def writeSyntheticCentralDirectoryZip(
      path: os.Path,
      actualEntries: Int,
      declaredEntries: Int,
      trailing: Array[Byte] = Array.emptyByteArray,
      firstNameLength: Int = 0
  ): Unit = {
    val centralBytes = actualEntries * 46 + trailing.length
    val bytes        = ByteBuffer.allocate(centralBytes + 22).order(ByteOrder.LITTLE_ENDIAN)
    (0 until actualEntries).foreach { index =>
      val start = bytes.position()
      bytes.putInt(0x02014b50)
      bytes.position(start + 28)
      bytes.putShort((if (index == 0) firstNameLength else 0).toShort)
      bytes.position(start + 46)
    }
    bytes.put(trailing)
    bytes.putInt(0x06054b50)
    bytes.putShort(0.toShort)
    bytes.putShort(0.toShort)
    bytes.putShort(declaredEntries.toShort)
    bytes.putShort(declaredEntries.toShort)
    bytes.putInt(centralBytes)
    bytes.putInt(0)
    bytes.putShort(0.toShort)
    os.write(path, bytes.array())
  }

  private def writeGzipBytes(path: os.Path, bytes: Array[Byte]): Unit = {
    val output = new java.util.zip.GZIPOutputStream(Files.newOutputStream(path.toNIO))
    try output.write(bytes)
    finally output.close()
  }

  private def appendBytes(path: os.Path, bytes: Array[Byte]): Unit = {
    val output = Files.newOutputStream(path.toNIO, StandardOpenOption.APPEND)
    try output.write(bytes)
    finally output.close()
  }

  private def readGzipBytes(path: os.Path): Array[Byte] = {
    val input = new java.util.zip.GZIPInputStream(Files.newInputStream(path.toNIO))
    try input.readAllBytes()
    finally input.close()
  }

  private def gzipMember(bytes: Array[Byte]): Array[Byte] = {
    val buffer = new ByteArrayOutputStream()
    val output = new java.util.zip.GZIPOutputStream(buffer)
    try output.write(bytes)
    finally output.close()
    buffer.toByteArray
  }

  private def writeConcatenatedGzip(path: os.Path, parts: Seq[Array[Byte]]): Unit = {
    val output = Files.newOutputStream(path.toNIO)
    try parts.foreach(part => output.write(gzipMember(part)))
    finally output.close()
  }

  private def addGzipHeaderCrc(path: os.Path): Unit = {
    val bytes  = os.read.bytes(path)
    val header = bytes.take(10)
    header(3) = (header(3) | 0x02).toByte
    val crc = new java.util.zip.CRC32()
    crc.update(header)
    val output = new ByteArrayOutputStream()
    output.write(header)
    output.write((crc.getValue & 0xff).toInt)
    output.write(((crc.getValue >>> 8) & 0xff).toInt)
    output.write(bytes, 10, bytes.length - 10)
    os.write.over(path, output.toByteArray)
  }

  private def writeSyntheticGzipHeader(
      path: os.Path,
      flags: Int,
      method: Int = 8,
      extra: Array[Byte] = Array.emptyByteArray,
      fileName: Array[Byte] = Array.emptyByteArray,
      terminateFileName: Boolean = true,
      comment: Array[Byte] = Array.emptyByteArray,
      terminateComment: Boolean = true,
      headerCrc: Boolean = false
  ): Unit = {
    val output = new ByteArrayOutputStream()
    output.write(Array[Byte](0x1f, 0x8b.toByte, method.toByte, flags.toByte, 0, 0, 0, 0, 0, 3))
    if ((flags & 0x04) != 0) {
      output.write(extra.length & 0xff)
      output.write((extra.length >>> 8) & 0xff)
      output.write(extra)
    }
    if ((flags & 0x08) != 0) {
      output.write(fileName)
      if (terminateFileName) output.write(0)
    }
    if ((flags & 0x10) != 0) {
      output.write(comment)
      if (terminateComment) output.write(0)
    }
    if ((flags & 0x02) != 0 && headerCrc) output.write(Array[Byte](0, 0))
    os.write(path, output.toByteArray)
  }

  private def writeRawTarGz(
      path: os.Path,
      entries: Seq[(Byte, Array[Byte])],
      base256Sizes: Set[Int] = Set.empty,
      trailingZeroBytes: Int = 0
  ): Unit = {
    val output = new java.util.zip.GZIPOutputStream(Files.newOutputStream(path.toNIO))
    try {
      entries.zipWithIndex.foreach { case ((entryType, contents), index) =>
        val header                                  = new Array[Byte](512)
        def ascii(offset: Int, value: String): Unit = {
          val bytes = value.getBytes(StandardCharsets.US_ASCII)
          System.arraycopy(bytes, 0, header, offset, bytes.length)
        }
        def octal(offset: Int, length: Int, value: Long): Unit = {
          val digits = java.lang.Long.toOctalString(value)
          ascii(offset, "0" * (length - 1 - digits.length) + digits + "\u0000")
        }

        ascii(0, s"root/entry-$index")
        octal(100, 8, 0x1a4)
        octal(108, 8, 0)
        octal(116, 8, 0)
        if (base256Sizes.contains(index)) {
          header(124) = 0x80.toByte
          var value    = contents.length.toLong
          var position = 135
          while (value > 0) {
            header(position) = (value & 0xff).toByte
            value >>>= 8
            position -= 1
          }
        } else octal(124, 12, contents.length.toLong)
        octal(136, 12, 0)
        java.util.Arrays.fill(header, 148, 156, ' '.toByte)
        header(156) = entryType
        ascii(257, "ustar\u0000")
        ascii(263, "00")
        val checksum = header.iterator.map(_ & 0xff).sum
        ascii(148, f"$checksum%06o\u0000 ")
        output.write(header)
        output.write(contents)
        val padding = (512 - contents.length % 512) % 512
        output.write(new Array[Byte](padding))
      }
      output.write(new Array[Byte](1024))
      output.write(new Array[Byte](trailingZeroBytes))
    } finally output.close()
  }

  val tests = Tests {
    test("rejects a checksum mismatch before extraction") {
      withTempDir { directory =>
        val archive = directory / "node.tar.gz"
        writeTarGz(
          archive,
          Seq(("node-v-test/bin/node", "node".getBytes(StandardCharsets.UTF_8), None))
        )

        val destination = directory / "rejected"
        val result      = scala.util.Try(
          VerifiedArchive.extract(VerifiedContent(archive, "0" * 64), ArchiveFormat.TarGz, destination)
        )

        assert(result.isFailure)
        assert(!os.exists(destination))
      }
    }

    test("computes SHA-256 and rejects unsafe archive paths") {
      val abc = "abc".getBytes(StandardCharsets.UTF_8)
      assert(VerifiedArchive.sha256(abc) == "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")

      withTempDir { directory =>
        val file = directory / "abc"
        os.write(file, abc)
        VerifiedArchive.verifySha256(file, VerifiedArchive.sha256(abc))
        assert(scala.util.Try(VerifiedArchive.verifySha256(file, "0" * 64)).isFailure)
        assert(VerifiedArchive.safeTarget(directory, "safe/node") == Right(directory / "safe" / "node"))
        Seq("../escape", "/absolute", "C:\\escape", "C:/escape").foreach { unsafe =>
          assert(VerifiedArchive.safeTarget(directory, unsafe).isLeft)
        }
      }
    }

    test("extracts a verified tar archive and preserves executable mode") {
      withTempDir { directory =>
        val archive = directory / "node.tar.gz"
        writeTarGz(
          archive,
          Seq(("node-v-test/bin/node", "node".getBytes(StandardCharsets.UTF_8), None))
        )
        val destination = directory / "extracted"

        VerifiedArchive.extract(
          VerifiedContent(archive, VerifiedArchive.sha256(archive)),
          ArchiveFormat.TarGz,
          destination
        )

        assert(os.read(destination / "bin" / "node") == "node")
        assert(Files.isExecutable((destination / "bin" / "node").toNIO))
      }
    }

    test("rejects an escaping tar symbolic link") {
      withTempDir { directory =>
        val archive = directory / "node.tar.gz"
        writeTarGz(
          archive,
          Seq(
            ("node-v-test/bin/node", "node".getBytes(StandardCharsets.UTF_8), None),
            ("node-v-test/link", Array.emptyByteArray, Some("../../escape"))
          )
        )

        val result = scala.util.Try(
          VerifiedArchive.extract(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.TarGz,
            directory / "extracted"
          )
        )

        assert(result.isFailure)
        assert(!os.exists(directory / "escape"))
      }
    }

    test("uses ZIP central-directory metadata for file mode") {
      withTempDir { directory =>
        val archive = directory / "node.zip"
        writeZip(
          archive,
          Seq(("node-v-test/node.exe", "node".getBytes(StandardCharsets.UTF_8), 0x81ed))
        )
        val destination = directory / "extracted"

        VerifiedArchive.extract(
          VerifiedContent(archive, VerifiedArchive.sha256(archive)),
          ArchiveFormat.Zip,
          destination
        )

        assert(os.read(destination / "node.exe") == "node")
        assert(Files.isExecutable((destination / "node.exe").toNIO))
      }
    }

    test("ZIP construction ignores malformed local-header extra data after bounded central preflight") {
      withTempDir { directory =>
        val archive = directory / "malformed-local-extra.zip"
        writeZip(archive, Seq(("root/bin/tool", "tool".getBytes(StandardCharsets.UTF_8), 0x81ed)))
        overwriteZipLocalExtraLength(archive, 32767)
        var parserOpened = false
        var constructed  = false

        val result = scala.util.Try(
          VerifiedArchive.extractObserved(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.Zip,
            directory / "extracted",
            parserObserver = _ => parserOpened = true,
            zipConstructedObserver = () => constructed = true
          ) {}
        )

        assert(result.isFailure)
        assert(parserOpened)
        assert(constructed)
        assert(!os.exists(directory / "extracted"))
      }
    }

    test("rejects ZIP traversal") {
      withTempDir { directory =>
        val archive = directory / "traversal.zip"
        writeZip(
          archive,
          Seq(("node-v-test/../escape", "escape".getBytes(StandardCharsets.UTF_8), 0x81a4))
        )

        val result = scala.util.Try(
          VerifiedArchive.extract(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.Zip,
            directory / "extracted"
          )
        )

        assert(result.isFailure)
        assert(!os.exists(directory / "escape"))
      }
    }

    test("rejects a ZIP symbolic link that escapes the destination") {
      withTempDir { directory =>
        val archive = directory / "symlink.zip"
        writeZip(
          archive,
          Seq(("node-v-test/link", "../../escape".getBytes(StandardCharsets.UTF_8), 0xa1ff))
        )

        val result = scala.util.Try(
          VerifiedArchive.extract(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.Zip,
            directory / "extracted"
          )
        )

        assert(result.isFailure)
        assert(!os.exists(directory / "escape"))
      }
    }

    test("cold acquisition verifies and stores content in the machine cache") {
      withTempDir { directory =>
        val bytes  = "verified bytes".getBytes(StandardCharsets.UTF_8)
        val digest = VerifiedArchive.sha256(bytes)
        val input  = new TrackingInputStream(bytes)
        val cache  = AcquisitionCache(
          AcquisitionSettings(cacheRoot = Some(directory / "machine-cache")),
          directory / "task"
        )

        val content = cache.acquire(digest, "memory:test")(input)

        assert(content.sha256 == digest)
        assert(os.read.bytes(content.path).sameElements(bytes))
        assert(content.path.toNIO.startsWith((directory / "machine-cache").toNIO))
        assert(input.wasClosed)
      }
    }

    test("warm acquisition reuses verified cached bytes without opening the source") {
      withTempDir { directory =>
        val bytes    = "warm bytes".getBytes(StandardCharsets.UTF_8)
        val digest   = VerifiedArchive.sha256(bytes)
        val settings = AcquisitionSettings(cacheRoot = Some(directory / "machine-cache"))
        val first    = AcquisitionCache(settings, directory / "task-one")
          .acquire(digest, "memory:first")(new ByteArrayInputStream(bytes))

        val second = AcquisitionCache(settings, directory / "task-two")
          .acquire(digest, "memory:must-not-open")(
            throw new java.lang.AssertionError("warm acquisition opened its byte source")
          )

        assert(second == first)
      }
    }

    test("warm machine-cache hits enforce the caller acquisition limit without removing content") {
      withTempDir { directory =>
        val bytes    = "12345".getBytes(StandardCharsets.UTF_8)
        val digest   = VerifiedArchive.sha256(bytes)
        val settings = AcquisitionSettings(cacheRoot = Some(directory / "machine-cache"))
        val cache    = AcquisitionCache(settings, directory / "task")
        val seeded   = cache.acquire(digest, "memory:seed", AcquisitionLimits(maxAcquiredBytes = 5))(
          new ByteArrayInputStream(bytes)
        )
        var opened = false

        val limited = scala.util.Try(
          cache.acquire(digest, "memory:limited", AcquisitionLimits(maxAcquiredBytes = 4)) {
            opened = true
            new ByteArrayInputStream(bytes)
          }
        )

        assert(limited.isFailure)
        assert(limited.failed.get.getMessage.contains("acquired byte limit"))
        assert(opened)
        assert(os.read.bytes(seeded.path).sameElements(bytes))

        val reused = cache.acquire(digest, "memory:larger", AcquisitionLimits(maxAcquiredBytes = 5))(
          throw new java.lang.AssertionError("larger warm acquisition opened its source")
        )
        assert(reused == seeded)
      }
    }

    test("oversized corrupt cache entries are reacquired without hashing the candidate") {
      withTempDir { directory =>
        val expected  = "1234".getBytes(StandardCharsets.UTF_8)
        val corrupt   = "abcde".getBytes(StandardCharsets.UTF_8)
        val digest    = VerifiedArchive.sha256(expected)
        val cacheRoot = directory / "machine-cache"
        val entry     = cacheRoot / "sha256" / digest
        os.makeDir.all(entry / os.up)
        os.write(entry, corrupt)
        var opened          = false
        var candidateHashes = 0

        val cache = AcquisitionCache.withVerifier(
          AcquisitionSettings(cacheRoot = Some(cacheRoot)),
          directory / "task"
        ) { (path, expectedDigest) =>
          if (path == entry) candidateHashes += 1
          VerifiedArchive.verifySha256(path, expectedDigest)
        }
        val reacquired = cache
          .acquire(digest, "memory:oversized-corrupt", AcquisitionLimits(maxAcquiredBytes = 4)) {
            opened = true
            new ByteArrayInputStream(expected)
          }

        assert(opened)
        assert(candidateHashes == 0)
        assert(reacquired.path == entry)
        assert(os.read.bytes(entry).sameElements(expected))
      }
    }

    test("offline oversized corrupt cache entries fail without hashing or acquisition") {
      withTempDir { directory =>
        val expected  = "1234".getBytes(StandardCharsets.UTF_8)
        val corrupt   = "abcde".getBytes(StandardCharsets.UTF_8)
        val digest    = VerifiedArchive.sha256(expected)
        val cacheRoot = directory / "machine-cache"
        val entry     = cacheRoot / "sha256" / digest
        os.makeDir.all(entry / os.up)
        os.write(entry, corrupt)
        var opened          = false
        var candidateHashes = 0

        val cache = AcquisitionCache.withVerifier(
          AcquisitionSettings(cacheRoot = Some(cacheRoot), offline = true),
          directory / "task"
        ) { (path, expectedDigest) =>
          if (path == entry) candidateHashes += 1
          VerifiedArchive.verifySha256(path, expectedDigest)
        }
        val result = scala.util.Try(
          cache.acquire(digest, "memory:offline-oversized-corrupt", AcquisitionLimits(maxAcquiredBytes = 4)) {
            opened = true
            new ByteArrayInputStream(expected)
          }
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("Offline acquisition"))
        assert(!opened)
        assert(candidateHashes == 0)
        assert(os.read.bytes(entry).sameElements(corrupt))
      }
    }

    test("failed oversized candidate reacquisition retains the candidate for recovery") {
      withTempDir { directory =>
        val expected  = "1234".getBytes(StandardCharsets.UTF_8)
        val corrupt   = "abcde".getBytes(StandardCharsets.UTF_8)
        val digest    = VerifiedArchive.sha256(expected)
        val cacheRoot = directory / "machine-cache"
        val entry     = cacheRoot / "sha256" / digest
        os.makeDir.all(entry / os.up)
        os.write(entry, corrupt)
        val cache = AcquisitionCache(AcquisitionSettings(cacheRoot = Some(cacheRoot)), directory / "task")

        val failed = scala.util.Try(
          cache.acquire(digest, "memory:retention", AcquisitionLimits(maxAcquiredBytes = 4)) {
            throw new java.io.IOException("source unavailable")
          }
        )

        assert(failed.isFailure)
        assert(failed.failed.get.getMessage == "source unavailable")
        assert(os.read.bytes(entry).sameElements(corrupt))

        val recovered = cache.acquire(digest, "memory:recovery", AcquisitionLimits(maxAcquiredBytes = 4))(
          new ByteArrayInputStream(expected)
        )
        assert(recovered.path == entry)
        assert(os.read.bytes(entry).sameElements(expected))
      }
    }

    test("disabled machine cache keeps verified content task-local") {
      withTempDir { directory =>
        val bytes       = "task-local bytes".getBytes(StandardCharsets.UTF_8)
        val digest      = VerifiedArchive.sha256(bytes)
        val machineRoot = directory / "machine-cache"
        val taskRoot    = directory / "task"
        val cache       = AcquisitionCache(
          AcquisitionSettings(cacheRoot = Some(machineRoot), useMachineCache = false),
          taskRoot
        )

        val content = cache.acquire(digest, "memory:disabled")(new ByteArrayInputStream(bytes))

        assert(content.path == taskRoot / ".morphir-acquisitions" / "sha256" / digest)
        assert(content.path.toNIO.startsWith(taskRoot.toNIO))
        assert(!os.exists(machineRoot))
        assert(os.read.bytes(content.path).sameElements(bytes))
      }
    }

    test("disabled acquisition rejects a reserved cache symlink without following it") {
      withTempDir { directory =>
        val bytes    = "task-local symlink".getBytes(StandardCharsets.UTF_8)
        val digest   = VerifiedArchive.sha256(bytes)
        val taskRoot = directory / "task"
        val external = directory / "external"
        os.makeDir.all(taskRoot)
        os.makeDir.all(external)
        Files.createSymbolicLink((taskRoot / ".morphir-acquisitions").toNIO, external.toNIO)
        var opened = false

        val result = scala.util.Try(
          AcquisitionCache(AcquisitionSettings(useMachineCache = false), taskRoot)
            .acquire(digest, "memory:reserved-symlink") {
              opened = true
              new ByteArrayInputStream(bytes)
            }
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("task-local acquisition cache"))
        assert(!opened)
        assert(Files.isSymbolicLink((taskRoot / ".morphir-acquisitions").toNIO))
        assert(os.list(external).isEmpty)
      }
    }

    test("disabled acquisition rejects a reserved cache file collision") {
      withTempDir { directory =>
        val bytes    = "task-local collision".getBytes(StandardCharsets.UTF_8)
        val digest   = VerifiedArchive.sha256(bytes)
        val taskRoot = directory / "task"
        val reserved = taskRoot / ".morphir-acquisitions"
        os.makeDir.all(taskRoot)
        os.write(reserved, "reserved collision")
        var opened = false

        val result = scala.util.Try(
          AcquisitionCache(AcquisitionSettings(useMachineCache = false), taskRoot)
            .acquire(digest, "memory:reserved-collision") {
              opened = true
              new ByteArrayInputStream(bytes)
            }
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("task-local acquisition cache"))
        assert(!opened)
        assert(os.read(reserved) == "reserved collision")
      }
    }

    test("warm task-local hits enforce the caller acquisition limit without removing content") {
      withTempDir { directory =>
        val bytes    = "12345".getBytes(StandardCharsets.UTF_8)
        val digest   = VerifiedArchive.sha256(bytes)
        val taskRoot = directory / "task"
        val cache    = AcquisitionCache(AcquisitionSettings(useMachineCache = false), taskRoot)
        val seeded   = cache.acquire(digest, "memory:seed", AcquisitionLimits(maxAcquiredBytes = 5))(
          new ByteArrayInputStream(bytes)
        )
        var opened = false

        val limited = scala.util.Try(
          cache.acquire(digest, "memory:limited", AcquisitionLimits(maxAcquiredBytes = 4)) {
            opened = true
            new ByteArrayInputStream(bytes)
          }
        )

        assert(limited.isFailure)
        assert(limited.failed.get.getMessage.contains("acquired byte limit"))
        assert(opened)
        assert(os.read.bytes(seeded.path).sameElements(bytes))

        val reused = cache.acquire(digest, "memory:larger", AcquisitionLimits(maxAcquiredBytes = 5))(
          throw new java.lang.AssertionError("larger task-local acquisition opened its source")
        )
        assert(reused == seeded)
      }
    }

    test("enabled and disabled acquisition extract identical task output") {
      withTempDir { directory =>
        val archive = directory / "node.tar.gz"
        writeTarGz(
          archive,
          Seq(("node-v-test/bin/node", "node".getBytes(StandardCharsets.UTF_8), None))
        )
        val bytes  = os.read.bytes(archive)
        val digest = VerifiedArchive.sha256(bytes)

        def acquireAndExtract(name: String, settings: AcquisitionSettings): os.Path = {
          val taskRoot    = directory / name
          val destination = taskRoot / "node"
          val content     = AcquisitionCache(settings, taskRoot)
            .acquire(digest, s"memory:$name")(new ByteArrayInputStream(bytes))
          VerifiedArchive.extract(content, ArchiveFormat.TarGz, destination)
          destination
        }

        val enabled = acquireAndExtract(
          "enabled",
          AcquisitionSettings(cacheRoot = Some(directory / "machine-cache"))
        )
        val disabled = acquireAndExtract(
          "disabled",
          AcquisitionSettings(useMachineCache = false)
        )

        assert(os.walk(enabled).map(_.relativeTo(enabled)).toSet == os.walk(disabled).map(_.relativeTo(disabled)).toSet)
        assert(os.read.bytes(enabled / "bin" / "node").sameElements(os.read.bytes(disabled / "bin" / "node")))
      }
    }

    test("corrupt cached bytes are rejected and reacquired") {
      withTempDir { directory =>
        val bytes    = "expected bytes".getBytes(StandardCharsets.UTF_8)
        val digest   = VerifiedArchive.sha256(bytes)
        val settings = AcquisitionSettings(cacheRoot = Some(directory / "machine-cache"))
        val cached   = AcquisitionCache(settings, directory / "task-one")
          .acquire(digest, "memory:first")(new ByteArrayInputStream(bytes))
        os.write.over(cached.path, "corrupt")

        val replacement = new TrackingInputStream(bytes)
        val reacquired  = AcquisitionCache(settings, directory / "task-two")
          .acquire(digest, "memory:replacement")(replacement)

        assert(reacquired == cached)
        assert(os.read.bytes(reacquired.path).sameElements(bytes))
        assert(replacement.wasClosed)
      }
    }

    test("offline cold acquisition fails without opening the source") {
      withTempDir { directory =>
        val bytes  = "offline cold".getBytes(StandardCharsets.UTF_8)
        val digest = VerifiedArchive.sha256(bytes)
        var opened = false
        val cache  = AcquisitionCache(
          AcquisitionSettings(cacheRoot = Some(directory / "machine-cache"), offline = true),
          directory / "task"
        )

        val result = scala.util.Try(
          cache.acquire(digest, "memory:offline-cold") {
            opened = true
            new ByteArrayInputStream(bytes)
          }
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.toLowerCase(java.util.Locale.ROOT).contains("offline"))
        assert(!opened)
      }
    }

    test("offline warm acquisition uses already verified content") {
      withTempDir { directory =>
        val bytes     = "offline warm".getBytes(StandardCharsets.UTF_8)
        val digest    = VerifiedArchive.sha256(bytes)
        val cacheRoot = directory / "machine-cache"
        val cached    = AcquisitionCache(AcquisitionSettings(cacheRoot = Some(cacheRoot)), directory / "online-task")
          .acquire(digest, "memory:online")(new ByteArrayInputStream(bytes))

        val offline = AcquisitionCache(
          AcquisitionSettings(cacheRoot = Some(cacheRoot), offline = true),
          directory / "offline-task"
        ).acquire(digest, "memory:must-not-open")(
          throw new java.lang.AssertionError("offline warm acquisition opened its byte source")
        )

        assert(offline == cached)
      }
    }

    test("offline warm hits enforce the caller acquisition limit without removing content") {
      withTempDir { directory =>
        val bytes     = "12345".getBytes(StandardCharsets.UTF_8)
        val digest    = VerifiedArchive.sha256(bytes)
        val cacheRoot = directory / "machine-cache"
        val seeded    = AcquisitionCache(AcquisitionSettings(cacheRoot = Some(cacheRoot)), directory / "online")
          .acquire(digest, "memory:seed", AcquisitionLimits(maxAcquiredBytes = 5))(
            new ByteArrayInputStream(bytes)
          )
        val offline = AcquisitionCache(
          AcquisitionSettings(cacheRoot = Some(cacheRoot), offline = true),
          directory / "offline"
        )
        var opened = false

        val limited = scala.util.Try(
          offline.acquire(digest, "memory:limited", AcquisitionLimits(maxAcquiredBytes = 4)) {
            opened = true
            new ByteArrayInputStream(bytes)
          }
        )

        assert(limited.isFailure)
        assert(limited.failed.get.getMessage.contains("Offline acquisition"))
        assert(!opened)
        assert(os.read.bytes(seeded.path).sameElements(bytes))

        val reused = offline.acquire(digest, "memory:larger", AcquisitionLimits(maxAcquiredBytes = 5))(
          throw new java.lang.AssertionError("larger offline acquisition opened its source")
        )
        assert(reused == seeded)
      }
    }

    test("offline corrupt acquisition fails without opening the source") {
      withTempDir { directory =>
        val bytes     = "offline corrupt".getBytes(StandardCharsets.UTF_8)
        val digest    = VerifiedArchive.sha256(bytes)
        val cacheRoot = directory / "machine-cache"
        val cached    = AcquisitionCache(AcquisitionSettings(cacheRoot = Some(cacheRoot)), directory / "online-task")
          .acquire(digest, "memory:online")(new ByteArrayInputStream(bytes))
        os.write.over(cached.path, "corrupt")
        var opened = false

        val result = scala.util.Try(
          AcquisitionCache(
            AcquisitionSettings(cacheRoot = Some(cacheRoot), offline = true),
            directory / "offline-task"
          ).acquire(digest, "memory:offline-corrupt") {
            opened = true
            new ByteArrayInputStream(bytes)
          }
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.toLowerCase(java.util.Locale.ROOT).contains("offline"))
        assert(!opened)
      }
    }

    test("concurrent acquisition coordinates one source read") {
      withTempDir { directory =>
        val bytes              = "concurrent bytes".getBytes(StandardCharsets.UTF_8)
        val digest             = VerifiedArchive.sha256(bytes)
        val settings           = AcquisitionSettings(cacheRoot = Some(directory / "machine-cache"))
        val attempts           = new AtomicInteger(0)
        val firstOpened        = new CountDownLatch(1)
        val releaseFirst       = new CountDownLatch(1)
        val contendersLive     = new CountDownLatch(7)
        given ExecutionContext = ExecutionContext.global

        def acquire(task: String): VerifiedContent =
          AcquisitionCache(settings, directory / task).acquire(digest, s"memory:$task") {
            val attempt = attempts.incrementAndGet()
            if (attempt == 1) {
              firstOpened.countDown()
              releaseFirst.await(5, TimeUnit.SECONDS)
            }
            new ByteArrayInputStream(bytes)
          }

        val first = Future(acquire("task-0"))
        assert(firstOpened.await(5, TimeUnit.SECONDS))
        val contenders = (1 to 7).map { index =>
          Future {
            contendersLive.countDown()
            acquire(s"task-$index")
          }
        }
        assert(contendersLive.await(5, TimeUnit.SECONDS))
        Thread.sleep(100)
        releaseFirst.countDown()

        val contents = Await.result(Future.sequence(first +: contenders), 10.seconds)
        assert(attempts.get() == 1)
        assert(contents.map(_.path).distinct.size == 1)
      }
    }

    test("overridden cache root contains the digest-keyed entry") {
      withTempDir { directory =>
        val bytes      = "overridden root".getBytes(StandardCharsets.UTF_8)
        val digest     = VerifiedArchive.sha256(bytes)
        val overridden = directory / "custom" / "cache"

        val content = AcquisitionCache(
          AcquisitionSettings(cacheRoot = Some(overridden)),
          directory / "task"
        ).acquire(digest, "memory:override")(new ByteArrayInputStream(bytes))

        assert(content.path == overridden / "sha256" / digest)
      }
    }

    test("failed verification closes the source and never promotes content") {
      withTempDir { directory =>
        val expectedBytes = "expected".getBytes(StandardCharsets.UTF_8)
        val suppliedBytes = "different".getBytes(StandardCharsets.UTF_8)
        val digest        = VerifiedArchive.sha256(expectedBytes)
        val cacheRoot     = directory / "machine-cache"
        val input         = new TrackingInputStream(suppliedBytes)

        val result = scala.util.Try(
          AcquisitionCache(AcquisitionSettings(cacheRoot = Some(cacheRoot)), directory / "task")
            .acquire(digest, "memory:mismatch")(input)
        )

        assert(result.isFailure)
        assert(input.wasClosed)
        assert(!os.exists(cacheRoot / "sha256" / digest))
        assert(os.list(cacheRoot / "sha256").forall(!_.last.endsWith(".tmp")))
      }
    }

    test("acquisition rejects source bytes beyond its configured limit") {
      withTempDir { directory =>
        val bytes     = "five!".getBytes(StandardCharsets.UTF_8)
        val digest    = VerifiedArchive.sha256(bytes)
        val cacheRoot = directory / "machine-cache"
        val input     = new TrackingInputStream(bytes)

        val result = scala.util.Try(
          AcquisitionCache(AcquisitionSettings(cacheRoot = Some(cacheRoot)), directory / "task")
            .acquire(digest, "memory:bounded", AcquisitionLimits(maxAcquiredBytes = 4))(input)
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("acquired byte limit"))
        assert(input.wasClosed)
        assert(!os.exists(cacheRoot / "sha256" / digest))
        assert(os.list(cacheRoot / "sha256").forall(!_.last.endsWith(".tmp")))
      }
    }

    test("output acquisition failure closes the already-opened source") {
      withTempDir { directory =>
        val bytes        = "output failure".getBytes(StandardCharsets.UTF_8)
        val digest       = VerifiedArchive.sha256(bytes)
        val taskRoot     = directory / "task"
        val outputParent = taskRoot / ".morphir-acquisitions" / "sha256"
        val displaced    = directory / "displaced-sha256"
        val input        = new TrackingInputStream(bytes)

        val result = scala.util.Try(
          AcquisitionCache(
            AcquisitionSettings(useMachineCache = false),
            taskRoot
          ).acquire(digest, "memory:output-failure") {
            os.move(outputParent, displaced)
            os.write(outputParent, "not a directory")
            input
          }
        )

        assert(result.isFailure)
        assert(input.wasClosed)
        assert(!os.exists(displaced / digest))
        assert(os.list(displaced).forall(!_.last.endsWith(".tmp")))
      }
    }

    test("extraction rejects a nonempty destination without changing it") {
      withTempDir { directory =>
        val archive = directory / "node.tar.gz"
        writeTarGz(
          archive,
          Seq(("node-v-test/bin/node", "node".getBytes(StandardCharsets.UTF_8), None))
        )
        val destination = directory / "existing"
        os.makeDir.all(destination)
        os.write(destination / "keep", "keep")

        val result = scala.util.Try(
          VerifiedArchive.extract(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.TarGz,
            destination
          )
        )

        assert(result.isFailure)
        assert(os.read(destination / "keep") == "keep")
        assert(!os.exists(destination / "bin"))
      }
    }

    test("extraction atomically replaces an empty destination") {
      withTempDir { directory =>
        val archive = directory / "node.tar.gz"
        writeTarGz(
          archive,
          Seq(("node-v-test/bin/node", "node".getBytes(StandardCharsets.UTF_8), None))
        )
        val destination = directory / "empty"
        os.makeDir(destination)

        VerifiedArchive.extract(
          VerifiedContent(archive, VerifiedArchive.sha256(archive)),
          ArchiveFormat.TarGz,
          destination
        )

        assert(os.read(destination / "bin" / "node") == "node")
      }
    }

    test("extraction reads only the verified snapshot after the boundary") {
      withTempDir { directory =>
        val archive = directory / "node.tar.gz"
        writeTarGz(
          archive,
          Seq(("node-v-test/bin/node", "verified".getBytes(StandardCharsets.UTF_8), None))
        )
        val replacement = directory / "replacement.tar.gz"
        writeTarGz(
          replacement,
          Seq(("node-v-test/bin/node", "changed".getBytes(StandardCharsets.UTF_8), None))
        )

        VerifiedArchive.extractObserved(
          VerifiedContent(archive, VerifiedArchive.sha256(archive)),
          ArchiveFormat.TarGz,
          directory / "extracted"
        ) {
          os.move(replacement, archive, replaceExisting = true)
        }

        assert(os.read(directory / "extracted" / "bin" / "node") == "verified")
      }
    }

    test("acquisition waits for an overlapping file lock and then succeeds") {
      withTempDir { directory =>
        val bytes     = "overlapping lock".getBytes(StandardCharsets.UTF_8)
        val digest    = VerifiedArchive.sha256(bytes)
        val cacheRoot = directory / "machine-cache"
        val lockDir   = cacheRoot / "sha256"
        val lockPath  = lockDir / s".$digest.lock"
        os.makeDir.all(lockDir)
        val attempts           = new AtomicInteger(0)
        given ExecutionContext = ExecutionContext.global

        val channel = FileChannel.open(lockPath.toNIO, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
        val held    = channel.lock()
        try {
          val acquisition = Future {
            AcquisitionCache(AcquisitionSettings(cacheRoot = Some(cacheRoot)), directory / "task")
              .acquire(digest, "memory:overlapping-lock") {
                attempts.incrementAndGet()
                new ByteArrayInputStream(bytes)
              }
          }
          Thread.sleep(100)
          assert(!acquisition.isCompleted)
          assert(attempts.get() == 0)
          held.release()

          val content = Await.result(acquisition, 5.seconds)
          assert(os.read.bytes(content.path).sameElements(bytes))
          assert(attempts.get() == 1)
        } finally {
          if (held.isValid) held.release()
          channel.close()
        }
      }
    }

    test("path coordination waits beyond the previous short test threshold") {
      withTempDir { directory =>
        val lockPath           = directory / ".content.lock"
        val channel            = FileChannel.open(lockPath.toNIO, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
        val held               = channel.lock()
        val started            = new CountDownLatch(1)
        given ExecutionContext = ExecutionContext.global
        try {
          val coordinated = Future {
            started.countDown()
            PathCoordinator.withLock(lockPath)("complete")
          }
          assert(started.await(5, TimeUnit.SECONDS))
          Thread.sleep(600)
          assert(!coordinated.isCompleted)
          held.release()

          assert(Await.result(coordinated, 5.seconds) == "complete")
        } finally {
          if (held.isValid) held.release()
          channel.close()
        }
      }
    }

    test("same-JVM path coordination wait is interruptible") {
      withTempDir { directory =>
        val lockPath       = directory / ".interrupt.lock"
        val ownerEntered   = new CountDownLatch(1)
        val releaseOwner   = new CountDownLatch(1)
        val contenderReady = new CountDownLatch(1)
        val failure        = new AtomicReference[Throwable]()
        val preserved      = new AtomicBoolean(false)

        val owner = new Thread(() =>
          PathCoordinator.withLock(lockPath) {
            ownerEntered.countDown()
            releaseOwner.await(5, TimeUnit.SECONDS)
            ()
          }
        )
        val contender = new Thread(() => {
          contenderReady.countDown()
          try PathCoordinator.withLock(lockPath)(())
          catch {
            case error: Throwable =>
              failure.set(error)
              preserved.set(Thread.currentThread().isInterrupted)
          }
        })

        owner.start()
        try {
          assert(ownerEntered.await(5, TimeUnit.SECONDS))
          contender.start()
          assert(contenderReady.await(5, TimeUnit.SECONDS))
          Thread.sleep(100)
          contender.interrupt()
          contender.join(5000)
          assert(!contender.isAlive)
          assert(failure.get().isInstanceOf[java.io.IOException])
          assert(failure.get().getMessage.contains("Interrupted"))
          assert(preserved.get())
        } finally {
          if (contender.isAlive) contender.interrupt()
          releaseOwner.countDown()
          contender.join(5000)
          owner.join(5000)
        }
      }
    }

    test("same-thread recursive path coordination fails clearly instead of deadlocking") {
      withTempDir { directory =>
        val lockPath = directory / ".recursive.lock"
        val result   = new AtomicReference[scala.util.Try[Unit]]()
        val worker   = new Thread(() =>
          result.set(
            scala.util.Try(
              PathCoordinator.withLock(lockPath) {
                PathCoordinator.withLock(lockPath)(())
              }
            )
          )
        )

        worker.start()
        worker.join(1000)
        val deadlocked = worker.isAlive
        if (deadlocked) {
          worker.interrupt()
          worker.join(1000)
        }

        assert(!deadlocked)
        assert(result.get().isFailure)
        assert(result.get().failed.get.getMessage.contains("Recursive path coordination"))
      }
    }

    test("online acquisition quarantines a nonempty digest directory without following it") {
      withTempDir { directory =>
        val bytes     = "directory replacement".getBytes(StandardCharsets.UTF_8)
        val digest    = VerifiedArchive.sha256(bytes)
        val cacheRoot = directory / "machine-cache"
        val entry     = cacheRoot / "sha256" / digest
        os.makeDir.all(entry)
        os.write(entry / "unexpected", "unexpected")

        val content = AcquisitionCache(AcquisitionSettings(cacheRoot = Some(cacheRoot)), directory / "task")
          .acquire(digest, "memory:directory-replacement")(new ByteArrayInputStream(bytes))

        assert(content.path == entry)
        assert(Files.isRegularFile(entry.toNIO, java.nio.file.LinkOption.NOFOLLOW_LINKS))
        assert(os.read.bytes(entry).sameElements(bytes))
        assert(os.list(cacheRoot / "sha256").forall(!_.last.contains("quarantine")))
      }
    }

    test("archive entry-count limit rejects a small synthetic archive") {
      withTempDir { directory =>
        val archive = directory / "entries.tar.gz"
        writeTarGz(
          archive,
          Seq(
            ("root/one", "1".getBytes(StandardCharsets.UTF_8), None),
            ("root/two", "2".getBytes(StandardCharsets.UTF_8), None)
          )
        )

        val result = scala.util.Try(
          VerifiedArchive.extract(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.TarGz,
            directory / "extracted",
            ArchiveLimits(maxEntries = 1)
          )
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("entry count"))
        assert(!os.exists(directory / "extracted"))
      }
    }

    test("ZIP entry-count preflight rejects before opening Commons ZipFile") {
      withTempDir { directory =>
        val archive = directory / "entries.zip"
        writeZip(
          archive,
          Seq(
            ("root/one", "1".getBytes(StandardCharsets.UTF_8), 0x81a4),
            ("root/two", "2".getBytes(StandardCharsets.UTF_8), 0x81a4)
          )
        )
        var parserOpened = false

        val result = scala.util.Try(
          VerifiedArchive.extractObserved(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.Zip,
            directory / "extracted",
            limits = ArchiveLimits(maxEntries = 1),
            parserObserver = _ => parserOpened = true
          ) {}
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("ZIP entry count"))
        assert(!parserOpened)
        assert(!os.exists(directory / "extracted"))
      }
    }

    test("ZIP preflight rejects a classic EOCD that under-declares central-directory entries") {
      withTempDir { directory =>
        val archive = directory / "under-declared.zip"
        writeSyntheticCentralDirectoryZip(archive, actualEntries = 2, declaredEntries = 1)
        var parserOpened = false

        val result = scala.util.Try(
          VerifiedArchive.extractObserved(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.Zip,
            directory / "extracted",
            parserObserver = _ => parserOpened = true
          ) {}
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("declares 1 entries but contains 2"))
        assert(!parserOpened)
      }
    }

    test("ZIP preflight rejects an over-declared central-directory entry count") {
      withTempDir { directory =>
        val archive = directory / "over-declared.zip"
        writeSyntheticCentralDirectoryZip(archive, actualEntries = 1, declaredEntries = 2)
        var parserOpened = false
        val result       = scala.util.Try(
          VerifiedArchive.extractObserved(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.Zip,
            directory / "extracted",
            parserObserver = _ => parserOpened = true
          ) {}
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("declares 2 entries but contains 1"))
        assert(!parserOpened)
      }
    }

    test("ZIP preflight rejects central-directory record lengths outside the bounded extent") {
      withTempDir { directory =>
        val archive = directory / "bad-record-length.zip"
        writeSyntheticCentralDirectoryZip(
          archive,
          actualEntries = 1,
          declaredEntries = 1,
          firstNameLength = 1
        )
        var parserOpened = false
        val result       = scala.util.Try(
          VerifiedArchive.extractObserved(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.Zip,
            directory / "extracted",
            parserObserver = _ => parserOpened = true
          ) {}
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("header lengths exceed"))
        assert(!parserOpened)
      }
    }

    test("ZIP preflight rejects unknown central-directory trailing records") {
      withTempDir { directory =>
        val archive  = directory / "unknown-central-record.zip"
        val trailing = ByteBuffer
          .allocate(4)
          .order(ByteOrder.LITTLE_ENDIAN)
          .putInt(0x12345678)
          .array()
        writeSyntheticCentralDirectoryZip(archive, actualEntries = 1, declaredEntries = 1, trailing = trailing)
        var parserOpened = false
        val result       = scala.util.Try(
          VerifiedArchive.extractObserved(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.Zip,
            directory / "extracted",
            parserObserver = _ => parserOpened = true
          ) {}
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("unsupported record signature"))
        assert(!parserOpened)
      }
    }

    test("ZIP preflight accepts a terminal central-directory digital signature") {
      withTempDir { directory =>
        val archive   = directory / "digital-signature.zip"
        val signature = ByteBuffer
          .allocate(9)
          .order(ByteOrder.LITTLE_ENDIAN)
          .putInt(0x05054b50)
          .putShort(3.toShort)
          .put(Array[Byte](1, 2, 3))
          .array()
        writeSyntheticCentralDirectoryZip(archive, actualEntries = 1, declaredEntries = 1, trailing = signature)
        var parserOpened = false

        scala.util.Try(
          VerifiedArchive.extractObserved(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.Zip,
            directory / "extracted",
            parserObserver = _ => parserOpened = true
          ) {}
        )

        assert(parserOpened)
      }
    }

    test("ZIP preflight rejects a truncated central-directory digital signature") {
      withTempDir { directory =>
        val archive   = directory / "truncated-digital-signature.zip"
        val signature = ByteBuffer
          .allocate(9)
          .order(ByteOrder.LITTLE_ENDIAN)
          .putInt(0x05054b50)
          .putShort(4.toShort)
          .put(Array[Byte](1, 2, 3))
          .array()
        writeSyntheticCentralDirectoryZip(archive, actualEntries = 1, declaredEntries = 1, trailing = signature)
        var parserOpened = false
        val result       = scala.util.Try(
          VerifiedArchive.extractObserved(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.Zip,
            directory / "extracted",
            parserObserver = _ => parserOpened = true
          ) {}
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("digital signature has trailing or truncated bytes"))
        assert(!parserOpened)
      }
    }

    test("ZIP central-directory byte limit rejects before opening Commons ZipFile") {
      withTempDir { directory =>
        val archive = directory / "central-directory.zip"
        writeZip(archive, Seq(("root/file", "data".getBytes(StandardCharsets.UTF_8), 0x81a4)))
        var parserOpened = false

        val result = scala.util.Try(
          VerifiedArchive.extractObserved(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.Zip,
            directory / "extracted",
            limits = ArchiveLimits(maxCentralDirectoryBytes = 1),
            parserObserver = _ => parserOpened = true
          ) {}
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("central-directory byte size"))
        assert(!parserOpened)
      }
    }

    test("ZIP64 sentinel entry count is bounded during preflight") {
      withTempDir { directory =>
        val archive = directory / "zip64.zip"
        writeSyntheticZip64(archive, entries = 2)
        var parserOpened = false

        val result = scala.util.Try(
          VerifiedArchive.extractObserved(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.Zip,
            directory / "extracted",
            limits = ArchiveLimits(maxEntries = 1),
            parserObserver = _ => parserOpened = true
          ) {}
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("ZIP entry count 2"))
        assert(!parserOpened)
      }
    }

    test("ZIP preflight rejects ZIP64 metadata that under-declares central-directory entries") {
      withTempDir { directory =>
        val archive = directory / "zip64-under-declared.zip"
        writeSyntheticZip64CentralDirectory(archive, actualEntries = 2, declaredEntries = 1)
        var parserOpened = false

        val result = scala.util.Try(
          VerifiedArchive.extractObserved(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.Zip,
            directory / "extracted",
            parserObserver = _ => parserOpened = true
          ) {}
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("declares 1 entries but contains 2"))
        assert(!parserOpened)
      }
    }

    test("ZIP preflight rejects malformed and multi-disk end records before Commons") {
      withTempDir { directory =>
        Seq(("multidisk", 1, 0, "Multi-disk"), ("malformed", 0, 1, "missing or malformed")).foreach {
          case (name, disk, commentBytes, expected) =>
            val archive = directory / s"$name.zip"
            writeSyntheticZipEnd(archive, disk, commentBytes)
            var parserOpened = false
            val result       = scala.util.Try(
              VerifiedArchive.extractObserved(
                VerifiedContent(archive, VerifiedArchive.sha256(archive)),
                ArchiveFormat.Zip,
                directory / s"$name-output",
                parserObserver = _ => parserOpened = true
              ) {}
            )
            assert(result.isFailure)
            assert(result.failed.get.getMessage.contains(expected))
            assert(!parserOpened)
        }
      }
    }

    test("TAR metadata entry limit rejects before opening Commons parser") {
      withTempDir { directory =>
        Seq(TarConstants.LF_GNUTYPE_LONGNAME, TarConstants.LF_GNUTYPE_LONGLINK).zipWithIndex.foreach {
          case (entryType, index) =>
            val archive = directory / s"metadata-$index.tar.gz"
            writeRawTarGz(archive, Seq((entryType, "12345".getBytes(StandardCharsets.US_ASCII))))
            var parserOpened = false

            val result = scala.util.Try(
              VerifiedArchive.extractObserved(
                VerifiedContent(archive, VerifiedArchive.sha256(archive)),
                ArchiveFormat.TarGz,
                directory / s"extracted-$index",
                limits = ArchiveLimits(maxMetadataEntryBytes = 4),
                parserObserver = _ => parserOpened = true
              ) {}
            )

            assert(result.isFailure)
            assert(result.failed.get.getMessage.contains("TAR metadata entry byte limit"))
            assert(!parserOpened)
            assert(!os.exists(directory / s"extracted-$index"))
        }
      }
    }

    test("TAR chained PAX metadata is bounded in aggregate before Commons") {
      withTempDir { directory =>
        val archive = directory / "pax-metadata.tar.gz"
        writeRawTarGz(
          archive,
          Seq(
            (TarConstants.LF_PAX_EXTENDED_HEADER_LC, "abc".getBytes(StandardCharsets.US_ASCII)),
            (TarConstants.LF_PAX_GLOBAL_EXTENDED_HEADER, "def".getBytes(StandardCharsets.US_ASCII))
          )
        )
        var parserOpened = false
        val result       = scala.util.Try(
          VerifiedArchive.extractObserved(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.TarGz,
            directory / "extracted",
            limits = ArchiveLimits(maxMetadataEntryBytes = 3, maxArchiveMetadataBytes = 5),
            parserObserver = _ => parserOpened = true
          ) {}
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("TAR aggregate metadata bytes"))
        assert(!parserOpened)
      }
    }

    test("TAR pseudo-entry headers count toward the entry limit") {
      withTempDir { directory =>
        val archive = directory / "pseudo-count.tar.gz"
        writeRawTarGz(
          archive,
          Seq(
            (TarConstants.LF_GNUTYPE_LONGNAME, "a".getBytes(StandardCharsets.US_ASCII)),
            (TarConstants.LF_PAX_EXTENDED_HEADER_LC, "b".getBytes(StandardCharsets.US_ASCII))
          )
        )
        var parserOpened = false
        val result       = scala.util.Try(
          VerifiedArchive.extractObserved(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.TarGz,
            directory / "extracted",
            limits = ArchiveLimits(maxEntries = 1),
            parserObserver = _ => parserOpened = true
          ) {}
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("TAR header and pseudo-entry count"))
        assert(!parserOpened)
      }
    }

    test("TAR preflight bounds consecutive parser-hidden metadata entries") {
      withTempDir { directory =>
        val archive = directory / "metadata-chain.tar.gz"
        writeRawTarGz(
          archive,
          Seq(
            (TarConstants.LF_GNUTYPE_LONGNAME, "a".getBytes(StandardCharsets.US_ASCII)),
            (TarConstants.LF_GNUTYPE_LONGLINK, "b".getBytes(StandardCharsets.US_ASCII)),
            (TarConstants.LF_PAX_EXTENDED_HEADER_LC, "c".getBytes(StandardCharsets.US_ASCII))
          )
        )
        var parserOpened = false
        val result       = scala.util.Try(
          VerifiedArchive.extractObserved(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.TarGz,
            directory / "extracted",
            limits = ArchiveLimits(maxConsecutiveMetadataEntries = 2),
            parserObserver = _ => parserOpened = true
          ) {}
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("consecutive metadata entry limit 2"))
        assert(!parserOpened)
      }
    }

    test("ordinary TAR entries reset the consecutive metadata limit") {
      withTempDir { directory =>
        val archive = directory / "reset-metadata-chain.tar.gz"
        writeRawTarGz(
          archive,
          Seq(
            (TarConstants.LF_GNUTYPE_LONGNAME, "a".getBytes(StandardCharsets.US_ASCII)),
            (TarConstants.LF_GNUTYPE_LONGLINK, "b".getBytes(StandardCharsets.US_ASCII)),
            (TarConstants.LF_NORMAL, "file".getBytes(StandardCharsets.US_ASCII)),
            (TarConstants.LF_PAX_EXTENDED_HEADER_LC, "c".getBytes(StandardCharsets.US_ASCII)),
            (TarConstants.LF_PAX_GLOBAL_EXTENDED_HEADER, "d".getBytes(StandardCharsets.US_ASCII))
          )
        )
        var parserOpened = false

        scala.util.Try(
          VerifiedArchive.extractObserved(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.TarGz,
            directory / "extracted",
            limits = ArchiveLimits(maxConsecutiveMetadataEntries = 2),
            parserObserver = _ => parserOpened = true
          ) {}
        )

        assert(parserOpened)
        assert(scala.util.Try(ArchiveLimits(maxConsecutiveMetadataEntries = 0)).isFailure)
      }
    }

    test("TAR malformed and truncated headers fail during preflight") {
      withTempDir { directory =>
        val malformed = directory / "malformed.tar.gz"
        val truncated = directory / "truncated.tar.gz"
        writeGzipBytes(malformed, Array.fill[Byte](512)(1))
        writeGzipBytes(truncated, Array.fill[Byte](100)(0))

        Seq(malformed, truncated).zipWithIndex.foreach { case (archive, index) =>
          var parserOpened = false
          val result       = scala.util.Try(
            VerifiedArchive.extractObserved(
              VerifiedContent(archive, VerifiedArchive.sha256(archive)),
              ArchiveFormat.TarGz,
              directory / s"malformed-output-$index",
              parserObserver = _ => parserOpened = true
            ) {}
          )
          assert(result.isFailure)
          assert(result.failed.get.getMessage.toLowerCase(java.util.Locale.ROOT).contains("tar"))
          assert(!parserOpened)
        }
      }
    }

    test("TAR preflight accepts a positive base-256 entry size") {
      withTempDir { directory =>
        val archive = directory / "base-256.tar.gz"
        writeRawTarGz(
          archive,
          Seq((TarConstants.LF_NORMAL, "x".getBytes(StandardCharsets.US_ASCII))),
          base256Sizes = Set(0)
        )

        VerifiedArchive.extract(
          VerifiedContent(archive, VerifiedArchive.sha256(archive)),
          ArchiveFormat.TarGz,
          directory / "extracted"
        )

        assert(os.read(directory / "extracted" / "entry-0") == "x")
      }
    }

    test("TAR preflight bounds the complete decompressed stream") {
      withTempDir { directory =>
        val archive = directory / "stream-limit.tar.gz"
        writeRawTarGz(
          archive,
          Seq((TarConstants.LF_NORMAL, "x".getBytes(StandardCharsets.US_ASCII))),
          trailingZeroBytes = 2
        )
        var parserOpened = false
        val result       = scala.util.Try(
          VerifiedArchive.extractObserved(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.TarGz,
            directory / "extracted",
            limits = ArchiveLimits(maxEntries = 1, maxTotalUncompressedBytes = 1, maxArchiveMetadataBytes = 1),
            parserObserver = _ => parserOpened = true
          ) {}
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("TAR decompressed stream"))
        assert(!parserOpened)
      }
    }

    test("compressed archive snapshot limit rejects before extraction") {
      withTempDir { directory =>
        val archive = directory / "snapshot.tar.gz"
        writeTarGz(
          archive,
          Seq(("root/file", "small".getBytes(StandardCharsets.UTF_8), None))
        )

        val result = scala.util.Try(
          VerifiedArchive.extract(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.TarGz,
            directory / "extracted",
            ArchiveLimits(maxCompressedArchiveBytes = 8)
          )
        )

        assert(result.isFailure)
        assert(
          result.failed.get.getMessage.toLowerCase(java.util.Locale.ROOT).contains("compressed archive byte limit")
        )
        assert(!os.exists(directory / "extracted"))
        assert(os.list(directory).forall(!_.last.endsWith(".archive")))
      }
    }

    test("per-entry uncompressed-byte limit rejects a small synthetic archive") {
      withTempDir { directory =>
        val archive = directory / "entry-size.tar.gz"
        writeTarGz(
          archive,
          Seq(("root/file", "four".getBytes(StandardCharsets.UTF_8), None))
        )

        val result = scala.util.Try(
          VerifiedArchive.extract(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.TarGz,
            directory / "extracted",
            ArchiveLimits(maxEntryUncompressedBytes = 3)
          )
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("per-entry"))
        assert(!os.exists(directory / "extracted"))
      }
    }

    test("total uncompressed-byte limit rejects a small synthetic archive") {
      withTempDir { directory =>
        val archive = directory / "total-size.tar.gz"
        writeTarGz(
          archive,
          Seq(
            ("root/one", "123".getBytes(StandardCharsets.UTF_8), None),
            ("root/two", "456".getBytes(StandardCharsets.UTF_8), None)
          )
        )

        val result = scala.util.Try(
          VerifiedArchive.extract(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.TarGz,
            directory / "extracted",
            ArchiveLimits(maxTotalUncompressedBytes = 5)
          )
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("total uncompressed"))
        assert(!os.exists(directory / "extracted"))
      }
    }

    test("ZIP entry compression-ratio limit rejects a small synthetic archive") {
      withTempDir { directory =>
        val archive = directory / "ratio.zip"
        writeZip(archive, Seq(("root/file", Array.fill[Byte](1024)(0), 0x81a4)))

        val result = scala.util.Try(
          VerifiedArchive.extract(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.Zip,
            directory / "extracted",
            ArchiveLimits(maxCompressionRatio = 2.0)
          )
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("compression ratio"))
        assert(!os.exists(directory / "extracted"))
      }
    }

    test("tar.gz overall compression-ratio limit rejects a small synthetic archive") {
      withTempDir { directory =>
        val archive = directory / "ratio.tar.gz"
        writeTarGz(
          archive,
          Seq(("root/file", Array.fill[Byte](4096)(0), None))
        )

        val result = scala.util.Try(
          VerifiedArchive.extract(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.TarGz,
            directory / "extracted",
            ArchiveLimits(maxCompressionRatio = 2.0)
          )
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("compression ratio"))
        assert(!os.exists(directory / "extracted"))
      }
    }

    test("tar.gz compression-ratio limit rejects during fixed-buffer preflight") {
      withTempDir { directory =>
        val archive = directory / "preflight-ratio.tar.gz"
        writeTarGz(archive, Seq(("root/file", Array.fill[Byte](4096)(0), None)))
        var parserOpened = false

        val result = scala.util.Try(
          VerifiedArchive.extractObserved(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.TarGz,
            directory / "extracted",
            limits = ArchiveLimits(maxCompressionRatio = 2.0),
            parserObserver = _ => parserOpened = true
          ) {}
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("TAR.GZ preflight compression ratio"))
        assert(!parserOpened)
      }
    }

    test("tar.gz trailing padding cannot inflate the compression-ratio denominator") {
      withTempDir { directory =>
        val archive = directory / "padded-ratio.tar.gz"
        writeTarGz(archive, Seq(("root/file", Array.fill[Byte](64 * 1024)(0), None)))
        appendBytes(archive, Array.fill[Byte](128 * 1024)(0))
        var parserOpened = false

        val result = scala.util.Try(
          VerifiedArchive.extractObserved(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.TarGz,
            directory / "extracted",
            limits = ArchiveLimits(maxCompressionRatio = 2.0),
            parserObserver = _ => parserOpened = true
          ) {}
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("TAR.GZ preflight compression ratio"))
        assert(!parserOpened)
      }
    }

    test("gzip preflight bounds FNAME before Commons construction") {
      withTempDir { directory =>
        val archive = directory / "oversized-name.tar.gz"
        writeSyntheticGzipHeader(
          archive,
          flags = 0x08,
          fileName = Array.fill[Byte](32)('n'.toByte),
          terminateFileName = false
        )
        var gzipConstructed = false
        var parserOpened    = false

        val result = scala.util.Try(
          VerifiedArchive.extractObserved(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.TarGz,
            directory / "extracted",
            limits = ArchiveLimits(maxGzipHeaderBytes = 16),
            parserObserver = _ => parserOpened = true,
            gzipConstructedObserver = () => gzipConstructed = true
          ) {}
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("gzip header byte limit"))
        assert(!gzipConstructed)
        assert(!parserOpened)
      }
    }

    test("gzip preflight bounds optional header fields before Commons construction") {
      withTempDir { directory =>
        val cases = Seq[(String, os.Path => Unit, Long, String)](
          (
            "oversized-name",
            path =>
              writeSyntheticGzipHeader(
                path,
                flags = 0x08,
                fileName = Array.fill[Byte](32)('n'.toByte)
              ),
            16L,
            "header byte limit"
          ),
          (
            "unterminated-name",
            path =>
              writeSyntheticGzipHeader(
                path,
                flags = 0x08,
                fileName = "name".getBytes(StandardCharsets.UTF_8),
                terminateFileName = false
              ),
            64L,
            "Truncated gzip FNAME"
          ),
          (
            "oversized-comment",
            path =>
              writeSyntheticGzipHeader(
                path,
                flags = 0x10,
                comment = Array.fill[Byte](32)('c'.toByte)
              ),
            16L,
            "header byte limit"
          ),
          (
            "unterminated-comment",
            path =>
              writeSyntheticGzipHeader(
                path,
                flags = 0x10,
                comment = "comment".getBytes(StandardCharsets.UTF_8),
                terminateComment = false
              ),
            64L,
            "Truncated gzip FCOMMENT"
          ),
          (
            "oversized-extra",
            path =>
              writeSyntheticGzipHeader(
                path,
                flags = 0x04,
                extra = Array.fill[Byte](32)(1)
              ),
            16L,
            "header byte limit"
          ),
          (
            "invalid-magic",
            path => os.write(path, Array.fill[Byte](10)(0)),
            64L,
            "Invalid gzip magic"
          ),
          (
            "unsupported-method",
            path => writeSyntheticGzipHeader(path, flags = 0, method = 0),
            64L,
            "Unsupported gzip compression method"
          ),
          (
            "reserved-flags",
            path => writeSyntheticGzipHeader(path, flags = 0x20),
            64L,
            "Reserved gzip flags"
          ),
          (
            "truncated-fixed-header",
            path => os.write(path, Array[Byte](0x1f, 0x8b.toByte, 8, 0, 0)),
            64L,
            "Truncated gzip fixed header"
          ),
          (
            "truncated-extra-length",
            path =>
              os.write(
                path,
                Array[Byte](0x1f, 0x8b.toByte, 8, 0x04, 0, 0, 0, 0, 0, 3, 1)
              ),
            64L,
            "Truncated gzip FEXTRA length"
          ),
          (
            "truncated-header-crc",
            path => writeSyntheticGzipHeader(path, flags = 0x02, headerCrc = false),
            64L,
            "Truncated gzip FHCRC"
          )
        )

        cases.foreach { case (label, writeArchive, limit, expectedMessage) =>
          val archive = directory / s"$label.tar.gz"
          writeArchive(archive)
          var gzipConstructed = false
          var parserOpened    = false
          val result          = scala.util.Try(
            VerifiedArchive.extractObserved(
              VerifiedContent(archive, VerifiedArchive.sha256(archive)),
              ArchiveFormat.TarGz,
              directory / s"extracted-$label",
              limits = ArchiveLimits(maxGzipHeaderBytes = limit),
              parserObserver = _ => parserOpened = true,
              gzipConstructedObserver = () => gzipConstructed = true
            ) {}
          )

          assert(result.isFailure)
          assert(result.failed.get.getMessage.contains(expectedMessage))
          assert(!gzipConstructed)
          assert(!parserOpened)
        }
      }
    }

    test("gzip preflight accepts a bounded optional FHCRC header") {
      withTempDir { directory =>
        val archive = directory / "header-crc.tar.gz"
        writeTarGz(archive, Seq(("root/file", "data".getBytes(StandardCharsets.UTF_8), None)))
        addGzipHeaderCrc(archive)
        var gzipConstructed = false
        var parserOpened    = false

        VerifiedArchive.extractObserved(
          VerifiedContent(archive, VerifiedArchive.sha256(archive)),
          ArchiveFormat.TarGz,
          directory / "extracted",
          parserObserver = _ => parserOpened = true,
          gzipConstructedObserver = () => gzipConstructed = true
        ) {}

        assert(gzipConstructed)
        assert(parserOpened)
        assert(os.read(directory / "extracted" / "file") == "data")
        assert(scala.util.Try(ArchiveLimits(maxGzipHeaderBytes = 0)).isFailure)
      }
    }

    test("tar.gz preflight rejects trailing non-gzip garbage before Commons TAR parsing") {
      withTempDir { directory =>
        val archive = directory / "trailing-garbage.tar.gz"
        writeTarGz(archive, Seq(("root/file", "data".getBytes(StandardCharsets.UTF_8), None)))
        appendBytes(archive, Array[Byte](1, 2, 3, 4))
        var parserOpened = false

        val result = scala.util.Try(
          VerifiedArchive.extractObserved(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.TarGz,
            directory / "extracted",
            parserObserver = _ => parserOpened = true
          ) {}
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("gzip member"))
        assert(!parserOpened)
      }
    }

    test("tar.gz preflight rejects a TAR stream split across concatenated gzip members") {
      withTempDir { directory =>
        val ordinary = directory / "ordinary.tar.gz"
        val archive  = directory / "concatenated.tar.gz"
        val contents = "concatenated".getBytes(StandardCharsets.UTF_8)
        writeTarGz(ordinary, Seq(("root/file", contents, None)))
        val tarBytes = readGzipBytes(ordinary)
        val split    = tarBytes.length / 2
        writeConcatenatedGzip(archive, Seq(tarBytes.take(split), tarBytes.drop(split)))
        var gzipConstructed = false
        var parserOpened    = false

        val result = scala.util.Try(
          VerifiedArchive.extractObserved(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.TarGz,
            directory / "extracted",
            parserObserver = _ => parserOpened = true,
            gzipConstructedObserver = () => gzipConstructed = true
          ) {}
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("single gzip member"))
        assert(gzipConstructed)
        assert(!parserOpened)
      }
    }

    test("symbolic-link target byte limit rejects a small synthetic archive") {
      withTempDir { directory =>
        val archive = directory / "symlink-target.tar.gz"
        writeTarGz(
          archive,
          Seq(("root/link", Array.emptyByteArray, Some("bin/node")))
        )

        val result = scala.util.Try(
          VerifiedArchive.extract(
            VerifiedContent(archive, VerifiedArchive.sha256(archive)),
            ArchiveFormat.TarGz,
            directory / "extracted",
            ArchiveLimits(maxSymlinkTargetBytes = 4)
          )
        )

        assert(result.isFailure)
        assert(result.failed.get.getMessage.contains("link target"))
        assert(!os.exists(directory / "extracted"))
      }
    }

    test("TAR extraction rejects FIFO, device, and unknown entry types") {
      withTempDir { directory =>
        Seq(
          "fifo"    -> TarConstants.LF_FIFO,
          "device"  -> TarConstants.LF_CHR,
          "unknown" -> 'Z'.toByte
        ).foreach { case (name, entryType) =>
          val archive = directory / s"$name.tar.gz"
          writeTarGzSpecial(archive, s"root/$name", entryType)

          val result = scala.util.Try(
            VerifiedArchive.extract(
              VerifiedContent(archive, VerifiedArchive.sha256(archive)),
              ArchiveFormat.TarGz,
              directory / s"extracted-$name"
            )
          )

          assert(result.isFailure)
          assert(result.failed.get.getMessage.contains("Unsupported TAR entry type"))
          assert(!os.exists(directory / s"extracted-$name"))
        }
      }
    }

    test("online acquisition replaces a matching digest symlink without following it") {
      withTempDir { directory =>
        val bytes     = "symlink target".getBytes(StandardCharsets.UTF_8)
        val digest    = VerifiedArchive.sha256(bytes)
        val cacheRoot = directory / "machine-cache"
        val entry     = cacheRoot / "sha256" / digest
        val external  = directory / "external"
        os.makeDir.all(entry / os.up)
        os.write(external, bytes)
        Files.createSymbolicLink(entry.toNIO, external.toNIO)

        val content = AcquisitionCache(AcquisitionSettings(cacheRoot = Some(cacheRoot)), directory / "task")
          .acquire(digest, "memory:symlink-replacement")(new ByteArrayInputStream(bytes))

        assert(content.path == entry)
        assert(Files.isRegularFile(entry.toNIO, java.nio.file.LinkOption.NOFOLLOW_LINKS))
        assert(os.read.bytes(entry).sameElements(bytes))
        assert(os.read.bytes(external).sameElements(bytes))
      }
    }

    test("offline acquisition rejects a matching digest symlink without opening the source") {
      withTempDir { directory =>
        val bytes     = "offline symlink".getBytes(StandardCharsets.UTF_8)
        val digest    = VerifiedArchive.sha256(bytes)
        val cacheRoot = directory / "machine-cache"
        val entry     = cacheRoot / "sha256" / digest
        val external  = directory / "external"
        os.makeDir.all(entry / os.up)
        os.write(external, bytes)
        Files.createSymbolicLink(entry.toNIO, external.toNIO)
        var opened = false

        val result = scala.util.Try(
          AcquisitionCache(
            AcquisitionSettings(cacheRoot = Some(cacheRoot), offline = true),
            directory / "task"
          ).acquire(digest, "memory:offline-symlink") {
            opened = true
            new ByteArrayInputStream(bytes)
          }
        )

        assert(result.isFailure)
        assert(!opened)
        assert(Files.isSymbolicLink(entry.toNIO))
        assert(os.read.bytes(external).sameElements(bytes))
      }
    }

    test("offline acquisition rejects a nonregular digest directory without opening the source") {
      withTempDir { directory =>
        val bytes     = "offline directory".getBytes(StandardCharsets.UTF_8)
        val digest    = VerifiedArchive.sha256(bytes)
        val cacheRoot = directory / "machine-cache"
        val entry     = cacheRoot / "sha256" / digest
        os.makeDir.all(entry)
        os.write(entry / "unexpected", "unexpected")
        var opened = false

        val result = scala.util.Try(
          AcquisitionCache(
            AcquisitionSettings(cacheRoot = Some(cacheRoot), offline = true),
            directory / "task"
          ).acquire(digest, "memory:offline-directory") {
            opened = true
            new ByteArrayInputStream(bytes)
          }
        )

        assert(result.isFailure)
        assert(!opened)
        assert(os.read(entry / "unexpected") == "unexpected")
      }
    }

    test("acquisition prunes stale digest siblings but retains its persistent lock") {
      withTempDir { directory =>
        val bytes     = "stale siblings".getBytes(StandardCharsets.UTF_8)
        val digest    = VerifiedArchive.sha256(bytes)
        val cacheRoot = directory / "machine-cache"
        val digestDir = cacheRoot / "sha256"
        val staleTemp = digestDir / s".$digest.stale.tmp"
        val staleBox  = digestDir / s".$digest.stale.quarantine"
        os.makeDir.all(staleBox)
        os.write(staleTemp, "stale")
        os.write(staleBox / "nested", "stale")

        AcquisitionCache(AcquisitionSettings(cacheRoot = Some(cacheRoot)), directory / "task")
          .acquire(digest, "memory:stale-siblings")(new ByteArrayInputStream(bytes))

        assert(!os.exists(staleTemp))
        assert(!os.exists(staleBox))
        assert(os.exists(digestDir / s".$digest.lock"))
      }
    }

    test("cleanup failures do not fail warm content or mask a primary failure") {
      withTempDir { directory =>
        val bytes     = "cleanup isolation".getBytes(StandardCharsets.UTF_8)
        val digest    = VerifiedArchive.sha256(bytes)
        val cacheRoot = directory / "machine-cache"
        val digestDir = cacheRoot / "sha256"
        val entry     = digestDir / digest
        os.makeDir.all(digestDir)
        os.write(entry, "corrupt")
        os.write(digestDir / s".$digest.stale.tmp", "stale")
        val cleanupAttempts = new AtomicInteger(0)
        val cache           = AcquisitionCache.withCleanup(
          AcquisitionSettings(cacheRoot = Some(cacheRoot)),
          directory / "task"
        ) { (_: os.Path) =>
          cleanupAttempts.incrementAndGet()
          throw new java.io.IOException("stubborn cleanup")
        }

        val acquired = cache.acquire(digest, "memory:cleanup")(new ByteArrayInputStream(bytes))
        val warm     = cache.acquire(digest, "memory:warm")(
          throw new java.lang.AssertionError("warm acquisition opened its source")
        )

        assert(warm == acquired)
        assert(os.read.bytes(warm.path).sameElements(bytes))
        assert(cleanupAttempts.get() > 0)

        val mismatch = scala.util.Try(
          cache.acquire(VerifiedArchive.sha256("expected".getBytes(StandardCharsets.UTF_8)), "memory:mismatch")(
            new ByteArrayInputStream("wrong".getBytes(StandardCharsets.UTF_8))
          )
        )
        assert(mismatch.isFailure)
        assert(mismatch.failed.get.getMessage.contains("SHA-256 mismatch"))
        assert(!mismatch.failed.get.getMessage.contains("stubborn cleanup"))
      }
    }

    test("archive extraction prunes stale snapshot and staging siblings without following links") {
      withTempDir { directory =>
        val archive = directory / "node.tar.gz"
        writeTarGz(archive, Seq(("root/bin/node", "node".getBytes(StandardCharsets.UTF_8), None)))
        val staleSnapshot = directory / ".extracted.stale.archive"
        val staleStaging  = directory / ".extracted.stale.extract"
        val external      = directory / "external"
        os.write(external, "keep")
        Files.createSymbolicLink(staleSnapshot.toNIO, external.toNIO)
        os.makeDir.all(staleStaging / "nested")
        os.write(staleStaging / "nested" / "file", "stale")

        VerifiedArchive.extract(
          VerifiedContent(archive, VerifiedArchive.sha256(archive)),
          ArchiveFormat.TarGz,
          directory / "extracted"
        )

        assert(!Files.exists(staleSnapshot.toNIO, java.nio.file.LinkOption.NOFOLLOW_LINKS))
        assert(!os.exists(staleStaging))
        assert(os.read(external) == "keep")
        assert(os.exists(directory / ".extracted.lock"))
        assert(os.read(directory / "extracted" / "bin" / "node") == "node")
      }
    }

    test("stale cleanup does not prune a concurrent extraction lease") {
      withTempDir { directory =>
        val archive = directory / "node.tar.gz"
        writeTarGz(archive, Seq(("root/bin/node", "node".getBytes(StandardCharsets.UTF_8), None)))
        val content            = VerifiedContent(archive, VerifiedArchive.sha256(archive))
        val destination        = directory / "extracted"
        val firstSnapshotReady = new CountDownLatch(1)
        val releaseFirst       = new CountDownLatch(1)
        given ExecutionContext = ExecutionContext.global

        val first = Future {
          scala.util.Try(
            VerifiedArchive.extractObserved(content, ArchiveFormat.TarGz, destination) {
              firstSnapshotReady.countDown()
              releaseFirst.await(5, TimeUnit.SECONDS)
            }
          )
        }
        assert(firstSnapshotReady.await(5, TimeUnit.SECONDS))
        val liveSnapshot = os.list(directory).find(_.last.endsWith(".archive")).get

        val second = Future(scala.util.Try(VerifiedArchive.extract(content, ArchiveFormat.TarGz, destination)))
        assert(Await.result(second, 5.seconds).isSuccess)
        assert(Files.exists(liveSnapshot.toNIO, java.nio.file.LinkOption.NOFOLLOW_LINKS))
        releaseFirst.countDown()

        assert(Await.result(first, 5.seconds).isFailure)
        assert(os.read(destination / "bin" / "node") == "node")
        assert(os.list(directory).forall(!_.last.endsWith(".lease")))
      }
    }

    test("archive cleanup failures are suppressed onto primary snapshot and extraction failures") {
      withTempDir { directory =>
        val validArchive = directory / "valid.tar.gz"
        writeTarGz(
          validArchive,
          Seq(("root/bin/node", "node".getBytes(StandardCharsets.UTF_8), None))
        )
        val malformedArchive = directory / "malformed.tar.gz"
        os.write(malformedArchive, "not a gzip archive")
        val cases = Seq(
          "snapshot"   -> VerifiedContent(validArchive, "0" * 64),
          "extraction" -> VerifiedContent(malformedArchive, VerifiedArchive.sha256(malformedArchive))
        )

        cases.foreach { case (label, content) =>
          val result = scala.util.Try(
            VerifiedArchive.extractObserved(
              content,
              ArchiveFormat.TarGz,
              directory / s"extracted-$label",
              cleanup = _ => throw new java.io.IOException(s"$label cleanup failure")
            ) {}
          )

          assert(result.isFailure)
          assert(!result.failed.get.getMessage.contains("cleanup failure"))
          assert(result.failed.get.getSuppressed.length == 2)
          assert(result.failed.get.getSuppressed.forall(_.getMessage == s"$label cleanup failure"))
          assert(!os.exists(directory / s"extracted-$label"))
        }
      }
    }

    test("archive cleanup failures after promotion do not fail a completed destination") {
      withTempDir { directory =>
        val archive = directory / "node.tar.gz"
        writeTarGz(
          archive,
          Seq(("root/bin/node", "node".getBytes(StandardCharsets.UTF_8), None))
        )
        val cleanupAttempts = new AtomicInteger(0)

        VerifiedArchive.extractObserved(
          VerifiedContent(archive, VerifiedArchive.sha256(archive)),
          ArchiveFormat.TarGz,
          directory / "extracted",
          cleanup = _ => {
            cleanupAttempts.incrementAndGet()
            throw new java.io.IOException("post-promotion cleanup failure")
          }
        ) {}

        assert(cleanupAttempts.get() == 2)
        assert(os.read(directory / "extracted" / "bin" / "node") == "node")
      }
    }

    test("failed extraction leaves no partial destination and can be retried") {
      withTempDir { directory =>
        val rejected = directory / "rejected.tar.gz"
        writeTarGzSpecial(rejected, "root/fifo", TarConstants.LF_FIFO)
        val destination = directory / "extracted"

        val first = scala.util.Try(
          VerifiedArchive.extract(
            VerifiedContent(rejected, VerifiedArchive.sha256(rejected)),
            ArchiveFormat.TarGz,
            destination
          )
        )
        assert(first.isFailure)
        assert(!os.exists(destination))

        val accepted = directory / "accepted.tar.gz"
        writeTarGz(
          accepted,
          Seq(("root/bin/node", "node".getBytes(StandardCharsets.UTF_8), None))
        )
        VerifiedArchive.extract(
          VerifiedContent(accepted, VerifiedArchive.sha256(accepted)),
          ArchiveFormat.TarGz,
          destination
        )

        assert(os.read(destination / "bin" / "node") == "node")
      }
    }

    test("concurrent extraction serializes promotion without replacing the winner") {
      withTempDir { directory =>
        val firstArchive = directory / "first.tar.gz"
        writeTarGz(
          firstArchive,
          Seq(("root/bin/node", "first".getBytes(StandardCharsets.UTF_8), None))
        )
        val secondArchive = directory / "second.tar.gz"
        writeTarGz(
          secondArchive,
          Seq(("root/bin/node", "second".getBytes(StandardCharsets.UTF_8), None))
        )
        val contents = Seq(
          "first"  -> VerifiedContent(firstArchive, VerifiedArchive.sha256(firstArchive)),
          "second" -> VerifiedContent(secondArchive, VerifiedArchive.sha256(secondArchive))
        )
        val destination        = directory / "extracted"
        val snapshotsReady     = new CountDownLatch(2)
        val releasePromotion   = new CountDownLatch(1)
        given ExecutionContext = ExecutionContext.global

        val extractions = contents.map { case (label, content) =>
          Future {
            label -> scala.util.Try(
              VerifiedArchive.extractObserved(content, ArchiveFormat.TarGz, destination) {
                snapshotsReady.countDown()
                releasePromotion.await(5, TimeUnit.SECONDS)
              }
            )
          }
        }
        assert(snapshotsReady.await(5, TimeUnit.SECONDS))
        releasePromotion.countDown()
        val results = Await.result(Future.sequence(extractions), 10.seconds)

        val winners = results.collect { case (label, result) if result.isSuccess => label }
        assert(winners.size == 1)
        assert(results.count(_._2.isFailure) == 1)
        assert(os.read(destination / "bin" / "node") == winners.head)
        assert(os.exists(directory / ".extracted.lock"))
        assert(os.list(directory).forall(!_.last.endsWith(".extract")))
      }
    }
  }
}
