package org.finos.morphir.mill.toolchain

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger
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

        assert(content.path.toNIO.startsWith(taskRoot.toNIO))
        assert(!os.exists(machineRoot))
        assert(os.read.bytes(content.path).sameElements(bytes))
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
  }
}
