package org.finos.morphir.mill.toolchain

import java.io.ByteArrayInputStream
import java.nio.channels.FileChannel
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

        assert(content.path == taskRoot / os.up / s".${taskRoot.last}.morphir-acquisitions" / "sha256" / digest)
        assert(!content.path.toNIO.startsWith(taskRoot.toNIO))
        assert(!os.exists(machineRoot))
        assert(os.read.bytes(content.path).sameElements(bytes))
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
          val destination = directory / name
          val content     = AcquisitionCache(settings, destination)
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
        val outputParent = taskRoot / os.up / s".${taskRoot.last}.morphir-acquisitions" / "sha256"
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
