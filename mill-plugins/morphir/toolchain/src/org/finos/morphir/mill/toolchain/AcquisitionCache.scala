package org.finos.morphir.mill.toolchain

import java.io.{IOException, InputStream}
import java.nio.channels.{FileChannel, FileLock, OverlappingFileLockException}
import java.nio.file.{
  AtomicMoveNotSupportedException,
  FileVisitResult,
  Files,
  LinkOption,
  Path,
  SimpleFileVisitor,
  StandardCopyOption,
  StandardOpenOption
}
import java.nio.file.attribute.BasicFileAttributes
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap
import scala.util.Using
import scala.util.control.NonFatal

final class AcquisitionCache(settings: AcquisitionSettings, taskRoot: os.Path) {

  /** Acquires verified content and always closes the supplied stream after opening it. */
  def acquire(expectedSha256: String, source: String)(openStream: => InputStream): VerifiedContent = {
    val digest = normalizedDigest(expectedSha256)
    val root   =
      if (settings.useMachineCache) settings.cacheRoot.getOrElse(AcquisitionSettings.defaultCacheRoot)
      else taskRoot / ".morphir-acquisitions"
    val entry = root / "sha256" / digest
    os.makeDir.all(entry / os.up)
    coordinated(entry) {
      acquireLocked(root, entry, digest, source)(openStream)
    }
  }

  private def acquireLocked(cacheRoot: os.Path, entry: os.Path, digest: String, source: String)(
      openStream: => InputStream
  ): VerifiedContent = {
    pruneStaleSiblings(entry)
    if (isVerifiedRegularFile(entry, digest))
      return VerifiedContent(entry, digest)
    if (settings.offline)
      throw new IllegalStateException(
        s"Offline acquisition cannot use $source: no verified cached content for SHA-256 $digest"
      )
    val temporary = entry / os.up / s".${entry.last}.${UUID.randomUUID()}.tmp"
    try {
      Using
        .Manager { use =>
          val input  = use(openStream)
          val output = use(
            Files.newOutputStream(temporary.toNIO, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
          )
          input.transferTo(output)
        }
        .get
      VerifiedArchive.verifySha256(temporary, digest)
      quarantineAndPromote(temporary, entry, source, cacheRoot)
      VerifiedContent(entry, digest)
    } catch {
      case error: IllegalArgumentException =>
        throw new IllegalArgumentException(s"Verified acquisition failed for $source: ${error.getMessage}", error)
    } finally removeNoFollow(temporary)
  }

  private def isVerifiedRegularFile(entry: os.Path, digest: String): Boolean =
    if (!Files.exists(entry.toNIO, LinkOption.NOFOLLOW_LINKS)) false
    else {
      val attributes = Files.readAttributes(
        entry.toNIO,
        classOf[BasicFileAttributes],
        LinkOption.NOFOLLOW_LINKS
      )
      attributes.isRegularFile && scala.util.Try(VerifiedArchive.verifySha256(entry, digest)).isSuccess
    }

  private def quarantineAndPromote(
      temporary: os.Path,
      entry: os.Path,
      source: String,
      cacheRoot: os.Path
  ): Unit = {
    val quarantine = Option.when(Files.exists(entry.toNIO, LinkOption.NOFOLLOW_LINKS))(
      entry / os.up / s".${entry.last}.${UUID.randomUUID()}.quarantine"
    )
    quarantine.foreach(path => atomicMove(entry, path, source, cacheRoot))
    var promoted = false
    try {
      atomicMove(temporary, entry, source, cacheRoot)
      promoted = true
    } catch {
      case NonFatal(error) =>
        quarantine.foreach { path =>
          if (
            Files.exists(path.toNIO, LinkOption.NOFOLLOW_LINKS) &&
            !Files.exists(entry.toNIO, LinkOption.NOFOLLOW_LINKS)
          )
            try atomicMove(path, entry, source, cacheRoot)
            catch { case NonFatal(restorationError) => error.addSuppressed(restorationError) }
        }
        throw error
    } finally if (promoted) quarantine.foreach(removeNoFollow)
  }

  private def atomicMove(from: os.Path, to: os.Path, source: String, cacheRoot: os.Path): Unit =
    try Files.move(from.toNIO, to.toNIO, StandardCopyOption.ATOMIC_MOVE)
    catch {
      case error: AtomicMoveNotSupportedException =>
        throw new IOException(
          s"Atomic cache promotion is unavailable for $source under $cacheRoot; " +
            "override AcquisitionSettings.cacheRoot or disable the machine cache",
          error
        )
    }

  private def pruneStaleSiblings(entry: os.Path): Unit = {
    val prefix = s".${entry.last}."
    Using.resource(Files.newDirectoryStream(entry.toNIO.getParent)) { siblings =>
      val iterator = siblings.iterator()
      while (iterator.hasNext) {
        val sibling = iterator.next()
        val name    = sibling.getFileName.toString
        if (name.startsWith(prefix) && (name.endsWith(".tmp") || name.endsWith(".quarantine")))
          removeNoFollow(os.Path(sibling))
      }
    }
  }

  private def removeNoFollow(path: os.Path): Unit =
    if (Files.exists(path.toNIO, LinkOption.NOFOLLOW_LINKS)) {
      val attributes = Files.readAttributes(
        path.toNIO,
        classOf[BasicFileAttributes],
        LinkOption.NOFOLLOW_LINKS
      )
      if (attributes.isDirectory)
        Files.walkFileTree(
          path.toNIO,
          new SimpleFileVisitor[Path] {
            override def visitFile(file: Path, attributes: BasicFileAttributes): FileVisitResult = {
              Files.delete(file)
              FileVisitResult.CONTINUE
            }
            override def postVisitDirectory(directory: Path, error: IOException): FileVisitResult = {
              if (error != null) throw error
              Files.delete(directory)
              FileVisitResult.CONTINUE
            }
          }
        )
      else Files.deleteIfExists(path.toNIO)
    }

  private def coordinated[A](entry: os.Path)(operation: => A): A = {
    val lockPath = entry / os.up / s".${entry.last}.lock"
    val lockKey  = lockPath.toNIO.getParent.toRealPath().resolve(lockPath.last).toString
    val monitor  = AcquisitionCache.LocalLocks.computeIfAbsent(lockKey, _ => new Object)
    monitor.synchronized {
      Using.resource(
        FileChannel.open(lockPath.toNIO, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
      ) { channel =>
        Using.resource(acquireFileLock(channel, lockPath))(_ => operation)
      }
    }
  }

  private def acquireFileLock(channel: FileChannel, lockPath: os.Path): FileLock = {
    val deadline = System.nanoTime() + AcquisitionCache.FileLockTimeoutNanos
    var delay    = AcquisitionCache.InitialLockDelayMillis
    while (System.nanoTime() < deadline) {
      val lock =
        try channel.tryLock()
        catch { case _: OverlappingFileLockException => null }
      if (lock != null) return lock
      try Thread.sleep(delay)
      catch {
        case error: InterruptedException =>
          Thread.currentThread().interrupt()
          throw new IOException(s"Interrupted while waiting for acquisition lock $lockPath", error)
      }
      delay = math.min(delay * 2, AcquisitionCache.MaximumLockDelayMillis)
    }
    throw new IOException(
      s"Timed out waiting for acquisition lock $lockPath after ${AcquisitionCache.FileLockTimeoutSeconds} seconds"
    )
  }

  private def normalizedDigest(value: String): String = {
    val normalized = value.toLowerCase(java.util.Locale.ROOT)
    if (!normalized.matches("[0-9a-f]{64}"))
      throw new IllegalArgumentException(s"Invalid SHA-256 digest for acquisition: '$value'")
    normalized
  }
}

object AcquisitionCache {
  // Entries intentionally persist: unsafe eviction could create two monitors for the same digest concurrently.
  private val LocalLocks             = new ConcurrentHashMap[String, Object]()
  private val FileLockTimeoutSeconds = 30L
  private val FileLockTimeoutNanos   = FileLockTimeoutSeconds * 1000000000L
  private val InitialLockDelayMillis = 10L
  private val MaximumLockDelayMillis = 250L

  def apply(settings: AcquisitionSettings, taskRoot: os.Path): AcquisitionCache =
    new AcquisitionCache(settings, taskRoot)
}
