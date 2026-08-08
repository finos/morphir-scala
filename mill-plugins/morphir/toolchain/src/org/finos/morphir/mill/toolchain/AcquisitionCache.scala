package org.finos.morphir.mill.toolchain

import java.io.{IOException, InputStream}
import java.nio.file.{
  AtomicMoveNotSupportedException,
  FileAlreadyExistsException,
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
import scala.util.Using
import scala.util.control.NonFatal

final class AcquisitionCache private (
    settings: AcquisitionSettings,
    taskRoot: os.Path,
    cleanup: os.Path => Unit,
    verify: (os.Path, String) => Unit
) {
  private enum CandidateState {
    case Verified, Unusable
  }

  def this(settings: AcquisitionSettings, taskRoot: os.Path) =
    this(settings, taskRoot, AcquisitionCache.removeNoFollow, VerifiedArchive.verifySha256)

  /** Acquires verified content and always closes the supplied stream after opening it. */
  def acquire(expectedSha256: String, source: String)(openStream: => InputStream): VerifiedContent =
    acquire(expectedSha256, source, AcquisitionLimits())(openStream)

  /** Acquires size-bounded verified content and always closes the supplied stream after opening it. */
  def acquire(expectedSha256: String, source: String, limits: AcquisitionLimits)(
      openStream: => InputStream
  ): VerifiedContent = {
    val digest = normalizedDigest(expectedSha256)
    val root   =
      if (settings.useMachineCache) settings.cacheRoot.getOrElse(AcquisitionSettings.defaultCacheRoot)
      else taskLocalCacheRoot()
    val entry = root / "sha256" / digest
    if (settings.useMachineCache) os.makeDir.all(entry / os.up)
    coordinated(entry) {
      acquireLocked(root, entry, digest, source, limits)(openStream)
    }
  }

  private def acquireLocked(
      cacheRoot: os.Path,
      entry: os.Path,
      digest: String,
      source: String,
      limits: AcquisitionLimits
  )(
      openStream: => InputStream
  ): VerifiedContent = {
    pruneStaleSiblings(entry)
    classifyCandidate(entry, digest, limits) match {
      case CandidateState.Verified => return checkedContent(entry, digest, source, limits)
      case CandidateState.Unusable => ()
    }
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
          copyBounded(input, output, limits.maxAcquiredBytes, source)
        }
        .get
      verify(temporary, digest)
      quarantineAndPromote(temporary, entry, source, cacheRoot)
      checkedContent(entry, digest, source, limits)
    } catch {
      case error: IllegalArgumentException =>
        throw new IllegalArgumentException(s"Verified acquisition failed for $source: ${error.getMessage}", error)
    } finally bestEffortCleanup(temporary)
  }

  private def classifyCandidate(
      entry: os.Path,
      digest: String,
      limits: AcquisitionLimits
  ): CandidateState =
    if (!Files.exists(entry.toNIO, LinkOption.NOFOLLOW_LINKS)) CandidateState.Unusable
    else {
      val attributes = Files.readAttributes(
        entry.toNIO,
        classOf[BasicFileAttributes],
        LinkOption.NOFOLLOW_LINKS
      )
      // An oversized candidate cannot be proven valid without violating this caller's I/O bound.
      // Treat it as unusable: online callers may replace it, while offline callers fail lazily.
      if (!attributes.isRegularFile || attributes.size() > limits.maxAcquiredBytes) CandidateState.Unusable
      else {
        if (scala.util.Try(verify(entry, digest)).isSuccess) CandidateState.Verified
        else CandidateState.Unusable
      }
    }

  private def checkedContent(
      entry: os.Path,
      digest: String,
      source: String,
      limits: AcquisitionLimits
  ): VerifiedContent = {
    val attributes = Files.readAttributes(
      entry.toNIO,
      classOf[BasicFileAttributes],
      LinkOption.NOFOLLOW_LINKS
    )
    if (!attributes.isRegularFile)
      throw new IllegalStateException(s"Verified acquisition content changed before use for $source: $entry")
    checkSize(attributes, source, limits)
    VerifiedContent(entry, digest)
  }

  private def checkSize(
      attributes: BasicFileAttributes,
      source: String,
      limits: AcquisitionLimits
  ): Unit =
    if (attributes.size() > limits.maxAcquiredBytes)
      throw new IllegalArgumentException(
        s"Verified acquisition acquired byte limit ${limits.maxAcquiredBytes} exceeded for $source: " +
          s"cached content is ${attributes.size()} bytes"
      )

  private def taskLocalCacheRoot(): os.Path = {
    os.makeDir.all(taskRoot)
    val root = taskRoot / ".morphir-acquisitions"
    ensureTaskLocalDirectory(root)
    ensureTaskLocalDirectory(root / "sha256")
    root
  }

  private def ensureTaskLocalDirectory(path: os.Path): Unit = {
    try Files.createDirectory(path.toNIO)
    catch { case _: FileAlreadyExistsException => () }
    val attributes = Files.readAttributes(
      path.toNIO,
      classOf[BasicFileAttributes],
      LinkOption.NOFOLLOW_LINKS
    )
    if (!attributes.isDirectory)
      throw new IllegalArgumentException(
        s"Reserved task-local acquisition cache path must be a directory and may not be a symbolic link: $path"
      )
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
    } finally if (promoted) quarantine.foreach(bestEffortCleanup)
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
          bestEffortCleanup(os.Path(sibling))
      }
    }
  }

  private def bestEffortCleanup(path: os.Path): Unit =
    try cleanup(path)
    catch { case NonFatal(_) => () }

  private def coordinated[A](entry: os.Path)(operation: => A): A = {
    val lockPath = entry / os.up / s".${entry.last}.lock"
    PathCoordinator.withLock(lockPath)(operation)
  }

  private def copyBounded(
      input: InputStream,
      output: java.io.OutputStream,
      maxBytes: Long,
      source: String
  ): Unit = {
    val buffer = new Array[Byte](64 * 1024)
    var total  = 0L
    Iterator.continually(input.read(buffer)).takeWhile(_ >= 0).foreach { count =>
      if (count > 0) {
        if (count > maxBytes - total)
          throw new IllegalArgumentException(
            s"Acquisition acquired byte limit $maxBytes exceeded for $source"
          )
        output.write(buffer, 0, count)
        total += count
      }
    }
  }

  private def normalizedDigest(value: String): String = {
    val normalized = value.toLowerCase(java.util.Locale.ROOT)
    if (!normalized.matches("[0-9a-f]{64}"))
      throw new IllegalArgumentException(s"Invalid SHA-256 digest for acquisition: '$value'")
    normalized
  }
}

object AcquisitionCache {
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

  def apply(settings: AcquisitionSettings, taskRoot: os.Path): AcquisitionCache =
    new AcquisitionCache(settings, taskRoot)

  private[toolchain] def withCleanup(settings: AcquisitionSettings, taskRoot: os.Path)(
      cleanup: os.Path => Unit
  ): AcquisitionCache =
    new AcquisitionCache(settings, taskRoot, cleanup, VerifiedArchive.verifySha256)

  private[toolchain] def withVerifier(settings: AcquisitionSettings, taskRoot: os.Path)(
      verify: (os.Path, String) => Unit
  ): AcquisitionCache =
    new AcquisitionCache(settings, taskRoot, removeNoFollow, verify)
}
