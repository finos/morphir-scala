package org.finos.morphir.mill.toolchain

import java.io.InputStream
import java.nio.channels.{FileChannel, OverlappingFileLockException}
import java.nio.file.{
  AtomicMoveNotSupportedException,
  FileVisitResult,
  FileAlreadyExistsException,
  Files,
  LinkOption,
  Path,
  Paths,
  SimpleFileVisitor,
  StandardCopyOption,
  StandardOpenOption
}
import java.nio.file.attribute.{BasicFileAttributes, PosixFilePermission}
import java.security.MessageDigest
import java.util.{EnumSet, UUID}
import java.util.zip.GZIPInputStream
import scala.jdk.CollectionConverters.*
import scala.collection.mutable.ArrayBuffer
import scala.util.Using
import scala.util.control.NonFatal

import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveInputStream}
import org.apache.commons.compress.archivers.zip.{ZipArchiveEntry, ZipFile}

object VerifiedArchive {
  private val BufferSize   = 64 * 1024
  private val WindowsDrive = "(?i)^[a-z]:.*".r

  def sha256(bytes: Array[Byte]): String =
    hex(MessageDigest.getInstance("SHA-256").digest(bytes))

  def sha256(path: os.Path): String = {
    val digest = MessageDigest.getInstance("SHA-256")
    Using.resource(Files.newInputStream(path.toNIO))(input => copy(input, None, digest))
    hex(digest.digest())
  }

  def verifySha256(path: os.Path, expected: String): Unit = {
    val actual = sha256(path)
    verifyDigest(actual, expected, path.toString)
  }

  def safeTarget(root: os.Path, archivePath: String): Either[String, os.Path] = {
    val normalized = archivePath.replace('\\', '/')
    if (
      normalized.isEmpty || normalized.startsWith("/") ||
      WindowsDrive.matches(normalized) || normalized.split('/').contains("..")
    ) Left(s"Unsafe archive path: '$archivePath'")
    else
      try {
        val target = root / os.RelPath(normalized)
        if (target.toNIO.normalize().startsWith(root.toNIO.normalize())) Right(target)
        else Left(s"Archive path escapes destination: '$archivePath'")
      } catch {
        case _: IllegalArgumentException => Left(s"Unsafe archive path: '$archivePath'")
      }
  }

  /** Extracts a verified archive into an exclusive destination, which must be absent or an empty directory. */
  def extract(
      archive: VerifiedContent,
      format: ArchiveFormat,
      destination: os.Path,
      limits: ArchiveLimits = ArchiveLimits()
  ): Unit =
    extractObserved(archive, format, destination, limits)(())

  private[toolchain] def extractObserved(
      archive: VerifiedContent,
      format: ArchiveFormat,
      destination: os.Path,
      limits: ArchiveLimits = ArchiveLimits(),
      cleanup: os.Path => Unit = removeTemporary,
      parserObserver: ArchiveFormat => Unit = _ => (),
      zipConstructedObserver: () => Unit = () => (),
      gzipConstructedObserver: () => Unit = () => ()
  )(afterSnapshotVerified: => Unit): Unit = {
    val parent   = destination / os.up
    val nonce    = UUID.randomUUID()
    val snapshot = parent / s".${destination.last}.$nonce.archive"
    val staging  = parent / s".${destination.last}.$nonce.extract"
    val lease    = parent / s".${destination.last}.$nonce.lease"
    os.makeDir.all(parent)
    withExtractionLease(lease) {
      PathCoordinator.withLock(destinationLock(destination)) {
        pruneStaleSiblings(parent, destination.last, cleanup)
        requireExclusiveDestination(destination)
      }
      try {
        snapshotAndVerify(archive, snapshot, limits.maxCompressedArchiveBytes)
        afterSnapshotVerified
        Files.createDirectory(staging.toNIO)
        format match {
          case ArchiveFormat.TarGz =>
            extractTarGz(snapshot, staging, limits, parserObserver, gzipConstructedObserver)
          case ArchiveFormat.Zip =>
            extractZip(snapshot, staging, limits, parserObserver, zipConstructedObserver)
        }
        promoteExclusive(staging, destination)
      } catch {
        case primary: Throwable =>
          cleanupSuppressed(snapshot, primary, cleanup)
          cleanupSuppressed(staging, primary, cleanup)
          throw primary
      }
      bestEffortCleanup(snapshot, cleanup)
      bestEffortCleanup(staging, cleanup)
    }
  }

  private def withExtractionLease[A](leasePath: os.Path)(body: => A): A = {
    val channel = FileChannel.open(
      leasePath.toNIO,
      StandardOpenOption.CREATE_NEW,
      StandardOpenOption.WRITE,
      LinkOption.NOFOLLOW_LINKS
    )
    val lease = channel.lock()
    try body
    finally {
      try lease.release()
      catch { case _: Throwable => () }
      try channel.close()
      catch { case _: Throwable => () }
      bestEffortCleanup(leasePath, removeTemporary)
    }
  }

  private def cleanupSuppressed(path: os.Path, primary: Throwable, cleanup: os.Path => Unit): Unit =
    try cleanup(path)
    catch {
      case cleanupError: Throwable if cleanupError ne primary => primary.addSuppressed(cleanupError)
      case _: Throwable                                       => ()
    }

  private def bestEffortCleanup(path: os.Path, cleanup: os.Path => Unit): Unit =
    try cleanup(path)
    catch { case _: Throwable => () }

  private def removeTemporary(path: os.Path): Unit = {
    if (!Files.exists(path.toNIO, LinkOption.NOFOLLOW_LINKS)) return
    val attributes = Files.readAttributes(path.toNIO, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
    if (!attributes.isDirectory) Files.deleteIfExists(path.toNIO)
    else
      Files.walkFileTree(
        path.toNIO,
        new SimpleFileVisitor[Path] {
          override def visitFile(file: Path, attributes: BasicFileAttributes): FileVisitResult = {
            Files.deleteIfExists(file)
            FileVisitResult.CONTINUE
          }

          override def postVisitDirectory(directory: Path, error: java.io.IOException): FileVisitResult = {
            if (error != null) throw error
            Files.deleteIfExists(directory)
            FileVisitResult.CONTINUE
          }
        }
      )
  }

  private def pruneStaleSiblings(parent: os.Path, destinationName: String, cleanup: os.Path => Unit): Unit =
    try
      Using.resource(Files.newDirectoryStream(parent.toNIO)) { entries =>
        entries.asScala.foreach { entry =>
          val name = entry.getFileName.toString
          if (
            name.startsWith(s".$destinationName.") &&
            (name.endsWith(".archive") || name.endsWith(".extract"))
          ) {
            val suffix    = if (name.endsWith(".archive")) ".archive" else ".extract"
            val leaseName = name.stripSuffix(suffix) + ".lease"
            pruneIfUnleased(os.Path(entry), parent / leaseName, cleanup)
          }
        }
      }
    catch { case _: Throwable => () }

  private def pruneIfUnleased(stale: os.Path, leasePath: os.Path, cleanup: os.Path => Unit): Unit = {
    var channel: FileChannel             = null
    var lock: java.nio.channels.FileLock = null
    try {
      channel = FileChannel.open(
        leasePath.toNIO,
        StandardOpenOption.CREATE,
        StandardOpenOption.WRITE,
        LinkOption.NOFOLLOW_LINKS
      )
      lock = channel.tryLock()
      if (lock != null) bestEffortCleanup(stale, cleanup)
    } catch {
      case _: OverlappingFileLockException => ()
      case _: Throwable                    => ()
    } finally {
      if (lock != null)
        try lock.release()
        catch { case _: Throwable => () }
      if (channel != null)
        try channel.close()
        catch { case _: Throwable => () }
      if (lock != null) bestEffortCleanup(leasePath, removeTemporary)
    }
  }

  private def snapshotAndVerify(content: VerifiedContent, snapshot: os.Path, maxBytes: Long): Unit = {
    val digest = MessageDigest.getInstance("SHA-256")
    Using
      .Manager { use =>
        val input  = use(Files.newInputStream(content.path.toNIO))
        val output = use(
          Files.newOutputStream(snapshot.toNIO, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
        )
        copySnapshotBounded(input, output, digest, maxBytes, content.path)
      }
      .get
    verifyDigest(hex(digest.digest()), content.sha256, content.path.toString)
  }

  private def copySnapshotBounded(
      input: InputStream,
      output: java.io.OutputStream,
      digest: MessageDigest,
      maxBytes: Long,
      source: os.Path
  ): Unit = {
    val buffer = new Array[Byte](BufferSize)
    var total  = 0L
    Iterator.continually(input.read(buffer)).takeWhile(_ >= 0).foreach { count =>
      if (count > 0) {
        if (count > maxBytes - total)
          throw new IllegalArgumentException(
            s"Compressed archive byte limit $maxBytes exceeded while snapshotting $source"
          )
        digest.update(buffer, 0, count)
        output.write(buffer, 0, count)
        total += count
      }
    }
  }

  private def requireExclusiveDestination(destination: os.Path): Unit =
    if (Files.exists(destination.toNIO, LinkOption.NOFOLLOW_LINKS)) {
      val attributes = Files.readAttributes(
        destination.toNIO,
        classOf[BasicFileAttributes],
        LinkOption.NOFOLLOW_LINKS
      )
      if (!attributes.isDirectory || os.list(destination).nonEmpty)
        throw new IllegalArgumentException(
          s"Archive extraction destination must be absent or empty: $destination"
        )
    }

  private def promoteExclusive(staging: os.Path, destination: os.Path): Unit =
    PathCoordinator.withLock(destinationLock(destination)) {
      promoteExclusiveLocked(staging, destination)
    }

  private def destinationLock(destination: os.Path): os.Path =
    destination / os.up / s".${destination.last}.lock"

  private def promoteExclusiveLocked(staging: os.Path, destination: os.Path): Unit = {
    requireExclusiveDestination(destination)
    val hadEmptyDestination = Files.exists(destination.toNIO, LinkOption.NOFOLLOW_LINKS)
    if (hadEmptyDestination) Files.delete(destination.toNIO)
    try Files.move(staging.toNIO, destination.toNIO, StandardCopyOption.ATOMIC_MOVE)
    catch {
      case error: FileAlreadyExistsException =>
        throw new IllegalStateException(s"Archive extraction destination became nonempty: $destination", error)
      case error: AtomicMoveNotSupportedException =>
        val failure = new java.io.IOException(
          s"Atomic archive extraction promotion is unavailable from $staging to $destination; " +
            "choose a destination on a filesystem that supports atomic moves",
          error
        )
        restoreEmptyDestination(destination, hadEmptyDestination, failure)
        throw failure
      case NonFatal(error) =>
        restoreEmptyDestination(destination, hadEmptyDestination, error)
        throw error
    }
  }

  private def restoreEmptyDestination(
      destination: os.Path,
      hadEmptyDestination: Boolean,
      failure: Throwable
  ): Unit =
    if (hadEmptyDestination && !Files.exists(destination.toNIO, LinkOption.NOFOLLOW_LINKS))
      try Files.createDirectory(destination.toNIO)
      catch {
        case _: FileAlreadyExistsException => ()
        case NonFatal(restorationError)    => failure.addSuppressed(restorationError)
      }

  private def extractTarGz(
      archive: os.Path,
      destination: os.Path,
      limits: ArchiveLimits,
      parserObserver: ArchiveFormat => Unit,
      gzipConstructedObserver: () => Unit
  ): Unit = {
    ArchivePreflight.tarGz(archive, limits, gzipConstructedObserver)
    parserObserver(ArchiveFormat.TarGz)
    Using
      .Manager { use =>
        val rawInput = use(Files.newInputStream(archive.toNIO))
        val gzip     = use(new GZIPInputStream(rawInput))
        val input    = use(new TarArchiveInputStream(gzip))
        val root     = ArrayBuffer.empty[String]
        val budget   = new ExtractionBudget(limits, Some(Files.size(archive.toNIO)))
        Iterator.continually(input.getNextEntry).takeWhile(_ != null).foreach { entry =>
          budget.beginEntry(entry.getName, entry.getSize)
          validateTarEntryType(entry)
          stripped(entry.getName, root).foreach { relative =>
            extractTarEntry(input, entry, destination, relative, root, budget)
          }
        }
      }
      .get
  }

  private def validateTarEntryType(entry: TarArchiveEntry): Unit =
    if (
      !(
        entry.isDirectory || entry.isSymbolicLink || entry.isLink ||
          entry.getLinkFlag == org.apache.commons.compress.archivers.tar.TarConstants.LF_NORMAL ||
          entry.getLinkFlag == org.apache.commons.compress.archivers.tar.TarConstants.LF_OLDNORM
      )
    )
      throw new IllegalArgumentException(
        s"Unsupported TAR entry type for '${entry.getName}' (link flag ${entry.getLinkFlag.toInt})"
      )

  private def extractZip(
      archive: os.Path,
      destination: os.Path,
      limits: ArchiveLimits,
      parserObserver: ArchiveFormat => Unit,
      constructedObserver: () => Unit
  ): Unit = {
    ArchivePreflight.zip(archive, limits)
    parserObserver(ArchiveFormat.Zip)
    Using.resource(ZipFile.builder().setPath(archive.toNIO).setIgnoreLocalFileHeader(true).get()) { zipFile =>
      constructedObserver()
      val root   = ArrayBuffer.empty[String]
      val budget = new ExtractionBudget(limits, None)
      zipFile.getEntries.asScala.foreach { entry =>
        validateZipCompressionRatio(entry, limits)
        budget.beginEntry(entry.getName, entry.getSize)
        stripped(entry.getName, root).foreach(relative =>
          extractZipEntry(zipFile, entry, destination, relative, budget)
        )
      }
    }
  }

  private def validateZipCompressionRatio(entry: ZipArchiveEntry, limits: ArchiveLimits): Unit = {
    val uncompressed = entry.getSize
    val compressed   = entry.getCompressedSize
    if (
      uncompressed > 0 && compressed >= 0 &&
      (compressed == 0 || uncompressed.toDouble / compressed.toDouble > limits.maxCompressionRatio)
    )
      throw new IllegalArgumentException(
        s"ZIP entry compression ratio exceeds limit ${limits.maxCompressionRatio} at '${entry.getName}'"
      )
  }

  private final class ExtractionBudget(limits: ArchiveLimits, tarGzCompressedBytes: Option[Long]) {
    private var entryCount       = 0L
    private var entryBytes       = 0L
    private var totalBytes       = 0L
    private var currentEntryName = ""

    def beginEntry(name: String, declaredSize: Long): Unit = {
      entryCount += 1
      entryBytes = 0L
      currentEntryName = name
      if (entryCount > limits.maxEntries)
        throw new IllegalArgumentException(
          s"Archive entry count exceeds limit ${limits.maxEntries} at '$name'"
        )
      if (declaredSize > limits.maxEntryUncompressedBytes)
        throw new IllegalArgumentException(
          s"Archive per-entry uncompressed byte limit ${limits.maxEntryUncompressedBytes} exceeded by '$name'"
        )
    }

    def recordBytes(count: Int): Unit = {
      if (count > limits.maxEntryUncompressedBytes - entryBytes)
        throw new IllegalArgumentException(
          s"Archive per-entry uncompressed byte limit ${limits.maxEntryUncompressedBytes} exceeded by '$currentEntryName'"
        )
      if (count > limits.maxTotalUncompressedBytes - totalBytes)
        throw new IllegalArgumentException(
          s"Archive total uncompressed byte limit ${limits.maxTotalUncompressedBytes} exceeded at '$currentEntryName'"
        )
      entryBytes += count
      totalBytes += count
      tarGzCompressedBytes.filter(_ > 0).foreach { compressedBytes =>
        if (totalBytes.toDouble / compressedBytes.toDouble > limits.maxCompressionRatio)
          throw new IllegalArgumentException(
            s"tar.gz overall compression ratio exceeds limit ${limits.maxCompressionRatio} at '$currentEntryName'"
          )
      }
    }

    def checkLinkTarget(target: String): Unit =
      checkLinkTargetBytes(target.getBytes(java.nio.charset.StandardCharsets.UTF_8).length)

    def checkLinkTargetBytes(size: Int): Unit =
      if (size > limits.maxSymlinkTargetBytes)
        throw new IllegalArgumentException(
          s"Archive link target byte limit ${limits.maxSymlinkTargetBytes} exceeded at '$currentEntryName'"
        )
  }

  private def stripped(name: String, root: ArrayBuffer[String]): Option[String] = {
    val normalized = name.replace('\\', '/')
    if (normalized.startsWith("/") || WindowsDrive.matches(normalized))
      throw new IllegalArgumentException(s"Unsafe archive path: '$name'")
    val parts = normalized.split('/').filter(_.nonEmpty)
    if (parts.isEmpty || parts.head == "." || parts.head == "..")
      throw new IllegalArgumentException(s"Archive entry has no safe root: '$name'")
    if (root.isEmpty) root += parts.head
    else if (root.head != parts.head)
      throw new IllegalArgumentException(s"Archive contains multiple roots: '${root.head}' and '${parts.head}'")
    val relative = parts.drop(1).mkString("/")
    Option.when(relative.nonEmpty)(relative)
  }

  private def extractTarEntry(
      input: TarArchiveInputStream,
      entry: TarArchiveEntry,
      destination: os.Path,
      relative: String,
      root: ArrayBuffer[String],
      budget: ExtractionBudget
  ): Unit = {
    val target = targetOrThrow(destination, relative)
    if (entry.isSymbolicLink) {
      budget.checkLinkTarget(entry.getLinkName)
      createSymbolicLink(destination, target, entry.getLinkName)
    } else if (entry.isLink) {
      budget.checkLinkTarget(entry.getLinkName)
      val linkRelative = stripKnownRoot(entry.getLinkName, root.head)
      val source       = targetOrThrow(destination, linkRelative)
      ensureNoSymlinkParents(destination, target)
      Files.createDirectories(target.toNIO.getParent)
      Files.createLink(target.toNIO, source.toNIO)
    } else if (entry.isDirectory) createDirectory(destination, target)
    else {
      writeFile(input, destination, target, budget)
      preserveMode(target, entry.getMode)
    }
  }

  private def extractZipEntry(
      zipFile: ZipFile,
      entry: ZipArchiveEntry,
      destination: os.Path,
      relative: String,
      budget: ExtractionBudget
  ): Unit = {
    val target = targetOrThrow(destination, relative)
    if (entry.isUnixSymlink) {
      val linkTarget = Using.resource(zipFile.getInputStream(entry)) { input =>
        new String(readLinkTarget(input, budget), java.nio.charset.StandardCharsets.UTF_8)
      }
      createSymbolicLink(destination, target, linkTarget)
    } else if (entry.isDirectory) createDirectory(destination, target)
    else {
      Using.resource(zipFile.getInputStream(entry))(input => writeFile(input, destination, target, budget))
      preserveMode(target, entry.getUnixMode)
    }
  }

  private def targetOrThrow(destination: os.Path, relative: String): os.Path =
    safeTarget(destination, relative).fold(message => throw new IllegalArgumentException(message), identity)

  private def stripKnownRoot(linkName: String, root: String): String = {
    val normalized = linkName.replace('\\', '/')
    val prefix     = s"$root/"
    if (normalized.startsWith(prefix)) normalized.stripPrefix(prefix) else normalized
  }

  private def createSymbolicLink(destination: os.Path, link: os.Path, linkTarget: String): Unit = {
    requireInside(destination, link, linkTarget)
    val targetPath = Paths.get(linkTarget.replace('\\', '/'))
    if (targetPath.isAbsolute || WindowsDrive.matches(linkTarget))
      throw new IllegalArgumentException(s"Unsafe symbolic link target: '$linkTarget'")
    val resolved = link.toNIO.getParent.resolve(targetPath).normalize()
    if (!resolved.startsWith(destination.toNIO.normalize()))
      throw new IllegalArgumentException(s"Symbolic link escapes destination: '$linkTarget'")
    ensureNoSymlinkParents(destination, link)
    Files.createDirectories(link.toNIO.getParent)
    Files.createSymbolicLink(link.toNIO, targetPath)
  }

  private def writeFile(
      input: InputStream,
      destination: os.Path,
      target: os.Path,
      budget: ExtractionBudget
  ): Unit = {
    ensureNoSymlinkParents(destination, target)
    Files.createDirectories(target.toNIO.getParent)
    Using.resource(
      Files.newOutputStream(
        target.toNIO,
        StandardOpenOption.CREATE_NEW,
        StandardOpenOption.WRITE
      )
    ) { output =>
      val buffer = new Array[Byte](BufferSize)
      Iterator.continually(input.read(buffer)).takeWhile(_ >= 0).foreach { count =>
        if (count > 0) {
          budget.recordBytes(count)
          output.write(buffer, 0, count)
        }
      }
    }
  }

  private def createDirectory(destination: os.Path, target: os.Path): Unit = {
    ensureNoSymlinkParents(destination, target)
    Files.createDirectories(target.toNIO)
  }

  private def ensureNoSymlinkParents(destination: os.Path, target: os.Path): Unit = {
    var current = target.toNIO.getParent
    val root    = destination.toNIO.normalize()
    while (current != null && current.startsWith(root) && current != root) {
      if (Files.isSymbolicLink(current))
        throw new IllegalArgumentException(s"Archive entry traverses symbolic link: $target")
      current = current.getParent
    }
  }

  private def requireInside(destination: os.Path, target: os.Path, original: String): Unit =
    if (!target.toNIO.normalize().startsWith(destination.toNIO.normalize()))
      throw new IllegalArgumentException(s"Path escapes destination: '$original'")

  private def preserveMode(path: os.Path, mode: Int): Unit = {
    if (mode == 0) return
    val permissions                                          = EnumSet.noneOf(classOf[PosixFilePermission])
    def add(bit: Int, permission: PosixFilePermission): Unit = if ((mode & bit) != 0) permissions.add(permission)
    add(0x100, PosixFilePermission.OWNER_READ)
    add(0x080, PosixFilePermission.OWNER_WRITE)
    add(0x040, PosixFilePermission.OWNER_EXECUTE)
    add(0x020, PosixFilePermission.GROUP_READ)
    add(0x010, PosixFilePermission.GROUP_WRITE)
    add(0x008, PosixFilePermission.GROUP_EXECUTE)
    add(0x004, PosixFilePermission.OTHERS_READ)
    add(0x002, PosixFilePermission.OTHERS_WRITE)
    add(0x001, PosixFilePermission.OTHERS_EXECUTE)
    try Files.setPosixFilePermissions(path.toNIO, permissions)
    catch { case _: UnsupportedOperationException => () }
  }

  private def readLinkTarget(input: InputStream, budget: ExtractionBudget): Array[Byte] = {
    val output      = new java.io.ByteArrayOutputStream()
    val buffer      = new Array[Byte](BufferSize)
    var targetBytes = 0
    Iterator.continually(input.read(buffer)).takeWhile(_ >= 0).foreach { count =>
      if (count > 0) {
        targetBytes += count
        budget.checkLinkTargetBytes(targetBytes)
        budget.recordBytes(count)
        output.write(buffer, 0, count)
      }
    }
    output.toByteArray
  }

  private def copy(input: InputStream, output: Option[java.io.OutputStream], digest: MessageDigest): Unit = {
    val buffer = new Array[Byte](BufferSize)
    Iterator.continually(input.read(buffer)).takeWhile(_ >= 0).foreach { count =>
      if (count > 0) {
        digest.update(buffer, 0, count)
        output.foreach(_.write(buffer, 0, count))
      }
    }
  }

  private def hex(bytes: Array[Byte]): String = bytes.map(byte => f"${byte & 0xff}%02x").mkString

  private def verifyDigest(actual: String, expected: String, source: String): Unit = {
    val charset = java.nio.charset.StandardCharsets.US_ASCII
    if (!MessageDigest.isEqual(actual.getBytes(charset), expected.toLowerCase.getBytes(charset)))
      throw new IllegalArgumentException(s"SHA-256 mismatch for $source: expected $expected, got $actual")
  }
}
