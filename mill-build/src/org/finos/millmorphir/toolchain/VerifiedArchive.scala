//| moduleDeps: ["//mill-build/src/org/finos/millmorphir/toolchain/NodeDistribution.scala"]
//| mvnDeps: ["org.apache.commons:commons-compress:1.28.0"]

package org.finos.millmorphir.toolchain

import java.io.{BufferedInputStream, InputStream}
import java.net.URL
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.nio.file.attribute.PosixFilePermission
import java.security.MessageDigest
import java.util.EnumSet
import java.util.zip.GZIPInputStream
import scala.jdk.CollectionConverters.*
import scala.collection.mutable.ArrayBuffer
import scala.util.Using

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

  def downloadAndExtract(url: URL, expectedSha256: String, format: ArchiveFormat, destination: os.Path): Unit = {
    os.makeDir.all(destination)
    val archive = destination / ".node-distribution.download"
    val staging = destination / ".node-distribution.extract"
    os.remove.all(staging)
    os.makeDir.all(staging)

    try {
      val actualSha256 = download(url, archive)
      verifyDigest(actualSha256, expectedSha256, url.toString)
      format match {
        case ArchiveFormat.TarGz => extractTarGz(archive, staging)
        case ArchiveFormat.Zip   => extractZip(archive, staging)
      }
      os.list(staging).foreach(path => os.move(path, destination / path.last, replaceExisting = true))
    } finally {
      os.remove.all(archive)
      os.remove.all(staging)
    }
  }

  private def download(url: URL, target: os.Path): String = {
    val connection = url.openConnection()
    connection.setConnectTimeout(30000)
    connection.setReadTimeout(60000)
    val digest = MessageDigest.getInstance("SHA-256")
    Using
      .Manager { use =>
        val rawInput = use(connection.getInputStream)
        val input    = use(new BufferedInputStream(rawInput))
        val output   = use(
          Files.newOutputStream(target.toNIO, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
        )
        copy(input, Some(output), digest)
      }
      .get
    hex(digest.digest())
  }

  private def extractTarGz(archive: os.Path, destination: os.Path): Unit =
    Using
      .Manager { use =>
        val rawInput = use(Files.newInputStream(archive.toNIO))
        val gzip     = use(new GZIPInputStream(rawInput))
        val input    = use(new TarArchiveInputStream(gzip))
        val root     = ArrayBuffer.empty[String]
        Iterator.continually(input.getNextEntry).takeWhile(_ != null).foreach { entry =>
          stripped(entry.getName, root).foreach(relative => extractTarEntry(input, entry, destination, relative, root))
        }
      }
      .get

  private def extractZip(archive: os.Path, destination: os.Path): Unit =
    Using.resource(ZipFile.builder().setPath(archive.toNIO).get()) { zipFile =>
      val root = ArrayBuffer.empty[String]
      zipFile.getEntries.asScala.foreach { entry =>
        stripped(entry.getName, root).foreach(relative => extractZipEntry(zipFile, entry, destination, relative))
      }
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
      root: ArrayBuffer[String]
  ): Unit = {
    val target = targetOrThrow(destination, relative)
    if (entry.isSymbolicLink) createSymbolicLink(destination, target, entry.getLinkName)
    else if (entry.isLink) {
      val linkRelative = stripKnownRoot(entry.getLinkName, root.head)
      val source       = targetOrThrow(destination, linkRelative)
      ensureNoSymlinkParents(destination, target)
      Files.createDirectories(target.toNIO.getParent)
      Files.createLink(target.toNIO, source.toNIO)
    } else if (entry.isDirectory) createDirectory(destination, target)
    else {
      writeFile(input, destination, target)
      preserveMode(target, entry.getMode)
    }
  }

  private def extractZipEntry(
      zipFile: ZipFile,
      entry: ZipArchiveEntry,
      destination: os.Path,
      relative: String
  ): Unit = {
    val target = targetOrThrow(destination, relative)
    if (entry.isUnixSymlink) {
      val linkTarget = Using.resource(zipFile.getInputStream(entry)) { input =>
        new String(readAll(input), java.nio.charset.StandardCharsets.UTF_8)
      }
      createSymbolicLink(destination, target, linkTarget)
    } else if (entry.isDirectory) createDirectory(destination, target)
    else {
      Using.resource(zipFile.getInputStream(entry))(input => writeFile(input, destination, target))
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

  private def writeFile(input: InputStream, destination: os.Path, target: os.Path): Unit = {
    ensureNoSymlinkParents(destination, target)
    Files.createDirectories(target.toNIO.getParent)
    Using.resource(
      Files.newOutputStream(
        target.toNIO,
        StandardOpenOption.CREATE_NEW,
        StandardOpenOption.WRITE
      )
    )(output => copy(input, Some(output), MessageDigest.getInstance("SHA-256")))
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

  private def readAll(input: InputStream): Array[Byte] = {
    val output = new java.io.ByteArrayOutputStream()
    val buffer = new Array[Byte](BufferSize)
    Iterator.continually(input.read(buffer)).takeWhile(_ >= 0).foreach(count => output.write(buffer, 0, count))
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
