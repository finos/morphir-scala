package org.finos.morphir.mill.elm

import java.nio.ByteBuffer
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{
  AtomicMoveNotSupportedException,
  FileVisitResult,
  Files,
  LinkOption,
  OpenOption,
  Path as JPath,
  SimpleFileVisitor,
  StandardCopyOption,
  StandardOpenOption
}
import java.security.MessageDigest
import java.util.Locale
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

import mill.PathRef
import org.finos.morphir.mill.toolchain.{StorageSize, storageSize}
import upickle.default.ReadWriter

final case class ElmInputLimits(
    maxEntries: Int = 10000,
    maxFileBytes: StorageSize = storageSize"64 MiB",
    maxTotalBytes: StorageSize = storageSize"512 MiB"
)

object ElmProjectSnapshot {
  enum InputRole derives ReadWriter {
    case ElmJson, Source
  }

  final case class TrackedElmInput(path: String, role: InputRole, fingerprint: Vector[Int]) derives ReadWriter {
    def pathRef: PathRef = {
      val inputPath = os.Path(path, os.pwd)
      PathRef(
        inputPath,
        quick = false,
        sig = fingerprintSignature(fingerprintHex(fingerprint)),
        revalidate = PathRef.Revalidate.Never
      )
    }
  }

  private final class Budget(val limits: ElmInputLimits) {
    private var entries      = 0
    private var bytes        = 0L
    private val destinations = mutable.Set.empty[String]

    def addEntry(path: JPath): Unit = {
      entries += 1
      if (entries > limits.maxEntries)
        throw invalid(s"Elm input entry count limit ${limits.maxEntries} exceeded at $path")
    }

    def checkFile(path: JPath, size: Long): Unit = {
      if (size > limits.maxFileBytes.toBytes)
        throw invalid(s"Elm input file bytes limit ${limits.maxFileBytes.show} exceeded at $path")
      if (size > limits.maxTotalBytes.toBytes - bytes)
        throw invalid(s"Elm input total bytes limit ${limits.maxTotalBytes.show} exceeded at $path")
    }

    def addBytes(path: JPath, count: Int): Unit = {
      if (count > limits.maxTotalBytes.toBytes - bytes)
        throw invalid(s"Elm input total bytes limit ${limits.maxTotalBytes.show} exceeded at $path")
      bytes += count
    }

    def addDestination(path: os.RelPath): Unit = {
      val normalized = path.toString.toLowerCase(Locale.ROOT)
      if (!destinations.add(normalized))
        throw invalid(s"Elm inputs have a duplicate staged destination: $path")
    }
  }

  def stage(
      taskRoot: os.Path,
      elmJson: PathRef,
      sources: Seq[PathRef],
      entrypoint: os.RelPath,
      limits: ElmInputLimits,
      beforeRevalidate: () => Unit = () => ()
  ): os.Path =
    stage(taskRoot, trackInputs(elmJson.path, sources.map(_.path), limits), entrypoint, limits, beforeRevalidate)

  def trackInputs(
      elmJson: os.Path,
      sources: Seq[os.Path],
      limits: ElmInputLimits
  ): Seq[TrackedElmInput] = {
    validateLimits(limits)
    val sourceDestinations = validateInputPaths(elmJson, sources)
    val budget             = new Budget(limits)
    val jsonFingerprint    = scan(elmJson.toNIO, None, os.rel / "elm.json", budget)
    val sourceFingerprints = sources.zip(sourceDestinations).map { case (source, destination) =>
      TrackedElmInput(
        source.toString,
        InputRole.Source,
        fingerprintBytes(scan(source.toNIO, None, destination, budget))
      )
    }
    TrackedElmInput(elmJson.toString, InputRole.ElmJson, fingerprintBytes(jsonFingerprint)) +: sourceFingerprints
  }

  def stage(
      taskRoot: os.Path,
      trackedInputs: Seq[TrackedElmInput],
      entrypoint: os.RelPath,
      limits: ElmInputLimits,
      beforeRevalidate: () => Unit
  ): os.Path = {
    validateLimits(limits)
    val jsonInputs   = trackedInputs.filter(_.role == InputRole.ElmJson)
    val sourceInputs = trackedInputs.filter(_.role == InputRole.Source)
    if (jsonInputs.size != 1)
      throw invalid(s"Elm tracked inputs require exactly one elm.json role, found ${jsonInputs.size}")
    val elmJson            = jsonInputs.head.pathRef
    val sources            = sourceInputs.map(_.pathRef)
    val sourceDestinations = validateInputPaths(elmJson.path, sources.map(_.path))
    validateEntrypoint(entrypoint, sourceDestinations)

    val project = taskRoot / "project"
    val staging = taskRoot / "project.staging"
    removeOwned(staging)
    removeOwned(project)
    os.makeDir.all(staging)
    try {
      val inputs         = (jsonInputs.head, os.rel / "elm.json") +: sourceInputs.zip(sourceDestinations)
      val baselineBudget = new Budget(limits)
      inputs.foreach { case (tracked, destination) =>
        val actual = fingerprintBytes(scan(tracked.pathRef.path.toNIO, None, destination, baselineBudget))
        if (actual != tracked.fingerprint)
          throw invalid(s"Elm input full fingerprint changed before snapshot: ${tracked.path}")
      }
      val snapshotBudget = new Budget(limits)
      val fingerprints   = inputs.map { case (tracked, destination) =>
        val actual = scan(
          tracked.pathRef.path.toNIO,
          Some((staging / destination).toNIO),
          destination,
          snapshotBudget
        )
        if (fingerprintBytes(actual) != tracked.fingerprint)
          throw invalid(s"Elm input changed while creating snapshot: ${tracked.path}")
        tracked -> actual
      }
      val stagedEntrypoint = staging / entrypoint
      if (!Files.isRegularFile(stagedEntrypoint.toNIO, LinkOption.NOFOLLOW_LINKS))
        throw invalid(s"Elm entrypoint is not a staged regular file: $entrypoint")
      beforeRevalidate()
      val revalidationBudget = new Budget(limits)
      fingerprints.foreach { case (tracked, expected) =>
        val destination =
          if (tracked.role == InputRole.ElmJson) os.rel / "elm.json" else os.rel / tracked.pathRef.path.last
        val actual = scan(tracked.pathRef.path.toNIO, None, destination, revalidationBudget)
        if (actual != expected)
          throw invalid(s"Elm input changed after snapshot: ${tracked.path}")
      }
      promote(staging, project)
      project
    } catch {
      case error: Throwable =>
        removeOwned(staging)
        removeOwned(project)
        throw error
    }
  }

  private def validateInputPaths(elmJson: os.Path, sources: Seq[os.Path]): Seq[os.RelPath] = {
    val sourceDestinations = sources.map(source => os.rel / source.last)
    val topLevel           = (os.rel / "elm.json") +: sourceDestinations
    val duplicateTopLevel  = topLevel
      .groupBy(_.toString.toLowerCase(Locale.ROOT))
      .collectFirst { case (_, paths) if paths.size > 1 => paths.head }
    duplicateTopLevel.foreach(path => throw invalid(s"Elm inputs have a duplicate staged destination: $path"))
    validateRoot(elmJson, expectDirectory = false)
    sources.foreach(source => validateRoot(source, expectDirectory = true))
    sourceDestinations
  }

  private def validateEntrypoint(entrypoint: os.RelPath, sources: Seq[os.RelPath]): Unit = {
    val segments = entrypoint.segments.toSeq
    if (
      entrypoint.ups > 0 || segments.isEmpty || segments.exists(segment => segment == "." || segment == "..") ||
      !sources.exists(source => segments.startsWith(source.segments.toSeq))
    ) throw invalid(s"Elm entrypoint must remain inside a staged source directory: $entrypoint")
  }

  private def validateLimits(limits: ElmInputLimits): Unit = {
    if (limits.maxEntries <= 0) throw invalid(s"Elm input entry count limit must be positive: ${limits.maxEntries}")
    if (limits.maxFileBytes.toBytes <= 0)
      throw invalid(s"Elm input file bytes limit must be positive: ${limits.maxFileBytes.show}")
    if (limits.maxTotalBytes.toBytes <= 0)
      throw invalid(s"Elm input total bytes limit must be positive: ${limits.maxTotalBytes.show}")
  }

  private def validateRoot(path: os.Path, expectDirectory: Boolean): Unit = {
    if (Files.isSymbolicLink(path.toNIO))
      throw invalid(s"Elm input must not be a symbolic link: $path")
    val valid =
      if (expectDirectory) Files.isDirectory(path.toNIO, LinkOption.NOFOLLOW_LINKS)
      else Files.isRegularFile(path.toNIO, LinkOption.NOFOLLOW_LINKS)
    if (!valid)
      throw invalid(s"Elm input is not a ${if (expectDirectory) "directory" else "regular file"}: $path")
  }

  private def scan(
      source: JPath,
      destination: Option[JPath],
      stagedRoot: os.RelPath,
      budget: Budget
  ): String = {
    if (Files.isSymbolicLink(source))
      throw invalid(s"Elm input must not be a symbolic link: $source")
    if (!Files.exists(source, LinkOption.NOFOLLOW_LINKS))
      throw invalid(s"Elm input does not exist: $source")
    val records = mutable.ArrayBuffer.empty[String]

    def relative(path: JPath): JPath =
      if (Files.isDirectory(source, LinkOption.NOFOLLOW_LINKS)) source.relativize(path)
      else source.getFileSystem.getPath("")

    def staged(path: JPath): os.RelPath = {
      val suffix = relative(path).iterator().asScala
        .map(_.toString)
        .filter(_.nonEmpty)
        .foldLeft(os.rel)((current, segment) => current / segment)
      if (suffix.segments.isEmpty) stagedRoot else stagedRoot / suffix
    }

    def destinationPath(path: JPath): Option[JPath] =
      destination.map { root =>
        if (Files.isDirectory(source, LinkOption.NOFOLLOW_LINKS)) root.resolve(relative(path)) else root
      }

    Files.walkFileTree(
      source,
      new SimpleFileVisitor[JPath] {
        override def preVisitDirectory(directory: JPath, attributes: BasicFileAttributes): FileVisitResult = {
          if (attributes.isSymbolicLink || Files.isSymbolicLink(directory))
            throw invalid(s"Elm input contains a symbolic link: $directory")
          budget.addEntry(directory)
          budget.addDestination(staged(directory))
          destinationPath(directory).foreach(Files.createDirectories(_))
          records += s"D:${relative(directory)}"
          FileVisitResult.CONTINUE
        }

        override def visitFile(file: JPath, attributes: BasicFileAttributes): FileVisitResult = {
          if (attributes.isSymbolicLink || Files.isSymbolicLink(file))
            throw invalid(s"Elm input contains a symbolic link: $file")
          if (!attributes.isRegularFile)
            throw invalid(s"Elm input must contain only regular files: $file")
          budget.addEntry(file)
          budget.addDestination(staged(file))
          budget.checkFile(file, attributes.size())
          val digest      = MessageDigest.getInstance("SHA-256")
          val readOptions = Set[OpenOption](StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS).asJava
          val input       = Files.newByteChannel(file, readOptions)
          val output      = destinationPath(file).map { target =>
            Option(target.getParent).foreach(Files.createDirectories(_))
            Files.newByteChannel(
              target,
              Set[OpenOption](StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE).asJava
            )
          }
          try {
            val buffer    = ByteBuffer.allocate(8192)
            var fileBytes = 0L
            var read      = input.read(buffer)
            while (read >= 0) {
              if (read > 0) {
                if (read > budget.limits.maxFileBytes.toBytes - fileBytes)
                  throw invalid(s"Elm input file bytes limit ${budget.limits.maxFileBytes.show} exceeded at $file")
                fileBytes += read
                budget.addBytes(file, read)
                buffer.flip()
                digest.update(buffer.asReadOnlyBuffer())
                output.foreach(channel => while (buffer.hasRemaining) channel.write(buffer))
                buffer.clear()
              }
              read = input.read(buffer)
            }
          } finally {
            output.foreach(_.close())
            input.close()
          }
          val after = Files.readAttributes(file, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
          if (
            !after.isRegularFile || after.size() != attributes.size() ||
            after.lastModifiedTime() != attributes.lastModifiedTime() ||
            (attributes.fileKey() != null && after.fileKey() != null && attributes.fileKey() != after.fileKey())
          ) throw invalid(s"Elm input changed while creating snapshot: $file")
          records += s"F:${relative(file)}:${after.size()}:${hex(digest.digest())}"
          FileVisitResult.CONTINUE
        }

        override def visitFileFailed(file: JPath, error: java.io.IOException): FileVisitResult =
          throw invalid(s"Unable to read Elm input $file: ${error.getMessage}", error)
      }
    )
    val digest = MessageDigest.getInstance("SHA-256")
    records.sorted.foreach { record =>
      digest.update(record.getBytes(java.nio.charset.StandardCharsets.UTF_8))
      digest.update(0.toByte)
    }
    hex(digest.digest())
  }

  private def promote(staging: os.Path, project: os.Path): Unit =
    try Files.move(staging.toNIO, project.toNIO, StandardCopyOption.ATOMIC_MOVE)
    catch {
      case _: AtomicMoveNotSupportedException => Files.move(staging.toNIO, project.toNIO)
    }

  private def removeOwned(path: os.Path): Unit =
    if (Files.isSymbolicLink(path.toNIO)) Files.deleteIfExists(path.toNIO)
    else if (Files.exists(path.toNIO, LinkOption.NOFOLLOW_LINKS)) os.remove.all(path)

  private def hex(bytes: Array[Byte]): String = bytes.iterator.map(byte => f"${byte & 0xff}%02x").mkString

  private def fingerprintSignature(fingerprint: String): Int =
    java.util.Arrays.hashCode(fingerprint.getBytes(java.nio.charset.StandardCharsets.US_ASCII))

  private def fingerprintBytes(fingerprint: String): Vector[Int] =
    fingerprint.grouped(2).map(Integer.parseInt(_, 16)).toVector

  private def fingerprintHex(fingerprint: Vector[Int]): String =
    fingerprint.iterator.map(byte => f"$byte%02x").mkString

  private def invalid(detail: String, cause: Throwable = null): IllegalArgumentException =
    new IllegalArgumentException(detail, cause)
}
