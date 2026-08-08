package org.finos.morphir.mill.elm.morphir

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
import scala.util.control.NonFatal

import mill.PathRef
import mill.api.JsonFormatters.*
import org.finos.morphir.mill.*
import org.finos.morphir.mill.elm.ElmInputLimits
import upickle.default.{ReadWriter, read, write}

final case class StagedMorphirProject(projectDir: PathRef, output: os.Path) derives ReadWriter

object MorphirElmProjectSandbox {
  private val SafeFilename         = "[A-Za-z0-9](?:[A-Za-z0-9._-]*[A-Za-z0-9])?".r
  private val WindowsReservedNames =
    Set("CON", "PRN", "AUX", "NUL") ++ (1 to 9).flatMap(number => Seq(s"COM$number", s"LPT$number"))
  private val WindowsInvalidPathCharacters = Set('<', '>', ':', '"', '|', '?', '*')
  private val ReservedSandboxPaths         = Set(".morphir-deps", "morphir.json", "elm.json")

  private final class Budget(limits: ElmInputLimits) {
    private var entries = 0L
    private var bytes   = 0L
    private val staged  = mutable.Set.empty[String]

    def addEntry(path: JPath): Unit = {
      if (entries == Long.MaxValue || entries + 1L > limits.maxEntries.toLong)
        throw invalid(s"source entry count limit ${limits.maxEntries} exceeded at $path")
      entries += 1L
    }

    def addDestination(path: JPath): Unit = {
      val normalized = path.normalize().toString.toLowerCase(Locale.ROOT)
      if (!staged.add(normalized)) throw invalid(s"duplicate staged source destination: $path")
    }

    def checkDeclaredSize(path: JPath, size: Long): Unit = {
      if (size > limits.maxFileBytes.toBytes)
        throw invalid(s"source file bytes limit ${limits.maxFileBytes.show} exceeded at $path")
      if (size > limits.maxTotalBytes.toBytes - bytes)
        throw invalid(s"source total bytes limit ${limits.maxTotalBytes.show} exceeded at $path")
    }

    def addBytes(path: JPath, count: Int, fileBytes: Long): Unit = {
      if (count > limits.maxFileBytes.toBytes - fileBytes)
        throw invalid(s"source file bytes limit ${limits.maxFileBytes.show} exceeded at $path")
      if (count > limits.maxTotalBytes.toBytes - bytes)
        throw invalid(s"source total bytes limit ${limits.maxTotalBytes.show} exceeded at $path")
      bytes += count
    }
  }

  def dependencyRelativePath(moduleId: ModuleId): os.RelPath =
    os.rel / ".morphir-deps" / moduleId.value / "morphir-ir.json"

  def rewrittenConfig(
      config: MorphirProjectConfig,
      dependencies: Seq[MorphirDependencyArtifact]
  ): Either[String, MorphirProjectConfig] = {
    val duplicates = dependencies
      .groupBy(_.moduleId)
      .collect { case (id, artifacts) if artifacts.size > 1 => id.value }
      .toSeq
      .sorted
    if (duplicates.nonEmpty)
      Left(s"Morphir Elm project has duplicate dependency module IDs: ${duplicates.mkString(", ")}")
    else
      Right(config.withLocalDependencies(dependencies.map(dependencyRelativePathFor).map(_.toString).toList))
  }

  def stage(
      root: os.Path,
      config: os.Path,
      elm: Option[os.Path],
      source: os.Path,
      dependencies: Seq[MorphirDependencyArtifact],
      limits: ElmInputLimits = ElmInputLimits()
  ): Either[String, StagedMorphirProject] =
    try stage(root, MorphirElmProjectInputs.capture(config, elm, source, limits), dependencies, limits)
    catch {
      case NonFatal(error) => Left(layerMessage(error))
    }

  def stage(
      root: os.Path,
      tracked: TrackedMorphirElmProject,
      dependencies: Seq[MorphirDependencyArtifact],
      limits: ElmInputLimits
  ): Either[String, StagedMorphirProject] = {
    val staging = root / os.up / s"${root.last}.staging"
    try {
      validateLimits(limits)
      MorphirElmProjectInputs.revalidate(tracked, limits)
      removeOwned(staging)
      removeOwned(root)
      os.makeDir.all(staging)

      val config = tracked.morphirJson.pathRef.path
      val elm    = tracked.elmJson.map(_.pathRef.path)
      val source = tracked.source.pathRef.path
      copyStableFile(config.toNIO, (staging / "morphir.json").toNIO, None, None)
      elm.foreach(path => copyStableFile(path.toNIO, (staging / "elm.json").toNIO, None, None))

      val original              = readConfig(staging / "morphir.json")
      val sourceRelative        = safeSourceDirectory(original.sourceDirectory)
      val trackedSourceRelative = MorphirElmProjectInputs.trackedSourceRelative(config, source)
      if (trackedSourceRelative.segments.exists(segment => !isPortablePathSegment(segment)))
        throw invalid(s"tracked source root has a non-portable project-relative path: $source")
      if (!sourceRelative.segments.toSeq.startsWith(trackedSourceRelative.segments.toSeq))
        throw invalid(
          s"configured source root ${original.sourceDirectory} is outside the tracked source root $source"
        )
      validateSandboxDestination(trackedSourceRelative)
      val rewritten = rewrittenConfig(original, dependencies).fold(message => throw invalid(message), identity)
      elm.foreach(_ => validateElmConfig(staging / "elm.json", sourceRelative))

      val sourceBudget = new Budget(limits)
      copySourceDirectory(source.toNIO, (staging / trackedSourceRelative).toNIO, sourceBudget)
      dependencies.foreach { dependency =>
        val destination = staging / dependencyRelativePathFor(dependency)
        copyStableFile(
          dependency.irFilePath.path.toNIO,
          destination.toNIO,
          Some(dependency.sha256),
          None
        )
      }

      MorphirElmProjectInputs.verifyCopied(
        tracked,
        staging / "morphir.json",
        elm.map(_ => staging / "elm.json"),
        staging / trackedSourceRelative,
        limits
      )
      MorphirElmProjectInputs.revalidate(tracked, limits)
      os.write.over(staging / "morphir.json", write(rewritten, indent = 2))
      promote(staging, root)
      Right(StagedMorphirProject(PathRef(root), root / "morphir-ir.json"))
    } catch {
      case NonFatal(error) =>
        removeOwned(staging)
        removeOwned(root)
        Left(layerMessage(error))
    }
  }

  def withOutputFilename(project: StagedMorphirProject, filename: String): Either[String, StagedMorphirProject] =
    filename match {
      case SafeFilename() if !isWindowsReserved(filename) && !hasStagedCollision(project.projectDir.path, filename) =>
        Right(project.copy(output = project.projectDir.path / filename))
      case SafeFilename() if !isWindowsReserved(filename) =>
        Left(s"Morphir Elm IR output filename collides with a staged project input: $filename")
      case _ => Left(s"Morphir Elm IR output filename must be a portable sandbox leaf: $filename")
    }

  private[morphir] def validateOutputAvailable(project: StagedMorphirProject): Either[String, StagedMorphirProject] =
    if (
      Files.isSymbolicLink(project.projectDir.path.toNIO) ||
      !Files.isDirectory(project.projectDir.path.toNIO, LinkOption.NOFOLLOW_LINKS)
    ) Left(s"Morphir Elm project sandbox root changed after sandbox extension: ${project.projectDir.path}")
    else if (hasStagedCollision(project.projectDir.path, project.output.last))
      Left(s"Morphir Elm IR output filename collides with a sandbox extension: ${project.output.last}")
    else Right(project)

  private[morphir] def discardOwnedProject(projectRoot: os.Path): Unit =
    removeOwned(projectRoot)

  private def dependencyRelativePathFor(dependency: MorphirDependencyArtifact): os.RelPath =
    dependencyRelativePath(dependency.moduleId)

  private def readConfig(path: os.Path): MorphirProjectConfig =
    try read[MorphirProjectConfig](os.read(path))
    catch {
      case NonFatal(error) => throw invalid(s"invalid morphir.json at $path: ${error.getMessage}", error)
    }

  private def safeSourceDirectory(value: String): os.RelPath =
    try {
      val segments = value.split("/", -1).toSeq
      if (
        value.isEmpty || value.contains('\\') || value.matches("^[A-Za-z]:.*") ||
        segments.exists(segment => !isPortablePathSegment(segment))
      ) throw invalid(s"unsafe Morphir source directory: $value")
      segments.foldLeft(os.rel)((relative, segment) => relative / segment)
    } catch {
      case error: IllegalArgumentException if error.getMessage.startsWith("Morphir Elm project sandbox:") =>
        throw error
      case _: IllegalArgumentException => throw invalid(s"unsafe Morphir source directory: $value")
    }

  private def validateSandboxDestination(source: os.RelPath): Unit =
    source.segments.headOption.foreach { segment =>
      if (ReservedSandboxPaths.contains(segment.toLowerCase(Locale.ROOT)))
        throw invalid(s"tracked source root uses a reserved sandbox path: $source")
    }

  private def isPortablePathSegment(segment: String): Boolean =
    segment.nonEmpty && segment != "." && segment != ".." &&
      !segment.exists(WindowsInvalidPathCharacters.contains) &&
      !segment.endsWith(".") && !segment.endsWith(" ") && !isWindowsReserved(segment)

  private def validateElmConfig(path: os.Path, morphirSource: os.RelPath): Unit =
    try {
      val config = ujson.read(os.read(path))
      config.obj.get("source-directories").foreach { value =>
        value.arr.foreach { sourceValue =>
          val elmSource       = safeSourceDirectory(sourceValue.str)
          val elmSegments     = elmSource.segments.toSeq
          val morphirSegments = morphirSource.segments.toSeq
          if (!(elmSegments.startsWith(morphirSegments) || morphirSegments.startsWith(elmSegments)))
            throw invalid(s"Elm source-directory is outside the staged Morphir source: ${sourceValue.str}")
        }
      }
    } catch {
      case error: IllegalArgumentException if error.getMessage.startsWith("Morphir Elm project sandbox:") =>
        throw error
      case NonFatal(error) => throw invalid(s"invalid elm.json at $path: ${error.getMessage}", error)
    }

  private def validateLimits(limits: ElmInputLimits): Unit = {
    if (limits.maxEntries <= 0) throw invalid(s"source entry count limit must be positive: ${limits.maxEntries}")
    if (limits.maxFileBytes.toBytes <= 0)
      throw invalid(s"source file bytes limit must be positive: ${limits.maxFileBytes.show}")
    if (limits.maxTotalBytes.toBytes <= 0)
      throw invalid(s"source total bytes limit must be positive: ${limits.maxTotalBytes.show}")
  }

  private def copySourceDirectory(source: JPath, destination: JPath, budget: Budget): Unit = {
    if (Files.isSymbolicLink(source) || !Files.isDirectory(source, LinkOption.NOFOLLOW_LINKS))
      throw invalid(s"Morphir source is not a non-symbolic-link directory: $source")

    Files.walkFileTree(
      source,
      new SimpleFileVisitor[JPath] {
        private def destinationFor(path: JPath): JPath = destination.resolve(source.relativize(path))

        override def preVisitDirectory(directory: JPath, attributes: BasicFileAttributes): FileVisitResult = {
          if (attributes.isSymbolicLink || Files.isSymbolicLink(directory))
            throw invalid(s"Morphir source contains a symbolic link: $directory")
          budget.addEntry(directory)
          val target = destinationFor(directory)
          budget.addDestination(target)
          Files.createDirectories(target)
          FileVisitResult.CONTINUE
        }

        override def visitFile(file: JPath, attributes: BasicFileAttributes): FileVisitResult = {
          if (attributes.isSymbolicLink || Files.isSymbolicLink(file) || !attributes.isRegularFile)
            throw invalid(s"Morphir source must contain only regular files: $file")
          budget.addEntry(file)
          val target = destinationFor(file)
          budget.addDestination(target)
          copyStableFile(file, target, None, Some(budget))
          FileVisitResult.CONTINUE
        }

        override def visitFileFailed(file: JPath, error: java.io.IOException): FileVisitResult =
          throw invalid(s"unable to read Morphir source $file: ${error.getMessage}", error)
      }
    )
  }

  private def copyStableFile(
      source: JPath,
      destination: JPath,
      expectedSha256: Option[String],
      budget: Option[Budget]
  ): Unit = {
    val before = Files.readAttributes(source, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
    if (before.isSymbolicLink || !before.isRegularFile || Files.isSymbolicLink(source))
      throw invalid(s"input must be a non-symbolic-link regular file: $source")
    budget.foreach(_.checkDeclaredSize(source, before.size()))
    Option(destination.getParent).foreach(Files.createDirectories(_))

    val inputOptions  = Set[OpenOption](StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS).asJava
    val outputOptions = Set[OpenOption](StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE).asJava
    val input         = Files.newByteChannel(source, inputOptions)
    val digest        = MessageDigest.getInstance("SHA-256")
    var fileBytes     = 0L
    try {
      val output = Files.newByteChannel(destination, outputOptions)
      try {
        val buffer = ByteBuffer.allocate(8192)
        var count  = input.read(buffer)
        while (count >= 0) {
          if (count > 0) {
            budget.foreach(_.addBytes(source, count, fileBytes))
            fileBytes += count
            buffer.flip()
            digest.update(buffer.asReadOnlyBuffer())
            while (buffer.hasRemaining) output.write(buffer)
            buffer.clear()
          }
          count = input.read(buffer)
        }
      } finally output.close()
    } finally input.close()

    val after       = Files.readAttributes(source, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
    val sameFileKey = before.fileKey() == null || after.fileKey() == null || before.fileKey() == after.fileKey()
    if (
      !after.isRegularFile || Files.isSymbolicLink(source) || fileBytes != before.size() ||
      after.size() != before.size() || after.lastModifiedTime() != before.lastModifiedTime() || !sameFileKey
    ) throw invalid(s"input changed while creating Morphir project snapshot: $source")

    val actual = digest.digest().map(byte => f"${byte & 0xff}%02x").mkString
    expectedSha256.foreach { expected =>
      if (actual != expected)
        throw invalid(s"dependency content identity changed for $source: expected $expected, got $actual")
    }
  }

  private def promote(staging: os.Path, destination: os.Path): Unit =
    try Files.move(staging.toNIO, destination.toNIO, StandardCopyOption.ATOMIC_MOVE)
    catch {
      case _: AtomicMoveNotSupportedException => Files.move(staging.toNIO, destination.toNIO)
    }

  private def removeOwned(path: os.Path): Unit =
    if (Files.isSymbolicLink(path.toNIO)) Files.deleteIfExists(path.toNIO)
    else if (Files.exists(path.toNIO, LinkOption.NOFOLLOW_LINKS)) os.remove.all(path)

  private def isWindowsReserved(value: String): Boolean = {
    val basename = value.takeWhile(_ != '.').toUpperCase(Locale.ROOT)
    WindowsReservedNames.contains(basename)
  }

  private def hasStagedCollision(project: os.Path, filename: String): Boolean =
    Files.isDirectory(project.toNIO, LinkOption.NOFOLLOW_LINKS) &&
      os.list(project).exists(path => path.last.equalsIgnoreCase(filename))

  private def layerMessage(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty) match {
      case Some(message) if message.startsWith("Morphir Elm project sandbox:") => message
      case Some(message) => s"Morphir Elm project sandbox: $message"
      case None          => s"Morphir Elm project sandbox: ${error.getClass.getSimpleName}"
    }

  private def invalid(detail: String, cause: Throwable = null): IllegalArgumentException =
    new IllegalArgumentException(s"Morphir Elm project sandbox: $detail", cause)
}
