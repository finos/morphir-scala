//| mvnDeps: ["com.lihaoyi::mill-libs:$MILL_VERSION"]
//| moduleDeps: ["//mill-build/src/org/finos/millmorphir/api/MorphirProjectConfig.scala", "//mill-plugins/morphir/elm-tooling/src/org/finos/morphir/mill/elm/ElmProcessEnvironment.scala"]

package org.finos.millmorphir.elm

import java.nio.file.{Files, LinkOption}
import java.util.Locale

import mill.PathRef
import mill.api.JsonFormatters.*
import org.finos.millmorphir.api.MorphirProjectConfig
import scala.jdk.CollectionConverters.*
import upickle.default.*

/** An already-materialized Morphir IR input.
  *
  * Generic resolution of unpublished Elm source packages is deliberately deferred to bead `zdy.8`; the sandbox
  * capability remains insulated from resolver, cache, registry, and shelm implementation details.
  */
final case class MorphirDependencyArtifact(moduleId: String, ir: PathRef) derives ReadWriter

final case class StagedMorphirProject(projectDir: PathRef, output: os.Path) derives ReadWriter

final case class ClassicRuntimeFixtureSet(
    evaluator: PathRef,
    defaults: PathRef,
    unitTestFramework: PathRef,
    unitTestExample: PathRef,
    unitTestFailing: PathRef,
    unitTestPassing: PathRef,
    unitTestIncomplete: PathRef
)

object ClassicRuntimeFixtureSource {
  def escapeScalaString(value: String): String =
    value.flatMap {
      case '\\' => "\\\\"
      case '"'  => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c    => c.toString
    }

  private def sha256(path: os.Path): String = {
    val digest = java.security.MessageDigest.getInstance("SHA-256")
    val input  = Files.newInputStream(path.toNIO)
    val buffer = new Array[Byte](8192)
    try {
      var read = input.read(buffer)
      while (read >= 0) {
        if (read > 0) digest.update(buffer, 0, read)
        read = input.read(buffer)
      }
    } finally input.close()
    digest.digest().map(byte => f"${byte & 0xff}%02x").mkString
  }

  def render(fixtures: ClassicRuntimeFixtureSet): String = {
    def field(name: String, path: PathRef): String =
      s"  // $name SHA-256: ${sha256(path.path)}\n" +
        s"  val $name: java.nio.file.Path = java.nio.file.Paths.get(\"${escapeScalaString(path.path.toString)}\")"

    Seq(
      "package org.finos.morphir.runtime.fixtures",
      "",
      "object GeneratedRuntimeFixtures {",
      field("evaluator", fixtures.evaluator),
      field("defaults", fixtures.defaults),
      field("unitTestFramework", fixtures.unitTestFramework),
      field("unitTestExample", fixtures.unitTestExample),
      field("unitTestFailing", fixtures.unitTestFailing),
      field("unitTestPassing", fixtures.unitTestPassing),
      field("unitTestIncomplete", fixtures.unitTestIncomplete),
      "}",
      ""
    ).mkString("\n")
  }
}

object MorphirElmProjectSandbox {
  private val SafeModuleId = "[A-Za-z0-9](?:[A-Za-z0-9._-]*[A-Za-z0-9])?".r
  private val SafeFilename = "[A-Za-z0-9](?:[A-Za-z0-9._-]*[A-Za-z0-9])?".r
  private val WindowsReservedNames =
    Set("CON", "PRN", "AUX", "NUL") ++ (1 to 9).flatMap(number => Seq(s"COM$number", s"LPT$number"))

  def dependencyRelativePath(moduleId: String): Either[String, os.RelPath] =
    moduleId match {
      case SafeModuleId() if !isWindowsReserved(moduleId) =>
        Right(os.rel / ".morphir-deps" / moduleId / "morphir-ir.json")
      case _              => Left(s"Unsafe Morphir dependency module ID: $moduleId")
    }

  def rewrittenConfig(
      config: MorphirProjectConfig,
      dependencies: Seq[MorphirDependencyArtifact]
  ): Either[String, MorphirProjectConfig] = {
    val duplicates = dependencies
      .groupBy(_.moduleId.toLowerCase(Locale.ROOT))
      .collect { case (_, artifacts) if artifacts.size > 1 => artifacts.map(_.moduleId).sorted.mkString("/") }
    if (duplicates.nonEmpty)
      Left(s"Duplicate Morphir dependency module IDs: ${duplicates.toSeq.sorted.mkString(", ")}")
    else
      dependencies
        .foldLeft[Either[String, List[String]]](Right(Nil)) { (paths, dependency) =>
          for {
            accumulated <- paths
            relative    <- dependencyRelativePath(dependency.moduleId)
          } yield accumulated :+ relative.toString
        }
        .map(config.withLocalDependencies)
  }

  def stage(
      root: os.Path,
      config: os.Path,
      elm: Option[os.Path],
      source: os.Path,
      dependencies: Seq[MorphirDependencyArtifact]
  ): Either[String, StagedMorphirProject] = {
    val result = for {
      original       <- readConfig(config)
      sourceRelative <- safeSourceDirectory(original.sourceDirectory)
      rewritten      <- rewrittenConfig(original, dependencies)
      _               <- validateInput(config, "Morphir project config")
      _               <- elm.map(validateInput(_, "Elm project config")).getOrElse(Right(()))
      _               <- elm.map(validateElmConfig(_, sourceRelative)).getOrElse(Right(()))
      _               <- validateDirectory(source, "Morphir project source")
      _               <- dependencies.foldLeft[Either[String, Unit]](Right(())) { (validated, dependency) =>
        validated.flatMap(_ => validateInput(dependency.ir.path, s"Morphir dependency ${dependency.moduleId}"))
      }
    } yield (sourceRelative, rewritten)

    result.map { case (sourceRelative, rewritten) =>
      os.makeDir.all(root)
      os.copy(source, root / sourceRelative, createFolders = true)
      os.copy(config, root / "morphir.json", replaceExisting = true)
      elm.foreach(path => os.copy(path, root / "elm.json", replaceExisting = true))
      dependencies.foreach { dependency =>
        val relative = dependencyRelativePath(dependency.moduleId).fold(message => throw new IllegalStateException(message), identity)
        os.copy(dependency.ir.path, root / relative, createFolders = true, replaceExisting = true)
      }
      os.write.over(root / "morphir.json", write(rewritten, indent = 2))
      StagedMorphirProject(PathRef(root), root / "morphir-ir.json")
    }
  }

  def withOutputFilename(
      project: StagedMorphirProject,
      filename: String
  ): Either[String, StagedMorphirProject] =
    filename match {
      case SafeFilename() if !isWindowsReserved(filename) => Right(project.copy(output = project.projectDir.path / filename))
      case _                                               => Left(s"Unsafe Morphir IR output filename: $filename")
    }

  private def readConfig(path: os.Path): Either[String, MorphirProjectConfig] =
    validateInput(path, "Morphir project config").flatMap { _ =>
      try Right(read[MorphirProjectConfig](os.read(path)))
      catch { case exception: Exception => Left(s"Invalid Morphir project config at $path: ${exception.getMessage}") }
    }

  private def safeSourceDirectory(value: String): Either[String, os.RelPath] =
    try {
      val relative = os.RelPath(value)
      if (
        value.isEmpty || value.contains('\\') || value.matches("^[A-Za-z]:.*") ||
        relative.ups > 0 || relative.segments.isEmpty ||
        relative.segments.exists(segment => segment == ".." || segment == ".")
      )
        Left(s"Unsafe Morphir source directory: $value")
      else Right(relative)
    } catch {
      case _: IllegalArgumentException => Left(s"Unsafe Morphir source directory: $value")
    }

  private def validateInput(path: os.Path, description: String): Either[String, Unit] =
    if (Files.isSymbolicLink(path.toNIO)) Left(s"$description must not be a symbolic link: $path")
    else if (Files.isRegularFile(path.toNIO, LinkOption.NOFOLLOW_LINKS)) Right(())
    else Left(s"$description is not a file: $path")

  private def validateDirectory(path: os.Path, description: String): Either[String, Unit] = {
    if (Files.isSymbolicLink(path.toNIO)) Left(s"$description must not be a symbolic link: $path")
    else if (!Files.isDirectory(path.toNIO, LinkOption.NOFOLLOW_LINKS)) Left(s"$description is not a directory: $path")
    else {
      val stream = Files.walk(path.toNIO)
      try
        stream.iterator().asScala.find(Files.isSymbolicLink) match {
          case Some(link) => Left(s"$description contains symbolic link: $link")
          case None       => Right(())
        }
      finally stream.close()
    }
  }

  private def validateElmConfig(path: os.Path, morphirSource: os.RelPath): Either[String, Unit] =
    try {
      val config = ujson.read(os.read(path))
      config.obj.get("source-directories") match {
        case None => Right(())
        case Some(value) =>
          value.arr.foldLeft[Either[String, Unit]](Right(())) { (validated, sourceValue) =>
            validated.flatMap { _ =>
              safeSourceDirectory(sourceValue.str).flatMap { elmSource =>
                val elmSegments     = elmSource.segments.toSeq
                val morphirSegments = morphirSource.segments.toSeq
                val intersectsStagedSource =
                  elmSegments.startsWith(morphirSegments) || morphirSegments.startsWith(elmSegments)
                if (intersectsStagedSource) Right(())
                else Left(s"Elm source-directory is outside the staged Morphir source: ${sourceValue.str}")
              }
            }
          }
      }
    } catch {
      case exception: Exception => Left(s"Invalid Elm project config at $path: ${exception.getMessage}")
    }

  private def isWindowsReserved(value: String): Boolean = {
    val basename = value.takeWhile(_ != '.').toUpperCase(Locale.ROOT)
    WindowsReservedNames.contains(basename)
  }
}

private[millmorphir] object MorphirElmProcessEnvironment {
  def create(taskRoot: os.Path, ambient: Map[String, String]): Map[String, String] =
    org.finos.morphir.mill.elm.ElmProcessEnvironment.create(taskRoot, ambient)

  def initialize(environment: Map[String, String]): Unit =
    org.finos.morphir.mill.elm.ElmProcessEnvironment.initialize(environment)
}
