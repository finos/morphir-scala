//| mvnDeps: ["com.lihaoyi::mill-libs:$MILL_VERSION"]
//| moduleDeps: ["//mill-build/src/org/finos/millmorphir/api/MorphirProjectConfig.scala"]

package org.finos.millmorphir.elm

import mill.PathRef
import mill.api.JsonFormatters.*
import org.finos.millmorphir.api.MorphirProjectConfig
import upickle.default.*

/** An already-materialized Morphir IR input.
  *
  * Generic resolution of unpublished Elm source packages is deliberately deferred to bead `zdy.8`; the sandbox
  * capability remains insulated from resolver, cache, registry, and shelm implementation details.
  */
final case class MorphirDependencyArtifact(moduleId: String, ir: PathRef) derives ReadWriter

final case class StagedMorphirProject(projectDir: PathRef, output: os.Path) derives ReadWriter

object MorphirElmProjectSandbox {
  private val SafeModuleId = "[A-Za-z0-9](?:[A-Za-z0-9._-]*[A-Za-z0-9])?".r

  def dependencyRelativePath(moduleId: String): Either[String, os.RelPath] =
    moduleId match {
      case SafeModuleId() => Right(os.rel / ".morphir-deps" / moduleId / "morphir-ir.json")
      case _              => Left(s"Unsafe Morphir dependency module ID: $moduleId")
    }

  def rewrittenConfig(
      config: MorphirProjectConfig,
      dependencies: Seq[MorphirDependencyArtifact]
  ): Either[String, MorphirProjectConfig] = {
    val duplicates = dependencies.groupBy(_.moduleId).collect { case (moduleId, artifacts) if artifacts.size > 1 => moduleId }
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

  private def readConfig(path: os.Path): Either[String, MorphirProjectConfig] =
    validateInput(path, "Morphir project config").flatMap { _ =>
      try Right(read[MorphirProjectConfig](os.read(path)))
      catch { case exception: Exception => Left(s"Invalid Morphir project config at $path: ${exception.getMessage}") }
    }

  private def safeSourceDirectory(value: String): Either[String, os.RelPath] =
    try {
      val relative = os.RelPath(value)
      if (
        value.isEmpty || relative.ups > 0 || relative.segments.isEmpty ||
        relative.segments.exists(segment => segment == ".." || segment == ".")
      )
        Left(s"Unsafe Morphir source directory: $value")
      else Right(relative)
    } catch {
      case _: IllegalArgumentException => Left(s"Unsafe Morphir source directory: $value")
    }

  private def validateInput(path: os.Path, description: String): Either[String, Unit] =
    if (os.isFile(path)) Right(()) else Left(s"$description is not a file: $path")

  private def validateDirectory(path: os.Path, description: String): Either[String, Unit] =
    if (os.isDir(path)) Right(()) else Left(s"$description is not a directory: $path")
}
