package org.finos.millmorphir

import millbuild.util.Collections._
import millbuild.util.ProcessHelper
import millbuild.util.{Jvm => MillbuildJvm}
import millbuild.jsruntime.JsRuntime
import org.finos.millmorphir.api._
import mill.*
import mill.api.JsonFormatters._
import mill.scalalib.*
import upickle.default.*

trait MorphirModule extends Module { self =>

  def clean() = Task.Command {
    var pendingDelete = List(morphirProjectDirResolved().path / "morphir-hashes.json")
    pendingDelete.foreach { path =>
      if (os.exists(path)) {
        os.remove.all(path)
      }
    }
  }

  def dist: T[Set[ArtifactRef]] = Task {
    val incrementalBuildFiles = incrementalMakeSourceFiles()
    val outputs               = make()

    val distPath = distFolder().path
    if (!os.exists(distPath)) {
      os.makeDir.all(distPath)
    }

    outputs.artifacts.map { artifact =>
      val path      = artifact.path
      val targetDir = distPath
      if (!os.exists(targetDir)) {
        os.makeDir.all(targetDir)
      }

      val targetPath = targetDir / path.last

      Task.log.debug(s"Copying ${path} to $targetPath")
      try {
        if (os.exists(path) && path != targetPath) {
          os.copy.over(path, targetPath)
        }
        Option(artifact.withPath(targetPath))
      } catch {
        case e: Exception =>
          Task.log.error(s"Failed to copy $path to $targetPath")
          Task.log.error(e.toString)
          None
      }

    }.collect { case Some(artifact) => artifact }
  }

  final def distOutputDirs: T[Seq[PathRef]] = Task {
    dist().map { case artifactRef: ArtifactRef =>
      val path = artifactRef.path / os.up
      PathRef(path)
    }.toSeq
  }

  def distFolder: T[PathRef] = Task {
    // PathRef(morphirProjectDirResolved().path / "dist")
    morphirProjectDirResolved()
  }

  def incrementalMakeSources: T[Seq[PathRef]] = Task {
    Seq(PathRef(moduleDir))
  }

  def incrementalMakeSourceFiles: T[Seq[PathRef]] = Task {
    val sourceFileNames = Set("morphir-hashes.json", "morphir-ir.json")
    for {
      source         <- incrementalMakeSources()
      sourceFileName <- sourceFileNames
      sourceFile = source.path / sourceFileName
      if os.exists(sourceFile)
    } yield PathRef(sourceFile)
  }

  /// Use indentation in the generated JSON file.
  def indentJson: T[Boolean] = Task(false)

  def morphirCommand: T[String]     = Task("morphir")
  def morphirProjectDir: T[PathRef] = Task(PathRef(moduleDir))
  final def morphirProjectDirResolved: T[PathRef] = Task {
    PathRef(makeArgs().projectDir)
  }
  def morphirHashesPath: T[PathRef] = Task {
    PathRef(morphirProjectDir().path / "morphir-hashes.json")
  }

  def morphirHashesContent: T[Option[ujson.Value]] = Task {
    if (os.exists(morphirProjectDirResolved().path)) {
      Option(ujson.read(os.read(morphirHashesPath().path)))
    } else {
      None
    }
  }

  def morphirProjectSources: T[Seq[PathRef]] = Task {
    Seq(PathRef(moduleDir))
  }

  def morphirProjectSourceFileNames: T[Set[String]] = Task {
    Set("morphir.json")
  }

  def morphirProjectSourceFiles: T[Seq[PathRef]] = Task {
    for {
      source         <- morphirProjectSources()
      sourceFileName <- morphirProjectSourceFileNames()
      sourceFile = source.path / sourceFileName
      if os.exists(sourceFile)
    } yield PathRef(sourceFile)
  }

  def sources: T[Seq[PathRef]] = Task.Sources(moduleDir / "src")

  def allSourceFiles: T[Seq[PathRef]] = Task {
    sources().map(_.path).flatMap(os.walk(_).filter(_.toIO.isFile)).map(PathRef(_))
  }

  def morphirIncrementalBuildSourceFiles: T[Seq[PathRef]] = Task {
    Seq(PathRef(morphirProjectDir().path / "morphir-hashes.json"))
  }

  def morphirProjectConfig: T[MorphirProjectConfig] = Task {
    val morphirProjectFile = morphirProjectDir().path / "morphir.json"
    if (os.exists(morphirProjectFile)) {
      read[MorphirProjectConfig](os.read(morphirProjectFile))
    } else {
      throw new Exception(s"morphir.json file not found, looked for it at ${morphirProjectFile}.")
    }
  }

  def makeCommandRunner: T[String] = Task {
    ProcessHelper.whereIs(morphirCommand())
  }

  def morphirIrFilename = Task("morphir-ir.json")

  def moduleId = Task {
    moduleDir.segments.toSeq.mkString(".")
  }

  def make: T[MakeOutputs] = Task {
    val makeResult = morphirMake()
    val artifacts: Set[ArtifactRef] =
      Set(ArtifactRef.morphirIR(makeResult.irFilePath, "morphir", "ir")) ++ makeResult.morphirHashesPath.map { path =>
        Seq(ArtifactRef.morphirHashes(path, "morphir", "hashes", "incremental"))
      }.getOrElse(Seq.empty).toSet

    MakeOutputs(moduleId(), artifacts)
  }

  def makeArgs: Task[MakeArgs] = Task.Anon {
    MakeArgs(
      projectDir = morphirProjectDir().path,
      output = Task.dest / morphirIrFilename(),
      indentJson = indentJson(),
      typesOnly = typesOnly(),
      fallbackCli = None
    )
  }

  /**
   * The direct dependencies of this module. This is meant to be overridden to add dependencies. To read the value, you
   * should use [[morphirModuleDepsChecked]] instead, which uses a cached result which is also checked to be free of
   * cycles.
   * @see
   *   [[morphirModuleDepsChecked]]
   */
  def morphirModuleDeps: Seq[MorphirModule] = Seq.empty

  /**
   * Same as [[morphirModuleDeps]], but checked to not contain cycles. Prefer this over using [[moduleDeps]] directly.
   */
  final def morphirModuleDepsChecked: Seq[MorphirModule] = {
    recMorphirModuleDeps
    morphirModuleDeps
  }

  /** Should only be called from [[moduleDepsChecked]] */
  private lazy val recMorphirModuleDeps: Seq[MorphirModule] = {
    def go(deps: Seq[MorphirModule], seen: Set[MorphirModule]): Seq[MorphirModule] = {
      deps.flatMap { m =>
        if (seen.contains(m)) Seq.empty
        else m +: go(m.morphirModuleDeps, seen + m)
      }
    }
    go(morphirModuleDeps, Set(this))
  }

  /** The direct and indirect dependencies of this module */
  def recursiveMorphirModuleDeps: Seq[MorphirModule] =
    recMorphirModuleDeps

  /**
   * Like `recursiveMorphirModuleDeps`, but includes this module itself as well.
   */
  def transitiveMorphirModuleDeps: Seq[MorphirModule] = Seq(this) ++ recursiveMorphirModuleDeps

  def upstreamMakeOutput: Task[Seq[Set[ArtifactRef]]] = Task.Anon {
    Task.traverse(recursiveMorphirModuleDeps.distinct)(_.dist)()
  }

  def morphirMake: T[MakeResult] = Task {
    val makeArgs: MakeArgs = self.makeArgs()
    val cli                = makeCommandRunner()

    val _           = upstreamMakeOutput()
    val _           = allSourceFiles()
    val commandArgs = makeArgs.toCommandArgs(cli)
    val workingDir  = makeArgs.projectDir
    val destPath    = makeArgs.output
    MillbuildJvm.runSubprocess(commandArgs, Task.env, workingDir)
    val hashesPath = morphirHashesPath()
    val hashesPathFinal =
      if (os.exists(hashesPath.path)) Option(hashesPath) else None
    MakeResult(makeArgs, PathRef(destPath), commandArgs, workingDir, morphirHashesPath = hashesPathFinal)
  }

  /// Only include type information in the IR, no values.
  def typesOnly: T[Boolean] = Task(false)

}
