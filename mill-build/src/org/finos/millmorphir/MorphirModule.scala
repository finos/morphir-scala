package org.finos.millmorphir

import mill.*
import mill.api.JsonFormatters.*
import millbuild.util.{Jvm => MillbuildJvm}
import org.finos.millmorphir.api.*
import org.finos.millmorphir.elm.*
import upickle.default.*

trait MorphirModule extends Module {
  def morphirElmTool: MorphirElmToolModule

  protected def additionalSandboxInputs: T[Seq[PathRef]] = Task(Seq.empty)

  protected def prepareSandboxExtension(project: StagedMorphirProject, inputs: Seq[PathRef]): Unit = ()

  def indentJson: T[Boolean] = Task(false)

  def typesOnly: T[Boolean] = Task(false)

  def morphirIrFilename: T[String] = Task("morphir-ir.json")

  def moduleId: T[String] = Task {
    val resolvedModule    = os.Path(moduleDir.toNIO.toRealPath())
    val resolvedWorkspace = os.Path(mill.api.BuildCtx.workspaceRoot.toNIO.toRealPath())
    resolvedModule.relativeTo(resolvedWorkspace).segments.toSeq.mkString(".")
  }

  def morphirProjectConfigFile: T[PathRef] = Task.Source(moduleDir / "morphir.json")

  def elmProjectConfigFiles: T[Seq[PathRef]] = Task.Sources(moduleDir / "elm.json")

  def morphirProjectSource: T[PathRef] = Task.Source(moduleDir / "src")

  def morphirProjectConfig: T[MorphirProjectConfig] = Task {
    read[MorphirProjectConfig](os.read(morphirProjectConfigFile().path))
  }

  /** Direct Morphir project dependencies whose generated IR is staged into this project's private sandbox. */
  def morphirModuleDeps: Seq[MorphirModule] = Seq.empty

  final def morphirModuleDepsChecked: Seq[MorphirModule] = {
    recursiveMorphirModuleDeps
    morphirModuleDeps
  }

  private lazy val recursiveMorphirModuleDeps: Seq[MorphirModule] = {
    def collect(dependencies: Seq[MorphirModule], seen: Set[MorphirModule]): Seq[MorphirModule] =
      dependencies.flatMap { dependency =>
        if (seen.contains(dependency))
          throw new IllegalArgumentException(s"Cyclic Morphir module dependency involving ${dependency.moduleDir}")
        else dependency +: collect(dependency.morphirModuleDeps, seen + dependency)
      }

    collect(morphirModuleDeps, Set(this))
  }

  def dependencyArtifacts: Task[Seq[MorphirDependencyArtifact]] = Task.Anon {
    Task.traverse(morphirModuleDepsChecked) { dependency =>
      Task.Anon {
        MorphirDependencyArtifact(dependency.moduleId(), dependency.make().irFilePath)
      }
    }()
  }

  def preparedProject: Task[StagedMorphirProject] = Task.Anon {
    val config         = morphirProjectConfig()
    val sourceRelative = os.RelPath(config.sourceDirectory)
    if (sourceRelative.ups > 0 || sourceRelative.segments.headOption != Some("src"))
      throw new IllegalArgumentException(
        s"Morphir sourceDirectory must be src or a child of src, got ${config.sourceDirectory}"
      )
    val source = sourceRelative.segments.drop(1).foldLeft(morphirProjectSource().path)(_ / _)
    val staged = MorphirElmProjectSandbox
      .stage(
        Task.dest / "project",
        morphirProjectConfigFile().path,
        elmProjectConfigFiles().headOption.filter(path => os.isFile(path.path)).map(_.path),
        source,
        dependencyArtifacts()
      )
      .fold(message => throw new IllegalArgumentException(message), identity)
    val stagedWithOutput = MorphirElmProjectSandbox
      .withOutputFilename(staged, morphirIrFilename())
      .fold(message => throw new IllegalArgumentException(message), identity)
    val extensionInputs = additionalSandboxInputs()
    prepareSandboxExtension(stagedWithOutput, extensionInputs)
    stagedWithOutput
  }

  def makeArgs: Task[MakeArgs] = Task.Anon {
    val project = preparedProject()
    MakeArgs(
      projectDir = project.projectDir.path,
      output = project.output,
      indentJson = indentJson(),
      typesOnly = typesOnly(),
      fallbackCli = None
    )
  }

  def make: T[MakeResult] = Task {
    val arguments = makeArgs()
    val command = MorphirElmCommand.cli(
      morphirElmTool.nodeToolchain.nodeExecutable().path,
      morphirElmTool.morphirElmInstall().path,
      arguments.toCommandArgs
    )
    val workingDir = arguments.projectDir
    val environment = MorphirElmProcessEnvironment.create(Task.dest / "tool-state", Task.env)
    MorphirElmProcessEnvironment.initialize(environment)
    MillbuildJvm.runSubprocess(command, environment, workingDir, propagateEnv = false)
    if (!os.isFile(arguments.output))
      throw new IllegalStateException(s"Morphir Elm did not produce ${arguments.output}")
    val hashesPath = workingDir / "morphir-hashes.json"
    MakeResult(
      arguments,
      PathRef(arguments.output),
      command,
      workingDir,
      Option.when(os.isFile(hashesPath))(PathRef(hashesPath))
    )
  }

  def dist: T[MakeOutputs] = Task {
    val result = make()
    val artifacts = Set(ArtifactRef.morphirIR(result.irFilePath, "morphir", "ir")) ++
      result.morphirHashesPath.map(ArtifactRef.morphirHashes(_, "morphir", "hashes", "incremental"))
    MakeOutputs(moduleId(), artifacts)
  }

  final def distOutputDirs: T[Seq[PathRef]] = Task {
    dist().artifacts.toSeq.map(artifact => PathRef(artifact.path / os.up)).distinct
  }
}
