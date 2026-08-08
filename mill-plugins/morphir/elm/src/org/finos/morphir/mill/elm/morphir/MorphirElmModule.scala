package org.finos.morphir.mill.elm.morphir

import scala.util.control.NonFatal

import mill.*
import org.finos.morphir.mill.*
import org.finos.morphir.mill.elm.ElmProcessEnvironment

final case class MorphirElmMakeArgs(
    projectDir: os.Path,
    output: os.Path,
    indentJson: Boolean,
    typesOnly: Boolean,
    fallbackCli: Option[Boolean]
) {
  def toCommandArgs: Seq[String] =
    Seq("make", "--project-dir", projectDir.toString, "--output", output.toString) ++
      Option.when(indentJson)("--indent-json") ++
      Option.when(typesOnly)("--types-only") ++
      Option.when(fallbackCli.contains(true))("--fallback-cli")
}

trait MorphirElmModule extends MorphirModule with MorphirElmProjectInputsModule {
  def morphirElmTool: MorphirElmToolModule

  protected def additionalSandboxInputs: T[Seq[PathRef]] = Task(Seq.empty)

  protected def prepareSandboxExtension(project: StagedMorphirProject, inputs: Seq[PathRef]): Unit = ()

  def indentJson: T[Boolean] = Task(false)

  def typesOnly: T[Boolean] = Task(false)

  def fallbackCli: T[Option[Boolean]] = Task(None)

  def morphirIrFilename: T[String] = Task("morphir-ir.json")

  def preparedProject: Task[StagedMorphirProject] = Task.Anon {
    val tracked = trackedMorphirProjectInputs()
    val staged  = MorphirElmProjectSandbox
      .stage(
        Task.dest / "project",
        tracked,
        dependencyArtifacts(),
        morphirInputLimits
      )
      .fold(message => throw new IllegalArgumentException(message), identity)
    val stagedWithOutput = MorphirElmProjectSandbox
      .withOutputFilename(staged, morphirIrFilename())
      .fold(message => throw new IllegalArgumentException(message), identity)
    prepareSandboxExtension(stagedWithOutput, additionalSandboxInputs())
    stagedWithOutput
  }

  def makeArgs: Task[MorphirElmMakeArgs] = Task.Anon {
    val project = preparedProject()
    MorphirElmMakeArgs(
      projectDir = project.projectDir.path,
      output = project.output,
      indentJson = indentJson(),
      typesOnly = typesOnly(),
      fallbackCli = fallbackCli()
    )
  }

  override def morphirIR: T[MorphirIrArtifact] = Task {
    val arguments   = makeArgs()
    val executable  = morphirElmTool.morphirElmExecutable()
    val commandArgs = executable.executable.path.toString +: (executable.arguments ++ arguments.toCommandArgs)
    val environment = ElmProcessEnvironment.create(Task.dest / "tool-state", Task.env)
    ElmProcessEnvironment.initialize(environment)
    try
      os.proc(commandArgs)
        .call(cwd = arguments.projectDir, env = environment, propagateEnv = false)
    catch {
      case NonFatal(error) =>
        throw new IllegalStateException(
          s"Morphir Elm generation failed in ${arguments.projectDir} with ${executable.executable.path}: " +
            Option(error.getMessage).getOrElse(error.getClass.getSimpleName),
          error
        )
    }
    if (!os.isFile(arguments.output, followLinks = false))
      throw new IllegalStateException(s"Morphir Elm did not produce a regular output file at ${arguments.output}")
    MorphirIrArtifact.fromFile(moduleId(), PathRef(arguments.output))
  }

  /** Compatibility alias. New consumers should depend on [[morphirIR]]. */
  def make: T[MorphirIrArtifact] = Task {
    morphirIR()
  }

}
