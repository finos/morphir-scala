package org.finos.morphir.mill.elm.morphir

import scala.util.control.NonFatal

import mill.*
import mill.api.BuildCtx
import org.finos.morphir.mill.*
import org.finos.morphir.mill.elm.{ElmProcessEnvironment, ElmToolModule}
import org.finos.morphir.mill.javascript.JavaScriptCommand

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

  /**
   * Shared elm-format binary (repo builds point at `toolchains.elmFormat`; unit tests stub a local
   * [[ElmToolModule]]).
   */
  def elmFormatTool: ElmToolModule

  protected def additionalSandboxInputs: T[Seq[PathRef]] = Task(Seq.empty)

  protected def prepareSandboxExtension(project: StagedMorphirProject, inputs: Seq[PathRef]): Unit = ()

  def indentJson: T[Boolean] = Task(false)

  def typesOnly: T[Boolean] = Task(false)

  def fallbackCli: T[Option[Boolean]] = Task(None)

  def morphirIrFilename: T[String] = Task("morphir-ir.json")

  def preparedProject: Task[StagedMorphirProject] = Task.Anon {
    val projectRoot = Task.dest / "project"
    val tracked     = trackedMorphirProjectInputs()
    val staged      = MorphirElmProjectSandbox
      .stage(
        projectRoot,
        tracked,
        dependencyArtifacts(),
        morphirInputLimits
      )
      .fold(message => throw new IllegalArgumentException(message), identity)
    try {
      val stagedWithOutput = MorphirElmProjectSandbox
        .withOutputFilename(staged, morphirIrFilename())
        .fold(message => throw new IllegalArgumentException(message), identity)
      prepareSandboxExtension(stagedWithOutput, additionalSandboxInputs())
      MorphirElmProjectSandbox
        .validateOutputAvailable(stagedWithOutput)
        .fold(message => throw new IllegalArgumentException(message), identity)
    } catch {
      case NonFatal(error) =>
        MorphirElmProjectSandbox.discardOwnedProject(projectRoot)
        throw error
    }
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

  /** Format this project's tracked Elm source root with `elm-format --yes`. */
  def format() = Task.Command {
    val files = MorphirElmModule.elmFilesUnder(morphirProjectSource().path)
    if (files.nonEmpty) {
      val command = elmFormatTool.elmFormatCommand(Seq.empty)()
      MorphirElmModule.invokeElmFormat(command, files, checkMode = false)
    }
  }

  /** Check this project's tracked Elm source root with `elm-format --validate`. */
  def formatCheck() = Task.Command {
    val files = MorphirElmModule.elmFilesUnder(morphirProjectSource().path)
    if (files.nonEmpty) {
      val command = elmFormatTool.elmFormatCommand(Seq.empty)()
      MorphirElmModule.invokeElmFormat(command, files, checkMode = true)
    }
  }

}

object MorphirElmModule {
  private[morphir] def elmFilesUnder(sourceRoot: os.Path): Seq[os.Path] =
    if (!os.isDir(sourceRoot)) Seq.empty
    else
      os.walk(
        sourceRoot,
        skip = p => p.last == "elm-stuff" || p.last == "node_modules" || p.last == "out"
      ).filter(p => os.isFile(p) && p.ext == "elm").sorted

  private[morphir] def invokeElmFormat(
      command: JavaScriptCommand,
      files: Seq[os.Path],
      checkMode: Boolean
  ): Unit = {
    val mode     = if checkMode then Seq("--validate") else Seq("--yes")
    val fileArgs = files.map(p => p.toNIO.toFile.getCanonicalPath)
    val args     = mode ++ Seq("--elm-version=0.19") ++ fileArgs
    val exeStr   = absoluteExecutable(command.executable.path)
    val argv     = exeStr +: (command.arguments ++ args)
    val result   =
      os.proc(argv)
        .call(
          cwd = os.Path(BuildCtx.workspaceRoot.toNIO.toFile.getCanonicalPath),
          stdout = os.Pipe,
          stderr = os.Pipe,
          check = false
        )
    if result.exitCode != 0 then
      val err    = result.err.text().trim
      val out    = result.out.text().trim
      val detail =
        if err.nonEmpty then err
        else if out.nonEmpty then out
        else s"elm-format exited ${result.exitCode}"
      throw new Exception(s"MorphirElmModule elm-format failed: $detail")
  }

  /**
   * Mill PathAliasing turns workspace paths into `../mill-workspace/...` strings. Build a real absolute path via
   * `java.io.File` so `os.proc` can exec elm-format.
   */
  private def absoluteExecutable(path: os.Path): String = {
    val asString       = path.toString
    val marker         = "mill-workspace/"
    val idx            = asString.indexOf(marker)
    val workspaceCanon = BuildCtx.workspaceRoot.toNIO.toFile.getCanonicalPath
    if idx >= 0 then
      val relative = asString.substring(idx + marker.length)
      new java.io.File(workspaceCanon, relative).getCanonicalPath
    else path.toNIO.toFile.getCanonicalPath
  }
}
