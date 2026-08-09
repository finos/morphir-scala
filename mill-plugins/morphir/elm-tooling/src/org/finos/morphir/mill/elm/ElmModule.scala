package org.finos.morphir.mill.elm

import mill.*

trait ElmModule extends Module {
  def elmTool: ElmToolModule

  def elmJsonPath: os.Path = moduleDir / "elm.json"

  def elmSourcePaths: Seq[os.Path] = Seq(moduleDir / "src")

  private[elm] final def trackedElmInputs: T[Seq[ElmProjectSnapshot.TrackedElmInput]] = Task.Input {
    ElmProjectSnapshot.trackInputs(elmJsonPath, elmSourcePaths, elmInputLimits)
  }

  final def elmJson: T[PathRef] = Task {
    trackedElmInputs().find(_.role == ElmProjectSnapshot.InputRole.ElmJson).map(_.pathRef).getOrElse {
      throw new IllegalStateException("Elm tracked inputs do not contain elm.json")
    }
  }

  final def elmSources: T[Seq[PathRef]] = Task {
    trackedElmInputs().collect { case input if input.role == ElmProjectSnapshot.InputRole.Source => input.pathRef }
  }

  def elmEntryPoint: os.RelPath = os.rel / "src" / "Main.elm"

  def elmInputLimits: ElmInputLimits = ElmInputLimits()

  def compile: T[PathRef] = Task {
    val project = ElmProjectSnapshot.stage(
      Task.dest,
      trackedElmInputs(),
      elmEntryPoint,
      elmInputLimits,
      beforeRevalidate = () => ()
    )
    val output      = project / "main.js"
    val command     = elmTool.elmCommand(Seq("make", elmEntryPoint.toString, "--output", "main.js"))()
    val environment = ElmProcessEnvironment.create(Task.dest / "tool-state", Task.env)
    ElmProcessEnvironment.initialize(environment)
    val _ = os.proc(command.executable.path.toString +: command.arguments)
      .call(cwd = project, env = environment, propagateEnv = false)
    if (!os.isFile(output))
      throw new IllegalStateException(s"Elm compiler did not produce $output")
    PathRef(output)
  }
}
