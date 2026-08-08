package org.finos.morphir.mill.elm

import mill.*

trait ElmModule extends Module {
  def elmTool: ElmToolModule

  def elmJson: T[PathRef] = Task.Source(moduleDir / "elm.json")

  def elmSources: T[Seq[PathRef]] = Task.Sources(moduleDir / "src")

  def elmEntryPoint: os.RelPath = os.rel / "src" / "Main.elm"

  def compile: T[PathRef] = Task {
    val project = Task.dest / "project"
    os.makeDir.all(project)
    os.copy.over(elmJson().path, project / "elm.json")
    elmSources().foreach { source =>
      os.copy.over(source.path, project / source.path.last, createFolders = true)
    }
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
