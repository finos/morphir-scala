package org.finos.millmorphir.elm

import mill.*
import org.finos.millmorphir.toolchain.NodeToolchainModule

trait MorphirElmToolModule extends Module {
  def nodeToolchain: NodeToolchainModule

  def toolManifest: T[PathRef] = Task.Source(
    mill.api.BuildCtx.workspaceRoot / "mill-build" / "morphir-elm-tool" / "package.json"
  )

  def toolLock: T[PathRef] = Task.Source(
    mill.api.BuildCtx.workspaceRoot / "mill-build" / "morphir-elm-tool" / "package-lock.json"
  )

  def morphirElmInstall: T[PathRef] = Task {
    val install = Task.dest / "install"
    val cache   = Task.dest / "npm-cache"
    val environment = MorphirElmProcessEnvironment.create(Task.dest / "tool-state", Task.env)
    MorphirElmProcessEnvironment.initialize(environment)
    os.makeDir.all(install)
    os.copy.over(toolManifest().path, install / "package.json")
    os.copy.over(toolLock().path, install / "package-lock.json")

    os.proc(
      MorphirElmCommand.npmCi(
        nodeToolchain.nodeExecutable().path,
        nodeToolchain.npmCli().path,
        cache
      )
    ).call(cwd = install, env = environment, propagateEnv = false)

    PathRef(install)
  }

  def morphirElmCommand(args: Seq[String]): mill.Task[Seq[String]] = Task.Anon {
    MorphirElmCommand.cli(
      nodeToolchain.nodeExecutable().path,
      morphirElmInstall().path,
      args
    )
  }
}
