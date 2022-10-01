package org.finos.morphir.cli
import zio._
import zio.Console.printLine
import zio.process.Command
import cmdlet._
object CliCommandRouter {

  def handleCliCommand(cmd: CliCommand): ZIO[ZIOAppArgs, Exception, Any] =
    (cmd match {
      case cmd: CliCommand.Elm.Develop => ElmDevelopCmdlet.run(cmd)
      case cmd: CliCommand.Elm.Make    => ElmMakeCmdlet.run(cmd)
      case cmd: CliCommand.Elm.Gen     => ElmGenCmdlet.run(cmd)
      case cmd: CliCommand.Elm         => ElmCmdlet.run(cmd)
      case cmd: CliCommand.Init        => InitCmdlet.run(cmd)
      case cmd: CliCommand.Setup       => SetupCmdlet.run(cmd)
      case cmd: CliCommand.Workspace   => WorkspaceCmdlet.run(cmd)
      case cmd: CliCommand.About.type  => AboutCmdlet.run(cmd)
    })
}
