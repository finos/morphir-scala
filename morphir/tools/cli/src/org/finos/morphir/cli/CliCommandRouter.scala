package org.finos.morphir.cli
import zio._
import zio.Console.printLine
object CliCommandRouter {

  def handleCliCommand(cmd: CliCommand): ZIO[Any, Exception, Any] =
    (cmd match {
      case CliCommand.Elm.Develop(port) => printLine(s"Running elm develop on port $port")
      case cmd: CliCommand.Elm.Make     => ElmMakeCmdlet.run(cmd)
      case CliCommand.Elm.Gen()         => printLine("Elm Gen")
      case CliCommand.Elm(args)         => printLine("Elm")
      case CliCommand.Init()            => printLine("Initializing...")
      case CliCommand.Setup             => printLine("Setting up...")
      case CliCommand.Workspace()       => printLine("Workspace selected")
      case _                            => printLine("Unknown command")
    })
}

object ElmMakeCmdlet {
  def run(cmd: CliCommand.Elm.Make): ZIO[Any, Exception, Any] =
    printLine(s"Elm Make: $cmd")
}
