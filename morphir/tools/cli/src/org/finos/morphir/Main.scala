package org.finos
package morphir

import java.nio.file.Paths
import morphir.cli._
import zio.Console.printLine
import zio.cli.HelpDoc.Span.text
import zio.cli._
import zio.Scope
import zio.ZIOAppArgs

object Main extends CliDefault:
  override def cliApp = CliApp.make(
    name = MorphirCliBuildInfo.product,
    version = MorphirCliBuildInfo.version,
    summary = text(MorphirCliBuildInfo.description),
    command = CliSetup.morphir
  ) {
    case CommandData.Elm.Develop(port)       => printLine(s"Running elm develop on port $port")
    case CommandData.Elm(args)               => printLine("Elm")
    case cmd @ CommandData.Elm.Make(_, _, _) => printLine(s"Elm Make: $cmd")
    case CommandData.Elm.Gen()               => printLine("Elm Gen")
    case CommandData.Init()                  => printLine("Initializing...")
    case CommandData.Setup                   => printLine("Setting up...")
    case CommandData.Workspace()             => printLine("Workspace selected")
  }
end Main
