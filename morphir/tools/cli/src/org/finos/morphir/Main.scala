package org.finos
package morphir

import java.nio.file.Paths
import morphir.cli.{CliSetup, CliCommand, MorphirCliBuildInfo}
import zio.Console.printLine
import zio.cli.HelpDoc.Span.text
import zio.cli._
import zio.Scope
import zio.ZIOAppArgs

object Main extends ZIOCliDefault:
  override def cliApp = CliApp.make(
    name = MorphirCliBuildInfo.product,
    version = MorphirCliBuildInfo.version,
    summary = text(MorphirCliBuildInfo.description),
    command = CliSetup.morphir
  ) {
    case CliCommand.Elm.Develop(port)       => printLine(s"Running elm develop on port $port")
    case CliCommand.Elm(args)               => printLine("Elm")
    case cmd @ CliCommand.Elm.Make(_, _, _) => printLine(s"Elm Make: $cmd")
    case CliCommand.Elm.Gen()               => printLine("Elm Gen")
    case CliCommand.Init()                  => printLine("Initializing...")
    case CliCommand.Setup                   => printLine("Setting up...")
    case CliCommand.Workspace()             => printLine("Workspace selected")
  }
