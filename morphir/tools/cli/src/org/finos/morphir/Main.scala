package org.finos.morphir.cli

import zio.Console.printLine
import zio.cli.HelpDoc.Span.text
import zio.cli._
import zio.Scope
import zio.ZIOAppArgs

object Main extends ZIOCliDefault:
  enum CliCommandData:
    case Init()
    case Workspace()

  override def cliApp = CliApp.make(
    name = MorphirCliBuildInfo.product,
    version = MorphirCliBuildInfo.version,
    summary = text(MorphirCliBuildInfo.description),
    command = cliCommand.morphir
  ) {
    case CliCommandData.Init()      => printLine("Initializing...")
    case CliCommandData.Workspace() => printLine("Workspace selected")
  }

  object cliCommand:
    val workspace =
      val help = HelpDoc.p("Configure and get information about the Morphir workspace.")
      Command("workspace", Options.none, Args.none).withHelp(help).map { _ =>
        CliCommandData.Workspace()
      }

    val init =
      val help = HelpDoc.p("Initialize a new Morphir workspace.")
      Command("init", Options.none, Args.none).withHelp(help).map { _ =>
        CliCommandData.Init()
      }

    val morphir: Command[CliCommandData] = Command("morphir", Options.none, Args.none).subcommands(workspace, init)
