package org.finos.morphir.cli

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
    command = cliCommand.morphir
  ) { case _ => printLine("Hello World!") }


  object cliCommand:
    import org.finos.morphir.cli.{commands => Cmd}
    object elm:
      def apply()      = command
      lazy val command = Command("elm", Options.none, Args.none).subcommands(make)
      lazy val make    = Command("make", Options.none, Args.none).map(_ => Cmd.Elm.Make)

    object workspace:
      val helpDoc      = HelpDoc.p("Init and get information about the current workspace")
      lazy val command = Command("workspace").withHelp(helpDoc).subcommands(init)

      val initHelp: HelpDoc = HelpDoc.p("Initialise a new Morphir project")
      lazy val init         = Command("init").withHelp(initHelp).map(_ => Cmd.Workspace.Init)
      def apply()           = command

    val morphir: Command[Cmd.MorphirSubcommand] =
      Command("morphir", Options.none, Args.none).subcommands(elm(), workspace())
