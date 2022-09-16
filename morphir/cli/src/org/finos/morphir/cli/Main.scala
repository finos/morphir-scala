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
    command = ??? // commands.morphir
  ) { case _ => printLine("Hello World!") }

// object commands:
//   sealed trait MorphirSubcommand
//   sealed trait MorphirElmSubcommand extends MorphirSubcommand
//   sealed trait WorkspaceSubCommand extends MorphirSubcommand

//   case object Elm extends MorphirSubcommand:
//     case object Make extends MorphirElmSubcommand

//   case object Workspace:
//     case object Init extends Workspace

//   sealed trait CommandData
//   object CommandData:
//     case object  Workspace() extends CommandData
//     case object Elm extends CommandData

//   val morphir = Command("morphir")

//   object elm:
//     def apply()      = command
//     lazy val command = Command("elm").subcommands(make)
//     lazy val make    = Command("make").map(_ => ElmSubCommand.Make)

//   object workspace:
//     def apply()      = command
//     lazy val command = Command("workspace").subcommands(init)
//     lazy val init    = Command("init").withHelp(initHelp).map(_ => WorkspaceSubCommand.Init)

//   val initHelp: HelpDoc = HelpDoc.p("Initialise a new Morphir project")

//   val morphir = Command("morphir").subcommands(elm(), workspace())
