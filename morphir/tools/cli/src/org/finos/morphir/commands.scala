package org.finos.morphir.cli

import zio.Console.printLine
import zio.cli.HelpDoc.Span.text
import zio.cli._
import zio.Scope
import zio.ZIOAppArgs

object commands:
  sealed trait MorphirSubcommand    extends Product with Serializable
  sealed trait MorphirElmSubcommand extends MorphirSubcommand
  sealed trait WorkspaceSubCommand  extends MorphirSubcommand

  case object Elm extends MorphirSubcommand:
    case object Make extends MorphirElmSubcommand

  case object Workspace:
    case object Init extends WorkspaceSubCommand
