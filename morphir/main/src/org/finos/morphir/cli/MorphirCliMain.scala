package org.finos.morphir.cli

import java.nio.file.Paths
import zio.*
import zio.cli.*
import zio.cli.HelpDoc.Span.text
import org.finos.morphir.command.*
import CommandExecutor.given
object MorphirCliMain extends ZIOCliDefault {
  val cliApp = CliApp.make(
    name = "morphir-cli",
    version = "0.1.0",
    summary = text("Morphir CLI"),
    command = commands.Morphir.root
  ) { cmd =>
    for {
      _ <- Console.printLine(s"Executing command: $cmd")
      _ <- cmd.execute()
      _ <- Console.printLine("Command executed")
    } yield ()

  }

  object commands extends SetupCommandModule {

    object Elm {
      val init = Command("init").withHelp("Initialize for use with Morphir's Elm tooling.").map { _ =>
        MorphirCommand.ElmInit()
      }

      val make = {
        val projectDir  = Options.directory("project-dir").alias("p").withDefault(Paths.get("."))
        val output      = Options.directory("output").alias("o").withDefault(Paths.get("morphir-ir.json"))
        val fallbackCli = Options.boolean("fallback-cli").alias("f").withDefault(false)

        Command("make", projectDir ++ output ++ fallbackCli).withHelp(
          "Make a Morphir project that uses Elm as its front-end modelling language.."
        ).map { case (projectDir, output, fallbackCli) =>
          MorphirCommand.ElmMake(projectDir, output, fallbackCli)
        }
      }

      val root = Command("elm").withHelp("Elm specific commands for morphir-cli.").subcommands(init, make)
    }

    object Morphir {

      val setup = Command("setup").withHelp("Setup morphir-cli for use.").map { _ => MorphirCommand.Setup() }
      val root  = Command("morphir-cli").subcommands(setup, Elm.root)
    }
  }
}
