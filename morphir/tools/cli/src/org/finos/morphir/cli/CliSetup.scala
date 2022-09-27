package org.finos.morphir.cli

import java.nio.file.Paths
import zio.Console.printLine
import zio.cli.HelpDoc.Span.text
import zio.cli._
import zio.Scope
import zio.ZIOAppArgs

object CliSetup:
  val elmDevelop =
    val portOpt = Options.integer("port").alias("p").withDefault(BigInt(3000)) ?? "The port to run the server on."
    Command("develop", portOpt, Args.none).map { case port => CliCommand.Elm.Develop() }

  val elmGen =
    Command("gen").map { case _ => CliCommand.Elm.Gen() }

  val elmMake =
    val projectDirOpt = Options
      .directory("project-dir", Exists.Either)
      .withDefault(Paths.get("."))
      .alias("p") ?? """Root directory of the project where morphir.json is located. (default: ".")"""
    val outputOpt =
      Options
        .file("output", Exists.Either)
        .withDefault(Paths.get("morphir-ir.json"))
        .alias("o") ?? "The path in which to output the compiled IR file."

    val typesOnlyOpt = Options.boolean("types-only").alias("t") ?? "Only generate types"
    Command("make", projectDirOpt ++ outputOpt ++ typesOnlyOpt, Args.none)
      .withHelp("Compile a morphir-elm project into the Morphir IR.")
      .map { case (projectDir, output, typesOnly) =>
        CliCommand.Elm.Make(os.Path(projectDir), os.Path(output), typesOnly)
      }

  val elm =
    val help = HelpDoc.p("Access the morphir-elm cli and tooling.")
    Command("elm").withHelp(help).subcommands(elmMake, elmGen, elmDevelop)

  val init =
    val help = HelpDoc.p("Initialize a new Morphir workspace.")
    Command("init", Options.none, Args.none).withHelp(help).map { _ =>
      CliCommand.Init()
    }

  val setup =
    val help = HelpDoc.p("Setup Morphir tooling.")
    Command("setup", Options.none, Args.none).withHelp(help).map { _ =>
      CliCommand.Setup
    }

  val workspace =
    val help = HelpDoc.p("Configure and get information about the Morphir workspace.")
    Command("workspace", Options.none, Args.none).withHelp(help).map { _ =>
      CliCommand.Workspace()
    }

  val morphir: Command[CliCommand] =
    Command("morphir", Options.none, Args.none).subcommands(elm, init, setup, workspace)
