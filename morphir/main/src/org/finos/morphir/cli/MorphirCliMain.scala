package org.finos.morphir.cli

import java.nio.file.{Path => JPath, Paths}
import org.finos.morphir.service._
import org.finos.morphir.runtime.service._
import org.finos.morphir.util.vfile._
import zio.{BuildInfo => _, _}
import zio.cli._
import zio.cli.HelpDoc.Span.text
object MorphirCliMain extends ZIOCliDefault {
  val cliApp = CliApp.make(
    name = "morphir-cli",
    version = BuildInfo.version,
    summary = text("Morphir CLI"),
    command = commands.Morphir.root
  )(executeCommand(_).provide(
    MorphirBundle.live,
    MorphirSetup.live,
    MorphirElmDriver.live,
    MorphirRuntimeDriver.live
  ))

  private def executeCommand(command: MorphirCommand) = command match {
    case MorphirCommand.Bundle(outputBundleIRFilePath, irFiles) =>
      MorphirBundle.bundle(VPath(outputBundleIRFilePath), irFiles.map(VPath(_)))
    case MorphirCommand.Develop(port, host, projectDir, openInBrowser) =>
      MorphirElmDriver.develop(port, host, VPath(projectDir), openInBrowser)
    case MorphirCommand.Setup(morphirHomeDir) => MorphirSetup.setup(VPath(morphirHomeDir))
    case MorphirCommand.Test(irFiles)         => MorphirRuntimeDriver.test()
    case MorphirCommand.ElmDevelop(port, host, projectDir, openInBrowser) =>
      MorphirElmDriver.develop(port, host, VPath(projectDir), openInBrowser)
    case MorphirCommand.ElmInit(morphirHomeDir, projectDir) =>
      MorphirElmDriver.init(VPath(morphirHomeDir), VPath(projectDir))
    case MorphirCommand.ElmMake(projectDir, output, typesOnly, fallbackCli, indentJson) =>
      MorphirElmDriver.make(VPath(projectDir), VPath(output), fallbackCli)
    case MorphirCommand.ElmRestore(elmHome, projectDir) =>
      MorphirElmDriver.restore(VPath(elmHome), VPath(projectDir))
    case MorphirCommand.ElmTest(projectDir) => MorphirElmDriver.test(VPath(projectDir))
  }

  object commands {

    object Elm {

      val develop = {
        val port = Options.integer("port").alias("p").withDefault(BigInt(3000)).map(
          _.intValue
        ) ?? "Port to bind the web server to."
        val host = Options.text("host").alias("h").withDefault("localhost") ?? "Host to bind the web server to."
        val projectDir = Options.directory("project-dir").alias("i").withDefault(
          Paths.get(".")
        ) ?? "Root directory of the project where morphir.json is located."
        val openInBrowser = Options.boolean("open-in-browser").alias("o") ?? "Open in browser."

        Command("develop", port ++ host ++ projectDir ++ openInBrowser).withHelp(
          "Start up a web server and expose developer tools through a web UI."
        ).map { case (port, host, projectDir, openInBrowser) =>
          MorphirCommand.ElmDevelop(port, host, projectDir, openInBrowser)
        }
      }

      val init = {
        val projectDir = Options.directory("project-dir").alias("p").withDefault(Paths.get("."))

        Command("init", projectDir).withHelp("Initialize for use with Morphir's Elm tooling.").map { projectDir =>
          MorphirCommand.ElmInit(Paths.get("~"), projectDir)
        }
      }

      val make = {
        val projectDir = Options.directory("project-dir").alias("p").withDefault(
          Paths.get(".")
        ) ?? "Root directory of the project where morphir.json is located."
        val output = Options.file("output").alias("o").withDefault(
          Paths.get("morphir-ir.json")
        ) ?? "Target file location where the Morphir IR will be saved."
        val typesOnly =
          Options.boolean("types-only").alias("t") ?? "Only include type information in the IR, no values."
        val fallbackCli =
          Options.boolean("fallback-cli").alias("f") ?? "Use the old (non-incremental) CLI make function."
        val indentJson = Options.boolean("indent-json").alias("i") ?? "Use indentation in the generated JSON file."

        Command("make", projectDir ++ output ++ typesOnly ++ fallbackCli ++ indentJson).withHelp(
          "Translate Elm sources to Morphir IR."
        ).map { case (projectDir, output, typesOnly, fallbackCli, indentJson) =>
          MorphirCommand.ElmMake(projectDir, output, typesOnly, fallbackCli, indentJson)
        }
      }

      val restore = {
        val elmHome    = Options.directory("elm-home").alias("e").withDefault(Paths.get("~/.elm"))
        val projectDir = Options.directory("project-dir").alias("p").withDefault(Paths.get("."))
        Command("restore", elmHome ++ projectDir).withHelp(
          "Restore a Morphir project that uses Elm as its front-end modelling language.."
        ).map { case (elmHome, projectDir) =>
          MorphirCommand.ElmRestore(elmHome, projectDir)
        }
      }

      lazy val root =
        Command("elm").withHelp("Elm specific commands for morphir-cli.").subcommands(
          develop,
          init,
          make,
          restore,
          test
        )

      val test = {
        val projectDir = Options.directory("project-dir").alias("p").withDefault(
          Paths.get(".")
        ) ?? "Root directory of the project where morphir.json is located."

        Command("test", projectDir).withHelp("Test Morphir models using morphir-elm.").map { projectDir =>
          MorphirCommand.ElmTest(projectDir)
        }
      }
    }

    object Morphir {

      val bundle = {
        val irFiles = Args.file("ir-files").repeat
        val outputBundleIRFilePath = Options.file("output").alias("o").withDefault(
          Paths.get("morphir-ir.json")
        ) ?? "Target file location where the Morphir Bundle IR file will be saved."

        Command("bundle", outputBundleIRFilePath, irFiles).withHelp(
          "Bundle Morphir IR models using the Morphir Runtime."
        ).map { irFiles =>
          MorphirCommand.Bundle(outputBundleIRFilePath, irFiles)
        }
      }

      val develop = {
        val port = Options.integer("port").alias("p").withDefault(BigInt(3000)).map(
          _.intValue
        ) ?? "Port to bind the web server to."
        val host = Options.text("host").alias("h").withDefault("localhost") ?? "Host to bind the web server to."
        val projectDir = Options.directory("project-dir").alias("i").withDefault(
          Paths.get(".")
        ) ?? "Root directory of the project where morphir.json is located."
        val openInBrowser = Options.boolean("open-in-browser").alias("o") ?? "Open in browser."

        Command("develop", port ++ host ++ projectDir ++ openInBrowser).withHelp(
          "Start up a web server and expose developer tools through a web UI."
        ).map { case (port, host, projectDir, openInBrowser) =>
          MorphirCommand.Develop(port, host, projectDir, openInBrowser)
        }
      }

      val setup = Command("setup").withHelp("Setup morphir-cli for use.").map { _ =>
        val morphirHomeDir = Paths.get("~")
        MorphirCommand.Setup(morphirHomeDir)
      }

      def root = Command("morphir-cli").subcommands(develop, Elm.root, setup, test)

      val test = {
        val irFiles = Args.file("ir-files").repeat
        Command("test", irFiles).withHelp("Test Morphir models using the Morphir Runtime.").map { irFiles =>
          MorphirCommand.Test(irFiles)
        }
      }
    }
  }
}
