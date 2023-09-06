package org.finos.morphir.cli

import java.nio.file.*

import org.finos.morphir.service.*
import org.finos.morphir.runtime.service.*
import org.finos.morphir.util.vfile.*
import zio.{BuildInfo => _, _}
import zio.cli.*
import zio.cli.HelpDoc.Span.text
object MorphirCliMain extends ZIOCliDefault {
  val cliApp = CliApp.make(
    name = "morphir-cli",
    version = BuildInfo.version,
    summary = text("Morphir CLI"),
    command = commands.Morphir.root
  )(executeCommand(_).provide(MorphirSetup.live, MorphirElmDriver.live, MorphirRuntimeDriver.live))

  private def executeCommand(command: MorphirCommand) = command match {
    case MorphirCommand.Develop(port, host, projectDir) =>
      MorphirElmDriver.develop(port, host, VFilePath.fromJava(projectDir))
    case MorphirCommand.Setup(morphirHomeDir) => MorphirSetup.setup(morphirHomeDir)
    case MorphirCommand.Test(irFiles)         => MorphirRuntimeDriver.test()
    case MorphirCommand.ElmInit(morphirHomeDir, projectDir) =>
      MorphirElmDriver.init(VFilePath.fromJava(morphirHomeDir), VFilePath.fromJava(projectDir))
    case MorphirCommand.ElmMake(projectDir, output, fallbackCli) =>
      MorphirElmDriver.make(VFilePath.fromJava(projectDir), VFilePath.fromJava(output), fallbackCli)
    case MorphirCommand.ElmRestore(elmHome, projectDir) =>
      MorphirElmDriver.restore(VFilePath.fromJava(elmHome), VFilePath.fromJava(projectDir))
  }

  object commands {

    object Elm {
      val init = {
        val projectDir = Options.directory("project-dir").alias("p").withDefault(Paths.get("."))

        Command("init", projectDir).withHelp("Initialize for use with Morphir's Elm tooling.").map { projectDir =>
          MorphirCommand.ElmInit(Paths.get("~"), projectDir)
        }
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

      val restore = {
        val elmHome    = Options.directory("elm-home").alias("e").withDefault(Paths.get("~/.elm"))
        val projectDir = Options.directory("project-dir").alias("p").withDefault(Paths.get("."))
        Command("restore", elmHome ++ projectDir).withHelp(
          "Restore a Morphir project that uses Elm as its front-end modelling language.."
        ).map { case (elmHome, projectDir) =>
          MorphirCommand.ElmRestore(elmHome, projectDir)
        }
      }

      val root =
        Command("elm").withHelp("Elm specific commands for morphir-cli.").subcommands(init, make, restore)

    }

    object Morphir {

      val develop = {
        val port       = Options.integer("port").alias("p").withDefault(BigInt(3000)).map(_.intValue)
        val host       = Options.text("host").alias("h").withDefault("localhost")
        val projectDir = Options.directory("project-dir").alias("i").withDefault(Paths.get("."))

        Command("develop", port ++ host ++ projectDir).withHelp(
          "Start up a web server and expose developer tools through a web UI."
        ).map { case (port, host, projectDir) =>
          MorphirCommand.Develop(port, host, projectDir)
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
