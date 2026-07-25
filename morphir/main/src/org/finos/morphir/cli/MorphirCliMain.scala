package org.finos.morphir.cli

import caseapp.*
import caseapp.core.RemainingArgs
import caseapp.core.app.{Command, CommandsEntryPoint}
import caseapp.core.help.{Help, RuntimeCommandHelp, RuntimeCommandsHelp}
import java.nio.file.{Path, Paths}
import kyo.*
import kyo.ZIOs
import org.finos.morphir.runtime.service.*
import org.finos.morphir.service.*
import org.finos.morphir.util.vfile.*
import zio.{BuildInfo => _, ZIO}

// ---------------------------------------------------------------------------
// Central ZIO dispatcher — services stay on ZIO; we bridge to Kyo at the edge
// ---------------------------------------------------------------------------

private def executeCommand(command: MorphirCommand): ZIO[Any, Throwable, Unit] = command match
  case MorphirCommand.Bundle(outputPath, irFiles) =>
    MorphirBundle.bundle(VPath(outputPath), irFiles.map(VPath(_))).provide(MorphirBundle.live)
  case MorphirCommand.Develop(port, host, projectDir, openInBrowser) =>
    MorphirElmDriver.develop(port, host, VPath(projectDir), openInBrowser).provide(MorphirElmDriver.live)
  case MorphirCommand.Library(outputDir, irFiles) =>
    MorphirBundle.library(VPath(outputDir), irFiles.map(VPath(_))).provide(MorphirBundle.live)
  case MorphirCommand.Setup(morphirHomeDir) =>
    MorphirSetup.setup(VPath(morphirHomeDir)).provide(MorphirSetup.live)
  case MorphirCommand.Test(_) =>
    MorphirRuntimeDriver.test().provide(MorphirRuntimeDriver.live)
  case MorphirCommand.ElmDevelop(port, host, projectDir, openInBrowser) =>
    MorphirElmDriver.develop(port, host, VPath(projectDir), openInBrowser).provide(MorphirElmDriver.live)
  case MorphirCommand.ElmInit(morphirHomeDir, projectDir) =>
    MorphirElmDriver.init(VPath(morphirHomeDir), VPath(projectDir)).provide(MorphirElmDriver.live)
  case MorphirCommand.ElmMake(projectDir, output, _, fallbackCli, _) =>
    MorphirElmDriver.make(VPath(projectDir), VPath(output), fallbackCli).provide(MorphirElmDriver.live).unit
  case MorphirCommand.ElmRestore(elmHome, projectDir) =>
    MorphirElmDriver.restore(VPath(elmHome), VPath(projectDir)).provide(MorphirElmDriver.live)
  case MorphirCommand.ElmTest(projectDir) =>
    MorphirElmDriver.test(VPath(projectDir)).provide(MorphirElmDriver.live)

// ---------------------------------------------------------------------------
// morphir-cli top-level commands
// ---------------------------------------------------------------------------

@AppName("Bundle Morphir IR models using the Morphir Runtime.")
final case class BundleOptions(
    @Name("o")
    @HelpMessage("Target file location where the Bundle Morphir IR file will be saved.")
    output: Path = Paths.get("morphir-ir.json")
)

object BundleCommand extends KyoCommand[BundleOptions]:
  override def name = "bundle"
  run { (opts: BundleOptions, remaining: RemainingArgs) =>
    val irFiles = remaining.remaining.map(Paths.get(_)).toList
    ZIOs.get(executeCommand(MorphirCommand.Bundle(opts.output, irFiles)))
  }

@AppName("Start up a web server and expose developer tools through a web UI.")
final case class DevelopOptions(
    @Name("p")
    @HelpMessage("Port to bind the web server to.")
    port: Int = 3000,
    @HelpMessage("Host to bind the web server to.")
    host: String = "localhost",
    @Name("i")
    @HelpMessage("Root directory of the project where morphir.json is located.")
    projectDir: Path = Paths.get("."),
    @Name("o")
    @HelpMessage("Open in browser.")
    openInBrowser: Boolean = false
)

object DevelopCommand extends KyoCommand[DevelopOptions]:
  override def name = "develop"
  run { (opts: DevelopOptions) =>
    ZIOs.get(executeCommand(MorphirCommand.Develop(opts.port, opts.host, opts.projectDir, opts.openInBrowser)))
  }

@AppName("Split Bundle Morphir IR model(s) into Library Morphir IR model(s).")
final case class LibraryOptions(
    @Name("o")
    @HelpMessage("Target directory where Library Morphir IR file(s) will be created.")
    output: Path = Paths.get(".")
)

object LibraryCommand extends KyoCommand[LibraryOptions]:
  override def name = "library"
  run { (opts: LibraryOptions, remaining: RemainingArgs) =>
    val irFiles = remaining.remaining.map(Paths.get(_)).toList
    ZIOs.get(executeCommand(MorphirCommand.Library(opts.output, irFiles)))
  }

@AppName("Setup morphir-cli for use.")
final case class SetupOptions()

object SetupCommand extends KyoCommand[SetupOptions]:
  override def name = "setup"
  run { (_: SetupOptions) =>
    ZIOs.get(executeCommand(MorphirCommand.Setup(Paths.get("~"))))
  }

@AppName("Test Morphir models using the Morphir Runtime.")
final case class TestOptions()

object TestCommand extends KyoCommand[TestOptions]:
  override def name = "test"
  run { (_: TestOptions, remaining: RemainingArgs) =>
    val irFiles = remaining.remaining.map(Paths.get(_)).toList
    ZIOs.get(executeCommand(MorphirCommand.Test(irFiles)))
  }

@AppName("Print the morphir-cli version.")
final case class VersionOptions()

object VersionCommand extends KyoCommand[VersionOptions]:
  override def name = "version"
  run { (_: VersionOptions) =>
    Console.printLine(BuildInfo.version)
  }

@AppName("Morphir Elm tooling.")
final case class ElmOptions()

object ElmCommand extends KyoCommand[ElmOptions]:
  override def names = List(List("elm"))

  override def helpAsked(progName: String, maybeOptions: Either[caseapp.core.Error, ElmOptions]): Nothing =
    val elmCommands = MorphirCliMain.commands
      .filter(_.names.exists(_.headOption.contains("elm")))
      .filterNot(_.names.exists(_ == List("elm")))
      .map(cmd => RuntimeCommandHelp(cmd.names, cmd.finalHelp, cmd.group, cmd.hidden))
    val dynamicHelp = RuntimeCommandsHelp(
      progName,
      Some("Access Morphir's Elm tooling."),
      Help[Unit](),
      elmCommands,
      None
    )
    println(dynamicHelp.help(helpFormat))
    exit(0)

  run { (_: ElmOptions) =>
    val elmCommands = MorphirCliMain.commands
      .filter(_.names.exists(_.headOption.contains("elm")))
      .filterNot(_.names.exists(_ == List("elm")))
      .map(cmd => RuntimeCommandHelp(cmd.names, cmd.finalHelp, cmd.group, cmd.hidden))
    val dynamicHelp = RuntimeCommandsHelp(
      "morphir-cli elm",
      Some("Access Morphir's Elm tooling."),
      Help[Unit](),
      elmCommands,
      None
    )
    Console.printLine(dynamicHelp.help(helpFormat))
  }

// ---------------------------------------------------------------------------
// elm sub-commands  (names = List(List("elm", "<sub>")) for nested dispatch)
// ---------------------------------------------------------------------------

@AppName("Start up a web server and expose developer tools through a web UI.")
final case class ElmDevelopOptions(
    @Name("p")
    @HelpMessage("Port to bind the web server to.")
    port: Int = 3000,
    @HelpMessage("Host to bind the web server to.")
    host: String = "localhost",
    @Name("i")
    @HelpMessage("Root directory of the project where morphir.json is located.")
    projectDir: Path = Paths.get("."),
    @Name("o")
    @HelpMessage("Open in browser.")
    openInBrowser: Boolean = false
)

object ElmDevelopCommand extends KyoCommand[ElmDevelopOptions]:
  override def names = List(List("elm", "develop"))
  run { (opts: ElmDevelopOptions) =>
    ZIOs.get(
      executeCommand(MorphirCommand.ElmDevelop(opts.port, opts.host, opts.projectDir, opts.openInBrowser))
    )
  }

@AppName("Initialize for use with Morphir's Elm tooling.")
final case class ElmInitOptions(
    @Name("p")
    @HelpMessage("Root directory of the project where morphir.json is located.")
    projectDir: Path = Paths.get(".")
)

object ElmInitCommand extends KyoCommand[ElmInitOptions]:
  override def names = List(List("elm", "init"))
  run { (opts: ElmInitOptions) =>
    ZIOs.get(executeCommand(MorphirCommand.ElmInit(Paths.get("~"), opts.projectDir)))
  }

@AppName("Translate Elm sources to Morphir IR.")
final case class ElmMakeOptions(
    @Name("p")
    @HelpMessage("Root directory of the project where morphir.json is located.")
    projectDir: Path = Paths.get("."),
    @Name("o")
    @HelpMessage("Target file location where the Morphir IR will be saved.")
    output: Path = Paths.get("morphir-ir.json"),
    @Name("t")
    @HelpMessage("Only include type information in the IR, no values.")
    typesOnly: Boolean = false,
    @Name("f")
    @HelpMessage("Use the old (non-incremental) CLI make function.")
    fallbackCli: Boolean = false,
    @Name("i")
    @HelpMessage("Use indentation in the generated JSON file.")
    indentJson: Boolean = false
)

object ElmMakeCommand extends KyoCommand[ElmMakeOptions]:
  override def names = List(List("elm", "make"))
  run { (opts: ElmMakeOptions) =>
    ZIOs.get(
      executeCommand(
        MorphirCommand.ElmMake(opts.projectDir, opts.output, opts.typesOnly, opts.fallbackCli, opts.indentJson)
      )
    )
  }

@AppName("Restore a Morphir project that uses Elm as its front-end modelling language.")
final case class ElmRestoreOptions(
    @Name("e")
    @HelpMessage("Path to the Elm home directory.")
    elmHome: Path = Paths.get("~/.elm"),
    @Name("p")
    @HelpMessage("Root directory of the project.")
    projectDir: Path = Paths.get(".")
)

object ElmRestoreCommand extends KyoCommand[ElmRestoreOptions]:
  override def names = List(List("elm", "restore"))
  run { (opts: ElmRestoreOptions) =>
    ZIOs.get(executeCommand(MorphirCommand.ElmRestore(opts.elmHome, opts.projectDir)))
  }

@AppName("Test Morphir models using morphir-elm.")
final case class ElmTestOptions(
    @Name("p")
    @HelpMessage("Root directory of the project where morphir.json is located.")
    projectDir: Path = Paths.get(".")
)

object ElmTestCommand extends KyoCommand[ElmTestOptions]:
  override def names = List(List("elm", "test"))
  run { (opts: ElmTestOptions) =>
    ZIOs.get(executeCommand(MorphirCommand.ElmTest(opts.projectDir)))
  }

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------

object MorphirCliMain extends CommandsEntryPoint:
  def progName                  = "morphir-cli"
  def commands: Seq[Command[?]] = Seq(
    BundleCommand,
    DevelopCommand,
    LibraryCommand,
    SetupCommand,
    TestCommand,
    VersionCommand,
    ElmCommand,
    ElmDevelopCommand,
    ElmInitCommand,
    ElmMakeCommand,
    ElmRestoreCommand,
    ElmTestCommand
  )
