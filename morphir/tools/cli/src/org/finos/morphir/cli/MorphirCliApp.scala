package org.finos.morphir.cli

import zio.Console.printLine
import zio.System.envs
import zio._
import zio.cli._
import zio.cli.BuiltInOption._
import zio.cli.HelpDoc.Span.{code, text}
import zio.cli.HelpDoc.{h1, p}
import zio.cli.completion.{Completion, CompletionScript}
import zio.cli.figlet.FigFont

import scala.annotation.tailrec
sealed trait MorphirCliApp[-R, +E, +Model]:
  def run(args: List[String]): ZIO[R, Nothing, ExitCode]

  def config(newConfig: CliConfig): MorphirCliApp[R, E, Model]

  def footer(newFooter: HelpDoc): MorphirCliApp[R, E, Model]

  def summary(s: HelpDoc.Span): MorphirCliApp[R, E, Model]

object MorphirCliApp:
  def make[R, E, Model](
      name: String,
      version: String,
      summary: HelpDoc.Span,
      command: Command[Model],
      footer: HelpDoc = HelpDoc.Empty,
      config: CliConfig = CliConfig.default,
      figFont: FigFont = FigFont.Default
  )(execute: Model => ZIO[R, E, Any]): MorphirCliApp[R, E, Model] =
    MorphirCliAppImpl(name, version, summary, command, execute, footer, config, figFont)

  private case class MorphirCliAppImpl[-R, +E, Model](
      name: String,
      version: String,
      summary: HelpDoc.Span,
      command: Command[Model],
      execute: Model => ZIO[R, E, Any],
      footer: HelpDoc = HelpDoc.Empty,
      config: CliConfig = CliConfig.default,
      figFont: FigFont = FigFont.Default
  ) extends MorphirCliApp[R, E, Model] { self =>
    def config(newConfig: CliConfig): MorphirCliApp[R, E, Model] = copy(config = newConfig)

    def footer(newFooter: HelpDoc): MorphirCliApp[R, E, Model] =
      copy(footer = self.footer + newFooter)

    def printDocs(helpDoc: HelpDoc): UIO[Unit] =
      printLine(helpDoc.toPlaintext(80)).!

    def run(args: List[String]): ZIO[R, Nothing, ExitCode] = {
      def executeBuiltIn(builtInOption: BuiltInOption): Task[Unit] =
        builtInOption match {
          case ShowHelp(synopsis, helpDoc) =>
            val fancyName = p(code(self.figFont.render(self.name)))

            val header = p(text(self.name) + text(" v") + text(self.version) + text(" -- ") + self.summary)

            val synopsisHelpDoc = h1("usage") + HelpDoc.p(text("$ ") + synopsis.helpDoc.getSpan)

            // TODO add rendering of built-in options such as help
            printLine((fancyName + header + synopsisHelpDoc + helpDoc + self.footer).toPlaintext(columnWidth = 300))

          case ShowCompletionScript(path, shellType) =>
            printLine(
              CompletionScript(path, if (self.command.names.nonEmpty) self.command.names else Set(self.name), shellType)
            )
          case ShowCompletions(index, _) =>
            envs.flatMap { envMap =>
              val compWords = envMap
                .collect {
                  case (idx, word) if idx.startsWith("COMP_WORD_") =>
                    (idx.drop("COMP_WORD_".length).toInt, word)
                }
                .toList
                .sortBy(_._1)
                .map(_._2)

              Completion
                .complete(compWords, index, self.command, self.config)
                .flatMap { completions =>
                  ZIO.foreachDiscard(completions)(word => printLine(word))
                }
            }
        }

      // prepend a first argument in case the CliApp's command is expected to consume it
      @tailrec
      def prefix(command: Command[_]): List[String] =
        command match {
          case Command.Single(name, _, _, _)  => List(name)
          case Command.Map(command, _)        => prefix(command)
          case Command.OrElse(_, _)           => Nil
          case Command.Subcommands(parent, _) => prefix(parent)
        }

      self.command
        .parse(prefix(self.command) ++ args, self.config)
        .foldZIO(
          e => printDocs(e.error),
          {
            case CommandDirective.UserDefined(_, value) => self.execute(value)
            case CommandDirective.BuiltIn(x)            => executeBuiltIn(x)
          }
        )
        .exitCode
    }

    def summary(s: HelpDoc.Span): MorphirCliApp[R, E, Model] =
      copy(summary = self.summary + s)
  } // Compare this snippet from morphir/tools/cli/src/org/finos/morphir/cli/CliSetup.scala:
end MorphirCliApp

trait MorphirCli extends ZIOApp {

  def cliApp: MorphirCliApp[Environment with ZIOAppArgs with Scope, Any, Any]

  override def run =
    for {
      args     <- ZIOAppArgs.getArgs
      exitCode <- cliApp.run(args.toList)
    } yield exitCode
}

trait MorphirCliDefault extends MorphirCli with ZIOAppDefault
