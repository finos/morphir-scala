package org.finos.morphir.cli
package cmdlet
import zio._
import zio.Console.printLine
import zio.process.Command

object SetupCmdlet extends Cmdlet[CliCommand.Setup, Exception] {
  def run(args: CliCommand.Setup) =
    for {
      _ <- printLine(s"Setting up: $args")
    } yield ()
}
