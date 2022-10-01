package org.finos.morphir.cli
package cmdlet
import zio._
import zio.Console.printLine
import zio.process.Command

object WorkspaceCmdlet extends Cmdlet[CliCommand.Workspace, Exception] {
  def run(args: CliCommand.Workspace) =
    for {
      _ <- printLine(s"Workspace: $args")
    } yield ()
}
