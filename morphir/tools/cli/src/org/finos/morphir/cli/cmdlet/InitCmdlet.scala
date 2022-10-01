package org.finos.morphir.cli
package cmdlet

import zio._
import zio.Console.printLine
import zio.process.Command

object InitCmdlet extends Cmdlet[CliCommand.Init, Exception] {
  def run(args: CliCommand.Init) =
    for {
      _ <- printLine(s"Initializing: $args")
    } yield ()
}
