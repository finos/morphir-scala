package org.finos.morphir.cli
package cmdlet

import zio._
import zio.Console.printLine
import zio.process.Command

object AboutCmdlet extends Cmdlet[CliCommand.About.type, Exception] {
  def run(args: CliCommand.About.type) =
    for {
      _ <- printLine(s"About: $args")
    } yield ()
}
