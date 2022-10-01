package org.finos.morphir.cli
package cmdlet

import zio._
import zio.Console.printLine
import zio.process.Command

object ElmCmdlet extends Cmdlet[CliCommand.Elm, Exception] {
  def run(args: CliCommand.Elm) = {
    val cmd = Command("morphir-elm").inheritIO
    for {
      _   <- printLine(s"Elm: $args")
      res <- cmd.linesStream.runDrain
      _   <- printLine(res)
    } yield ()
  }
}
