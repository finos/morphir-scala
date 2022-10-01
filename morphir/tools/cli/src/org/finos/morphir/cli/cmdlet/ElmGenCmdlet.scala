package org.finos.morphir.cli
package cmdlet
import zio._
import zio.Console.printLine
import zio.process.Command

object ElmGenCmdlet extends Cmdlet[CliCommand.Elm.Gen, Exception] {
  def run(args: CliCommand.Elm.Gen) = {
    val cmd = Command("morphir-elm", "gen").inheritIO
    for {
      _   <- printLine(s"Elm Gen: $args")
      res <- cmd.linesStream.runDrain
      _   <- printLine(res)
    } yield ()
  }
}
