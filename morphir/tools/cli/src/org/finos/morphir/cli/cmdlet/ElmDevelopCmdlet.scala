package org.finos.morphir.cli
package cmdlet
import zio._
import zio.Console.printLine
import zio.process.Command

object ElmDevelopCmdlet extends Cmdlet[CliCommand.Elm.Develop, Exception] {
  def run(args: CliCommand.Elm.Develop) = {
    val cmd = Command("morphir-elm", "develop", "-p", args.port.toString).inheritIO
    for {
      _   <- printLine(s"Elm Develop: $args")
      _   <- printLine(s"Running elm develop on port ${args.port}")
      res <- cmd.linesStream.runDrain
      _   <- printLine(res)
    } yield ()
  }
}
