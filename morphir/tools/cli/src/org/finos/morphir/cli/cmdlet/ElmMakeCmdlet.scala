package org.finos.morphir.cli
package cmdlet
import zio._
import zio.Console.printLine
import zio.process.Command

object ElmMakeCmdlet extends Cmdlet[CliCommand.Elm.Make, Exception] {

  def run(args: CliCommand.Elm.Make) = {
    val cmd = Command("morphir-elm", "make", "-p", args.projectDir.toString).inheritIO
    for {
      _   <- printLine(s"Elm Make: $args")
      res <- cmd.linesStream.runDrain
      _   <- printLine(res)
    } yield ()
  }

}
