package org.finos.morphir.command

import zio._
import java.nio.file.Path

trait MorphirElmDriver {
  def init(projectDir: Path): Task[Unit]
  def make(projectDir: Path, output: Path, fallbackCli: Boolean = false): Task[Seq[Path]]
}

object MorphirElmDriver {

  object MorphirElmDriverLive extends MorphirElmDriver {
    def init(projectDir: Path): Task[Unit] = ZIO.fail(new Exception("Not implemented"))
    def make(projectDir: Path, output: Path, fallbackCli: Boolean = false): Task[Seq[Path]] = for {
      _ <- Console.printLine("Elm make command executed")
      _ <- Console.printLine(s"\tprojectDir: $projectDir")
      _ <- Console.printLine(s"\toutput: $output")
      _ <- Console.printLine(s"\tfallbackCli: $fallbackCli")
      _ <- Console.printLine("Elm make command executed")
    } yield Seq.empty
  }

  def make(projectDir: Path, output: Path, fallbackCli: Boolean = false) =
    ZIO.serviceWithZIO[MorphirElmDriver](_.make(projectDir, output, fallbackCli))
}
