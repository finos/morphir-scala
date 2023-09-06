package org.finos.morphir.service

import org.finos.morphir.util.vfile._
import zio._

trait MorphirElmDriverPlatformSpecific {
  val live: ULayer[MorphirElmDriver] = ZLayer.succeed(MorphirElmDriverLive)

  object MorphirElmDriverLive extends MorphirElmDriver {
    def init(morphirHomeDir: VFilePath, projectDir: VFilePath): Task[Unit] = for {
      _ <- Console.printLine("Elm init command executing")
      _ <- Console.printLine(s"\tmorphirHomeDir: $morphirHomeDir")
      _ <- Console.printLine(s"\tprojectDir: $projectDir")
      _ <- Console.printLine("Elm init command executed")
    } yield ()

    def make(projectDir: VFilePath, output: VFilePath, fallbackCli: Boolean = false): Task[Seq[VFile]] = for {
      _ <- Console.printLine("Elm make command executed")
      _ <- Console.printLine(s"\tprojectDir: $projectDir")
      _ <- Console.printLine(s"\toutput: $output")
      _ <- Console.printLine(s"\tfallbackCli: $fallbackCli")
      _ <- Console.printLine("Elm make command executed")
    } yield Seq.empty

    def restore(elmHome: VFilePath, projectDir: VFilePath): Task[Unit] =
      for {
        _ <- Console.printLine("Elm restore command executed")
        _ <- Console.printLine(s"\telmHome: $elmHome")
        _ <- Console.printLine(s"\tprojectDir: $projectDir")
        _ <- Console.printLine("Elm restore command executed")
      } yield ()
  }
}
