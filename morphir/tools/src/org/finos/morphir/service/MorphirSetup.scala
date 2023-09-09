package org.finos.morphir.service
import zio._
import fs2.io.file.{Files, Path}

trait MorphirSetup {
  def setup(morphirHomeDir: Path): zio.Task[Unit]
}

object MorphirSetup {
  val live: ULayer[MorphirSetup] = ZLayer.succeed(MorphirSetupLive)

  object MorphirSetupLive extends MorphirSetup {
    def setup(morphirHomeDir: Path): zio.Task[Unit] =
      for {
        _ <- Console.printLine("Setup command executing")
        _ <- Console.printLine(s"\tmorphirHomeDir: $morphirHomeDir")
        _ <- Console.printLine("Setup command executed")
      } yield ()
  }

  def setup(morphirHomeDir: Path): zio.ZIO[MorphirSetup, Throwable, Unit] =
    ZIO.serviceWithZIO[MorphirSetup](_.setup(morphirHomeDir))
}
