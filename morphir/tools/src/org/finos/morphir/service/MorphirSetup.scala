package org.finos.morphir.service

import org.finos.morphir.util.vfile._
import zio._

trait MorphirSetup {
  def setup(morphirHomeDir: VFilePath): zio.Task[Unit]
}

object MorphirSetup {
  val live: ULayer[MorphirSetup] = ZLayer.succeed(MorphirSetupLive)

  object MorphirSetupLive extends MorphirSetup {
    def setup(morphirHomeDir: VFilePath): zio.Task[Unit] =
      for {
        _ <- Console.printLine("Setup command executing")
        _ <- Console.printLine(s"\tmorphirHomeDir: $morphirHomeDir")
        _ <- Console.printLine("Setup command executed")
      } yield ()
  }

  def setup(morphirHomeDir: VFilePath): zio.ZIO[MorphirSetup, Throwable, Unit] =
    ZIO.serviceWithZIO[MorphirSetup](_.setup(morphirHomeDir))
}
