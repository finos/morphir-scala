package org.finos.morphir.runtime.service

import zio._

trait MorphirRuntimeDriver {
  def test(): Task[Unit]
}

object MorphirRuntimeDriver {
  val live: ULayer[MorphirRuntimeDriver] = ZLayer.succeed(MorphirRuntimeDriverLive)

  object MorphirRuntimeDriverLive extends MorphirRuntimeDriver {
    def test(): Task[Unit] =
      for {
        _ <- Console.printLine("MorphirRuntimeDriver test command executing")
        _ <- Console.printLine("MorphirRuntimeDriver test command executed")
      } yield ()
  }

  def test(): ZIO[MorphirRuntimeDriver, Throwable, Unit] =
    ZIO.serviceWithZIO[MorphirRuntimeDriver](_.test())
}
