package org.finos.morphir.runtime.service
import zio._

trait MorphirRuntimeDriverPlatformSpecific {
  val live: ULayer[MorphirRuntimeDriver] = ZLayer.succeed(MorphirRuntimeDriverLive)

  object MorphirRuntimeDriverLive extends MorphirRuntimeDriver {
    def test(): Task[Unit] =
      for {
        _ <- Console.printLine("MorphirRuntimeDriver test command executing")
        _ <- Console.printLine("MorphirRuntimeDriver test command executed")
      } yield ()
  }
}
