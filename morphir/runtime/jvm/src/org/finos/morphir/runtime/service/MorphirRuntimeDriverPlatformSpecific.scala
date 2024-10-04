package org.finos.morphir.runtime.service
import org.finos.morphir.datamodel.Data
import org.finos.morphir.naming
import org.finos.morphir.runtime._
import zio.*

trait MorphirRuntimeDriverPlatformSpecific {
  val live: ULayer[MorphirRuntimeDriver] = ZLayer.succeed(MorphirRuntimeDriverLive)

  object MorphirRuntimeDriverLive extends MorphirRuntimeDriver {
    override def evaluate(entryPoint: naming.FQName, param: Data, params: Data*): IO[MorphirRuntimeError, Data] =
      ZIO.fail(NotImplemented("The evaluate method on MorphirRuntimeLive is not implemented yet"))

    def test(): Task[Unit] =
      for {
        _ <- Console.printLine("MorphirRuntimeDriver test command executing")
        _ <- Console.printLine("MorphirRuntimeDriver test command executed")
      } yield ()

  }
}
