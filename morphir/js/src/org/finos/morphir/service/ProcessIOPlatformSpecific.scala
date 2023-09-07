package org.finos.morphir.service

import zio._

trait ProcessIOPlatformSpecific {
  val live: ULayer[ProcessIO] = ZLayer.succeed(ProcessIOLive)

  object ProcessIOLive extends ProcessIO { self =>
    def exec(command: String, args: String*)(implicit trace: Trace): Task[ExitCode] =
      ZIO.fail(new Exception(s"${self.getClass().getSimpleName()}::exec Not implemented"))
  }
}
