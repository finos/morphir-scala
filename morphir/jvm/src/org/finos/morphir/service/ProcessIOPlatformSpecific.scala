package org.finos.morphir.service

import zio._
import zio.process._

trait ProcessIOPlatformSpecific {
  val live: ULayer[ProcessIO] = ZLayer.succeed(ProcessIOLive)

  object ProcessIOLive extends ProcessIO {
    def exec(command: String, args: String*)(implicit trace: Trace): Task[ExitCode] =
      Command(command, args: _*).inheritIO.exitCode
  }
}
