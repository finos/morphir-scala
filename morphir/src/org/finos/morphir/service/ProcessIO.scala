package org.finos.morphir.service

import zio._

trait ProcessIO {
  def exec(command: String, args: String*): Task[ExitCode]
}

object ProcessIO extends ProcessIOPlatformSpecific {
  def exec(command: String, args: String*): ZIO[ProcessIO, Throwable, ExitCode] =
    ZIO.serviceWithZIO[ProcessIO](_.exec(command, args: _*))
}
